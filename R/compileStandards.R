#' compileStandards
#'
#' Compile alignment info from a lesson standards matrix worksheet
#' @param standardsFile file location of the lesson standards alignment XLSX worksheet; default: "meta/standards_GSheetsOnly.xlsx"
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder, 1 level up from the working directory
#' @param fileName output file name; default= "standards.json"
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @param standardsRef where do you want to pull down statements and other info for the supplied standards codes? Default="standardX" (i.e. the \href{https://github.com/galacticpolymath/standardX}{standardX repository}); Options= "standardX" or "myFile" (which will use Tab 2 on the supplied XLSX workbook)
#' @return list of the compiled standards data with 3 objects: $input (the input file as a tibble); $compiled (the compiled tibble); $problem_entries (a tibble of entries with 'TBD' or missing values in the "How this aligns..." colum). A JSON is saved to the destFolder location.
#' @export
#'
compileStandards <- function(standardsFile = "meta/standards_GSheetsOnly.xlsx",
                             destFolder = "meta/JSON/" ,
                             fileName = "standards.json",
                             standardsRef = "standardX",
                             WD= getwd()) {


   .=NULL #to avoid errors with dplyr syntax

   #if WD supplied, append it to destFolder
if(!identical(WD,getwd())){
  destFolder<-paste0(WD,destFolder)
  standardsFile<-paste0(WD,standardsFile)}

 # Import XLSX files -------------------------------------------------------
#Import master alignment with ALL standards from https://github.com/galacticpolymath/standardX or the supplied standardsFile

  if(standardsRef=="standardX"){
    a_master <-
      openxlsx::read.xlsx(
        "https://raw.githubusercontent.com/galacticpolymath/standardX/gh-pages/align-to-all-subject-standards.xlsx",
        sheet = 1,
        colNames = T
      )
  } else if (standardsRef == "myFile") {
    a_master <- openxlsx::read.xlsx(standardsFile, sheet = 2,colNames=TRUE,startRow=2)
  } else{
    stop("standardsRef must be 'standardX' or 'myFile'")
  }


# #initialize list for output
# output=list() #initialize list
# in_out=list()#initialize list for tracking n standards codes input

# import alignment --------------------------------------------------------

a0<-openxlsx::read.xlsx(standardsFile,sheet=4,colNames=TRUE,startRow=2)
#a0 is original standardsFile without blank rows
a0<-a0[which(a0$Code!=""),]

#rename so easier to deal with
names(a0)[1:10] <- c("code","statement","subject","grade","lg","lg_stmnt","set","target","grp", "how")


# manage TBDs and flagged, undocumented alignments ------------------------
tbds<-grepl("tbd",a0$how,ignore.case=TRUE)
#a1 does not have records with lg_statements containing "TBD" or no entry for "how"
if(sum(tbds)>0) {
  message(
    "\nThe following were removed because Learning Goal documentation contained 'TBD':\n\t\u2022",
    paste0(a0$code[tbds], collapse = "\n\t\u2022"),
           "\n"
  )
}

#undocumented alignments
undoc<-is.na(a0$how)
if(sum(undoc)>0) {
  message(
    "\nThe following were removed because learning goal documentation was blank:\n\t\u2022",
    paste0(a0$code[undoc], collapse = "\n\t\u2022"),
    "\n"
  )
}
a1<-a0[!undoc&!tbds,]

# a2 has markdown bullets ("- ") added if missing
# Add markdown bullet to front of lines that don't start with it
# Figure out grade band(s)
a2<-a1
a2$how<-ifelse(!grepl("^- ",a1$how),paste0("- ",a1$how),a1$how)


# #///////////////////////////
# # Output Integrity Check 1: verify that codes have been entered for every lesson that has alignment notes
# n_code_entries<-nrow(alignment_matrix_stacked)
# n_alignmentNote_entries<-nrow(alignment_matrix_stacked0 %>% dplyr::filter(.data$AlignmentNotes!=""))
# test_code_v_alignment<-n_code_entries==n_alignmentNote_entries
# msg_code_v_alignment<-ifelse(test_code_v_alignment,"TEST 1 PASS","TEST 1 FAIL: Make sure you have codes listed for every set of alignment notes.")
# cat("\n",paste0(rep("-",30),collapse=""),"\n  Integrity Check 1\n",paste0(rep("-",30),collapse=""),"\n  N Alignment Entries: \t",
#     n_alignmentNote_entries,"\n  N Code Entries: \t",n_code_entries,
#     "\n\n")
# message(msg_code_v_alignment,"\n")


# interpret target values -------------------------------------------------
# a3 has logical values for target instead of "n" and blank
# also added grouping variable
a3<- a2
a3$target<-ifelse(is.na(a3$target)|tolower(a3$target)=="n",FALSE,TRUE)
a3$grouping <-
  sapply(1:nrow(a3), function(i) {
    ifelse(is.na(a3$grp[i]),
           paste0("singlet", "_", i),
           paste0("group", "_", a3$grp[i]))
  })


# Match alignment to master records ---------------------------------------
a3$code_set<-paste(a3$code,a3$set,sep="_")
a_master$code_set<-paste(a_master$code,a_master$set,sep="_")
#A is a merge of the provided alignment and the master reference document (with preference fo code defs, etc. from the provided standardsRef)
A<-dplyr::left_join(a3[,c("code_set","lg","lg_stmnt","target","grp","grouping","how")],a_master,by="code_set")
#factor subjects for desired order
A$subject<-factor(A$subject,levels=c("Math","ELA","Science","Social Studies"),ordered=T)
A <- A %>% dplyr::arrange(.data$subject)


# Add grade band to final data set ----------------------------------------
gradeBandBreaks<-list(5:6,7:8,9:12)
gradeBandTxt<-sapply(gradeBandBreaks,function(x) paste0(x[1],"-",x[length(x)]))
gradeL<-sapply(A$grade, function(x) {
  #Ignore K-12 wide standards for assigning grade bands
  if (grepl("K", x, ignore.case = TRUE)) {
    NA
  } else{
    grades <- unlist(strsplit(x, ",", fixed = T))
    bands<-sapply(grades, function(g_i) {
      hits = unlist(sapply(gradeBandBreaks, FUN=function(brk) {
        g_i %in% brk
      }))
      gradeBandTxt[which(hits)]
    })
    unique(bands)
  }
})


# Define grade bands this alignment touches ------------

A$gradeBand<-sapply(gradeL,function(x) paste(x,collapse=","))%>% unlist()



# Make json structured output ----------------------------------------------
# ta_i=su_i=se_i=di_i=NA
l_ta <- list()
for(ta_i in 1:length(unique(A$target))) {
  d_ta <- A %>% dplyr::filter(.data$target == unique(A$target)[ta_i])
  l_su<-list()
  for (su_i in 1:length(unique(d_ta$subject))) {
    d_su <- d_ta %>% dplyr::filter(.data$subject == unique(d_ta$subject)[su_i])
    l_se <- list()
    for (se_i in 1:length(unique(d_su$set))) {
      d_se <- d_su %>% dplyr::filter(.data$set == unique(d_su$set)[se_i])
      l_di <- list()
      for (di_i in 1:length(unique(d_se$dimension))) {
        d_di <-
          d_su %>% dplyr::filter(.data$dimension == unique(d_se$dimension)[di_i])
        l_gr <- lapply(unique(d_di$grouping), function(gr) {
          d_gr <- d_di %>% dplyr::filter(.data$grouping == gr)
          # Build inner-level standard code data
          # use 1st 'how' alignment note if they're all the same, otherwise collapse alignment
          # notes that map to diff. learning targets into a bulleted list
          if (length(unique(d_gr$how)) == 1) {
            aNotes = d_gr$how[1]
          } else{
            aNotes = paste0(unique(d_gr$how), collapse = "\n")
          }
          list(
            codes = unique(d_gr$code),
            #in case I ever change grade to grades or vice versa
            grades = d_gr %>% dplyr::select(dplyr::starts_with("grade")) %>% unique() %>% as.character(),
            statements = unique(d_gr$statement),
            alignmentNotes = aNotes,
            subcat = d_gr$subcat[1]
          )
        })#end Group lapply

        l_di[[di_i]] <- c(slug = d_di$dim[1],name = d_di$dimension[1],standardsGroup = list(l_gr))
      }#end dimension loop

      set_abbrev <- gsub("[a-z| ]", "", d_se$set[[1]])
      l_se[[se_i]] <- c(slug = set_abbrev,name = d_se$set[[1]], dimensions = list(l_di))
    }#end set loop
    l_su[[su_i]]<-c(subject=as.character(d_su$subject[[1]]),target=d_su$target[[1]],sets=list(l_se))
  }#end subject loop
  l_ta[[ta_i]]<-l_su
}#end target loop



out<-do.call(c,l_ta)

# create directory if necessary & prep output filename --------------------
dir.create(destFolder,showWarnings=FALSE,recursive=T)
outFile<-fs::path(destFolder,paste0(sub(pattern="(.*?)\\..*$",replacement="\\1",x=basename(fileName)),collapse=""),ext="json")


# Write JSON for GP Simple Lesson Plan -----------------------------------
compiled_json<-jsonlite::toJSON(out,pretty=TRUE,auto_unbox = TRUE)
con<-file(outFile)
writeLines(compiled_json,con)
close(con)


# return compiled tibble --------------------------------------------------
message(rep("=",30),"\n\tSUMMARY\n",rep("=",30))
message("\nStandards submitted:\t",nrow(a0),"\nRemoved due to issues:\t",sum(tbds)+sum(undoc),"\nSuccessfully compiled:\t",nrow(A),"\n")
message("JSON file saved\n@ ",outFile)
message(rep("=",30))

#add grades information to output
return(list(input=dplyr::as_tibble(a0),compiled=dplyr::as_tibble(A),problem_entries=dplyr::as_tibble(a0[(tbds+undoc)>0,])))

}#end compileAlignment function def
