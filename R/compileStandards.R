#' compileStandards
#'
#' Compile alignment info from a lesson standards matrix worksheet
#' @param standardsFile file location of the lesson standards alignment XLSX worksheet; default: "meta/standards_GSheetsOnly.xlsx"
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder, 1 level up from the working directory
#' @param fileName output file name; default= "standards.json"
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @param standardsRef where do you want to pull down statements and other info for the supplied standards codes? Default="standardX" (i.e. the \href{https://github.com/galacticpolymath/standardX}{standardX repository}); Options= "standardX" or "myFile" (which will use Tab 2 on the supplied XLSX workbook)
#' @param targetSubj which subject`(`s`)` is `(`are`)` the focus of the lesson? opts= "math","ela","science","social studies"
#' @param structureForWeb default=TRUE; Do you want to preface JSON output with component & nest output in Data element?
#' @return list of the compiled standards data with 3 objects: $input (the input file as a tibble); $compiled (the compiled tibble); $problem_entries (a tibble of entries with 'TBD' or missing values in the "How this aligns..." colum). A JSON is saved to the destFolder location.
#' @export
#'
compileStandards <- function(standardsFile = "meta/standards_GSheetsOnly.xlsx",
                             destFolder = "meta/JSON/" ,
                             fileName = "standards.json",
                             standardsRef = "standardX",
                             targetSubj= NULL,
                             WD= getwd(),
                             structureForWeb= TRUE) {


   .=NULL #to avoid errors with dplyr syntax

   #if WD supplied, append it to destFolder
if(!identical(WD,getwd())){
  destFolder<-fs::path(WD,destFolder)
  standardsFile<-fs::path(WD,standardsFile)}

#If targetSubj not provided, use front-matter.yml
if(missing(targetSubj)){
   tempSubj<-safe_read_yaml(fs::path(WD,"meta","front-matter.yml"))$TargetSubj
   if(!galacticPubs::is_empty(tempSubj)){targetSubj<-tolower(tempSubj)}
}

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
names(a0)[1:11] <- c("code","statement","subject","grade","lo","lo_stmnt","set","dim","target","grp", "how")


# manage TBDs and flagged, undocumented alignments ------------------------
tbds<-grepl("tbd",a0$how,ignore.case=TRUE)
#a1 does not have records with lo_statements containing "TBD" or no entry for "how"
if(sum(tbds)>0) {
  message(
    "\nThe following were removed because Learning Objective documentation contained 'TBD':\n\t\u2022",
    paste0(a0$code[tbds], collapse = "\n\t\u2022"),
           "\n"
  )
}

#undocumented alignments
undoc<-is.na(a0$how)&is.na(a0$grp)
if(sum(undoc)>0) {
  message(
    "\nThe following were removed because learning objective documentation was blank:\n\t\u2022",
    paste0(a0$code[undoc], collapse = "\n\t\u2022"),
    "\n"
  )
}
a1<-a0[!undoc&!tbds,]

# a2 has markdown bullets ("- ") added if missing
# Add markdown bullet to front of lines that don't start with it
# Figure out grade band(s)
a2<-a1

#swap out bullets for - to maintain consistency in markdown syntax
a2$how<-gsub("\u2022","-",a2$how)
a2$how<-ifelse(!grepl("^- ",a2$how),paste0("- ",a2$how),a2$how)


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
           paste0("group", "_", gsub("(-.*)","",a3$grp[i])))
  })


# Match alignment to master records ---------------------------------------
a3$code_set<-paste(a3$code,a3$set,sep="_")
a_master$code_set<-paste(a_master$code,a_master$set,sep="_")
#A is a merge of the provided alignment and the master reference document (with preference fo code defs, etc. from the provided standardsRef)
A<-dplyr::left_join(a3[,c("code_set","lo","lo_stmnt","target","grp","grouping","how")],a_master,by="code_set")
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
        as.numeric(g_i) %in% brk
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
            #make sure grade is never changed to grades...
            grades = d_gr$grade %>% unique() %>% as.character(),
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



out0<-do.call(c,l_ta)


# Prefix with component and title, and nest output in Data if structuring for web deployment
out<-if(structureForWeb){
  list(  `__component` = "lesson-plan.standards",
                             Data=out0)
}else{out0}

# create directory if necessary & prep output filename --------------------
dir.create(destFolder,showWarnings=FALSE,recursive=T)
outFile<-fs::path(destFolder,paste0(sub(pattern="(.*?)\\..*$",replacement="\\1",x=basename(fileName)),collapse=""),ext="json")



# Write JSON for GP Simple Lesson Plan -----------------------------------
jsonlite::write_json( out,
                      outFile,pretty=TRUE,auto_unbox = TRUE,na="null")



# return compiled tibble --------------------------------------------------
message(rep("=",30),"\n\tSUMMARY\n",rep("=",30))
message("\nStandards submitted:\t",nrow(a0),"\nRemoved due to issues:\t",sum(tbds)+sum(undoc),"\nSuccessfully compiled:\t",nrow(A),"\n")
message("JSON file saved\n@ ",outFile)
message(rep("=",30))

uniqueGradeBands<-subset(A,A$gradeBand!="NA")$gradeBand %>%stringr::str_split(",") %>% unlist() %>% unique()


# Prep learningEpaulette data ---------------------------------------------
    #bring in empty matrix to merge in, in case some subjects are missing
    a_template <-  readRDS(system.file("emptyStandardsCountForAllDims.rds",package="galacticPubs"))
    #super important to refactor subject on the imported data to ensure order
    a_template$subject=factor(a_template$subject,levels=c("Math","ELA","Science","Social Studies"),ordered=T)

    a_summ<-A %>% dplyr::group_by(.data$subject,.data$dimension) %>% dplyr::tally()

    #gotta combine missing rows, sort, & repeat the entries N times
    a_combined<-dplyr::anti_join(a_template,a_summ,by="dimension") %>% dplyr::bind_rows(a_summ) %>% dplyr::arrange(.data$subject,.data$dimension)%>% dplyr::mutate(binary=ifelse(.data$n>0,1,0))

    #Account for bias in the number of standards
    bias<-readRDS(system.file("standardCountsByDimension.rds",package="galacticPubs"))
    bias_by_subj<-bias %>% dplyr::summarise(tot_n_subj=sum(.data$n),.groups="drop")
    a_combined<-dplyr::left_join(a_combined, (bias %>% dplyr::rename("tot_n_dim"="n")),by = c("subject", "dimension") )
    a_combined<-dplyr::left_join(a_combined,bias_by_subj,by = c("subject"))

    #correct the lesson's n standards by Tot possible for the subject
    #*Because there aren't an equal number of standards per dimension, (and they're not all equal),
    #*It's more intuitive to treat them as if they are all equal.
    #*So to make the correction, we'll weight the proportions by total N for subject
    a_combined$id=1:nrow(a_combined)
    a_combined$n_adj<-a_combined$n/a_combined$tot_n_subj
    a_combined$n_prop<-a_combined$n/sum(a_combined$n)
    a_combined$n_prop_adj<-a_combined$n_adj/sum(a_combined$n_adj)

    #Remind r that a_combined factors are ORDERED
    a_combined$subject <- factor(a_combined$subject,levels=c("Math","ELA","Science","Social Studies"),ordered=T)


    #Calculate corrected proportions
    proportions=a_combined  %>% dplyr::group_by(.data$subject)%>% dplyr::summarise(proportion=round(sum(.data$n_prop_adj),2),.groups="drop")



    xlabels<-sapply(proportions$proportion,scales::percent) %>% dplyr::as_tibble()
    xlabels$x.prop=(proportions$proportion)
    xlabels$x=cumsum(proportions$proportion)-(proportions$proportion/2)




    xlabels$subj<-c("math","ela","science","socstudies")
    xlabels$subject<-c("Math","ELA","Sci","SocStd")
    xlabels$lab<-paste(t(xlabels$value),t(xlabels$subject))
    xlabels$hjust<-.5#c(0,0,1,1)
    xlabels$fontface<-"plain"
    xlabels$stroke<-1
    xlabels$strokeCol<-gpColors(xlabels$subj)
    xlabels$lightCol<-c("#fdebe8","#fef6ed","#f8f5fe","#efebf6")
    xlabels$size<-9


    xlabels$y<-0.6


    #thickness of epaulette bar
    thickness= 0.2

    rectangles<-dplyr::tibble(proportion=proportions$proportion,xmin=c(0,cumsum(proportions$proportion)[-4]),xmax=cumsum(proportions$proportion),ymin=1-thickness,ymax=1,subject=c("Math","ELA","Science","Soc. Studies")) %>% dplyr::filter(.data$proportion>0)
    rectangles$subject<-factor(rectangles$subject,ordered=T,levels=c("Math","ELA","Science","Soc. Studies"))
    rectangles$border<-"transparent"

    # segs<-dplyr::tibble(x=xlabels$x,xend=xlabels$x,y=1-thickness-0.04,yend=xlabels$yend,subject=xlabels$subject,segCol=clrs,targetSegCol=NA)

    #boldenize & embiggenate if targetSubj indicated
    if(!is.null(targetSubj)){
      (targetRows<-which(!is.na(charmatch(tolower(xlabels$subj),tolower(targetSubj))) ))
      xlabels$stroke[targetRows]<-2
      xlabels$strokeCol[targetRows] <- gpColors("galactic black")
      xlabels$fontface[targetRows]<-"bold"
      xlabels$size[targetRows]<-11
      rectangles$border[targetRows]<- gpColors("galactic black")
    }



# prep for LearningChart  -------------------------------------------------

    a_combined$dimAbbrev<-c(" Algebra, Geometry,\n Trig, Calculus,\n Other Adv Math"," Measurement, Data,\n Probability, Statistics"," Number Systems, Operations,\n Symbolic Representation"," Language, Speaking,\n Listening"," Reading"," Writing"," Cross-Cutting \n Concepts "," Disciplinary\n Core Ideas"," Science & Engineering\n Practices"," Civics, Economics,\n Geography, History"," Develop Questions,\n Plan Inquiries"," Evaluate, \n Communicate, \n Take Action ")






# Save Standards Data -----------------------------------------------------
toSave<- list(
            data=list(
              input = dplyr::as_tibble(a0),
              compiled = dplyr::as_tibble(A),
              problem_entries = dplyr::as_tibble(a0[(tbds + undoc) > 0, ]),
              gradeBands= uniqueGradeBands
              ),
            a_combined=a_combined,
            xlabels=xlabels,
            rectangles=rectangles,
            targetSubj=targetSubj
            )
saveRDS(toSave,file=fs::path(WD,"meta/standards.RDS"))



#add grades information to output
return(
  list(
    input = dplyr::as_tibble(a0),
    compiled = dplyr::as_tibble(A),
    problem_entries = dplyr::as_tibble(a0[(tbds + undoc) > 0, ]),
    gradeBands= uniqueGradeBands,
    targetSubj=targetSubj
  )
)

}#end compileAlignment function def
