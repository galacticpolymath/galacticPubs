#' compileStandards_OLD
#'
#' Compile alignment info from a lesson standards matrix worksheet. This was the first compilation method, used for the assumptionsMatter lesson, and it's basically garbage. Keeping it here till I can update that lesson for the new workflow.
#'
#' @param alignmentMatrixFile file location of the lesson alignment matrix XLSX worksheet; default: "meta/alignment-matrix.xlsx"
#' @param grades grade band on alignment matrix worksheet options= "5-6", "7-8", "9-12" or "5-12"
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder, 1 level up from the working directory
#' @param fileName output file name; default= "processedStandards.json"
#' @return Compiled standards
#' @export
#'
compileStandards_OLD <- function(alignmentMatrixFile="meta/alignment-matrix.xlsx",grades="5-12",destFolder="meta/JSON/" ,fileName="standards.json"){

  warning("deprecated; use compileStandards()")

   .=NULL #to avoid errors with dplyr syntax

 # Import XLSX files -------------------------------------------------------
#Import master alignment with ALL standards from https://github.com/galacticpolymath/standardX

alignmentMaster<-openxlsx::read.xlsx("https://raw.githubusercontent.com/galacticpolymath/standardX/gh-pages/align-to-all-subject-standards.xlsx",sheet=1,colNames=T)

gradeBands<-switch(grades,"5-6"="grades 5-6","7-8"="grades 7-8","9-12"="grades 9-12","5-12"=paste0("grades ",c("5-6","7-8","9-12")))

#initialize list for output
output=list() #initialize list
in_out=list()#initialize list for tracking n standards codes input

#GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG
# loop the rest of the script if "5-12" grade bands are chosen -------------

for(grades_i in 1:length(gradeBands)){
gradeBandSheetName<-gradeBands[grades_i]

# Tell user what grade we're working on
cat("\n",paste0(c(rep("#",40),"\n"),collapse = "")," Compiling ",gradeBandSheetName,"\n",paste0(c(rep("#",40),"\n"),collapse = ""))

#Import the sheet, removing blank columns that start with X.
# fileLoc<-normalizePath(fs::path("meta/",alignmentMatrixFile))
alignment_matrix0<-openxlsx::read.xlsx(alignmentMatrixFile,startRow=2,sheet=gradeBandSheetName,check.names=T)
# subject order
subj_order<-c("CCmath","CCela","NGSSsci","C3ss")
begin_subj_colnames<-c("Learn","Target","Codes","How")#start of column names to select them
alignment_matrix<-alignment_matrix0 %>% dplyr::select(dplyr::starts_with(begin_subj_colnames))
names(alignment_matrix)

subjColIndexes<-function(subj){
  jump=length(subj_order) #how many subjects are we jumping over?
  numIndexes=length(begin_subj_colnames)-1
  start=1+(match(subj,subj_order))
  c(1,seq(start,jump*numIndexes+1,jump))
  }

CCmathCols=subjColIndexes("CCmath")
CCelaCols=subjColIndexes("CCela")
ngssCols=subjColIndexes("NGSSsci")
c3Cols=subjColIndexes("C3ss")

#Create separate data frames, rename, and stack
newNames<-c("LearningTargets","Target","Codes","AlignmentNotes")
CCmath_matrix<-alignment_matrix[,CCmathCols]
names(CCmath_matrix)=newNames
CCela_matrix<-alignment_matrix[,CCelaCols]
names(CCela_matrix)=newNames
ngss_matrix<-alignment_matrix[,ngssCols]
names(ngss_matrix)<-newNames
c3_matrix<-alignment_matrix[,c3Cols]
names(c3_matrix)<-newNames

alignment_matrix_stacked0=rbind(CCmath_matrix,CCela_matrix,ngss_matrix,c3_matrix)
alignment_matrix_stacked=alignment_matrix_stacked0 %>% dplyr::filter(.data$Codes!="")

#///////////////////////////
# Output Integrity Check 1: verify that codes have been entered for every lesson that has alignment notes
n_code_entries<-nrow(alignment_matrix_stacked)
n_alignmentNote_entries<-nrow(alignment_matrix_stacked0 %>% dplyr::filter(.data$AlignmentNotes!=""))
test_code_v_alignment<-n_code_entries==n_alignmentNote_entries
msg_code_v_alignment<-ifelse(test_code_v_alignment,"TEST 1 PASS","TEST 1 FAIL: Make sure you have codes listed for every set of alignment notes.")
cat("\n",paste0(rep("-",30),collapse=""),"\n  Integrity Check 1\n",paste0(rep("-",30),collapse=""),"\n  N Alignment Entries: \t",
    n_alignmentNote_entries,"\n  N Code Entries: \t",n_code_entries,
    "\n\n")
message(msg_code_v_alignment,"\n")



# Make ragged list of standards -------------------------------------------
mismatches=vector()
compiled<-dplyr::tibble()
#Piece apart code entries, row by row
for (i in 1:nrow(alignment_matrix_stacked)){
  d<-alignment_matrix_stacked[i,]
  #split codes separated by a comma & remove extra spaces
  code_vec_i<-d$Codes %>% stringr::str_split(pattern="[ ]*,[ ]*",simplify=T) %>%
   stringr::str_remove_all(stringr::fixed(" "))
  code_master_rows<-sapply(code_vec_i,function(x){match(tolower(x),tolower(alignmentMaster$code))})
  #build list of mismatches b/w lesson alignment Matrix and alignment Master to output in Test 2
  if(NA%in%code_master_rows){mismatches<-append(mismatches,names(code_master_rows)[which(is.na(code_master_rows))])}
  good_rows<-code_master_rows[stats::complete.cases(code_master_rows)] #rows that match master codes

  if(length(good_rows)>0)
  {
  #For all rows that match the alignment master doc, perform tests & aggregate info
  master_data_i<-lapply(1:length(good_rows),function(ii){
    #Get Standards Statements
      code_indiv_ii<-names(good_rows)[ii] %>% toupper()
      dim_indiv_ii<-alignmentMaster$dim[good_rows[ii]]
      dimension_indiv_ii<-alignmentMaster$dimension[good_rows[ii]]
      set_indiv_ii<-alignmentMaster$set[good_rows[ii]]
      subcat_indiv_ii<-alignmentMaster$subcategory[good_rows[ii]]

      # special adaptation of NGSS standards:
      # if dim=DCI, tack on Performance Expectation to statement, otherwise stmt is the code statement
            if(dim_indiv_ii=="DCI"){
      stmt_indiv_ii <- paste0(alignmentMaster$statement[good_rows[ii]],"\n\nPerformance Expectation: ",alignmentMaster$performanceExpectation[good_rows[ii]],collapse="")
      }else{stmt_indiv_ii<- alignmentMaster$statement[good_rows[ii]]}
    #Get other stuffs
      subj_indiv_ii<-alignmentMaster$subject[good_rows[ii]]
      link_indiv_ii<-alignmentMaster$link[good_rows[ii]]
      grades_indiv_ii<-alignmentMaster$grade[good_rows[ii]]
      out_ii<-dplyr::tibble(code=code_indiv_ii,statement=stmt_indiv_ii,set=set_indiv_ii,dim=dim_indiv_ii,
                         dimension=dimension_indiv_ii,subcat=subcat_indiv_ii,
                         subject=subj_indiv_ii,grades=grades_indiv_ii)
      out_ii
      }) %>% dplyr::bind_rows()

  #Test if master_data_i codes are all in the same dimension. If so, combine, if not, repeat learning targets
  if(length(unique(master_data_i$dim))==1){ #if same dimensions
    grpng=paste0(master_data_i$code,collapse="_")
    master_data_i<-cbind(grouping=grpng,master_data_i)
  }else{#if different dimensions
    grpng<-sapply(unique(master_data_i$dim),function(x){
            grp_member_indx<-which(master_data_i$dim==x)
            paste0(master_data_i$code[grp_member_indx],collapse="_")})
    master_data_i<-cbind(grouping=grpng,master_data_i)
  }


  #Now combine grouped standards info with lesson standards matrix data for final output
  out_i<-cbind(learningTargets=d$LearningTargets,target=ifelse((tolower(d$Target) != "n"&!is.na(d$Target)),TRUE,FALSE),
               master_data_i,alignmentNotes=d$AlignmentNotes) %>% dplyr::tibble()

  #Add gradeBand to out_i
  out_i$gradeBand<-gradeBands[grades_i]

  #Add this output to growing tibble (data frame)
  compiled<-dplyr::bind_rows(compiled,out_i)
  }#end test for nonempty good_rows vector
}#end for loop with i

#Store info for this grade band
output[[grades_i]]<-compiled
in_out[[grades_i]]<-data.frame(grades=gradeBandSheetName,input_code_entries=n_code_entries,output_code_entries=length(compiled$code))

#///////////////////////////
# Output Integrity Check 2: Codes in Lesson alignment matrix that don't match anything in the master (prob. typos)
msg_test2<-ifelse(length(mismatches)==0,"TEST 2 PASS\n","TEST 2 FAIL\n  These codes in alignment matrix didn't match\n  anything in the standards master doc:\n\t")
cat("\n",paste0(rep("-",30),collapse=""),"\n  Integrity Check 2\n",paste0(rep("-",30),collapse=""),
    "\n Num. standards code mismatches: ",length(mismatches) ,
    "\n\n")
message(msg_test2,"\n  ",paste0(mismatches,collapse=", "))


}#end for loop over grade bands
#GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG





# combine final data if all grade bands selected --------------------------

COMPILED<-if(length(gradeBands)>1){do.call(rbind,output)}else{output[[1]]}

#Factor subjects in order which will be used for learningChart
COMPILED$subject<-factor(COMPILED$subject,levels=c("Math","ELA","Science","Social Studies"),ordered=T)

# concise version of final output with lots of undocumented missing values
COMPILED



# Make json structured output ----------------------------------------------

#make blank dataframe with in missing subjects in both target & nontarget
allSubjects<-alignmentMaster$subject %>% unique() %>% subset(.!="")
allDimensions<-alignmentMaster$dimension%>% unique() %>% subset(.!="")

emptyStandardsMatrix<-lapply(allSubjects,function(x){
                          alignmentMaster %>% dplyr::group_by(.data$subject,.data$set) %>%
                          dplyr::filter(.data$subject==x) %>% dplyr::select("subject","set","dimension","dim") %>%
                          dplyr::filter(!duplicated(.data$dimension)) %>% dplyr::as_tibble() #last step removes grouping metastructure
                          })    %>%  do.call(rbind,.)

missingData<-dplyr::anti_join(emptyStandardsMatrix,COMPILED,by = c("subject", "set", "dimension", "dim"))

## Final tibble with at least 1 observation (e.g. NA) for every dim for every subject (all "missings" are added to target=F, not a ton of missings in target=T)
COMPILED_filled_sorted<-dplyr::left_join(COMPILED,missingData,by = c("subject", "set", "dimension", "dim")) %>%
                        dplyr::arrange(dplyr::desc(.data$target),.data$subject,.data$set,.data$dim,.data$grouping,.data$code)



l_ta <- list()
for(ta_i in 1:length(unique(COMPILED_filled_sorted$target))) {
  d_ta <- COMPILED_filled_sorted %>% dplyr::filter(.data$target == unique(COMPILED_filled_sorted$target)[ta_i])
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
          # use 1st alignmentNote if they're all the same, otherwise collapse alignment
          # notes that map to diff. learning targets into a bulleted list
          if (length(unique(d_gr$alignmentNotes)) == 1) {
            aNotes = d_gr$alignmentNotes[1]
          } else{
            aNotes = paste0(unique(d_gr$alignmentNotes), collapse = "\n")
          }
          list(
            codes = unique(d_gr$code),
            grades = unique(d_gr$grades),
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
    l_su[[su_i]]<-c(subject=d_su$subject[[1]],target=d_su$target[[1]],sets=list(l_se))
  }#end subject loop
  l_ta[[ta_i]]<-l_su
}#end target loop



out<-c(l_ta[[1]],l_ta[[2]])

# create directory if necessary & prep output filename --------------------
dir.create(destFolder,showWarnings=FALSE,recursive=T)
outFile<-fs::path(destFolder,paste0(sub(pattern="(.*?)\\..*$",replacement="\\1",x=basename(fileName)),"_",paste0(grades),collapse=""),ext="json")


# Write JSON for GP Simple Lesson Plan -----------------------------------
compiled_json<-jsonlite::toJSON(out,pretty=TRUE,auto_unbox = TRUE)
con<-file(outFile)
writeLines(compiled_json,con)
close(con)


# return compiled tibble --------------------------------------------------
print(COMPILED)
In_Outs<-do.call(rbind,in_out)
message("Total of ",sum(In_Outs$output_code_entries)," standards compiled\n")
message("JSON file saved\n@ ",outFile)

#add grades information to output
return(list(grades=grades,data_w_NA=COMPILED_filled_sorted,data=COMPILED))

}#end compileAlignment function def
