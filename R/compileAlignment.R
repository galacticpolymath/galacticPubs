#' compileAlignment
#'
#' Compile alignment info from a lesson standards matrix worksheet
#' @param lessonAlignmentMatrix file location of the lesson alignment matrix XLSX worksheet
#' @param grades grade band on alignment matrix worksheet options= "5-6", "7-8", "9-12" or "all"
#' @param fileName output filename (including path if you don't want it in the working directory)
#'
compileAlignment <- function(lessonAlignmentMatrix,grades="all",fileName="processedStandards.json"){

  .=NULL #to avoid errors with dplyr syntax

 # Import XLSX files -------------------------------------------------------
#Import master alignment with ALL standards from https://github.com/galacticpolymath/standardX
tmp<-tempfile("allStandards_temp",fileext="csv")


utils::download.file("https://github.com/galacticpolymath/standardX/blob/master/data/allStandards.csv?raw=true",destfile=tmp)
alignmentMaster<-utils::read.csv(tmp)

gradeBands<-switch(grades,"5-6"="grades 5-6","7-8"="grades 7-8","9-12"="grades 9-12","all"=paste0("grades ",c("5-6","7-8","9-12")))

#initialize list for output
output=list() #initialize list
in_out=list()#initialize list for tracking n standards codes input

#GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG
# loop the rest of the script if "all" grade bands are chosen -------------

for(grades_i in 1:length(gradeBands)){
gradeBandSheetName<-gradeBands[grades_i]

# Tell user what grade we're working on
cat("\n",paste0(c(rep("#",40),"\n"),collapse = "")," Compiling ",gradeBandSheetName,"\n",paste0(c(rep("#",40),"\n"),collapse = ""))

#Import the sheet, removing blank columns that start with X.
alignment_matrix0<-xlsx::read.xlsx2(lessonAlignmentMatrix,startRow=2,sheetName=gradeBandSheetName) %>% dplyr::select(-dplyr::starts_with("X."))
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
alignment_matrix_stacked=alignment_matrix_stacked0 %>% dplyr::filter("Codes"!="")

#///////////////////////////
# Output Integrity Check 1: verify that codes have been entered for every lesson that has alignment notes
n_code_entries<-nrow(alignment_matrix_stacked)
n_alignmentNote_entries<-nrow(alignment_matrix_stacked0 %>% dplyr::filter("AlignmentNotes"!=""))
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
  #split codes separated by a comma
  code_vec_i<-d$Codes %>% stringr::str_split(pattern="[ ]*,[ ]*",simplify=T)
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
  out_i<-cbind(learningTargets=d$LearningTargets,target=ifelse(tolower(d$Target)=="y",TRUE,FALSE),
               master_data_i,alignmentNotes=d$AlignmentNotes) %>% dplyr::tibble()
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
# concise version of final output with lots of undocumented missing values
COMPILED



# Make json structured output ----------------------------------------------

#make blank dataframe with in missing subjects in both target & nontarget
allSubjects<-alignmentMaster$subject %>% unique() %>% subset(.!="")
allDimensions<-alignmentMaster$dimension%>% unique() %>% subset(.!="")
emptyStandardsMatrix<-lapply(allSubjects,function(x){
                        subjDims<-alignmentMaster %>% dplyr::filter("subject"==x) %>% dplyr::select("dimension") %>%
                          unique()%>% unlist()
                        dplyr::tibble(subject=x,subjDims)}) %>% do.call(rbind,.) %>% dplyr::rename("dimension"="subjDims")
emptyStandardsMatrix<-rbind(emptyStandardsMatrix,emptyStandardsMatrix) %>% dplyr::mutate("target"=c(rep(TRUE,
                        nrow(emptyStandardsMatrix)),rep(FALSE,nrow(emptyStandardsMatrix)))) %>% dplyr::mutate(
                        code=NA,alignmentNotes=NA)
missingDimRows<-match(paste(emptyStandardsMatrix$target,emptyStandardsMatrix$dimension), paste(COMPILED$target,COMPILED$dimension)) %>% is.na() %>% which()


## Final tibble with at least 1 observation (e.g. NA) for every dim for every subject 2x (for target=T,F)
COMPILED_filled_sorted<-dplyr::bind_rows(emptyStandardsMatrix[missingDimRows,],COMPILED)%>% dplyr::arrange(dplyr::desc("target"),"subject","set","dim","grouping")


#iterate across target and connected standards categories
l_ta<-lapply(unique(COMPILED_filled_sorted$target),function(ta){
    d_ta<-COMPILED_filled_sorted %>% dplyr::filter("target"==ta)
    #iterate across unique subjects within target/nontarget types
    l_su<-lapply(unique(d_ta$subject),function(su){
        d_su<-d_ta %>% dplyr::filter("subject"==su)
          l_se<-lapply(unique(d_su$set),function(se){
            d_se<-d_su %>% dplyr::filter("set"==se)
              #iterate across unique dimensions within subjects
              l_di<-lapply(unique(d_se$dimension),function(di){
                d_di<-d_su %>% dplyr::filter("dimension"==di)
                #iterate across unique groups of standards (may be indiv. or groups of substandards w/in a dimension)
                l_gr<-lapply(unique(d_di$grouping),function(gr){
                   d_gr<-d_di %>% dplyr::filter("grouping"==gr)
                     # Build inner-level standard code data
                     # use 1st alignmentNote if they're all the same, otherwise collapse alignment
                     # notes that map to diff. learning targets into a bulleted list
                     if(length(unique(d_gr$alignmentNotes))==1){aNotes=d_gr$alignmentNotes[1]}else{
                       aNotes=paste0(unique(d_gr$alignmentNotes),collapse="\n")
                     }
                     list(codes=unique(d_gr$code),grades=unique(d_gr$grades ),statements=unique(d_gr$statement),alignmentNotes=aNotes,subcat=d_gr$subcat[1],dim=d_gr$dim[1])
                   })#end Group lapply
                c(dimension=di,standardsGroupContainer.=l_gr)
              })#end dimensions lapply
            c(set=se,dimContainer.=l_di)
        })#end set lapply
        c(subject=su,setContainer.=l_se)
      })#end subjects lapply
    c(target=ta,subjContainer.=l_su)
  })#end target target lapply






# Write JSON for GP Simple Lesson Plan -----------------------------------
compiled_json<-jsonlite::toJSON(list(targContainer.=l_ta),pretty=T)
con<-file(fileName)
writeLines(compiled_json,con)
close(con)


# return compiled tibble --------------------------------------------------
print(COMPILED)
In_Outs<-do.call(rbind,in_out)
message("Total of ",sum(In_Outs$output_code_entries)," standards compiled\n")
message("JSON file saved\n@ ",fileName)

return(COMPILED)

}#end compileAlignment function def
