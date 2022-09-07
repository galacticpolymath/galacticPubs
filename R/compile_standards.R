#' compile_standards
#'
#' Compile alignment info from a lesson standards matrix worksheet
#' @param standardsFile file location of the lesson standards alignment XLSX worksheet; default: "meta/standards_GSheetsOnly.xlsx"
#' @param learningplot_correction do you want to correct proportions (for learningEpaulette and learningChart) for the total possible standards in each subject? i.e. Common Core Math has a LOT more standards than C3 Social Studies. default=T; this will scale proportions of subjects by the relative total proportions of standards in each subject. If FALSE, the proportions will just be raw proportions of each subject out of total standards aligned.
#' @param standardsRef where do you want to pull down statements and other info for the supplied standards codes? Default="myFile" (i.e. Tab2 of the standards workbook.) if "standardX" is chosen, it'll pull down the latest version here: \href{https://github.com/galacticpolymath/standardX}{standardX repository})
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder, 1 level up from the working directory
#' @param fileName output file name; default= "standards.json"
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @param targetSubj which subject`(`s`)` is `(`are`)` the focus of the lesson? opts= "math","ela","science","social studies"
#' @param structureForWeb default=TRUE; Do you want to preface JSON output with component & nest output in Data element?
#' @return list of the compiled standards data with 3 objects: $input (the input file as a tibble); $compiled (the compiled tibble); $problem_entries (a tibble of entries with 'TBD' or missing values in the "How this aligns..." column). A JSON is saved to the destFolder location.
#' @export
#'
compile_standards <- function(standardsFile = "meta/standards_GSheetsOnly.xlsx",
                             learningplot_correction= TRUE,
                             standardsRef = "myFile",
                             destFolder = "meta/JSON/" ,
                             fileName = "standards.json",
                             targetSubj= NULL,
                             WD= getwd(),
                             structureForWeb= TRUE) {


   .=NULL #to avoid errors with dplyr syntax

#############
# IMPORTANT: Add Subjects here if you need to align new ones --------------
   ordered_subjects<-  c("Math","ELA","Science","Social Studies","Art","Sustainability","Technology")
   #learning chart subject titles
   ordered_subj_chart<-c("Math","ELA","Sci"    ,"SocStd",        "Art","SDGs",          "Tech")
   #here for legacy reasons
   ordered_subj<-      c("math","ela","science","socstudies",     "art","sdgs",         "tech")


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
    tmp<-tempfile()
    utils::download.file(
        "https://raw.githubusercontent.com/galacticpolymath/standardX/gh-pages/align-to-all-subject-standards.xlsx",destfile = tmp)
    a_master <-
      readxl::read_excel(tmp,
        sheet = 1,
        col_names = TRUE,
        col_types="text"
      ) %>% dplyr::filter(!is.na(.data$code))
    #If you want to supply custom standards sheet for just this lesson (to supply statements and other columns that will be matched by set*code)
  } else if (standardsRef == "myFile") {
    a_master <-readxl::read_excel(
      standardsFile,
      sheet = 2,
      col_names = TRUE,
      skip = 1,
      col_types = "text"
    ) %>% dplyr::filter(!is.na(.data$code))
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
browser()
undoc<-(is.na(a0$how)&is.na(a0$grp))|sapply(a0$target,function(x) identical(x,"skip"),USE.NAMES = F)
if(sum(undoc)>0) {
  message(
    "\nThe following were removed because learning objective was blank or you said 'skip':\n\t\u2022",
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

  #if either code_set has non-unique entries, hopefully can be matched by their order...but warn user matching might not work
duplicated_a3<-duplicated(a3$code_set)
duplicated_a_master <- duplicated(a_master$code_set)
some_duplicated<-sum(duplicated_a3)>0|sum(duplicated_a_master)>0

if(some_duplicated){
  warning("Some duplicated code*set values found:\n  -",paste(a3$code_set[duplicated_a3],collapse="\n  -"))
  #try to match using unique code_set that depends on them being entered in order (problematic)
  a3$code_set<-make.names(a3$code_set,unique=TRUE)
  a_master$code_set<-make.names(a_master$code_set,unique=TRUE)
  warning("Code<->Statement matching was done based on order of duplicates (i.e. must be same order in alignment and master). Check output!")
}
#A is a merge of the provided alignment and the master reference document (with preference fo code defs, etc. from the provided standardsRef)
A<-dplyr::left_join(a3[,c("code_set","lo","lo_stmnt","target","grp","grouping","how")],a_master,by="code_set")


#factor subjects for desired order

A$subject<-factor(A$subject,levels=ordered_subjects,ordered=T)
A <- A %>% dplyr::arrange(.data$subject)


# warn if statements missing (indicates bad merge) -----------------------
if(sum(stats::complete.cases(A$statement))==0){
  warning("Bad merge. No 'Statements' matched standards code for each set. Try changing 'standardsRef' in compile_standards(); currently, standardsRef = '",standardsRef,"'")
}



# Add dims if only dimensions provided ------------------------------------
A$dim<-sapply(1:nrow(A),function(i){
  if(is.na(A$dim[i])){
    if(is.na(A$dimension[i])){
      NA
    }else{
    #Just take capital letters of dim
    stringr::str_extract_all(A$dimension[i],"[A-Z]*",simplify=TRUE) %>% paste0(collapse="")
    }
  }else{
    A$dim[i]
  }
})



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

if(sum(A$target)==0){
  warning("No standards selected as 'Target' in standards alignment worksheet")
}

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


# return summary tibble --------------------------------------------------
message(rep("=",30),"\n\tSUMMARY\n",rep("=",30))
message("\nStandards submitted:\t",nrow(a0),"\nRemoved due to issues:\t",sum(tbds)+sum(undoc),"\nSuccessfully compiled:\t",nrow(A),"\n")
message(rep("=",30))

uniqueGradeBands<-subset(A,A$gradeBand!="NA")$gradeBand %>%stringr::str_split(",") %>% unlist() %>% unique()


# Prep learningEpaulette data ---------------------------------------------
    #create an empty matrix to merge in, in case some subjects are missing
    a_template <-  a_master %>% dplyr::select("subject","dimension") %>%
                  dplyr::distinct() %>% dplyr::mutate(n=0) %>%
                #Get rid of subjects not included in the alignment
                 dplyr::filter(.data$subject %in% unique(A$subject))

    #super important to refactor subject on the imported data to ensure order

    a_template$subject=factor(a_template$subject,levels=ordered_subjects,ordered=T)

    a_summ<-A %>% dplyr::group_by(.data$subject,.data$dimension) %>% dplyr::tally()

    #gotta combine missing rows, sort, & repeat the entries N times
    a_combined<-dplyr::anti_join(a_template,a_summ,by="dimension") %>% dplyr::bind_rows(a_summ) %>% dplyr::arrange(.data$subject,.data$dimension)%>% dplyr::mutate(binary=ifelse(.data$n>0,1,0))



    #### This correction is kinda problematic as we add other standards to our database!
    #### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1

    #Account for bias in the number of standards
    bias <-
      a_master %>%
      dplyr::group_by(.data$subject, .data$dimension) %>%
      dplyr::summarise(n =dplyr::n()) %>%
      #Get rid of subjects not included in the alignment
      dplyr::filter(.data$subject %in% unique(A$subject))

    bias_by_subj<-bias %>% dplyr::summarise(tot_n_subj=sum(.data$n),.groups="drop")
    a_combined<-dplyr::left_join(a_combined, (bias %>% dplyr::rename("tot_n_dim"="n")),by = c("subject", "dimension") )
    a_combined<-dplyr::left_join(a_combined,bias_by_subj,by = c("subject"))

    #*Because there aren't an equal number of standards per dimension, (and they're not all equal),
    #*It's more intuitive to treat them as if they are all equal.
    #*So to make the correction, we'll weight the proportions by total N for subject
    a_combined$id=1:nrow(a_combined)
    a_combined$n_adj<-a_combined$n/a_combined$tot_n_subj
    a_combined$n_prop<-a_combined$n/sum(a_combined$n)
    a_combined$n_prop_adj<-a_combined$n_adj/sum(a_combined$n_adj)

    #Remind r that a_combined factors are ORDERED
    a_combined$subject <- factor(a_combined$subject,levels=ordered_subjects,ordered=T)


    #Calculate corrected proportions if requested
    proportions0=a_combined  %>% dplyr::group_by(.data$subject)
    if(learningplot_correction){
    proportions=proportions0 %>% dplyr::summarise(proportion=round(sum(.data$n_prop_adj),2),.groups="drop")
    }else{
      #don't use adjusted proportions if not requested
      proportions=proportions0 %>% dplyr::summarise(proportion=round(sum(.data$n_prop),2),.groups="drop")
    }

    #Set up vector to recode subject levels according to key (for learning chart subject abbrevs.)
    convert_labels<-ordered_subj_chart
    names(convert_labels)<-ordered_subjects

    convert_labels2<-ordered_subj
    names(convert_labels2)<-ordered_subjects

    xlabels <- proportions %>%
      dplyr::rename("subject_orig" = "subject") %>%
      dplyr::mutate(
        value = scales::percent(.data$proportion),
        x.prop = proportion,
        x = (cumsum(.data$proportion) -
               .data$proportion / 2),
        subject = dplyr::recode(.data$subject_orig, !!!convert_labels),
        #not sure the next one is necessary...should remove if not
        subj = dplyr::recode(.data$subject_orig, !!!convert_labels2),
        lab = paste0(t(.data$value), t(.data$subject)),
        hjust = 0.5,
        fontface = "plain",
        stroke = 1,
        strokeCol = gpColors(.data$subj),
        lightCol = colorspace::lighten(strokeCol, 0.8),
        size = 9,
        y = 0.6
      )


    #thickness of epaulette bar
    thickness= 0.2

    #I think learning chart can handle sustainability above...this epaulette code CANNOT...
    rectangles<-
      dplyr::tibble(proportion=proportions$proportion,xmin=c(0,cumsum(proportions$proportion)[-4]),xmax=cumsum(proportions$proportion),ymin=1-thickness,ymax=1,subject=c("Math","ELA","Science","Soc. Studies")) %>% dplyr::filter(.data$proportion>0)
    rectangles$subject<-factor(rectangles$subject,ordered=T,levels=c("Math","ELA","Science","Soc. Studies"))
    rectangles$border<-"transparent"

    #Ensure that xmax[4] is 1, so there's never a gap at the right
  rectangles$xmax[4] <- 1


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
#supported sets of standards for generating learning chart
  supported_sets <-
    c("Common Core Math", "Common Core ELA", "NGSS", "C3")

  #check if standards contain an unsupported set of standards
  which_alignment_supported <-unique_sans_na(A$set) %in% supported_sets
  test_alignment_supported <-which_alignment_supported %>% sum() == length(which_alignment_supported) &
    (length(which_alignment_supported) > 0)

  if (!test_alignment_supported) {
    warning(
      "No Learning Chart created. Currently supported Standards sets:\n  -",
      paste(supported_sets, collapse = "\n  -"),
      "\nStandard sets found:\n  -",
      paste(unique_sans_na(A$set),
            collapse = "\n  -")
    )
    #Store LearningChart Friendliness in front-matter and also save to RDS file
    update_fm(change_this=list(LearningChartFriendly=F),WD=WD)
    learning_chart_friendly <- FALSE
    a_combined$dimAbbrev <- NA
  } else{

    update_fm(change_this=list(LearningChartFriendly=T),WD=WD)
    learning_chart_friendly <- TRUE
    a_combined$dimAbbrev <-
      c(
        " Algebra, Geometry,\n Trig, Calculus,\n Other Adv Math",
        " Measurement, Data,\n Probability, Statistics",
        " Number Systems, Operations,\n Symbolic Representation",
        " Language, Speaking,\n Listening",
        " Reading",
        " Writing",
        " Cross-Cutting \n Concepts ",
        " Disciplinary\n Core Ideas",
        " Science & Engineering\n Practices",
        " Civics, Economics,\n Geography, History",
        " Develop Questions,\n Plan Inquiries",
        " Evaluate, \n Communicate, \n Take Action "
      )
  }







# Save Standards Data -----------------------------------------------------
toSave<- list(
            data=list(
              input = dplyr::as_tibble(a0),
              compiled = dplyr::as_tibble(A),
              problem_entries = dplyr::as_tibble(a0[(tbds + undoc) > 0, ]),
              gradeBands= uniqueGradeBands,
              list_for_json= out
              ),
            a_combined=a_combined,
            xlabels=xlabels,
            rectangles=rectangles,
            targetSubj=targetSubj,
            learning_chart_friendly=learning_chart_friendly
            )
saveRDS(toSave,file=fs::path(WD,"meta","standards.RDS"))



#add grades information to output
return(
  list(
    input = dplyr::as_tibble(a0),
    compiled = dplyr::as_tibble(A),
    problem_entries = dplyr::as_tibble(a0[(tbds + undoc) > 0, ]),
    gradeBands= uniqueGradeBands,
    targetSubj=targetSubj,
    subject_proportions=proportions,
    learning_chart_friendly=learning_chart_friendly
  )
)

}#end compile_alignment function def
