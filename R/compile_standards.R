#' compile_standards
#'
#' Does the following:
#' 1. Compile alignment info from the `standards_*.gsheet` in the project's `meta/` folder
#' 2. Output standards.RDS file with the alignment info (this will be read in by [learningEpaulette()] and [learningChart()])

#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment); If you put "?", it will invoke [pick_lesson()]
#' @param learningplot_correction do you want to correct proportions (for learningEpaulette and learningChart) for the total possible standards in each subject? i.e. Common Core Math has a LOT more standards than C3 Social Studies. default=F, the proportions will just be raw proportions of each subject out of total standards aligned. If TRUE, this will scale proportions of subjects by the relative total proportions of standards in each subject.
#' @param targetSubj which subject(s) are the focus of the lesson? opts= "math","ela","science","social studies"; default=NULL
#' @return list with 4 objects: $success (did it work?); $input (the input file as a tibble); $compiled (the compiled tibble); $problem_entries (a tibble of entries with 'TBD' or missing values in the "How this aligns..." column). A JSON is saved to the destFolder location.
#' @export
#'
compile_standards <- function(WD = "?",
                              learningplot_correction = FALSE,
                              targetSubj = NULL) {
  . = NULL #to avoid errors with dplyr syntax
  message("compiling standards...")
  #The google drive working directory for the project assets
  WD <- parse_wd(WD)

  #The github gp-lessons directory for the code
  WD_git <- get_wd_git(WD=WD)

  #############
  # IMPORTANT: Add Subjects here if you need to align new ones --------------


  ordered_subjects <-
    c("Math",
      "ELA",
      "Science",
      "Social Studies",
      "Art",
      "Sustainability",
      "Technology")
  #learning chart/learningEpaulette subject titles (<=5 char)
  ordered_subj_chart <-
    c("Math", "ELA", "Sci"    , "SocSt",        "Art", "SDGs",          "Tech")
  #here for legacy reasons
  ordered_subj <-
    c("math", "ela", "science", "socstudies",     "art", "sdgs",         "tech")




  #define paths
  destFolder <- fs::path(WD_git, "JSONs")
  standardsFile <-
    fs::path(WD, "meta", "standards_GSheetsOnly.xlsx")

  #Read in front-matter
  fm <- get_fm(WD = WD)
  #check specifically for standards link
  stnds_id <- get_fm("GdriveStandardsID" , WD = WD)
  checkmate::assert_character(stnds_id, min.chars = 6)

  if (is.na(stnds_id)) {
    warning("No standards link found. Try running updatefm() for ", WD)
    success = FALSE

    # Big else for everything----------------------------------------------------------------

  } else{
    #If targetSubj not provided, use front-matter.yml
    if (missing(targetSubj)) {
      tempSubj <- fm$TargetSubj
      if (!galacticPubs::is_empty(tempSubj)) {
        targetSubj <- tolower(tempSubj)
      }
    }

    # Import standards alignment--------------------------------------------------------
    # get standards from supplied file, tab 2
    # Note if you change these import calls, there's retry logic below, so you need to
    # change it there, as well.

    stnds_drib <- drive_find_path(stnds_id)
    checkmate::assert_data_frame(stnds_drib)

    LOs <-    googlesheets4::read_sheet(
      stnds_drib$id,
      sheet = 1,
      skip = 1,
      col_types = "c",
      .name_repair = "minimal"
    ) %>%
      #Blank column is just used as a flexible marker
      dplyr::select(1:"blank") %>%
      dplyr::select(-"blank") %>%
      dplyr::filter(!is.na(.data$`LO#`))

      a_master <- googlesheets4::read_sheet(
        stnds_drib$id,
        sheet = "2.Standard-Selection",
        skip = 1,
        col_types = "c"
      ) %>% dplyr::filter(!is.na(.data$Code))

      # a0 is the "Finalize" tab
      a0 <- googlesheets4::read_sheet(
        stnds_drib$id,
        sheet = "4.Finalize",
        skip = 1,
        col_types = "c"
      ) %>% dplyr::filter(!is.na(.data$Code))

    is_valid_tab1 <- checkmate::test_data_frame(LOs, min.rows = 1)
    is_valid_tab2 <-
      checkmate::test_data_frame(a_master, min.rows = 1)
    is_valid_tab4 <- checkmate::test_data_frame(a0, min.rows = 1)


    # If missing all rows or not a data frame, try updating gdrive links --------
    if (!is_valid_tab1 | !is_valid_tab2 | !is_valid_tab4) {
      message(
        "Found invalid/empty gsheets_link. Trying to reconnect...running update_fm(WD=WD,drive_reconnect=TRUE)."
      )
      update_fm(WD = WD, drive_reconnect = TRUE)
      stnds_id <- get_fm("GdriveStandardsID" , WD = WD)
      #Try again to import
      stnds_drib <- drive_find_path(stnds_id)

      LOs <-    googlesheets4::read_sheet(
      stnds_drib$id,
      sheet = 1,
      skip = 1,
      col_types = "c",
      .name_repair = "minimal"
    ) %>%
      #Blank column is just used as a flexible marker
      dplyr::select(1:"blank") %>%
      dplyr::select(-"blank") %>%
      dplyr::filter(!is.na(.data$`LO#`))

      a_master <- googlesheets4::read_sheet(
        stnds_drib$id,
        sheet = "2.Standard-Selection",
        skip = 1,
        col_types = "c"
      ) %>% dplyr::filter(!is.na(.data$Code))

      # a0 is the "Finalize" tab
      a0 <- googlesheets4::read_sheet(
        stnds_drib$id,
        sheet = "4.Finalize",
        skip = 1,
        col_types = "c"
      ) %>% dplyr::filter(!is.na(.data$Code))

      is_valid_tab2 <-
        checkmate::test_data_frame(a_master, min.rows = 1)
      is_valid_tab4 <- checkmate::test_data_frame(a0, min.rows = 1)

      if (!is_valid_tab2 | !is_valid_tab4) {
        stop("Failed to compile standards")# Need to handle this better later, going all the way to the end of the function
      } else{
        message("Standards file imported successfully this time.")
      }

    }


    # Test if imported standards are initiated --------------------------------
    message("Checking if standards gsheet has been initialized...")
    #check that "Help Text" does not occur anywhere in the Learning Objective statement column in Tab1
    names(LOs)[2] <- "lo_statement"
    tab1_initiated <-
      (LOs %>% dplyr::pull("lo_statement") %>% grepl("^[Hh]elp [Tt]ext", .) %>% sum()) == 0
    #Expect some LO#s
    tab2_initiated <-
      (a_master %>% dplyr::filter(!is.na(.data$`LO#`)) %>% dplyr::pull(.data$`LO#`)  %>%
         length()) > 0
    #Shouldn't be "Paste here" helper text in first cell of Tab4
    tab4_initiated <-
      !grepl("[Pp]aste", a0$Code[1])


    # Report initiation checks ------------------------------------------------
    print(dplyr::tibble(
      initialized = convert_T_to_check(c(
        tab1_initiated, tab2_initiated, tab4_initiated
      )),
      tab = c(
        "1.Learning-Objectives",
        "2.Standard-Selection",
        "4.Finalize"
      )
    ))

    if (!tab1_initiated | !tab2_initiated | !tab4_initiated) {
      warning("compile_standards() aborted because some tab(s) have not been initiated.")
      success <- FALSE
    } else{
      # Start processing standards ----------------------------------------------

      #rename so easier to deal with
      names(a0)[1:12] <-
        c(
          "code",
          "statement",
          "subject",
          "grade",
          "lo",
          "lo_stmnt",
          "set",
          "dim",
          "part",
          "target",
          "grp",
          "how"
        )

      #for convenience, just make master columns (i.e. standards_*.gsheet!2.Standard-Selection) lower case
      names(a_master) <- tolower(names(a_master))



      # Check supported subjects ------------------------------------------------
      supported_subjects <-
        c("ELA",
          "Math",
          "Science",
          "Social Studies",
          "Sustainability")

      #required subjects for learning chart
      req_subjects <- c("ELA",
                        "Math",
                        "Science",
                        "Social Studies")

      #supported sets of standards for generating learning chart
      supported_sets <-
        c("Common Core Math", "Common Core ELA", "NGSS", "C3")


      found_subjects <- unique_sans_na(a0$subject)

      #are all required subjects found?
      check_all_req <-
        sapply(req_subjects, function(x)
          x %in% found_subjects)

      #are any found subjects not supported?
      unsupported <-
        sapply(found_subjects, function(x)
          ! x %in% supported_subjects)

      if (!(sum(check_all_req) == length(req_subjects))) {
        warning(
          "Not fully interdisciplinary lesson. Subjects found: \n",
          paste(
            utils::capture.output(check_all_req, type = "output"),
            collapse = "\n"
          )
        )
      }

      if (!sum(unsupported) == 0) {
        warning("\nUnsupported subjects found: \n -",
                paste0(names(unsupported[which(unsupported)]), collapse = "\n -"))
      }


      # manage TBDs and flagged, undocumented alignments ------------------------
      tbds <- grepl("tbd", a0$how, ignore.case = TRUE)
      #a1 does not have records with lo_statements containing "TBD" or no entry for "how"
      if (sum(tbds) > 0) {
        message(
          "\nThe following were removed because Learning Objective documentation contained 'TBD':\n\t\u2022",
          paste0(a0$code[tbds], collapse = "\n\t\u2022"),
          "\n"
        )
      } else{
        tbds <- rep(FALSE, nrow(a0))
      }

      #undocumented alignments
      #target says "skip" or no "how this lesson aligns"
      undoc <-
        (is.na(a0$how) &
           is.na(a0$grp)) |
        sapply(a0$target, function(x)
          identical(x, "skip"), USE.NAMES = F)

      if (sum(undoc) > 0) {
        message(
          "\nThe following were removed because learning objective was blank or you said 'skip':\n\t\u2022",
          paste0(a0$code[undoc], collapse = "\n\t\u2022"),
          "\n"
        )
      } else{
        undoc <- rep(FALSE, nrow(a0))
      }

      a1 <- a0[!undoc & !tbds, ]

      # a2 has markdown bullets ("- ") added if missing
      # Add markdown bullet to front of lines that don't start with it
      # Figure out grade band(s)
      a2 <- a1

      #swap out bullets for - to maintain consistency in markdown syntax
      a2$how <- gsub("\u2022", "-", a2$how)
      a2$how <-
        ifelse(!grepl("^- ", a2$how), paste0("- ", a2$how), a2$how)





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
      a3 <- a2
      a3$target <-
        ifelse(is.na(a3$target) |
                 tolower(a3$target) == "n", FALSE, TRUE)
      a3$grouping <-
        sapply(1:nrow(a3), function(i) {
          ifelse(is.na(a3$grp[i]),
                 paste0("singlet", "_", i),
                 paste0("group", "_", gsub("(-.*)", "", a3$grp[i])))
        })


      # Match alignment to master records ---------------------------------------
      a3$code_set <- paste(a3$code, a3$set, sep = "_")
      a_master$code_set <-
        paste(a_master$code, a_master$set, sep = "_")

      #if either code_set has non-unique entries, hopefully can be matched by their order...but warn user matching might not work
      duplicated_a3 <- duplicated(a3$code_set)
      duplicated_a_master <- duplicated(a_master$code_set)
      some_duplicated <-
        sum(duplicated_a3) > 0 | sum(duplicated_a_master) > 0

      if (some_duplicated) {
        warning(
          "Some duplicated code*set values found:\n  -",
          paste(a3$code_set[duplicated_a3], collapse = "\n  -")
        )
        #try to match using unique code_set that depends on them being entered in order (problematic)
        a3$code_set <- make.names(a3$code_set, unique = TRUE)
        a_master$code_set <-
          make.names(a_master$code_set, unique = TRUE)
        warning(
          "Code<->Statement matching was done based on order of duplicates (i.e. must be same order in alignment and master). Check output!"
        )
      }

      #A is a merge of the provided alignment and the master reference document (with preference fo code defs, etc. from the provided standardsRef)
      #Remove "Part from a_master to avoid conflicts with changes made in tab 4.Finalize"
      A0 <-
        dplyr::left_join(a3[, c("code_set",
                                "lo",
                                "lo_stmnt",
                                "target",
                                "grp",
                                "grouping",
                                "how",
                                "part")], a_master[-which(tolower(names(a_master)) ==
                                                            "part")], by = "code_set")


      #factor subjects for desired order

      A0$subject <-
        factor(A0$subject, levels = ordered_subjects, ordered = T)
      A <-
        A0 %>% dplyr::filter(!is.na(.data$how)) %>%  dplyr::arrange(.data$subject)


      # warn if statements missing (indicates bad merge) -----------------------
      if (nrow(A) == 0) {
        warning(
          "Bad merge. No 'Statements' matched standards code for each set. Try changing 'standardsRef' in compile_standards(); currently, standardsRef = '",
          standardsRef,
          "'"
        )
      }



      # Add dims if only dimensions provided ------------------------------------
      A$dim <- sapply(1:nrow(A), function(i) {
        if (is.na(A$dim[i])) {
          if (is.na(A$dimension[i])) {
            NA
          } else{
            #Just take capital letters of dim
            stringr::str_extract_all(A$dimension[i], "[A-Z]*", simplify = TRUE) %>% paste0(collapse =
                                                                                             "")
          }
        } else{
          A$dim[i]
        }
      })



      # Add grade band to final data set ----------------------------------------
      gradeBandBreaks <- list(5:6, 7:8, 9:12)
      gradeBandTxt <-
        sapply(gradeBandBreaks, function(x)
          paste0(x[1], "-", x[length(x)]))

      gradeBand <- sapply(A$grade, function(x) {
        #Ignore K-12 wide standards for assigning grade bands
        if (grepl("K", x, ignore.case = TRUE)) {
          NA
        } else{
          grades <- unlist(strsplit(x, ",", fixed = T))
          bands <- sapply(grades, function(g_i) {
            hits = unlist(sapply(
              gradeBandBreaks,
              FUN = function(brk) {
                as.integer(g_i) %in% brk
              }
            ))
            gradeBandTxt[which(hits)]
          })
          unique(bands)
        }
      }) %>% unlist() %>% unique() %>% sort()


      # Define grade bands this alignment touches ------------
      #
      # A$gradeBand <-
      #   sapply(gradeL, function(x)
      #     paste(x, collapse = ",")) %>% unlist() %>% unique()
      #
      if (sum(A$target) == 0) {
        warning("No standards selected as 'Target' in standards alignment worksheet")
      }

      # Make json structured output ----------------------------------------------
      # ta_i=su_i=se_i=di_i=NA

      #God this is ugly, need to redo this entirely!
      l_ta <- list()
      for (ta_i in 1:length(unique(A$target))) {
        d_ta <- A %>% dplyr::filter(.data$target == unique(A$target)[ta_i])
        l_su <- list()
        for (su_i in 1:length(unique(d_ta$subject))) {
          d_su <-
            d_ta %>% dplyr::filter(.data$subject == unique(d_ta$subject)[su_i])
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

                #Get parts assignments for this standard
                uniq_parts <-
                  strsplit(unique(d_gr$part), split = ",") %>% unlist() %>% trimws() %>% unique() %>% sort()

                list(
                  parts = as.list(uniq_parts),
                  codes = unique(d_gr$code),
                  #make sure grade is never changed to grades in spreadsheet...

                  #Man, pretty annoying string of pipes necessary to get grades formatted right
                  grades = d_gr$grade %>% gsub("[a-zA-Z]", "", .) %>%
                    strsplit(split = ",") %>% unlist() %>% trimws() %>%
                    lapply(., \(x) ifelse(is_empty(x), NA, x)) %>% unique_sans_na() %>%
                    as.integer() %>% sort() %>% as.character(),
                  statements = unique(d_gr$statement),
                  alignmentNotes = aNotes,
                  subcat = d_gr$subcat[1]
                )
              })#end Group lapply
              l_di[[di_i]] <-
                c(
                  slug = d_di$dim[1],
                  name = d_di$dimension[1],
                  standardsGroup = list(l_gr)
                )
            }#end dimension loop

            set_abbrev <- gsub("[a-z| ]", "", d_se$set[[1]])
            l_se[[se_i]] <-
              c(
                slug = set_abbrev,
                name = d_se$set[[1]],
                dimensions = list(l_di)
              )
          }#end set loop
          l_su[[su_i]] <-
            c(
              subject = as.character(d_su$subject[[1]]),
              target = d_su$target[[1]],
              sets = list(l_se)
            )
        }#end subject loop
        l_ta[[ta_i]] <- l_su
      }#end target loop


      out0 <- do.call(c, l_ta)


      # Create JSON-style list, but only exported as JSON by compile_lesson() --------
      # Prefix with component and title, and nest output in Data if structuring for web deployment
      out <-
        list(
          `__component` = "lesson-plan.standards",
          LearningObj =  fm$LearningObj,
          Data = out0
        )


      # return summary tibble --------------------------------------------------
      message(rep("=", 30), "\n\tSUMMARY\n", rep("=", 30))
      message(
        "\nStandards submitted:\t",
        nrow(a0),
        "\nRemoved due to issues:\t",
        sum(tbds) + sum(undoc),
        "\nSuccessfully compiled:\t",
        nrow(A),
        "\n"
      )
      message(rep("=", 30))


      # Prep learningEpaulette data ---------------------------------------------
      #create an empty matrix to merge in, in case some subjects are missing
      a_template <-
        a_master %>%
        #Get rid of sets not included in the alignment
        dplyr::filter(.data$set %in% c(unique(A$set))) %>%
        dplyr::select("subject", "dimension") %>%
        dplyr::distinct() %>% dplyr::mutate(n = 0)


      #super important to refactor subject on the imported data to ensure order

      a_template$subject = factor(a_template$subject,
                                  levels = ordered_subjects,
                                  ordered = T)

      a_summ <-
        A %>% dplyr::group_by(.data$subject, .data$dimension) %>%
        # filter out unsupported subjects
        # dplyr::filter(.data$set %in% supported_sets) %>%
        dplyr::tally()

      #gotta combine missing rows, sort, & repeat the entries N times
      a_combined <-
        dplyr::anti_join(a_template, a_summ, by = "dimension") %>%
        dplyr::bind_rows(a_summ) %>%
        dplyr::arrange(.data$subject, .data$dimension) %>%
        dplyr::mutate(binary = ifelse(.data$n > 0, 1, 0))



      #### This correction is kinda problematic as we add other standards to our database!
      #### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1

      #Account for bias in the number of standards
      bias <-
        a_master %>%
        #Get rid of sets not included in the alignment
        dplyr::filter(.data$set %in% unique(A$set)) %>%
        dplyr::group_by(.data$subject, .data$dimension) %>%
        dplyr::summarise(n = dplyr::n())


      bias_by_subj <-
        bias %>% dplyr::summarise(tot_n_subj = sum(.data$n, na.rm = T),
                                  .groups = "drop")
      a_combined <-
        dplyr::left_join(a_combined,
                         (bias %>% dplyr::rename("tot_n_dim" = "n")),
                         by = c("subject", "dimension"))
      a_combined <-
        dplyr::left_join(a_combined, bias_by_subj, by = c("subject"))

      #*Because there aren't an equal number of standards per dimension, (and they're not all equal),
      #*It's more intuitive to treat them as if they are all equal.
      #*So to make the correction, we'll weight the proportions by total N for subject
      a_combined$id = 1:nrow(a_combined)
      a_combined$n_adj <- a_combined$n / a_combined$tot_n_subj
      a_combined$n_prop <-
        a_combined$n / sum(a_combined$n, na.rm = T)
      a_combined$n_prop_adj <-
        a_combined$n_adj / sum(a_combined$n_adj, na.rm = T)

      #Remind r that a_combined factors are ORDERED
      a_combined$subject <-
        factor(a_combined$subject,
               levels = ordered_subjects,
               ordered = T)


      #Calculate corrected proportions if requested
      proportions0 = a_combined  %>% dplyr::group_by(.data$subject)
      if (learningplot_correction) {
        proportions = proportions0 %>% dplyr::summarise(proportion = round(sum(.data$n_prop_adj, na.rm =
                                                                                 T), 2), .groups =
                                                          "drop")
      } else{
        #don't use adjusted proportions if not requested
        proportions = proportions0 %>% dplyr::summarise(proportion = round(sum(.data$n_prop, na.rm =
                                                                                 T), 2), .groups =
                                                          "drop")
      }

      #Make sure proportions = 100%

      tot_prop <- sum(proportions$proportion)
      if (tot_prop != 1) {
        remaining <- 1 - tot_prop
        where_add <- proportions$proportion %>% which.max()
        message(
          "In output for learningEpaulette(), adding ",
          round(remaining, 2),
          " to ",
          proportions$subject[where_add],
          " so proportions add to 100%"
        )
        proportions$proportion[where_add] <-
          proportions$proportion[where_add] + remaining
      }

      #Set up vector to recode subject levels according to key (for learning chart subject abbrevs.)
      convert_labels <- ordered_subj_chart
      names(convert_labels) <- ordered_subjects

      convert_labels2 <- ordered_subj
      names(convert_labels2) <- ordered_subjects

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
      thickness = 0.2

      #I think learning chart can handle sustainability above...this epaulette code CANNOT...

      epaulette_names <-
        ordered_subj_chart[match(proportions$subject, ordered_subjects)]

      rectangles <-
        dplyr::tibble(
          proportion = proportions$proportion,
          xmin = c(0, cumsum(proportions$proportion)[-length(proportions$proportion)]),
          xmax = cumsum(proportions$proportion),
          ymin = 1 - thickness,
          ymax = 1,
          subject = epaulette_names
        ) %>% dplyr::filter(.data$proportion > 0)
      rectangles$subject <-
        factor(rectangles$subject,
               ordered = T,
               levels = epaulette_names)
      rectangles$border <- "transparent"

      #Ensure that xmax[4] is 1, so there's never a gap at the right
      rectangles$xmax[nrow(rectangles)] <- 1


      #boldenize & embiggenate if targetSubj indicated
      if (!is.null(targetSubj)) {
        (targetRows <-
           which(!is.na(charmatch(
             tolower(xlabels$subj), tolower(targetSubj)
           ))))
        xlabels$stroke[targetRows] <- 2
        xlabels$strokeCol[targetRows] <- gpColors("galactic black")
        xlabels$fontface[targetRows] <- "bold"
        xlabels$size[targetRows] <- 11
        rectangles$border[targetRows] <- gpColors("galactic black")
      }



      # prep for LearningChart  -------------------------------------------------


      #check if standards contain an unsupported set of standards
      which_alignment_supported <-
        unique_sans_na(A$set) %in% supported_sets

      test_alignment_supported <-
        sum(which_alignment_supported) > 0

      if (!test_alignment_supported) {
        warning(
          "No Learning Chart created. Currently supported Standards sets:\n  -",
          paste(supported_sets, collapse = "\n  -"),
          "\nStandard sets found:\n  -",
          paste(unique_sans_na(A$set),
                collapse = "\n  -")
        )
        #Store LearningChart Friendliness in front-matter and also save to RDS file
        update_fm(change_this = list(LearningChartFriendly = F),
                  WD = WD)
        learning_chart_friendly <- FALSE
        a_combined$dimAbbrev <- NA
      } else{
        update_fm(change_this = list(LearningChartFriendly = T),
                  WD = WD)
        learning_chart_friendly <- TRUE




        #Make a custom dimAbbrev tibble for sets supported for learning Chart
        supported_dims <- a_master %>% dplyr::select(subject,dimension) %>% dplyr::distinct(.data$dimension,.keep_all = T) %>% dplyr::arrange(.data$subject,.data$dimension)

        #Manual abbreviations for long dimensions
        supported_dims$dimAbbrev <-sapply(supported_dims$dimension,function(x) switch(x,
            #CCSS Math
            "Algebra, Geometry, Trig, Calculus & Higher Level Thinking"= "Algebra, Geometry,\n Trig, Calculus,\n Other Adv Math",
            "Measurement, Data, Probability & Statistics"= "Measurement, Data,\n Probability, Statistics",
            "Number Systems, Operations & Abstract Representation" ="Number Systems, Operations,\n Symbolic Representation",
            #CCSS ELA
            "Language, Speaking & Listening"= "Language, Speaking,\n Listening",
            "Reading"= "Reading",
            "Writing" = "Writing",
            #NGSS
            "Cross-Cutting Concepts"="Cross-Cutting \n Concepts ",
            "Disciplinary Core ideas"= "Disciplinary\n Core Ideas",
            "Science & Engineering Practices"= "Science & Engineering\n Practices",
            #C3 Soc Studies
            "Civics, Economics, Geography & History" = "Civics, Economics,\n Geography, History",
            "Developing Questions & Planning Inquiries" = "Develop Questions,\n Plan Inquiries",
            "Evaluating Sources, Communicating Conclusions & Taking Action" = "Evaluate, \n Communicate, \n Take Action ",
            #else
            x
            )) %>%
          paste0(" ",.) #pad labels with space for alignment

      }


      #Add abbrev to a_combined output
      a_out <- a_combined %>% dplyr::full_join(.,
                                      supported_dims )





      # Save Standards Data -----------------------------------------------------
      toSave <- list(
        data = list(
          input = dplyr::as_tibble(a0),
          compiled = dplyr::as_tibble(A),
          problem_entries = dplyr::as_tibble(a0[(tbds + undoc) > 0,]),
          gradeBand = gradeBand,
          list_for_json = out
        ),
        a_combined = a_out,
        xlabels = xlabels,
        rectangles = rectangles,
        targetSubj = targetSubj,
        learning_chart_friendly = learning_chart_friendly
      )
      #

      rds_saveFile <- fs::path(WD_git, "saves", "standards.RDS")
      message("Saving compiled standards data to '", rds_saveFile, "'")
      saveRDS(toSave, file = rds_saveFile)

      json_saveFile <-
        fs::path(WD_git, "JSONs", "standards.json")
      message("Saving web-formatted standards to '", json_saveFile, "'")
      save_json(out, json_saveFile)

      problem_entries <- dplyr::as_tibble(a0[(tbds + undoc) > 0,])
      #need to build better checks than this
      success = TRUE
    }
  }#End Big else

  #add grades information to output
  return(
    list(
      success = success,
      input = dplyr::as_tibble(a0),
      compiled = dplyr::as_tibble(A),
      problem_entries = problem_entries,
      gradeBand = gradeBand,
      learningObj = fm$LearningObj,
      targetSubj = targetSubj,
      subject_proportions = proportions,
      learning_chart_friendly = learning_chart_friendly
    )
  )


}#end compile_alignment function def
