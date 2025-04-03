#' compile_standards
#'
#' Does the following:
#' 1. Compile alignment info from the `standards_*.gsheet` in the project's `meta/` folder
#' 2. Output standards.RDS file with the alignment info (this will be read in by [learningEpaulette()] )

#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment); If you put "?", it will invoke [pick_lesson()]
#' @param targetSubj which subject(s) are the focus of the lesson? opts= "math","ela","science","social studies"; default=NULL
#' @return list with 4 objects: $success (did it work?); $input (the input file as a tibble); $compiled (the compiled tibble); $problem_entries (a tibble of entries with 'skip' or missing values in the "How this aligns..." column). A JSON is saved to the destFolder location.
#' @export
#'
compile_standards <- function(WD = "?", targetSubj = NULL) {
  . = NULL #to avoid errors with dplyr syntax
  message("compiling standards...")
  #The google drive working directory for the project assets
  WD <- parse_wd(WD)

  #The github gp-lessons directory for the code

  WD_git <- get_wd_git(WD = WD)

  #authenticate with default email for this user
  oauth_email <- Sys.getenv("galacticPubs_gdrive_user")
  checkmate::assert_string(oauth_email, .var.name = "galacticPubs_gdrive_user")
  googledrive::drive_auth(email = oauth_email)
  googlesheets4::gs4_auth(email = oauth_email)


  #############
  # IMPORTANT: Add Subjects here if you need to align new ones --------------


  ordered_subjects <-
    c(
      "Math",
      "ELA",
      "Science",
      "Social Studies",
      "Art",
      "Sustainability",
      "SEL",
      "Technology"
    )
  #learning chart/learningEpaulette subject titles (<=5 char)
  ordered_subj_chart <-
    c("Math", "ELA", "Sci"    , "SocSt", "Art", "SDGs", "SEL", "Tech")



  #here for legacy reasons (for gpColors)
  ordered_subj <-
    c("math",
      "ela",
      "science",
      "socstudies",
      "art",
      "sust",
      "sel",
      "tech")

  #custom labels for learning chart quadrants
  learning_chart_labs <-
    c(
      "CCSS\nMath",
      "CCSS\nELA",
      "NGSS\nScience",
      "C3\nSoc Studies",
      "Art",
      "SDGs\nSustain.",
      "SEL",
      "Tech"
    )

  #named short and full names for epaulette and learningchart
  chart_labels <- dplyr::tibble(
    full_subj = ordered_subjects,
    abbrev_subj = ordered_subj_chart,
    gp_pal_subj = ordered_subj,
    learning_chart_labs = learning_chart_labs
  )



  subj_tib <- dplyr::tibble(Subjects = ordered_subjects,
                            Subj = ordered_subj_chart,
                            subj = ordered_subj) %>%
    dplyr::mutate(n = 1:dplyr::n()) %>% dplyr::relocate(.data$n) %>%
    dplyr::mutate(color = gpColors(.data$subj))


  #define paths
  destFolder <- fs::path(WD_git, "JSONs")

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
      if (!is_empty(tempSubj)) {
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
      dplyr::select(1:"Notes") %>%
      dplyr::select(-"Notes") %>%
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
      update_fm(WD = WD,
                drive_reconnect = TRUE,
                recompile = FALSE)
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

    #Rename learning statement to be friendlier, but in a way that's robust to changing the header title
    lo_col <-
      stringr::str_detect(names(LOs), "Objective") %>% which()
    names(LOs)[lo_col] <- "lo_statement"
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
      success <- test_json <- test_save <- FALSE
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
          "lsn",
          "target",
          "grp",
          "how"
        )

      #for convenience, just make master columns (i.e. standards_*.gsheet!2.Standard-Selection) lower case
      names(a_master) <- tolower(names(a_master))



      # Extract learning objectives by lesson -------------------------------------
      LOs
      if (nrow(LOs) > 0) {
        LO_tib <- LOs %>%
          dplyr::select("LO#", "Lsn", "lo_statement") %>%
          tidyr::separate_longer_delim(cols = "Lsn", delim = ",") %>%
          dplyr::arrange(.data$Lsn, .data$`LO#`) %>%
          dplyr::select(-"LO#")
        learningObj <- lapply(unique(LO_tib$Lsn), \(i) {
          LO_tib %>% dplyr::filter(.data$Lsn == i) %>% dplyr::pull("lo_statement")
        })

      } else{
        learningObj <- NULL
      }


      # Check supported subjects ------------------------------------------------
      supported_subjects <-
        c("ELA",
          "Math",
          "Science",
          "Social Studies",
          "SEL",
          "Sustainability")

      #required subjects for learning chart
      req_subjects <- c("ELA", "Math", "Science", "Social Studies")

      #supported sets of standards for generating learning chart
      #Also treated as REQUIRED for learning chart output
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


      # manage "skip" and flagged, undocumented alignments ------------------------
      #useful if you have multiple locales aligned in same doc.
      #skip one country's outputs in 1 version
      skips <- grepl("skip", a0$how, ignore.case = TRUE)

      if (sum(skips) > 0) {
        message(
          "\nThe following were removed because Learning Objective documentation contained 'skip':\n\t\u2022",
          paste0(a0$code[skips], collapse = "\n\t\u2022"),
          "\n"
        )
      } else{
        skips <- rep(FALSE, nrow(a0))
      }

      #undocumented alignments
      #target has blank for "how this lesson aligns"

      undoc <-
        (is.na(a0$how))
      #If you got more than 1 alignment, and they're just not documented yet, don't remove them, just report message
      # undoc_ok <- sum(undoc)==nrow(a0) & nrow(a0)>1

      #making undoc always ok
      undoc_ok <- TRUE

      if (sum(undoc) > 0 & !undoc_ok) {
        message(
          "\nThe following were removed because 'How does lesson align...' was blank and undoc_ok=FALSE :\n\t\u2022",
          paste0(a0$code[undoc], collapse = "\n\t\u2022"),
          "\n"
        )
        a1 <- a0[!undoc & !skips, ]
      } else if (undoc_ok) {
        message(
          "\nWe kept these aligned standards in, but they're NOT been documented! Fill in 'How does lesson align...' for:\n\t\u2022",
          paste0(a0$code[undoc], collapse = "\n\t\u2022"),
          "\n"
        )
        a1 <- a0[!skips, ]
      } else{
        undoc <- rep(FALSE, nrow(a0))
        a1 <- a0[!undoc & !skips, ]
      }




      # a2 has markdown bullets ("- ") added if missing
      # Add markdown bullet to front of lines that don't start with it
      # Figure out grade band(s)
      a2 <- a1

      #swap out bullets for - to maintain consistency in markdown syntax
      a2$how <- gsub("\u2022", "-", a2$how)
      # a2$how <-
      #   ifelse(!grepl("^- ", a2$how), paste0("- ", a2$how), a2$how)

      #Make sure any blanks have placeholder text
      a2$how <- ifelse(is.na(a2$how), "'How Aligned' not yet documented.", a2$how)




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

      #A is a merge of the provided alignment and the master reference document (with preference fo code defs, etc. from the provided standards reference Tab 2)
      #Remove "lsn from a_master to avoid conflicts with changes made in tab 4.Finalize"
      A0 <-
        dplyr::left_join(a3[, c("code_set",
                                "lo",
                                "lo_stmnt",
                                "target",
                                "grp",
                                "grouping",
                                "how",
                                "lsn")], a_master[-which(tolower(names(a_master)) ==
                                                           "lsn")], by = "code_set")


      #factor subjects for desired order

      A0$subject <-
        factor(A0$subject, levels = ordered_subjects, ordered = T)
      A <-
        A0 %>%  dplyr::arrange(.data$subject)


      # warn if statements missing (indicates bad merge) -----------------------
      if (nrow(A) == 0 & !undoc_ok) {
        warning("Bad merge. No 'Statements' matched standards code for each set.")
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

                #Get lessons assignments for this standard
                uniq_lessons <-
                  strsplit(unique(d_gr$lsn), split = ",") %>% unlist() %>% trimws() %>% unique() %>% sort()

                list(
                  lessons = as.list(uniq_lessons),
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


      # Create JSON-style list, but only exported as JSON by compile_unit() --------
      # Prefix with component and title, and nest output in Data if structuring for web deployment
      out <-
        list(`__component` = "lesson-plan.standards",
             SectionTitle= "Standards Section",
             sortOrder= 1,
             InitiallyExpanded=TRUE,
             Data = out0)


      # return summary tibble --------------------------------------------------
      message(rep("=", 30), "\n\tSUMMARY\n", rep("=", 30))
      message(
        "\nStandards submitted:\t",
        nrow(a0),
        "\nRemoved due to issues:\t",
        sum(skips) + sum(undoc),
        "\nSuccessfully compiled:\t",
        nrow(A),
        "\n"
      )
      message(rep("=", 30))



      #create an empty matrix to merge in, in case some subjects are missing
      a_template <-
        a_master %>%
        #Get rid of sets not included in the alignment
        #EXCEPT the 4 required sets
        dplyr::filter(.data$set %in% c(unique(A$set), supported_sets)) %>%
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

      # prep for LearningChart  -------------------------------------------------


      #check if standards contain an unsupported set of standards
      which_alignment_supported <-
        unique_sans_na(A$set) %in% supported_sets

      test_alignment_supported <-
        sum(which_alignment_supported) > 0

      if (!test_alignment_supported) {
        warning(
          "No Learning Chart will be created. Currently supported Standards sets:\n  -",
          paste(supported_sets, collapse = "\n  -"),
          "\nStandard sets found:\n  -",
          paste(unique_sans_na(A$set), collapse = "\n  -")
        )
        #Save to RDS file
        update_fm(WD = WD, recompile = FALSE)

        a_combined$dimAbbrev <- NA
        supported_dims <- NULL
      } else{
        update_fm(WD = WD, recompile = FALSE)





        #Make a custom dimAbbrev tibble for sets supported for learning Chart
        supported_dims <-
          a_master %>% dplyr::select(subject, dimension) %>% dplyr::distinct(.data$dimension, .keep_all = T) %>% dplyr::arrange(.data$subject, .data$dimension)

        #Manual abbreviations for long dimensions
        supported_dims$dimAbbrev <-
          sapply(supported_dims$dimension, function(x)
            switch(
              x,
              #CCSS Math
              "Algebra, Geometry, Trig, Calculus & Higher Level Thinking" = "Algebra, Geometry,\n Trig, Calculus,\n Other Adv Math",
              "Measurement, Data, Probability & Statistics" = "Measurement, Data,\n Probability, Statistics",
              "Number Systems, Operations & Abstract Representation" = "Number Systems, Operations,\n Symbolic Representation",
              #CCSS ELA
              "Language, Speaking & Listening" = "Language, Speaking,\n Listening",
              "Reading" = "Reading",
              "Writing" = "Writing",
              #NGSS
              "Cross-Cutting Concepts" = "Cross-Cutting \n Concepts ",
              "Disciplinary Core ideas" = "Disciplinary\n Core Ideas",
              "Science & Engineering Practices" = "Science & Engineering\n Practices",
              #C3 Soc Studies
              "Civics, Economics, Geography & History" = "Civics, Economics,\n Geography, History",
              "Developing Questions & Planning Inquiries" = "Develop Questions,\n Plan Inquiries",
              "Evaluating Sources, Communicating Conclusions & Taking Action" = "Evaluate, \n Communicate, \n Take Action ",
              #else
              x
            )) %>%
          paste0(" ", .) #pad labels with space for alignment

      }


      #Add abbrev to a_combined output
      if (!is.null(supported_dims)) {
        a_out <- a_combined %>% dplyr::full_join(., supported_dims)
      } else{
        a_out <- a_combined
      }





      # Save Standards Data -----------------------------------------------------
      toSave <- list(
        learningObj = learningObj,
        data = list(
          input = dplyr::as_tibble(a0),
          compiled = dplyr::as_tibble(A),
          problem_entries = dplyr::as_tibble(a0[(skips + undoc) > 0, ]),
          gradeBand = gradeBand,
          list_for_json = out
        ),
        a_combined = a_out,
        chart_labels = chart_labels,
        targetSubj = targetSubj
      )
      #

      rds_saveFile <- fs::path(WD_git, "saves", "standards.RDS")
      message("Saving compiled standards data to '", rds_saveFile, "'")
      test_save <-
        saveRDS(toSave, file = rds_saveFile) %>% catch_err()

      if (!test_save) {
        test_json <- FALSE
        success <- FALSE
      } else{
        json_saveFile <-
          fs::path(WD_git, "JSONs", "standards.json")
        message("Saving web-formatted standards to '",
                json_saveFile,
                "'")
        test_json <- save_json(out, json_saveFile) %>% catch_err()

        problem_entries <-
          dplyr::as_tibble(a0[(skips + undoc) > 0, ])
        #need to build better checks than this
        success <-  TRUE
      }
    }
  }#End Big else




  # Store Target Standards in fm--------------------------------------------------
  #only include targeted standards that are explicitly in targetSubj
  target_standards_df <- A %>% dplyr::filter(.data$target) %>%
    dplyr::select(c("subject", "code", "set", "dim")) %>%
    #targetSubj isn't validated, so if socstudies is used, might cause a prob
    dplyr::filter(tolower(.data$subject) %in% tolower(targetSubj))


  #Make into an array needed for front end

  target_standards_list <- lapply(unique(target_standards_df$set), \(set_i) {
    df_i <- target_standards_df %>% dplyr::filter(.data$set == set_i)
    if (nrow(df_i) == 0) {
      out <- NULL
    } else{
      #Custom for NGSS (b/c they don't really have codes; summarize)
      # Just want to figure out how many SEPs or CCCs there are and count them
      select_str <- c("SEP", "CCC")
      if (any(df_i$dim %in% select_str)) {
        summ_sep_ccc <- df_i %>%
          dplyr::count(.data$dim) %>%
          dplyr::mutate(code = dplyr::case_when(
            .data$dim == "SEP" ~ paste(n, "Science and Engineering Practices (SEPs)"),
            .data$dim == "CCC" ~ paste(n, "Cross-Cutting Concepts (CCCs)")
          ),subject="Science",set="NGSS") %>% dplyr::select(-"n") %>%
          dplyr::filter(!is.na(.data$code))

        df_i <- df_i %>% dplyr::filter(!.data$dim %in% select_str)  %>%
         dplyr::bind_rows(summ_sep_ccc)


      }

      out <- df_i %>% as.list() %>% purrr::list_transpose(simplify=FALSE)
      # names(out) <- df_i$set[1]
    }
    out
  }) %>% .[[1]]



  test_save_target_stds <- update_fm(WD=WD,
                                     change_this=
                                       list(TargetStandardsCodes=target_standards_list))

  message("Standards Compiled: ", success)
  #add grades information to output
  invisible(
    list(
      success = success,
      input = dplyr::as_tibble(a0),
      compiled = dplyr::as_tibble(A),
      problem_entries = problem_entries,
      gradeBand = gradeBand,
      learningObj = learningObj,
      targetSubj = targetSubj,
      target_subj_standards = target_standards_df,
      chart_labels = chart_labels
    )
  )


}#end compile_alignment function def
