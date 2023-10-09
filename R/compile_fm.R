#' compile_fm
#'
#' compile front-matter found in /meta/front-matter.yml
#'
#' Produces the following json outputs in github project `gp-lessons/Lessons/[PROJ_NAME]/JSONs`:
#' - header.json
#' - overview.json
#' - background.json
#' - credits.json
#' - acknowledgments.json
#' - versions.json
#'
#' @param WD working directory, passed to [parse_wd()]
#' @export
#' @return logical of success

compile_fm <- \(WD = "?") {
  WD <- parse_wd(WD)
  WD_git <- get_wd_git(WD=WD)
  #just make sure everything's updated in the cloud
  upload_assets(WD=WD)
  json_dir <- fs::path(WD_git, "JSONs")
  fm <- get_fm(WD_git = WD_git)
  fm_keys <- get_fm_names()
  which_fm_keys <- fm_keys[1:which(fm_keys == "GradesOrYears")]
  #Header includes everything up to GradesOrYears
  header <- fm[which_fm_keys]

  # Make a few assertions to require minimally functional header ------------
  checkmate::assert_character(fm$ShortTitle,
                              min.chars = 2,
                              any.missing = F)
  checkmate::assert_choice(fm$PublicationStatus, c("Proto", "Draft", "Live"))
  checkmate::assert_character(fm$locale, n.chars = 5, any.missing = F)


#
#   # make full catalog paths following naming conventions the frontend --------
#   header$SponsorImage = list(url = ifelse(is.na(fm$SponsorLogo),
#                                           NA,
#                                           catalogURL(basename(fm$SponsorLogo), WD=WD)))
#   header$CoverImage = list(url = ifelse(is.na(fm$LessonBanner),
#                                         NA,
#                                         catalogURL(basename(fm$LessonBanner), WD=WD)))

  #output header.json
  save_json(header,
            filename = fs::path(json_dir,
                                "header", ext = "json"))

  # Export overview.json ----------------------------------------------------
  overview <- list(
    `__component` = "lesson-plan.overview",
    LearningSummary = fm$LearningSummary,
    EstLessonTime = fm$EstLessonTime,
    GradesOrYears = fm$GradesOrYears,
    ForGrades = fm$ForGrades,
    TargetSubject = fm$TargetSubject,
    #lump the Driving Questions, Essential Questions, Learning Objectives, etc into one text element

    Text = lumpItems(
      c("DrivingQ",
        "EssentialQ",
        "Hooks",
        "MiscMD"),
      item.labs = c(
        "Driving Question(s):",
        "Essential Question(s):",
        "Hook(s):",
        ""
      ),
      list.obj = fm,
      new.name = "Text"
    )$Text,
    Tags = lapply(fm$Tags, function(x)
      list(Value = x)),
    SteamEpaulette =  fm$LearningEpaulette[1],
    #might want to add more complex image handling later),
    SteamEpaulette_vert = fm$LearningEpaulette_vert[1]

    #might want to add more complex image handling later),
  )


  save_json(overview,
            filename = fs::path(json_dir,
                                "overview", ext = "json"))






  # read in multimedia file created from multimedia tab of teach-it. --------

  mmExists <-
    file.exists(fs::path(json_dir, "multimedia.json"))
  if (mmExists) {
    mm <-
      jsonlite::read_json(fs::path(json_dir, "multimedia.json"), null =
                            "null")
    if(is_empty(mm)){
      message("No multimedia found.")
    }
  }else{
    mm <- NULL
    message("No multimedia found.")
  }





  # Create preview.json -----------------------------------------------------
  #Multimedia browser
  preview <- list(
    `__component` = "lesson-plan.lesson-preview",
    SectionTitle = "Lesson Preview",
    #allow smooth-scrolling to in-page references (with Anchor Links)
    QuickPrep = fm$QuickPrep %>% fixAnchorLinks(),
    Multimedia = mm,
    InitiallyExpanded = TRUE
  )

  #write preview json even if empty
  save_json(preview,
            filename = fs::path(json_dir,
                                "preview", ext = "json"))

  #BONUS (optional section)
  # markdown links to supporting materials allowed
  Bonus <- get_fm("Bonus", WD = WD)
  if(!is_empty(Bonus)){
  bonus_web <- list(
      `__component` = "lesson-plan.collapsible-text-section",
      SectionTitle = "Bonus Content",
      Content = expand_md_links(Bonus, WD=WD) %>% fixAnchorLinks(),
      #allow smooth-scrolling to in-page references
      InitiallyExpanded = TRUE
    )
    save_json(bonus_web,
              filename = fs::path(json_dir, "bonus", ext = "json"))
}

  # extensions.json ---------------------------------------------------------


  #EXTENSIONS (optional section)
  # markdown links to supporting materials allowed

  Extensions <- get_fm("Extensions", WD = WD)

  if (!is_empty(Extensions)) {
    extensions_web <- list(
      `__component` = "lesson-plan.collapsible-text-section",
      SectionTitle = "Extensions",
      Content = expand_md_links(Extensions, WD=WD) %>% fixAnchorLinks(),
      #allow smooth-scrolling to in-page references
      InitiallyExpanded = TRUE
    )
    save_json(extensions_web,
              filename = fs::path(json_dir, "extensions", ext = "json"))
  }

  # background.json ---------------------------------------------------------
  #Combine Sci Background and Lesson Connections to Research
  # markdown links to supporting materials allowed
  # expand_md_links takes relative links in [](x.jpg) format and makes a full path to GP catalog
  # parseGPmarkdown allows references to {vid1} videos listed in the multimedia tab of the teaching-materials.xlsx file
  # BACKGROUND

  Background <- get_fm("Background", WD = WD)
  C2R <- get_fm("ConnectionToResearch", WD = WD)
  if (!is_empty(Background)) {
    background_web <-
      list(
        `__component` = "lesson-plan.collapsible-text-section",
        SectionTitle = "Background",
        Content = ifelse(
          is.na(C2R),
          Background,
          paste(
            "#### Connection to Research\n",
            C2R,
            "\n#### Research Background\n",
            Background
          )
        ) %>% expand_md_links(WD = WD) %>%
          fixAnchorLinks() %>% parseGPmarkdown(WD = WD),
        InitiallyExpanded = TRUE
      )

    save_json(background_web,
              fs::path(json_dir,
                       "background", ext = "json"))
  }


  # feedback.json -----------------------------------------------------------
  Feedback <- get_fm("Feedback", WD = WD)

  if (!is_empty(Feedback)) {
    feedback_web <-
      list(
        `__component` = "lesson-plan.collapsible-text-section",
        SectionTitle = "Feedback",
        Content = expand_md_links(Feedback,
                                  WD=WD) %>% fixAnchorLinks(),
        InitiallyExpanded = TRUE
      )

    save_json(feedback_web,
              fs::path(json_dir,
                       "feedback", ext = "json"))
  }


  # credits.json ------------------------------------------------------------
  Credits <- get_fm("Credits", WD = WD)
  if (!is_empty(Credits)) {
    credits_web <-
      list(
        `__component` = "lesson-plan.collapsible-text-section",
        SectionTitle = "Credits",
        Content = expand_md_links(unlist(Credits),
                                  WD=WD) %>% fixAnchorLinks(),
        InitiallyExpanded = TRUE
      )

    save_json(credits_web,
              filename = fs::path(json_dir,
                                  "credits", ext = "json"))
  }


  # acknowledgments.json ----------------------------------------------------
  #
  ack <-
    get_fm("Acknowledgments", WD = WD,standardize_NA = T)[[1]] %>% dplyr::as_tibble()


  if (is_empty(ack,names_meaningful = F) ) {
    ack_out0 <- NULL
  } else{
    roles <- unique(ack$Role)

    ack_out0 <- list()
    for (i in 1:length(roles)) {
      #Also allow {vid} shortcodes
      role_i <-
        roles[i] %>% parseGPmarkdown(WD = WD) %>%
        expand_md_links(WD =WD)
      ack_i <- subset(ack, ack$Role == role_i)
      def_i <-
        ack_i$Role_def[1] %>%
        parseGPmarkdown(WD = WD) %>%
        expand_md_links(WD = WD)
      #capitalize first letter if necessary
      if (!substr(def_i, 1, 1) %in% LETTERS) {
        substr(def_i, 1, 1) <- toupper(substr(def_i, 1, 1))
      }
      # #put parentheses around definition if necessary
      # if (substr(def_i, 1, 1) != "(") {
      #   def_i <- paste0("(", def_i, ")")
      # }
      persons_i <- lapply(1:nrow(ack_i), function(row) {
        tmp <-
          list(
            ack_i$Name[row],
            ack_i$Link[row],
            ack_i$Title[row],
            ack_i$Affiliation[row],
            ack_i$Location[row]
          )
        names(tmp) <-
          c("name", "url", "title", "affiliation", "location")
        tmp
      })


      ack_out0[[i]] <- c(role = role_i,
                         def = def_i,
                         records = list(persons_i))
    }
  }

  # Prefix with component and title, and nest output in Data if structuring for web deployment
  ack_out <-  list(`__component` = "lesson-plan.acknowledgments",
                   SectionTitle = "Acknowledgments",
                   Data = ack_out0)


  save_json(ack_out, fs::path(json_dir, "acknowledgments.json"))



  # versions.json -----------------------------------------------------------

  ver <-
    get_fm("Versions", WD = WD,standardize_NA = TRUE)[[1]] %>% dplyr::as_tibble()

  if (is_empty(ver)) {
    ver_out0 <- NULL
  } else{
    # ver$date <-
    #   sapply(ver$date, function(x) {
    #     as.character(as.Date(as.numeric(x), origin = "1899-12-30"), format = "%b %d, %Y")
    #   }, USE.NAMES = FALSE)
    ver$major <- gsub("(^[^\\.]*)\\..*", "\\1", ver$ver_num)
    #Change 0 release to beta for hierarchy
    ver$major <-
      sapply(ver$major, function(x)
        if (x == 0) {
          x <- "Beta"
        } else{
          x <- x
        })
    ver_out0 <- list()
    for (mjr in 1:length(unique(ver$major))) {
      ver_mjr <- subset(ver, ver$major == unique(ver$major)[mjr])
      out_mjr <- list()
      for (i in 1:nrow(ver_mjr)) {
        ver_i <- ver_mjr[i, ]
        out_mjr[[i]] <-
          list(
            version = ver_i$ver_num,
            date = ver_i$date,
            summary = ver_i$ver_summary,
            notes = ver_i$ver_notes,
            acknowledgments = ver_i$ver_acknowledgments
          )
      }
      ver_out0[[mjr]] <-
        list(major_release = unique(ver$major)[mjr],
             sub_releases = (out_mjr))
    }
  }


  # Prefix with component and title, and nest output in Data if structuring for web deployment
  ver_out <- list(`__component` = "lesson-plan.versions",
                  SectionTitle = "Version Notes",
                  Data = ver_out0)

  save_json(ver_out, fs::path(json_dir, "versions.json"))


  message("front-matter compiled")

}

#' fm_compile
#'
#' alias for for [compile_fm()]
#'
#' @describeIn compile_fm
#'
#' @export

fm_compile <- compile_fm
