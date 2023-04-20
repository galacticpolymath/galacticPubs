#' compile_fm
#'
#' compile front-matter found in /meta/front-matter.yml
#'
#' Produces the following json outputs in /meta/JSON/:
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

compile_fm <- \(WD=getwd()){
WD <- parse_wd(WD)

#Include everything down to SponsoredBy in the header
      header <-
        current_data[(1:which(names(current_data) == "SponsoredBy"))]
      #make full catalog paths following naming conventions the frontend expects

      header$SponsorImage = list(url = ifelse(
        is.na(current_data$SponsorLogo),
        NA,
        catalogURL(basename(current_data$SponsorLogo), repo)
      ))
      header$CoverImage = list(url = ifelse(
        is.na(current_data$LessonBanner),
        NA,
        catalogURL(basename(current_data$LessonBanner), repo)
      ))


      overview <- list(
        `__component` = "lesson-plan.overview",
        EstLessonTime = current_data$EstLessonTime,
        GradesOrYears = current_data$GradesOrYears,
        ForGrades = current_data$ForGrades,
        TargetSubject = current_data$TargetSubject,
        #lump the Driving Questions, Essential Questions, Learning Objectives, etc into one text element

        Text = lumpItems(
          c(
            "DrivingQ",
            "EssentialQ",
            "Hooks",
            "LearningSummary",
            "MiscMD"
          ),
          item.labs = c(
            "Driving Question(s):",
            "Essential Question(s):",
            "Hook(s):",
            "Learning Summary:",
            ""
          ),
          list.obj = current_data,
          new.name = "Text"
        )$Text,
        Tags = lapply(current_data$Tags, function(x)
          list(Value = x)),
        SteamEpaulette = list(url = ifelse(
          is.na(current_data$LearningEpaulette[1]),
          NA,
          catalogURL(basename(current_data$LearningEpaulette[1]), repo)
        )),
        #might want to add more complex image handling later),
        SteamEpaulette_vert = list(url = ifelse(
          is.na(current_data$LearningEpaulette_vert[1]),
          NA,
          catalogURL(basename(
            current_data$LearningEpaulette_vert[1]
          ), repo)
        )),
        #might want to add more complex image handling later),
        Description = current_data$Description %>% fixAnchorLinks()
      ) #allow smooth-scrolling to in-page references

      #read in multimedia file created from multimedia tab of teaching-materials.xlsx if that file exists
      mmExists <-
        file.exists(fs::path(WD, "meta", "JSON", "multimedia.json"))
      if (mmExists) {
        mm <-
          jsonlite::read_json(fs::path(WD, "meta", "JSON", "multimedia.json"), null =
                                "null")
      } else{
        mm <- NULL
        message("No multimedia found.")
      }


      #PREVIEW
      preview <- list(
        `__component` = "lesson-plan.lesson-preview",
        SectionTitle = "Lesson Preview",
        QuickPrep = current_data$QuickPrep %>% fixAnchorLinks(),
        #allow smooth-scrolling to in-page references
        Multimedia = mm,
        InitiallyExpanded = TRUE
      )
      #write preview json
      jsonlite::write_json(
        preview,
        path = fs::path(destFolder,
                        "preview", ext = "json"),
        pretty = TRUE,
        auto_unbox = TRUE,
        na = "null",
        null = "null"
      )

      #BONUS (optional section)
      # markdown links to supporting materials allowed
      if (!is_empty(current_data$Bonus)) {
        bonus <- list(
          `__component` = "lesson-plan.collapsible-text-section",
          SectionTitle = "Bonus Content",
          Content = expand_md_links(current_data$Bonus, repo) %>% fixAnchorLinks(),
          #allow smooth-scrolling to in-page references
          InitiallyExpanded = TRUE
        )
        jsonlite::write_json(
          bonus,
          path = fs::path(destFolder, "bonus", ext = "json"),
          pretty = TRUE,
          auto_unbox = TRUE,
          na = "null",
          null = "null"
        )
      }

      #EXTENSIONS (optional section)
      # markdown links to supporting materials allowed
      if (!is_empty(current_data$Extensions)) {
        extensions <- list(
          `__component` = "lesson-plan.collapsible-text-section",
          SectionTitle = "Extensions",
          Content = expand_md_links(current_data$Extensions, repo) %>% fixAnchorLinks(),
          #allow smooth-scrolling to in-page references
          InitiallyExpanded = TRUE
        )
        jsonlite::write_json(
          extensions,
          path = fs::path(destFolder, "extensions", ext = "json"),
          pretty = TRUE,
          auto_unbox = TRUE,
          na = "null",
          null = "null"
        )
      }

      #Combine Sci Background and Lesson Connections to Research
      # markdown links to supporting materials allowed
      # expand_md_links takes relative links in [](x.jpg) format and makes a full path to GP catalog
      # parseGPmarkdown allows references to {vid1} videos listed in the multimedia tab of the teaching-materials.xlsx file
      # BACKGROUND
      if (!is_empty(current_data$Background)) {
        background <-
          list(
            `__component` = "lesson-plan.collapsible-text-section",
            SectionTitle = "Background",
            Content = ifelse(
              is.na(current_data$ConnectionToResearch),
              current_data$Background,
              paste(
                "#### Connection to Research\n",
                current_data$ConnectionToResearch,
                "\n#### Research Background\n",
                current_data$Background
              )
            ) %>% expand_md_links(repo = repo) %>%
              fixAnchorLinks() %>% parseGPmarkdown(WD = WD),
            InitiallyExpanded = TRUE
          )

        jsonlite::write_json(
          background,
          path = fs::path(destFolder,
                          "background", ext = "json"),
          pretty = TRUE,
          auto_unbox = TRUE,
          na = "null",
          null = "null"
        )
      }

      # FEEDBACK
      if (!is_empty(current_data$Feedback)) {
        feedback <-
          list(
            `__component` = "lesson-plan.collapsible-text-section",
            SectionTitle = "Feedback",
            Content = expand_md_links(current_data$Feedback,
                                      repo) %>% fixAnchorLinks(),
            InitiallyExpanded = TRUE
          )

        jsonlite::write_json(
          feedback,
          path = fs::path(destFolder,
                          "feedback", ext = "json"),
          pretty = TRUE,
          auto_unbox = TRUE,
          na = "null",
          null = "null"
        )
      }

      #CREDITS
      if (!is_empty(current_data$Credits)) {
        credits <-
          list(
            `__component` = "lesson-plan.collapsible-text-section",
            SectionTitle = "Credits",
            Content = expand_md_links(current_data$Credits,
                                      repo) %>% fixAnchorLinks(),
            InitiallyExpanded = TRUE
          )

        save_json(credits,
                  filename = fs::path(destFolder,
                                      "credits", ext = "json"))
      }

      #always output this stuff
      save_json(header,
                filename = fs::path(destFolder,
                                    "header", ext = "json"))
      save_json(overview,
                filename = fs::path(destFolder,
                                    "overview", ext = "json"))


}

#' fm_compile
#'
#' alias for for [compile_fm()]
#'
#' @describeIn compile_fm
#'
#' @export

fm_compile <- compile_fm
