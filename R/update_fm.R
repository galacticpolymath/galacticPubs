#' update_fm (Update Front Matter)
#'
#' Checks for new galacticPubs front-matter_TEMPLATE.yml. If found, it will add new fields,  return the updated object, and write to drive (meta/front-matter.yml) if requested.
#'
#' If meta/front-matter.yml not found, it is created from the template. Will also combine language and country info to create locale, and add GPCatalogPath if those fields are blank. Attempts to find the lesson on Google Drive in GP-Workshop and add GdriveDirID if missing.
#'
#' @param WD Working drive; default=getwd()
#' @param save_output do you want to save the updated front-matter to WD/meta/front-matter.yml? Default=TRUE
#' @param reorder do you want to reorder the resulting list, based on template order? default=TRUE
#' @param change_this A list of values to change in the front matter. Default=NULL. Example: list(RebuildAllMaterials=TRUE,Language="Italian) will trigger a full lesson rebuild when [compile_lesson()] is run and change the Language and locale.
#' @return silently returns updated front-matter.yml object as a list
#' @export
#'

update_fm <-
  function(WD = getwd(),
           save_output = TRUE,
           reorder = TRUE,
           change_this = NULL) {
    yaml_path <- fs::path(WD, "meta", "front-matter.yml")

    #safe_read_yaml will create yaml if it's missing
    old_yaml <- safe_read_yaml(yaml_path)

    galacticPubs_template <-
      safe_read_yaml(yaml_path = system.file("extdata",
                                             "front-matter_TEMPLATE.yml",
                                             package = "galacticPubs"))
    new_yaml <-
      add_missing_fields(old_yaml, galacticPubs_template, reorder = reorder)

    # Make manual changes if requested ----------------------------------------
    if (!is.null(change_this)) {
      for (i in 1:length(change_this)) {
        element_i <- names(change_this)[i]
        new_yaml[[element_i]] <- change_this[[i]]
      }
    }


    # If front-matter exists,  do certain routine processes -------------------
    #Add/Update the locale and lang fields with a nonexported internal function parse_locale()
    # overwrites existing lang and locale fields and returns the modified current_data list
    new_yaml <- new_yaml %>% parse_locale()
    # overwrite MediumTitle used for sensible folder naming in public-facing GalacticPolymath network drive
    new_yaml$MediumTitle<-paste0(paste0("'",new_yaml$Title,"'"),
                                 ifelse(is_empty(new_yaml$TargetSubject),"",paste0("_",new_yaml$TargetSubject,"_")),
                                 ifelse(is_empty(new_yaml$ForGrades),"",paste0(new_yaml$ForGrades," ")),
                                 "(",
                                 new_yaml$locale,
                                 ")")

    #Add path to this lesson for once it's published to gp-catalog (if it doesn't exist)
    if (is.na(new_yaml$GPCatalogPath) |
        is.na(new_yaml$GdriveDirName)) {
      repo <- whichRepo(WD = WD)

      checkmate::assert_character(repo,any.missing=FALSE)

      new_yaml$GdriveDirName <- basename(WD)
      new_yaml$GPCatalogPath <- catalogURL("LESSON.json", repo)
    }

    #Add Gdrive ID and URL if one is missing
    if (is.na(new_yaml$GdriveDirID) |
        is.na(new_yaml$GdriveDirURL)|
        is.na(new_yaml$GdriveMetaID)|
        is.na(new_yaml$GdrivePublishedID)) {
      #try to find path for the project name
      message(
        "\nTrying to link local virtual lesson '",
        new_yaml$GdriveDirName,
        "' to its cloud Google Drive ID...\n"
      )

      proj_dribble_test <-
        drive_find_path(paste0("GP-Workshop/Edu/Lessons/", new_yaml$GdriveDirName)) %>%
        catch_err(keep_results = TRUE)
      if (proj_dribble_test$success) {
        proj_dribble <- proj_dribble_test$result
        checkmate::assert(checkmate::check_class(proj_dribble, "dribble"),
                          .var.name = "GdriveDirName")

        new_yaml$GdriveDirID <- proj_dribble$id
        new_yaml$GdriveDirURL <-
          googledrive::drive_link(proj_dribble)

        #now look up other subfolders
        gMetaID<-drive_find_path("../meta", root=new_yaml$GdriveDirID) %>% catch_err(keep_results=T)
        new_yaml$GdriveMetaID<-gMetaID$result$id
        gPublishedID<-drive_find_path("../published", root=new_yaml$GdriveDirID) %>% catch_err(keep_results=T)
        new_yaml$GdrivePublishedID<-gPublishedID$result$id

        checkmate::assert(
          checkmate::check_character(new_yaml$GdriveDirID),
          checkmate::check_character(new_yaml$GdriveDirURL),
          checkmate::check_character(new_yaml$GdriveMetaID),
          checkmate::check_character(new_yaml$GdrivePublishedID),
          combine = "and"
        )
        message("GdriveDirID, GdriveDirURL, GdriveMetaID, & GdrivePublishedID added to front-matter.yml")
      }

    }

    #test if it's a new version
    version_bumped <-
      old_yaml$TemplateVer != galacticPubs_template$TemplateVer
    if (!version_bumped) {
      # message("\nfront-matter.yml template v.",old_yaml$TemplateVer," is up-to-date with galacticPubs v.",as.character(utils::packageVersion("galacticPubs")))

      #otherwise change TemplateVer and let user know it's been upgraded
    } else{
      #reassign new templatever
      new_yaml$TemplateVer <- galacticPubs_template$TemplateVer
      message(
        "\nfront-matter.yml template will be upgraded upon save: ",
        old_yaml$TemplateVer,
        "->",
        new_yaml$TemplateVer
      )
    }



    #save updated file if requested
    if (save_output) {
      #Change LastUpdated field
      new_yaml$LastUpdated <- Sys.time() %>% as.character()
      yaml::write_yaml(new_yaml, yaml_path)
      message("\n@ Updated meta/front-matter.yml saved to disk.")
    }



invisible(new_yaml)
}
