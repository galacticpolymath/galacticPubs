#' Copy updated assets in a specific folder to a destination
#'
#' Helper for [upload_assets()].
#'
#' @param WD what's the project working directory? passed to parse_wd()
#' @param rel_path what's the subfolder path within WD? e.g."assets/_learning-plots"
#' @param pattern a regular expression for allowable file types; default=NULL
#' @param exclude a regular expression to exclude from uploading; default=NULL excludes nothing; ".ai$" would exclude Adobe Illustrator files for example.
#' @param dest_folder full path to where you want things to go (defaults to WD/published)
#' @param clear do you want to delete everything in the target directory? default= FALSE
#' @export

stage_assets <-
  function(WD,
           rel_path,
           pattern = NULL,
           exclude = NULL,
           dest_folder = NULL,
           clear = FALSE) {
    WD <- parse_wd(WD)

    current_data <- get_fm(WD = WD)


    #this defaults to published
    if (is.null(dest_folder)) {
      dest_folder <- fs::path(WD, "published")
    }

    #check if published folder exists
    if (!dir.exists(dest_folder)) {
      dir.create(dest_folder)
      message("@ Folder Created: ", dest_folder)
      clear <- FALSE #no need to clear folder we're just creating
    }

    #copy images over to dest_folder folder for previewing
    #list front-matter items that point to necessary assets for the publishing bundle
    # items2copy <-
    #   c(
    #     "LessonBanner",
    #     "SponsorLogo",
    #     "LearningEpaulette",
    #     "LearningEpaulette_vert",
    #     "LearningChart",
    #     "SupportingMedia"
    #   )
    full_path <- fs::path(WD, rel_path)


# Check if there's anything to copy ---------------------------------------

    to_copy <-
      dplyr::tibble(
        path = fs::dir_ls(full_path),
        name = basename(.data$path)
      ) %>%
      dplyr::relocate("name")
    #not sure why the regexp parameter in dir_ls() doesn't work...
    if(!is.null(pattern)){
      to_copy <- to_copy %>%
        dplyr::filter(grepl(pattern,.data$name))
    }

    if (!is.null(exclude)) {
      to_copy <- to_copy %>%
        dplyr::filter(!grepl(exclude, .data$name))
    }

    # clear target directory if requested and copy updated files
    if(nrow(to_copy)==0){
      out <- NULL
    }else{
    out <-copy_updated_files(paths = to_copy$path, dest_folder, clear = clear,WD=WD) %>%
      catch_err(keep_results = TRUE)
    }

    return(invisible(out))

  }
