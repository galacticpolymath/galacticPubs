#' Compile all the JSONs to create 1 file for the web
#'
#' Combine all JSON components into 1 UNIT.json
#'
#' @param WD working directory, passed to [parse_wd()]
#' @param WD_git location of gp-lessons github repo. Default=NULL will get this for the current workspace with [get_wd_git()]
#' @param destFolder where you want to save the folder; by default in the "WD_git" folder
#' @return tibble of the compiled standards data; a JSON is saved to meta/JSON/UNIT.json
#' @importFrom rlang .data
#' @export
#'
compile_json <- function(WD = NULL,
                         WD_git = NULL,
                         destFolder) {
  message("Recompiling UNIT.json ")
  if (!is.null(WD)) {
    WD <- parse_wd(WD)
  }

  if (is.null(WD_git)) {
    WD_git <- get_wd_git(WD = WD)
  }
  checkmate::assert_directory_exists(WD_git)

  LessonTitle <- get_fm("MediumTitle",WD_git = WD_git)

  if (missing(destFolder)) {
    destFolder = WD_git
  }

  srcFolder <- fs::path(WD_git, "JSONs")

  #   jsonNames should be ordered; this is telling which json files to look for and assemble them in this order
  jsonNames <-
    c(
      "header",
      "overview",
      "preview",
      "teachingMaterials",
      "feedback",
      "extensions",
      "bonus",
      "background",
      "standards",
      "credits",
      "acknowledgments"
    )

  potentialFilenames <- paste0(jsonNames, ".json")

  #test for missings or duplicates
  json_ls <- list.files(srcFolder)

  matches <-
    data.frame(file = potentialFilenames, found = potentialFilenames %in% json_ls)
  format(matches, justify = "none")
  #point out missing sections
  if (sum(matches$found) < length(jsonNames)) {
    missingJSON <- subset(matches, !matches$found)$file
    message("\n\tFYI, you're missing:\n\t -",
            paste(missingJSON, collapse = "\n\t -"),
            "\n")
  }

  filenamez.df <- subset(matches, matches$found)
  #read in all the json pieces
  lesson_data <- lapply(filenamez.df$file, function(x) {
    jsonlite::read_json(fs::path(srcFolder, x), na = "null", null = "null")
  })
  names(lesson_data) <-
    gsub("^(.*)\\..*", "\\1", filenamez.df$file) #removes file extension


  #body of the lesson plan (minus header)
  lesson_body <-
    list(lapply(2:length(lesson_data), function(x) {
      lesson_data[[x]]
    }))

  names(lesson_body[[1]]) <- names(lesson_data)[-1]

  #reorganize slightly to match legacy structure
  lesson <- c(
    lesson_data[["header"]],
    Sections = lesson_body,
    SponsorImage = lesson_data[["images"]]$SponsorImage
  )


  # create directory if necessary & prep output filename --------------------
  dir.create(destFolder, showWarnings = FALSE, recursive = T)
  outFile <- fs::path(destFolder, "UNIT.json")


  # Write JSON for GP Simple Lesson Plan -----------------------------------
  save_json(lesson, outFile)


  # return compiled output --------------------------------------------------
  message(" ", rep("-", 30), "\n Lesson successfully compiled:\n  ",LessonTitle)
  # print(printToScreenTable)
  message("\n Combined JSON file saved\n @ ", outFile, "\n")
  message(" ", rep("-", 30))


}
