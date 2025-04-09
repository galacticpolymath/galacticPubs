#' batch [compile_unit()]
#'
#' Function to rebuild a lesson or lessons that found in a virtualized Google Drive for Desktop folder. It will also stage the lesson in the published folder if requested, but will not publish. To do that, you should call [batch_publish()]
#'
#' This assumes that you have Google Drive for Desktop set up and have permissions to access the lesson files.
#'
#' @param shared_drive passed to [pick_lesson()] which shared drive do you want to find the lessons in? default= "s" Options:
#' - "s" or "?" = GP-Studio (draft working directory, many users with access)
#' - "l" or "??" = GP-Live (private, admin only)
#' @param change_this A list of values to change in the front matter before rebuilding. Default=NULL. Example: list(Title="Stormy Misty's Foal") would change the title of the lesson to the name of a horsey novel. If gh_proj_name=="all", make sure you set this to something you want to change for everything.
#' @param clean Do you want to clean the meta/JSON folder and build everything from scratch? (Gets passed to [compile_unit()]). Default=FALSE
#' @param rebuild Do you want to force rebuild everything (even if a particular item seems up to date?) default=FALSE (This par gets passed on as rebuild to [compile_unit()])
#'
#' @export
#'
#'
batch_compile <-
  function(shared_drive = "s",
           change_this = NULL,
           clean = FALSE,
           rebuild = FALSE) {
    timer <- FALSE
    #If Suggested tictoc package is available, time how long the rebuild takes
    if (requireNamespace("tictoc")) {
      tictoc::tic()
      timer <- TRUE
    }

    projects <-
      pick_lesson(shared_drive = shared_drive, show_all = TRUE)


    # #Now validate these projects as another safeguard (using unexported function)
    # good_projects <-
    #   projects[validate_lesson_dir(projects)] %>% sort()
    # if (length(good_projects) == 0) {
    #   stop("Invalid lesson project.")
    # }

    update_list <- lapply(projects, function(WD) {
      #1. compile all lessons of the unit
      compile_success <-
        compile_unit(WD = WD,
                     rebuild = rebuild,
                     clean = clean) %>% catch_err()

      dplyr::tibble(Compiled = convert_T_to_check(compile_success),
                    Lesson = basename(WD))
    })


    hl <- paste0(c("\n", rep("_", 30), "\n"), collapse = "")
    message(paste0(hl, paste0("Lesson Rebuilding Summary:\n")))
    print(update_list %>% dplyr::bind_rows())
    message(hl)
    # message(paste0(hl,
    #                paste0("Lessons rebuilt:\n - ",
    #         paste0(basename(good_projects),collapse="\n - "),
    #         hl)))

    #turn off timer if it was started
    if (timer) {
      # Stop the timer and capture the output
      elapsed_time <- tictoc::toc(TRUE)$toc - tictoc::toc(TRUE)$tic

      # Convert to minutes and format the output
      minutes <- floor(elapsed_time / 60)
      seconds <- round(elapsed_time %% 60, 2)

      # Display the output
      cat(sprintf("%d minutes and %f seconds elapsed", minutes, seconds),
          "\n")
    }
    invisible(update_list)

  }
