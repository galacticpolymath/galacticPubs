#' Launch the Galactic Polymath Editor Shiny App
#'
#' Starts the Shiny app for editing front matter of lessons, supporting markdown syntax.
#' The app is located in the \code{inst/shiny/gp_editor} directory of the \code{galacticPubs} package.
#'
#' @param WD Character. A local virtualized path to a lesson folder where Google Drive (Web) path
#'   will be extracted from front matter. Passing \code{"?"} invokes \code{\link{pick_lesson}()}.
#'   Default is \code{"?"}.
#' @param system_browser Logical. If \code{TRUE} (default), the app opens in the system default web browser.
#'   If \code{FALSE}, it opens in the RStudio viewer pane (if available).
#' @param run_in_bg Logical. If \code{TRUE} (default), runs the app in a background R process using \code{callr::r_bg}.
#'   The app URL will be opened in a browser from the main R session, and the R session remains free.
#'   If \code{FALSE}, runs the app blocking in the current R session (useful for debugging).
#'
#' @return Invisibly returns the \code{callr} background process object if \code{run_in_bg = TRUE}.
#'   Returns \code{NULL} invisibly if \code{run_in_bg = FALSE}.
#'
#' @details
#' When run in background mode, the Shiny app runs in a separate R process,
#' and the browser window is opened from the main R session.
#' The background process will keep running until stopped manually.
#' In blocking mode, the app runs in the current session and blocks until closed.
#'
#' @examples
#' \dontrun{
#' # Run the editor in background mode (default)
#' editor()
#'
#' # Run the editor blocking in the current R session for debugging
#' editor(run_in_bg = FALSE)
#' }
#'
#' @export

editor <- function(WD = "?",
                   system_browser = TRUE,
                   run_in_bg = TRUE) {
  if (!interactive())
    stop("This function is only available in interactive mode.")

  WD <- parse_wd(WD)

  if (run_in_bg) {
    # Run in background with callr
    editor_sess <- callr::r_bg(
      function(WD) {
        # Load necessary packages inside the new R session
        if (!requireNamespace("pacman", quietly = TRUE)) {
          stop("Package 'pacman' must be installed for the editor to run.")
        }
        pacman::p_load(shiny)  # or other packages used in app

        Sys.setenv(editor_path = WD)

        app_dir <- system.file("shiny", "gp_editor", package = "galacticPubs")
        if (!dir.exists(app_dir))
          stop("App directory not found: ", app_dir)

        shiny::runApp(
          appDir = app_dir,
          host = "127.0.0.1",
          port = 8080,
          launch.browser = FALSE
        )
      },
      args = list(WD = WD),
      stdout = "editor_stdout.log",
      stderr = "editor_stderr.log"
    )

    # file.show("editor_stdout.log")
    errlog <- read_file("editor_stderr.log")



    # Wait briefly to allow app to start
    Sys.sleep(2)

    # Check for startup errors
    if (!editor_sess$is_alive()) {
      cat("❌ Editor failed to launch.\n")
      cat("Error output:\n")
      cat(editor_sess$read_error_lines(), sep = "\n")
      stop("Shiny app failed to start in background.")
    }

    # Open the app in browser or viewer
    app_url <- "http://127.0.0.1:8080"
    if (system_browser) {
      utils::browseURL(app_url)
    } else if (rstudioapi::isAvailable()) {
      rstudioapi::viewer(app_url)
    } else {
      message("Open the app in your browser: ", app_url)
    }

    invisible(editor_sess)

  } else {
    # Run app blocking in current session (for debugging)
    assign(".editor_path", WD, envir = .GlobalEnv)
    shiny::runApp(
      appDir = system.file("shiny", "gp_editor", package = "galacticPubs"),
      host = "127.0.0.1",
      port = 8080,
      launch.browser = system_browser
    )
    invisible(NULL)
  }
}
