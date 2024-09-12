#' gpsave
#'
#' Wrapper for [ggplot2::ggsave()] with galacticPubs defaults
#'
#' Most important thing it does is default to saving in the assets/_R_outputs/ folder in the unit working directory and save with reasonable dimensions that maintain large, readable text size of graph labels
#'
#' @param filename expects filename, with file extension (e.g. "plot.png"); can also include subfolder (e.g. "newfolder/plot.png")
#' @param obj ggplot or grid object; default= ggplot2::last_plot()
#' @param WD working directory; default="?" or a unit picker dialog
#' @param width plot width in inches (default= 7)
#' @param height plot height in inches default= NULL will calculate from aspect
#' @param aspect ratio of width to height; default= 16/9; aspect is ignored if width and height are supplied
#' @param open logical; open file after save? default=FALSE
#' @param dpi dots per inch resolution (default= 300); note changing this will unfortunately also change the output relative text size
#' @param bg background color (default= "transparent")
#' @param ... other parameters from [ggplot2::ggsave()]
#' @export

gpsave <- function(filename,
                   obj = ggplot2::last_plot(),
                   WD = "?",
                   width = 7,
                   height = NULL,
                   aspect = 16 / 9,
                   dpi = 300,
                   open = FALSE,
                   bg = "transparent",
                   ...) {
  WD = parse_wd(WD)
  fn <- fs::path(WD, "assets", "_R_outputs", filename)
  if (is.null(height)) {
    height = width / aspect
  }

  if (is.null(width)) {
    width = height * aspect
  }


  test_save <- ggplot2::ggsave(
    filename = fn,
    plot = obj,
    width = width,
    height = height,
    dpi = dpi,
    ...
  ) %>% catch_err()
  if (test_save) {
    message("@Saved: ", fn)
    message("with width=", width, "  height=", height, "  aspect=",
            MASS::fractions(width/height))
    if (open) {
      system(sprintf('open %s', shQuote(fn)))
    }

  } else{
    message("Something went wrong saving ", fn)
  }

  test_save
}
