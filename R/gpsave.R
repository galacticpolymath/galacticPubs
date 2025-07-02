#' gpsave
#'
#' Wrapper for [ggplot2::ggsave()] with galacticPubs defaults
#'
#' Most important thing it does is default to saving in the assets/_R_outputs/ folder in the unit working directory and save with reasonable dimensions that maintain large, readable text size of graph labels
#'
#' @param filename expects filename, with file extension (e.g. "plot.png"); can also include subfolder (e.g. "newfolder/plot.png")
#' @param obj ggplot or grid object; default= ggplot2::last_plot()
#' @param WD working directory; default="?" or a unit picker dialog
#' @param save_dir  which subfolder of WD do you want to save file to? default="assets/_R_outputs"
#' @param width plot width in inches (default= 7)
#' @param height plot height in inches default= NULL will calculate from aspect
#' @param aspect ratio of width to height; default= 16/9; aspect is ignored if width and height are supplied
#' @param open logical; open file after save? default=FALSE
#' @param dpi dots per inch resolution (default= 300); note changing this will unfortunately also change the output relative text size
#' @param bg background color (default= "transparent")
#' @param units default= "in"; options c("in", "cm", "mm", "px")
#' @param ... other parameters from [ggplot2::ggsave()]
#' @export

#co-rewritten with ChatGPT
gpsave <- function(filename,
                   obj = ggplot2::last_plot(),
                   WD = "?",
                   save_dir = "assets/_R_outputs",
                   width = NULL,
                   height = NULL,
                   aspect = 16 / 9,
                   dpi = 300,
                   open = FALSE,
                   bg = "transparent",
                   units = "in",
                   ...) {
  WD = parse_wd(WD)
  showtext::showtext_auto()

  if (is.null(obj)) {
    message("You might need to specify 'obj'")
  }

  fn <- fs::path(WD, save_dir, filename)
  if (!fs::dir_exists(path_parent_dir(fn))) {
    fs::dir_create(path_parent_dir(fn))
  }
  checkmate::assert_access(path_parent_dir(fn), access = "w")


  if (is.list(obj)&!inherits(obj,"graf_w_footer")) {
    isgraf <- FALSE
    grob_list_tests <- lapply(1:length(obj), \(i) {
      isgraf <- inherits(obj[[i]], what = c("grob"))
      if (!isgraf) {
        stop("The supplied list must contain all grob layers (nothing else).")
      }
      return(isgraf)
    })
    is_grob_list <- all(unlist(grob_list_tests))
  } else{
    isgraf <- inherits(obj, what = c("graf_w_footer", "ggplot", "grob"))
    is_grob_list <- FALSE
  }

  if (!isgraf & !is_grob_list) {
    stop("The object must be either a ggplot, a grob, or a list of grobs.")
  }

  # Set default width if no dims supplied
  if (is.null(height) & is.null(width)) {
    width = 7
  }

  if (is.null(height)) {
    height = width / aspect
  }

  if (is.null(width)) {
    width = height * aspect
  }

  # Save file
  test_save <- {
    png(
      filename = fn,
      width = width,
      height = height,
      res = dpi,
      bg = bg,
      units = units,
      type = "quartz"
    )
    if (is_grob_list) {
      #if object is a graf_w_footer, treat as one object, else go through layers

        message("Trying to draw list of objects with grid::grid.draw()")
        grid::grid.newpage()
        lapply(1:length(obj), \(i) grid::grid.draw(obj[[i]]))

    } else{

      plot(obj)
    }
    dev.off()
  } %>% catch_err()


  # Check if save was successful
  if (test_save) {
    message("@Saved: ", fn)
    message(
      "with width=",
      round(width, 2),
      "  height=",
      round(height, 2),
      "  aspect=",
      MASS::fractions(width / height)
    )
    if (open) {
      system(sprintf('open %s', shQuote(fn)))
    }
  } else {
    message("Something went wrong saving ", fn)
  }

  invisible(test_save)
}

#matt's OG function
# gpsave <- function(filename,
#                    obj = ggplot2::last_plot(),
#                    WD = "?",
#                    width = NULL,
#                    height = NULL,
#                    aspect = 16 / 9,
#                    dpi = 300,
#                    open = FALSE,
#                    bg = "transparent",
#                    ...) {
#   WD = parse_wd(WD)
#
#   fn <- fs::path(WD, "assets", "_R_outputs", filename)
#
#   #set default width if no dims supplied
#   if(is.null(height)&is.null(width)){
#     width=7
#   }
#
#   if (is.null(height)) {
#     height = width / aspect
#   }
#
#   if (is.null(width)) {
#     width = height * aspect
#   }
#
#
#   test_save <- ggplot2::ggsave(
#     filename = fn,
#     plot = obj,
#     width = width,
#     height = height,
#     dpi = dpi,
#     ...
#   ) %>% catch_err()
#
#   if (test_save) {
#     message("@Saved: ", fn)
#     message("with width=", round(width,2), "  height=", round(height,2), "  aspect=",
#             MASS::fractions(width/height))
#     if (open) {
#       system(sprintf('open %s', shQuote(fn)))
#     }
#
#   } else{
#     message("Something went wrong saving ", fn)
#   }
#
#   test_save
# }
