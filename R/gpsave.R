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
#' @param gp_footer_border_width multiplier; default=1. if using [gp_footer()], used to scale output of the plot border. Kind of futzy, might need to play around with numbers
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
                   gp_footer_border_width = 3,
                   ...) {

  WD <- parse_wd(WD)
  if (requireNamespace("showtext", quietly = TRUE)) {
    showtext::showtext_auto(enable = TRUE)
  }

  if (is.null(obj)) {
    message("You might need to specify 'obj'")
  }

  fn <- fs::path(WD, save_dir, filename)
  if (!fs::dir_exists(path_parent_dir(fn))) {
    fs::dir_create(path_parent_dir(fn))
  }
  checkmate::assert_access(path_parent_dir(fn), access = "w")

  # Validate object types
  is_grob_list <- FALSE
  isgraf <- FALSE

  if (is.list(obj) && !inherits(obj, c("graf_w_footer", "ggplot", "grob", "patchwork", "gp_footer_obj"))) {
    grob_list_tests <- vapply(obj, inherits, logical(1), what = "grob")
    if (!all(grob_list_tests)) stop("The supplied list must contain all grob layers (nothing else).")
    is_grob_list <- TRUE
  } else if (inherits(obj, c("graf_w_footer", "ggplot", "grob", "patchwork", "gp_footer_obj"))) {
    isgraf <- TRUE
  }

  if (!isgraf && !is_grob_list) {
    stop("The object must be either a ggplot, patchwork, grob, gp_footer_obj, or a list of grobs.")
  }

  # Dimensions
  if (is.null(height) && is.null(width)) width <- 7
  if (is.null(height)) height <- width / aspect
  if (is.null(width))  width  <- height * aspect

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

    grid::grid.newpage()

    # ---- Special case: gp_footer output ----
    if (inherits(obj, "gp_footer_obj")) {

      # pull metadata
      y <- attr(obj, "gp_y", exact = TRUE)
      if (is.null(y)) y <- 0.05

      add_plot_border <- isTRUE(attr(obj, "gp_add_plot_border", exact = TRUE))

      border_col_graph <- attr(obj, "gp_border_col_graph", exact = TRUE)
      if (is.null(border_col_graph)) border_col_graph <- "#363636"

      border_width_graph <- attr(obj, "gp_border_width_graph", exact = TRUE)
      if (is.null(border_width_graph)) border_width_graph <- 3

      # arrangeGrob stores children in obj$grobs (typically plot then footer)
      if (is.null(obj$grobs) || length(obj$grobs) < 2) {

        # fallback (shouldn't happen)
        grid::grid.draw(obj)

      } else {

        plot_grob <- obj$grobs[[1]]
        footer_grob <- obj$grobs[[2]]

        # Draw in the same 2-row layout
        grid::pushViewport(grid::viewport(layout = grid::grid.layout(
          nrow = 2, ncol = 1,
          heights = grid::unit.c(grid::unit(1 - y, "npc"), grid::unit(y, "npc"))
        )))

        # Top row: plot
        grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
        grid::grid.draw(plot_grob)

        if (add_plot_border) {
          # --- DPI-stable border width using pixel quantization ---
          # baseline (points) -> pixels at ~96 dpi
          px_base <- border_width_graph * 96 / 72
          # apply multiplier and snap to integer pixels (ensures visible changes)
          px_target <- max(1, round(px_base * gp_footer_border_width))
          # pixels at output dpi -> points for grid
          lwd_saved <- px_target * 72 / dpi

          grid::grid.draw(grid::rectGrob(
            x = 0.5, y = 0.5, width = 1, height = 1,
            gp = grid::gpar(col = border_col_graph, fill = NA, lwd = lwd_saved)
          ))
        }

        grid::upViewport()

        # Bottom row: footer
        grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
        grid::grid.draw(footer_grob)
        grid::upViewport(2)
      }

    } else if (is_grob_list) {

      for (i in seq_along(obj)) grid::grid.draw(obj[[i]])

    } else if (inherits(obj, "patchwork")) {

      grid::grid.draw(patchwork::patchworkGrob(obj))

    } else if (inherits(obj, "grob")) {

      grid::grid.draw(obj)

    } else {

      print(obj)

    }

    dev.off()
  } %>% catch_err()

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
