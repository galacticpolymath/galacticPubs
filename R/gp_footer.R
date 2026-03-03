#' gp_footer
#'
#' Add a Galactic Polymath footer to the bottom of ggplot objects
#'
#' @param obj ggplot object
#' @param caption plot caption
#' @param data_attrib concise citation for data source (will be smaller font and follow "Data: ")
#' @param data_attrib_prefix How to prefix data_attrib text. By default "Data: "
#' @param x x how much to inset caption as a fraction; default=0.01
#' @param y y height of box as a fraction; default=0.08;
#' @param text_size text size; default=9;
#' @param text_color text color; default="#363636" aka Galactic Black
#' @param data_attrib_scale default=0.6 from 0 to 1, how much smaller to scale attribution text?
#' @param data_attrib_x where do you want to place the right edge of the data attribution text (0 to 1); default= 0.75
#' @param logo default="black"; which GP logo do you want to use?
#' @param logo_scale on a scale of 0 to 1 (npc units), how high to make the logo in footer? default=0.7
#' @param add_border logical; add border around main plot? default=TRUE
#' @param border_col_graph color of border around main plot. default="#363636"; put "transparent" to remove border
#' @param border_col_foot color of border around plot footer. default="#363636"; put "transparent" to remove border
#' @param border_width_graph width of border around main plot. default=1
#' @param border_width_foot width of border around footer. default=1
#' @param fill_col fill color for caption at bottom; default is "gray90"
#' @param show_plot logical; plot resulting figure? default=T
#' @param clear_cache logical; clear cached logo file? default=FALSE
#'
#' @export

gp_footer <- function(obj,
                      caption = NULL,
                      x = 0.015,
                      y = 0.05,
                      text_size = 9,
                      add_border = TRUE,
                      border_col_graph = "#363636",
                      border_col_foot = "#363636",
                      border_width_graph = 3,   # "screen-ish" width baseline
                      border_width_foot = 1,
                      text_color = "#363636",
                      data_attrib = NULL,
                      data_attrib_prefix = "Data: ",
                      data_attrib_scale = 0.9,
                      data_attrib_x = 0.75,
                      logo = "black",
                      logo_scale = 0.7,
                      fill_col = "gray90",
                      show_plot = TRUE,
                      clear_cache = FALSE) {

  if (requireNamespace("showtext", quietly = TRUE)) {
    showtext::showtext_auto(enable = TRUE)
  }

  # Try to inherit font family from ggplot theme (patchwork doesn't reliably expose)
  footer_family <- "sans"
  if (inherits(obj, "ggplot")) {
    merged_theme <- if (is.null(obj$theme)) ggplot2::theme_get() else ggplot2::theme_get() + obj$theme
    if (!is.null(merged_theme$text) &&
        inherits(merged_theme$text, "element_text") &&
        !is.null(merged_theme$text$family) &&
        merged_theme$text$family != "") {
      footer_family <- merged_theme$text$family
    }
  }

  # ---- Logo ----
  checkmate::assert_choice(logo, choices = c("black"))
  logo_fullname <- "GP_horiz_logo+wordmark_black.png"

  cached_path <- fs::path(tempdir(), logo_fullname)
  test_cached_logo <- file.exists(cached_path)

  if (isTRUE(test_cached_logo) && isTRUE(clear_cache)) {
    base::unlink(cached_path)
    test_cached_logo <- FALSE
  }

  if (isTRUE(test_cached_logo)) {
    logo_path <- cached_path
  } else {
    logo_url <- paste0("https://storage.googleapis.com/gp-cloud/logos/", logo_fullname)
    checkmate::assert_true(RCurl::url.exists(logo_url))
    utils::download.file(url = logo_url, destfile = cached_path)
    logo_path <- cached_path
    checkmate::assert_file_exists(logo_path)
  }

  logo_png <- png::readPNG(logo_path, native = TRUE)
  logo_grob <- grid::rasterGrob(
    logo_png,
    x = grid::unit(0.98, "npc"),
    y = grid::unit(0.5, "npc"),
    height = grid::unit(logo_scale, "npc"),
    just = c("right", "center")
  )

  # ---- Footer (thin border baked here, as you want) ----
  footer_grob <- grid::grobTree(
    grid::rectGrob(
      x = 0.5, y = 0.5, width = 1, height = 1,
      gp = grid::gpar(fill = fill_col, col = border_col_foot, lwd = border_width_foot)
    ),
    if (!is.null(caption)) grid::textGrob(
      label = caption,
      x = x, y = 0.5,
      just = c("left", "center"),
      gp = grid::gpar(col = text_color, fontsize = text_size, fontface = "bold",
                      fontfamily = footer_family)
    ),
    if (!is.null(data_attrib)) grid::textGrob(
      label = paste0(data_attrib_prefix, data_attrib),
      x = data_attrib_x, y = 0.5,
      just = c("right", "center"),
      gp = grid::gpar(col = text_color, fontsize = text_size * data_attrib_scale,
                      fontfamily = footer_family)
    ),
    logo_grob
  )

  # ---- Plot grob (ggplot OR patchwork) ----
  plot_grob <- if (inherits(obj, "patchwork")) {
    patchwork::patchworkGrob(obj)
  } else if (inherits(obj, "ggplot")) {
    ggplot2::ggplotGrob(obj)
  } else if (inherits(obj, "grob")) {
    obj
  } else {
    stop("obj must be a ggplot, patchwork, or grob.")
  }

  # ---- Arrange plot + footer (your original two-row grid layout) ----
  combined <- gridExtra::arrangeGrob(
    plot_grob,
    footer_grob,
    ncol = 1,
    heights = grid::unit.c(
      grid::unit(1, "npc") - grid::unit(y, "npc"),
      grid::unit(y, "npc")
    )
  )

  # ---- Metadata so gpsave can draw the thick plot border correctly ----
  class(combined) <- unique(c("gp_footer_obj", class(combined)))
  attr(combined, "gp_y") <- y
  attr(combined, "gp_add_plot_border") <- isTRUE(add_border)
  attr(combined, "gp_border_col_graph") <- border_col_graph
  attr(combined, "gp_border_width_graph") <- border_width_graph

  if (isTRUE(show_plot)) {
    # draw combined + draw plot border in the plot viewport (viewer baseline ~96dpi-ish)
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(
      nrow = 2, ncol = 1,
      heights = grid::unit.c(grid::unit(1 - y, "npc"), grid::unit(y, "npc"))
    )))
    grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
    grid::grid.draw(plot_grob)

    if (isTRUE(add_border)) {
      grid::grid.draw(grid::rectGrob(
        gp = grid::gpar(col = border_col_graph, fill = NA, lwd = border_width_graph)
      ))
    }
    grid::upViewport()

    grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
    grid::grid.draw(footer_grob)
    grid::upViewport(2)
  }

  invisible(combined)
}
