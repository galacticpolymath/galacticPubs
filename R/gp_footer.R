#' gp_footer
#'
#' Add a Galactic Polymath footer to the bottom of ggplot objects
#'
#' @param obj ggplot object
#' @param caption plot caption
#' @param data_attrib concise citation for data source (will be smaller font and follow "Data: ")
#' @param x x how much to inset caption as a fraction; default=0.01
#' @param y y height of box as a fraction; default=0.08;
#' @param text_size text size; default=9;
#' @param text_color text color; default="#363636" aka Galactic Black
#' @param data_attrib_scale default=0.6 from 0 to 1, how much smaller to scale attribution text?
#' @param logo default="black"; which GP logo do you want to use?
#' @param fill_col fill color for caption at bottom; default is gpColors("sparkle white")
#'
#' @export

gp_footer <-
  function(obj,
           caption,
           x = 0.01,
           y = 0.08,
           text_size = 8,
           text_col = "#363636",
           data_attrib= NULL,
           data_attrib_scale = 0.9,
           logo = "black",
           fill_col = gpColors("sparkle")) {
    checkmate::assert_choice(logo, "black")

    logo_fullname = switch(logo, black = "GP_horiz_logo+wordmark_black.png")
    logo_url <- paste0("https://storage.googleapis.com/gp-cloud/logos/",
                       logo_fullname)
    logo_png <-
      png::readPNG(RCurl::getURLContent(logo_url), native = T)

    #Make background for plot
    grid::grid.rect(
      x = .5,
      y = 0,
      width = 1,
      height = y,
      just = "bottom",
      gp = grid::gpar(fill = fill_col)
    ) #"#090816"

    # grid::grid.lines(
    #   x = c(0, 1),
    #   y = c(.06, .06),
    #   gp = grid::gpar(col = "white")
    # )
    if (!is.null(caption)) {
      grid::grid.text(
        label = caption,
        x = x,
        y = y/2,
        just = c( "left","center"),
        gp = grid::gpar(col = text_col, fontsize = text_size,
                          font=2)
      )
    }

    if (!is.null(data_attrib)) {

      grid::grid.text(
        label = paste0("Data: ",data_attrib),
        x = 0.75,
        y = y/2,
        just = c("right","center"),
        gp = grid::gpar(col = text_col,
                        fontsize = text_size* data_attrib_scale)
      )
    }

    grid::grid.raster(
      logo_png,
      x = grid::unit(0.98, "npc"),
      y = grid::unit(y/2, "npc"),
      height = grid::unit(y*0.7, "npc"),
      just = c("right", "center")
    )
  }
