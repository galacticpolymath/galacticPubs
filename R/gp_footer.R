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
#' @param data_attrib_x where do you want to place the right edge of the data attribution text (0 to 1); default= 0.75
#' @param logo default="black"; which GP logo do you want to use?
#' @param border_col_graph color of border around main plot. default="#363636"; put "transparent" to remove border
#' @param border_col_foot color of border around plot footer. default="#363636"; put "transparent" to remove border
#' @param fill_col fill color for caption at bottom; default is gpColors("sparkle white")
#' @param show_plot logical; plot resulting figure? default=T
#'
#' @export


#rewritten by ChatGPT to be able to add as a ggplot layer!!
gp_footer <- function(obj,
                      caption = NULL,
                      x = 0.015,
                      y = 0.05,
                      text_size = 9,
                      border_col_graph = "#363636",
                      border_col_foot = "#363636",
                      border_width_graph= 1,
                      border_width_foot=1,
                      text_col = "#363636",
                      data_attrib = NULL,
                      data_attrib_scale = 0.9,
                      data_attrib_x=0.75,
                      logo = "black",
                      fill_col = "#D3D3D3",
                      show_plot = TRUE,
                      clear_cache=FALSE) {

  checkmate::assert_choice(logo, choices = c("black"))

  # Prepare the logo
  logo_fullname <- switch(logo, black = "GP_horiz_logo+wordmark_black.png")


# Check for cached logo: --------------------------------------------------
  cached_path <- fs::path(tempdir(),logo_fullname)
  test_cached_logo <- file.exists(cached_path)

  #Delete cached logo if desired
  if(test_cached_logo & clear_cache){
    base::unlink(cached_path)
    test_cached_logo <- FALSE
  }

  if(test_cached_logo){
    logo_path <- cached_path
  }else{
    logo_url <- paste0("https://storage.googleapis.com/gp-cloud/logos/", logo_fullname)
    test_url <- RCurl::url.exists(logo_url)
    checkmate::assert_true(test_url,.var.name = "Logo URL")
    test_dl <- utils::download.file(url=logo_url,destfile = cached_path) %>% catch_err()
    if(!test_dl){
      message("Download failed for: ",logo_url)
    }
    checkmate::assert_file_exists(logo_path)
  }


  logo_png <- png::readPNG(logo_path, native = TRUE)

  # Set logo height proportional to text size
  logo_height <- grid::unit(text_size * 1.2, "points")  # Adjust multiplier for finer control

  logo_grob <- grid::rasterGrob(
    logo_png,
    x = grid::unit(0.98, "npc"),
    y = grid::unit(0.5, "npc"),
    height = logo_height,
    just = c("right", "center")
  )

  # Create footer grob
  footer_grob <- grid::grobTree(
    grid::rectGrob(
      x = 0.5, y = 0.5,
      width = 1, height = 1,
      just = "center",
      gp = grid::gpar(fill = fill_col, col = border_col_foot,lwd=border_width_foot)
    ),
    if (!is.null(caption)) grid::textGrob(
      label = caption,
      x = x, y = 0.5,
      just = c("left", "center"),
      gp = grid::gpar(col = text_col, fontsize = text_size, fontface = "bold")
    ),
    if (!is.null(data_attrib)) grid::textGrob(
      label = paste0("Data: ", data_attrib),
      x = data_attrib_x, y = 0.5,
      just = c("right", "center"),
      gp = grid::gpar(col = text_col, fontsize = text_size * data_attrib_scale)
    ),
    logo_grob
  )

  # Add the border to the ggplot object itself
  obj_with_border <- obj +
    theme(
      plot.margin = unit(c(1, 1, 1, 1), "lines"),  # Add margin around the plot
      plot.background = element_rect(color = border_col_graph, size = border_width_graph)  # Border color and size
    )

  # Combine the plot with the footer
  combined_plot <- gridExtra::grid.arrange(
    obj_with_border, footer_grob,
    nrow = 2,  # Only two rows: plot with border and footer
    heights = grid::unit.c(
      grid::unit(1, "npc") - grid::unit(y, "npc"),  # Plot height minus footer height
      grid::unit(y, "npc")  # Footer height
    )
  )

  # Add a class to differentiate it
  class(combined_plot) <- c(class(combined_plot), "graf_w_footer")

  # Display the plot if required
  if (show_plot) {
    grid::grid.newpage()
    grid::grid.draw(combined_plot)
  }

  invisible(combined_plot)
}





#Old code written by Matt that doesn't play well with ggplot
#
# gp_footer <-
#   function(obj,
#            caption=NULL,
#            x = 0.01,
#            y = 0.05,
#            text_size = 9,
#            border_col="black",
#            text_col = "#363636",
#            data_attrib = NULL,
#            data_attrib_scale = 0.9,
#            logo = "black",
#            fill_col = gpColors("sparkle"),
#            show_plot=TRUE) {
#     checkmate::assert_choice(logo, "black")
#
#     logo_fullname = switch(logo, black = "GP_horiz_logo+wordmark_black.png")
#     logo_url <- paste0("https://storage.googleapis.com/gp-cloud/logos/",
#                        logo_fullname)
#     logo_png <-
#       png::readPNG(RCurl::getURLContent(logo_url), native = T)
#
#
#     # Capture all output ------------------------------------------------------
#     G <- grid::grid.grabExpr({
#       ###Plot background object
#       grid::grid.draw(obj)
#
#       #Make background for plot
#       grid::grid.rect(
#         x = .5,
#         y = 0,
#         width = 1,
#         height = y,
#         just = "bottom",
#         gp = grid::gpar(
#           col=border_col,
#           fill = fill_col)
#       ) #"#090816"
#
#       # grid::grid.lines(
#       #   x = c(0, 1),
#       #   y = c(.06, .06),
#       #   gp = grid::gpar(col = "white")
#       # )
#       if (!is.null(caption)) {
#         grid::grid.text(
#           label = caption,
#           x = x,
#           y = y / 2,
#           just = c("left", "center"),
#           gp = grid::gpar(
#             col = text_col,
#             fontsize = text_size,
#             font = 2
#           )
#         )
#       }
#
#       if (!is.null(data_attrib)) {
#         grid::grid.text(
#           label = paste0("Data: ", data_attrib),
#           x = 0.75,
#           y = y / 2,
#           just = c("right", "center"),
#           gp = grid::gpar(col = text_col, fontsize = text_size * data_attrib_scale)
#         )
#       }
#
#       grid::grid.raster(
#         logo_png,
#         x = grid::unit(0.98, "npc"),
#         y = grid::unit(y / 2, "npc"),
#         height = grid::unit(y * 0.6, "npc"),
#         just = c("right", "center")
#       )
#     })
#     if(show_plot){
#     grid::grid.draw(G)
#     }
#     G
#
#   }
