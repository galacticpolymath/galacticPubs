#' make_shareable_assets
#'
#' Make an HTML file with shareable assets for creating a unit Press Kit for clients
#'
#' @param WD "?"
#'
#' @return success
#' @export
#'

make_shareable_assets <- \(WD = "?") {
  WD <- parse_wd(WD)
  ShortTitle <- get_fm("ShortTitle", WD = WD)
  unit_url <- get_fm("URL",WD=WD)
  checkmate::assert_string(unit_url,min.chars = 10)
  unit_name <- basename(WD)

  qr_path <- fs::path(WD,"assets","_banners_logos_etc",paste0(unit_name,"__QR-code.png"))

  if(!file.exists(qr_path)){
  grDevices::png( qr_path)
 plot(qrcode::qr_code(unit_url))
 grDevices::dev.off()
 upload_assets()
 message("QR Code generated for ",unit_name," at:\n",qr_path,"\n")
  }


  cloud_assets <- gcs_contents(WD = WD) %>%
    dplyr::filter(!grepl("vocab.csv|GP-Learning-Chart.png|_vert.png", .data$name))

  links <- paste0("https://storage.googleapis.com/",
                  "gp-cloud",
                  "/",
                  cloud_assets$name)
  thumbs <- lapply(1:length(links), \(i) {
    htmltools::div(style = "display: block;",
                   htmltools::a(
                     href = links[i],
                     htmltools::img(style = "width: 120px; height:auto; max-height:120px; object-fit: contain;", src =
                                      links[i])
                   ),
                   a(href = links[i], h3(style = "display:inline;", basename(links[i]))))
  })
  body <- htmltools::tagList(thumbs)

  filename <- fs::path(WD,
                       "assets",
                       paste0(ShortTitle, "_", "Shareable_Assets.html"))
  test_success <- htmltools::save_html(body, filename) %>% catch_err()

  if(test_success){
    message("SUCCESS! Shareable Assets Webpage saved to:\n ",filename,"\n")
  }
  test_success
}
