#' make_shareable_assets
#'
#' Make an HTML file with shareable assets for creating a unit Press Kit for clients
#'
#' @param WD "?"
#' @param open_file logical; open resulting HTML file? Default=TRUE
#'
#' @return success
#' @export
#'

make_shareable_assets <- \(WD = "?", open_file = TRUE) {
  WD <- parse_wd(WD)
  fm <- get_fm(WD = WD)
  ShortTitle <- get_fm("ShortTitle", WD = WD)
  unit_url <- get_fm("URL", WD = WD)
  checkmate::assert_string(unit_url, min.chars = 10)
  unit_name <- basename(WD)
  upload_assets(WD = WD)

  qr_path <- fs::path(WD,
                      "assets",
                      "_banners_logos_etc",
                      paste0(unit_name, "__QR-code.png"))

  if (!file.exists(qr_path)) {
    grDevices::png(qr_path)
    plot(qrcode::qr_code(unit_url))
    grDevices::dev.off()
    message("QR Code generated for ", unit_name, " at:\n", qr_path, "\n")
  }
  #make sure we have updated assets online
  upload_assets(WD = WD)


  cloud_assets <- gcs_contents(WD = WD) %>%
    dplyr::filter(!grepl("vocab.csv|GP-Learning-Chart.png|_vert.png", .data$name))

  links <- paste0("https://storage.googleapis.com/",
                  "gp-cloud",
                  "/",
                  cloud_assets$name)

  # Make divs for each GP-Cloud image ---------------------------------------
  thumbs <- lapply(1:length(links), \(i) {
    htmltools::div(
      style = "display: block;",
      htmltools::a(
        href = links[i],
        target = "_blank",
        rel = "noopener noreferrer",
        htmltools::img(style = "width: 120px; height:auto; max-height:120px; object-fit: contain;", src =
                         links[i])
      ),
      htmltools::a(
        href = links[i],
        target = "_blank",
        rel = "noopener noreferrer",
        htmltools::h3(style = "display:inline;", basename(links[i]))
      )
    )
  })
  gcloud_divs <- htmltools::tagList(thumbs)


  # Create YT previews for youtube vids attached to unit --------------------
  WD_git <- get_wd_git(WD = WD)
  cache_path <- fs::path(WD_git, "saves", "multimedia.RDS")


  # Look for multimedia cache file
  if (!file.exists(cache_path)) {
    message("No multimedia info cache found at : ", cache_path)
    body <- gcloud_divs
  } else{
    mlinks <- readRDS(cache_path)
    checkmate::assert_data_frame(mlinks)
    mlinks$isYT <- ifelse(mlinks$type == "video" &
                            grepl("youtu.be|youtube", mlinks$mainLink),
                          TRUE,
                          FALSE)
    mlinks2 <- mlinks %>% dplyr::filter(.data$isYT)


    if (is_empty(mlinks2)) {
      message("No valid YouTube vids found")
      body <- gcloud_divs
    } else{
      yt_links <- mlinks2$mainLink
      #handle weird watchlink format separately
      #(like https://www.youtube.com/watch?v=DREGrkSnZ2g)
      yt_pattern <- "(?:https?:\\/\\/)?(?:www\\.)?(?:youtube\\.com\\/(?:[^\\/\\n\\s]+\\/.+\\/|(?:v|embed|shorts|watch|video)\\/|.*[?&]v=)|youtu\\.be\\/|studio\\.youtube\\.com\\/video\\/)([a-zA-Z0-9_-]{11})[^&\\?]?.*$"
      yt_codes <-  sapply(yt_links, \(link_i) {
        #for URLs formatted as "https://www.youtube.com/watch?v=Xr1SstxYW8w", get id from v= part

        if (grepl(yt_pattern, link_i)) {
          gsub(yt_pattern,
               "\\1",
               link_i)
        } else{
          link_i
        }
      })

      yt_thumbs <- paste0("https://i3.ytimg.com/vi/", yt_codes, "/hqdefault.jpg")

      # TODO: create taglist with images and links to YT vids -------------------
      YT_divs <- lapply(1:length(yt_thumbs), \(i) {
        htmltools::div(
          style = "display: block;",
          htmltools::a(
            href = yt_links[i],
            target = "_blank",
            rel = "noopener noreferrer",
            htmltools::img(style = "width: 120px; height:auto; max-height:120px; object-fit: contain;", src =
                             yt_thumbs[i])
          ),
          htmltools::a(
            href = yt_links[i],
            target = "_blank",
            rel = "noopener noreferrer",
            htmltools::h3(style = "display:inline;", mlinks2$title[i])
          )
        )
      })

      body <- list(
        htmltools::h2("Google Cloud Links"),
        gcloud_divs,
        htmltools::h2("YouTube Links"),
        YT_divs
      )

    }

  }

  head <- htmltools::tagList(
    htmltools::h4("SHAREABLE ASSETS FOR:"),
    htmltools::h1(fm$MediumTitle),
    htmltools::h3(
      htmltools::a(
        href = fm$URL,
        fm$URL,
        target = "_blank",
        rel = "noopener noreferrer"
      )
    ),
    htmltools::h3(
      htmltools::a(
        href = fm$ShortURL,
        gsub("https://(.*)", "\\1", fm$ShortURL),
        target = "_blank",
        rel = "noopener noreferrer"
      )
    ),

  )
  page <- c(head, body)

  filename <- fs::path(WD,
                       "assets",
                       paste0("~Shareable_Assets_", ShortTitle, ".html"))
  test_success <- htmltools::save_html(page, filename) %>% catch_err()

  if (test_success) {
    message("SUCCESS! Shareable Assets Webpage saved to:\n ",
            filename,
            "\n")
  }
  if (open_file) {
    utils::browseURL(filename)
  }
  test_success


}
