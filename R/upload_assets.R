#' upload_assets()
#'
#' Upload lesson assets to galacticPubs Google Cloud Storage bucket
#'
#' @param WD WD working directory, passed to [parse_wd()]
#' @returns a Tibble with success, filenames and download links
#' @export

upload_assets <- \(WD = "?",
                   bucket = "gp-cloud") {
  WD <- parse_wd(WD)
  init_gcs()


  GdriveDirName = get_fm("GdriveDirName", WD = WD)
  #The relative path where we will be storing (or have stored)
  #this lesson's assets
  cloud_prefix = paste0(c("lessons", GdriveDirName), collapse = "/")

  #copy everything except LESSON.json to the cloud
  assets <- fs::dir_info(fs::path(WD, "published")) %>%
    dplyr::mutate(name = basename(.data$path)) %>%
    dplyr::relocate(.data$name) %>%
    dplyr::filter(.data$name != "LESSON.json") %>%
    dplyr::mutate(cloud_path = paste0(paste0(cloud_prefix, "/"), .data$name)) %>%
    dplyr::select("name", "path", "modification_time", "size", "cloud_path") %>%
    dplyr::arrange(dplyr::desc(.data$size))

  #Should do a check here to see if things are up-to-date on the web already
  #Otherwise delete records with cloud_prefix and upload anew
  message("Uploading files to galacticPubs GCS bucket: ", bucket, "\n")

  out <- purrr::map_df(1:nrow(assets), .progress = TRUE, .f = \(i) {
    asset_i <- assets[i, ]
    test_upload_i <-
      googleCloudStorageR::gcs_upload(
        file = asset_i$path,
        bucket = bucket,
        name = asset_i$cloud_path,
        predefinedAcl = 'bucketLevel'
      )  %>%
      catch_err(keep_results = T)

    if (test_upload_i$success) {
      asset_i$download_url <-
        paste0("https://storage.googleapis.com/",
               bucket,
               "/",
               asset_i$cloud_path)
    } else{
      asset_i$download_url <- NA
    }


    #output report
    dplyr::tibble(
      success = test_upload_i$success,
      name = asset_i$name,
      cloud_path = asset_i$cloud_path,
      download_url = asset_i$download_url
    )

  })
  #Show output with nice checks
  out %>% dplyr::mutate(success=convert_T_to_check(.data$success)) %>% print()

  #output results with logical T/F for success
  message("Upload results")
  out

}
