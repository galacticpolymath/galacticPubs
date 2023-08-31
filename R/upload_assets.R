#' upload_assets()
#'
#' Upload lesson assets to galacticPubs Google Cloud Storage bucket
#'
#' @param WD WD working directory, passed to [parse_wd()]
#' @returns a Tibble with success, filenames and download links
#' @family google cloud storage
#' @export

upload_assets <- \(WD = "?",
                   bucket = "gp-cloud") {
  WD <- parse_wd(WD)
  test_init <- init_gcs(bucket=bucket)
  checkmate::assert_true(test_init,.var.name="GCS cloud connection initialized")

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

  out <- gcs_add(assets,
                bucket=bucket) %>% catch_err(keep_results = TRUE)

  #Show output with nice checks
  out %>% dplyr::mutate(success=convert_T_to_check(.data$success)) %>% print()

  #output results with logical T/F for success
  message("Upload results")
  out

}

#' gcs_upload_assets()
#'
#' Alias for upload_assets
#'
#' @describeIn upload_assets alias
#' @family google cloud storage
#' @export

gcs_upload_assets <- upload_assets
