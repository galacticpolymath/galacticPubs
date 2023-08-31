#' Helper for [upload_assets()]: Add a file to Google Cloud Storage bucket
#'
#' A shallow wrapper for [googleCloudStorageR::gcs_upload()] with GP error checking. Will validate:
#' - file(s) exist
#'
#' @param assets expects a tibble with local path(s) to a file. Must have columns:
#' - path: local file path
#' - cloud_path: the relative hierarchy for cloud file, e.g. "/Project_en-US/whatever.jpeg"
#' @param bucket GCS bucket; default= "gp-cloud"
#' @return boolean of success
#' @family google cloud storage
#' @export

gcs_add <- \(assets,
             bucket = "gp-cloud") {
  #colnames required for tibble
  req_names <- c("path", "cloud_path")

  checkmate::assert(
    checkmate::check_data_frame(
      assets,
      min.rows = 1,
      col.names = checkmate::assert_subset(names(assets), req_names)
    )
  )
  #currently only gp-cloud upload supported
  checkmate::assert_choice(bucket, c("gp-cloud"))


  message("Uploading files to galacticPubs GCS bucket: ", bucket, "\n")

  if (nrow(assets) == 0) {
    #output report
    dplyr::tibble(
      success = FALSE,
      filename = NA,
      name = NA,
      cloud_path = NA,
      download_url = NA
    )
  } else{
    out <- purrr::map_df(1:nrow(assets), .progress = TRUE, .f = \(i) {
      asset_i <- assets[i,]
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
        asset_i$download_url <- asset_i$cloud_path <- NA
      }


      #output report
      dplyr::tibble(
        success = test_upload_i$success,
        filename = basename(asset_i$path),
        name = asset_i$name,
        cloud_path = asset_i$cloud_path,
        download_url = asset_i$download_url
      )

    })
  }
}
