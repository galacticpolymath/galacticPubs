#' Helper for [upload_assets()]: Add a file to Google Cloud Storage bucket
#'
#' A shallow wrapper for [googleCloudStorageR::gcs_upload()] with GP error checking. Will validate:
#' - file(s) exist
#'
#' @param assets expects a tibble with local path(s) to a file. Must have columns:
#' - path: local file path
#' - cloud_path: the relative hierarchy for cloud file, e.g. "/Project_en-US/whatever.jpeg"
#' @param bucket GCS bucket; default= "gp-cloud"
#' @param key name of the galacticPubs key to add to Google Cloud meta data. Passed to [googleCloudStorageR::gcs_metadata_object()]; default=NULL. *Right now it will go to the web, but there doesn't seem to be a way to access it in R.
#' @return boolean of success
#' @family google cloud storage
#' @export

gcs_add <- \(assets,
             bucket = "gp-cloud",
             key = NULL) {

  #currently only gp-cloud upload supported
  checkmate::assert_choice(bucket, c("gp-cloud"))


  message("Uploading files to galacticPubs GCS bucket: ", bucket, "\n")

  if (nrow(assets) == 0) {
    #output report
    dplyr::tibble(
      success = FALSE,
      filename = NA,
      cloud_path = NA,
      download_url = NA
    )
  } else{

    out <- purrr::map_df(1:nrow(assets), .progress = TRUE, .f = \(i) {
      asset_i <- assets[i, ]

      if (!is.null(key)) {
        key_i <- ifelse(length(key)==1,key, key[i])
        #create metadata object from key=value pairs
        metadata <-
          googleCloudStorageR::gcs_metadata_object(object_name=asset_i$cloud_path,metadata = list("author"="Galactic Polymath","galacticPubs_key"=key_i))
      } else{
        metadata <- NULL
      }

      test_upload_i <-
        googleCloudStorageR::gcs_upload(
          file = asset_i$path,
          bucket = bucket,
          name = asset_i$cloud_path,
          object_metadata = metadata,
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
        cloud_path = asset_i$cloud_path,
        download_url = asset_i$download_url
      )

    })
  }
}
