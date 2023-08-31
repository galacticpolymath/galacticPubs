#' Helper for [upload_assets()]: Delete a file from Google Cloud Storage bucket if not found locally
#'
#' A shallow wrapper for [googleCloudStorageR::gcs_delete_object()] with GP error checking. Will validate:
#' - file(s) exist
#'
#' @param cloud_path expects a vector of 1 or more cloud_paths in the form: 'lessons/PROJ/x.jpg'
#' @param bucket GCS bucket; default= "gp-cloud"
#' @return tibble of successes
#' @family google cloud storage
#' @export

gcs_delete <- \(cloud_path,
             bucket = "gp-cloud") {


  checkmate::assert_character(cloud_path,min.len=1)
  #currently only gp-cloud upload supported
  checkmate::assert_choice(bucket, c("gp-cloud"))
  message("Removing outdated files from galacticPubs GCS bucket: ", bucket, "\n")

  if (nrow(assets) == 0) {
    #output report
    dplyr::tibble(
      success = FALSE,
      cloud_path = cloud_path
    )
  } else{
    out <- purrr::map_df(cloud_path, .progress = TRUE, .f = \(x) {

      test_del_i <-
        googleCloudStorageR::gcs_delete_object(
          object_name = x,
          bucket = bucket
        )  %>%
        catch_err(keep_results = T)

      #output report
      dplyr::tibble(
        deleted = test_del_i$success,
        cloud_path = x
      )

    })
  }
}
