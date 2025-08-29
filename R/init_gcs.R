#' init_gcs
#'
#' Initialize connection to Google Cloud Storage galacticPubs buckets
#'
#' This function uses 'googleCloudStorageR' functions to read/write image files to our asset server. To authenticate, it needs access to a JSON file stored in the GP-Dev folder. To have access to this, you need to have Google Drive for Desktop installed and to be in the galacticPubs-user Google Group with our organization. You also need a galacticpolymath.com email address.
#' @param bucket the cloud storage bucket you want to initialize
#' @return logical; did we succeed in connecting to the cloud?
#' @export
#'

init_gcs <- \(bucket="gp-cloud"){
  authfile <- Sys.getenv("GCS_AUTH_FILE")
  if(is_empty(authfile)){
    message("GCS_AUTH_FILE not found in environment. Running init_galacticPubs()")
    init_galacticPubs()
    authfile <- Sys.getenv("GCS_AUTH_FILE")
  }
  checkmate::assert_file_exists(authfile,.var.name = "GCS_AUTH_FILE, aka the JSON file used to authenticate with Google Cloud Storage")
  test_auth <- googleCloudStorageR::gcs_auth(json_file = authfile) %>% catch_err()
  test_connect <- googleCloudStorageR::gcs_get_bucket(bucket)%>% catch_err()

  test_auth & test_connect
}

#' gcs_init
#'
#' @describeIn init_gcs alias for init_gcs
#' @export

gcs_init <- init_gcs
