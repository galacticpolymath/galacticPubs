#' Get contents of GP Google Cloud Storage bucket
#'
#' A shallow wrapper for [googleCloudStorageR::gcs_list_objects()]. If WD supplied, it will only show objects for that project.
#'
#' @param WD WD working directory, passed to [parse_wd()]. Default=NULL will show all bucket objects. Supplying WD will give you the subset for a given unit.
#' @param detail the detail parameter for [googleCloudStorageR::gcs_list_objects()]; options= c("summary", "more", "full"); default="more"
#' @param ... other parameters passed to [googleCloudStorageR::gcs_list_objects()]
#' @returns a Tibble with success, filenames and download links
#' @family google cloud storage
#' @export

gcs_contents <- \(WD = "?",
                   bucket = "gp-cloud",
                   detail="summary",
                  ...) {
  if(!is.null(WD)){
  WD <- parse_wd(WD)
  prefix <- paste0("lessons/",get_fm("GdriveDirName", WD=WD),"/")
  }else{prefix <- NULL}
  test_init <- init_gcs(bucket=bucket)
  checkmate::assert_true(test_init,.var.name="GCS cloud connection initialized")

  res <- googleCloudStorageR::gcs_list_objects(
    bucket=bucket,
    prefix=prefix,
    detail=detail,
    ...
  ) %>% dplyr::as_tibble() %>% catch_err(keep_results = TRUE)

  res$result
}
