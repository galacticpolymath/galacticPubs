#' Get contents of GP Google Cloud Storage bucket
#'
#' A shallow wrapper for [googleCloudStorageR::gcs_list_objects()]. If WD supplied, it will only show objects for that project.
#'
#' @param WD WD working directory, passed to [parse_wd()]. Default="?" will pick unit. Supplying WD will give you the subset for a given unit.
#' @param cloud_dir default=NULL; which directory in the chosen bucket do you want to list? Options= "css","icons","lessons","logos","people"
#' @param bucket default="GP-Studio"; which bucket do you want to pick?
#' @param detail the detail parameter for [googleCloudStorageR::gcs_list_objects()]; options= c("summary", "more", "full"); default="more"
#' @param pattern default=NULL; pattern for filtering rows
#' @param show_all default=FALSE; if TRUE, will show all rows (may take a while)
#' @param ... other parameters passed to [googleCloudStorageR::gcs_list_objects()]
#' @returns a Tibble with success, filenames and download links
#' @family google cloud storage
#' @export

gcs_contents <- \(
  WD = "?",
  cloud_dir = NULL,
  bucket = "gp-cloud",
  detail = "summary",
  pattern = NULL,
  show_all = FALSE,
  ...
) {
  if (!is.null(WD) & is.null(cloud_dir)) {
    WD <- parse_wd(WD)
    prefix <- paste0("lessons/", get_fm("GdriveDirName", WD = WD), "/")
  } else if (!is.null(cloud_dir)) {
    prefix <- paste0(cloud_dir, "/")
  } else{
    prefix <- NULL
  }
  test_init <- init_gcs(bucket = bucket)
  checkmate::assert_true(test_init, .var.name = "GCS cloud connection initialized")

  res <- googleCloudStorageR::gcs_list_objects(bucket = bucket,
                                               prefix = prefix,
                                               detail = detail,
                                               ...) %>% dplyr::as_tibble() %>% catch_err(keep_results = TRUE)

  url_prefix <- paste0("https://storage.googleapis.com/", bucket, "/")

  out0 <- res$result %>%
    dplyr::mutate(link = paste0(url_prefix, .data$name)) %>%
    dplyr::relocate(.data$link, .after = 2)

  if (!is.null(pattern)) {
    good_rows <- grepl(pattern, out0$name, perl = TRUE)
    out <- out0[good_rows, ]
  } else{
    out <- out0
  }
  if (show_all) {
    nr <- nrow(out)
  } else{
    nr <- 5
  }
  print(out, n = nr)
  invisible(out)
}
