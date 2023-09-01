#' upload_assets()
#'
#' Upload lesson assets to galacticPubs Google Cloud Storage bucket
#'
#' @param WD WD working directory, passed to [parse_wd()]
#' @returns invisibly returns a Tibble with filenames and download links
#' @family google cloud storage
#' @export

upload_assets <- \(WD = "?",
                   bucket = "gp-cloud") {
  WD <- parse_wd(WD)
  test_init <- init_gcs(bucket = bucket)
  checkmate::assert_true(test_init, .var.name = "GCS cloud connection initialized")

  GdriveDirName = get_fm("GdriveDirName", WD = WD)
  #The relative path where we will be storing (or have stored)
  #this lesson's assets
  cloud_prefix = paste0(c("lessons", GdriveDirName), collapse = "/")


  # define some regex patterns ----------------------------------------------
  img_patt <-
    c("\\.png$", "\\.jp[e]?g$", "\\.svg$", "\\.gif$", "\\.webp$")
  logo_patt <- paste0("\\.*[Ll]ogo.*", img_patt, collapse = "|")

  # Define where and how to look for files ----------------------------------
  tasks <-
    dplyr::tribble(
      ~ key,
      ~ rel_path,
      ~ pattern,
      ~ exclude,
      "SponsorLogo",
      "assets/_banners_logos_etc",
      logo_patt,
      "help.txt",

      "LessonBanner",
      "assets/_banners_logos_etc",
      "[Bb]anner\\.png$",
      "help.txt",

      "LessonTiles",
      "assets/_banners_logos_etc",
      "_?[Tt]ile\\.png$",
      "help.txt",

      "LessonCard",
      "assets/_banners_logos_etc",
      "_?[Cc]ard\\.png$",
      "help.txt",

      "LearningChart",
      "assets/_learning-plots",
      "GP-Learning-Chart\\.png",
      NA,

      "LearningEpaulette",
      "assets/_learning-plots",
      "GP-Learning-Epaulette\\.png",
      NA,

      "LearningEpaulette_vert",
      "assets/_learning-plots",
      "GP-Learning-Epaulette_vert\\.png",
      NA,

      "SupportingMedia",
      "assets/_other-media-to-publish",
      NA,
      "help.txt"
    )



  # #copy everything except LESSON.json to the cloud
  # assets <- fs::dir_info(fs::path(WD, "published")) %>%
  #   dplyr::mutate(name = basename(.data$path)) %>%
  #   dplyr::relocate(.data$name) %>%
  #   dplyr::filter(.data$name != "LESSON.json") %>%
  #   dplyr::mutate(cloud_path = paste0(paste0(cloud_prefix, "/"), .data$name)) %>%
  #   dplyr::select("name", "path", "modification_time", "size", "cloud_path") %>%
  #   dplyr::arrange(dplyr::desc(.data$size))


  # Stage assets: aggregate and copy to published/ --------------------------
  assets <- purrr::map(1:nrow(tasks), \(i) {
    df_i <- tasks[i, ]
    if (is.na(df_i$exclude)) {
      ex <- NULL
    } else{
      ex <- df_i$exclude
    }
    if (is.na(df_i$pattern)) {
      patt <- NULL
    } else{
      patt <- df_i$pattern
    }
    res_i <-
      stage_assets(
        WD = WD,
        rel_path = df_i$rel_path,
        pattern = patt,
        exclude = ex
      )
    res_i$result %>% dplyr::mutate(key = df_i$key) %>% dplyr::relocate("key")
  }) %>% dplyr::bind_rows()

  #add modified date to assets
  assets$updated <- fs::file_info(assets$path1)$modification_time

  #add expected cloud_path
  assets$cloud_path <- paste0(cloud_prefix, "/", assets$name)

  # See what's already in the cloud ------------------------------------------------
  uploaded <- gcs_contents(WD = WD, detail = "more")
  if (nrow(uploaded) == 0) {
    to_upload <- assets
  } else{
    uploaded$updated <-
      lubridate::force_tz(uploaded$updated, tz = "UTC")
    #local TZ
    loc_tz <-  Sys.timezone()
    #reassign to local timezone
    uploaded$updated <-
      lubridate::with_tz(uploaded$updated, tz = loc_tz)


    merged_mod_times <-
      dplyr::left_join(
        assets[, c("cloud_path", "updated", "path1", "key")],
        uploaded[, c("name", "updated")],
        by = c("cloud_path" = "name"),
        suffix = c(".local", ".cloud")
      )


    to_upload <- merged_mod_times %>%
      dplyr::filter(.data$updated.local > .data$updated.cloud |
                      is.na(.data$updated.cloud)) %>%
      dplyr::rename(path = .data$path1)
  }


  # UPLOAD STUFF ------------------------------------------------------------

  if (nrow(to_upload) > 0) {
    upload_L <- gcs_add(assets = to_upload,
                        bucket = bucket,
                        key = to_upload$key) %>% catch_err(keep_results = TRUE)
    message("the following missing assets uploaded to the cloud: \n")
    print(upload_L$result)
  }


  # DELETE STUFF ------------------------------------------------------------


  to_del <-
    dplyr::anti_join(uploaded[, "name"], assets[, "cloud_path"], by = c("name" =
                                                                          "cloud_path"))

  if (nrow(to_del) > 0) {
    delete_L <-
      gcs_delete(cloud_path = to_del$name, bucket = bucket) %>% catch_err(keep_results = TRUE)
    message("the following items deleted from the cloud: \n")
    print(delete_L$result)
  }


  # Check that all assets in cloud now --------------------------------------
  uploaded_now <- gcs_contents(WD = WD, detail = "more")
  test_uploaded <-
    sum(assets$cloud_path %in% uploaded_now$name) == nrow(assets)

  if (test_uploaded) {
    message("All assets in the cloud")
  }


  # Create download links ---------------------------------
  url_prefix <- paste0("https://storage.googleapis.com/",
                       bucket,
                       "/")
  out <-
    assets %>% dplyr::select("key", "name", "log", "cloud_path") %>%
    dplyr::mutate(download_url =
                    paste0(url_prefix, .data$cloud_path))


  #  assign keys with update_fm -----------------------------------------
  #Always go ahead and re-assign. It's instantaneous anyway
  keys <- unique(out$key)
  fm_update_list <- purrr::set_names(keys) %>%
    purrr::map(., \(x) {
      df_x <- out %>% dplyr::filter(.data$key == x)
      df_x$download_url
    })

  test_fm_update <- update_fm(WD = WD, change_this = fm_update_list)

  summ <-
    dplyr::tibble(
      success = convert_T_to_check(c(test_uploaded, test_fm_update)),
      task = c(
        "synced assets with the cloud",
        "updated all URLs in front-matter"
      )
    )
  print(summ)


  invisible(out)

}

#' gcs_upload_assets()
#'
#' Alias for upload_assets
#'
#' @describeIn upload_assets alias
#' @family google cloud storage
#' @export

gcs_upload_assets <- upload_assets
