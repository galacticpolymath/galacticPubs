#' Synchronize media assets with the cloud
#'
#' Upload lesson assets to galacticPubs Google Cloud Storage bucket. It looks for the following patterns in the following folders:
#' - SponsorLogo  | assets/_banners_logos_etc: anything with 'logo' and .png, .jpg, .webp suffix
#' - UnitBanner | assets/_banners_logos_etc: ideally just named banner.png; should work with 'ShortTitle_banner.png' and exclude 'old_banner.png'
#' - LessonTiles  | assets/_banners_logos_etc: must be format 'L1_tile.png'; other img formats also supported: .png, .jpeg, .webp
#' - UnitCard   | assets/_banners_logos_etc: format 'card.png' or 'ShortTitle.png'
#' - QR-code | assets/_banners_logos_etc: format 'ProjTitle_QR-code.png'
#' - LearningChart| assets/_learning-plots: file must be called 'GP-Learning-Chart.png' (autogenerated from [learningChart()])
#' - LearningEpaulette | assets/_learning-plots: file must be called 'GP-Learning-Epaulette.png' (autogenerated from [learningEpaulette()])
#' - LearningEpaulette_vert | assets/_learning-plots: file must be called 'GP-Learning-Epaulette_vert.png' (autogenerated from [learningEpaulette()])
#' - SupportingMedia | assets/_other-media-to-publish: anything except help.txt found in this folder
#'
#' @param WD WD working directory, passed to [parse_wd()]
#' @param bucket which bucket? default= "gp-cloud"
#' @param clear do you want to clear all existing files for this project before uploading? default=FALSE
#' @returns invisibly returns a Tibble with filenames and download links
#' @family google cloud storage
#' @export

upload_assets <- \(WD = "?",
                   bucket = "gp-cloud",
                   clear = FALSE) {
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
  qr_patt <- "QR-code"
  # Define where and how to look for files ----------------------------------
  tasks <-
    dplyr::tribble(
      ~ task,
      ~ key,
      ~ rel_path,
      ~ pattern,
      ~ exclude,

      "1",
      "SponsorLogo",
      "assets/_banners_logos_etc",
      logo_patt,
      "help.txt",

      "2",
      "UnitBanner",
      "assets/_banners_logos_etc",
      paste0("(?!old)_?[Bb]anner[^\\.]*",img_patt,collapse="|"),
      "help.txt|_OLD|_old",

      "3",
      "LessonTiles",
      "assets/_banners_logos_etc",
      paste0("_?[Tt]ile[^\\.]*",img_patt,collapse="|"),
      "help.txt",

      "4",
      "UnitCard",
      "assets/_banners_logos_etc",
      paste0("^(?![Oo]ld_).*[Cc]ard.*",img_patt,collapse="|"),
      "help.txt",

      "5",
      "QRcode",
      "assets/_banners_logos_etc",
      "QR-code",
      NA,

      "6",
      "LearningEpaulette",
      "assets/_learning-plots",
      "GP-Learning-Epaulette\\.png",
      NA,

      "7",
      "LearningEpaulette_vert",
      "assets/_learning-plots",
      "GP-Learning-Epaulette_vert\\.png",
      NA,

      "8",
      "SupportingMedia",
      "assets/_other-media-to-publish",
      NA,
      "help.txt",

      "9",
      NA,
      "assets/_banners_logos_etc",
      "linkedin",
      NA,
      "10",
      NA,
      "assets/_banners_logos_etc",
      "bluesky",
      NA

    )


  # #copy everything except UNIT.json to the cloud
  # assets <- fs::dir_info(fs::path(WD, "published")) %>%
  #   dplyr::mutate(name = basename(.data$path)) %>%
  #   dplyr::relocate(.data$name) %>%
  #   dplyr::filter(.data$name != "UNIT.json") %>%
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
    # if(i==3){browser()}
    res_i <-
      stage_assets(
        WD = WD,
        rel_path = df_i$rel_path,
        pattern = patt,
        exclude = ex
      )
    if (!is_empty(res_i$result) & !inherits(res_i$result,"error")) {

      res_i$result %>% dplyr::mutate(key = df_i$key) %>% dplyr::relocate("key")
    }
  })

  if (!is_empty(assets)) {
    assets <- assets  %>%
      dplyr::bind_rows() %>%
      dplyr::rename(path = .data$path1)


    #local TZ
      loc_tz <-  Sys.timezone()
    #add modified date to assets
    assets$updated <- fs::file_info(assets$path)$modification_time
    #reassign to local timezone (for some weird reason, CST timestamps are in future)
     assets$updated <-
        lubridate::force_tz(assets$updated, tz = "UTC")


      assets$updated <-
        lubridate::with_tz(assets$updated, tz = loc_tz)


    #add expected cloud_path
    assets$cloud_path <- paste0(cloud_prefix, "/", assets$name)
  }




  #Don't do anything if there are no assets to upload
  if (is_empty(assets)) {
    message("No assets found. Skipping upload")
    out <-
      dplyr::tibble(
        key = NA,
        name = NA,
        log = NA,
        cloud_path = NA,
        download_url = NA
      )[0, ]

    #Start Big Else
  } else{
    # DELETE STUFF ------------------------------------------------------------
    # See what's already in the cloud
    upload_try <- gcs_contents(WD = WD, detail = "more") %>% catch_err(keep_results = TRUE)
    if(upload_try$success){
      uploaded <- upload_try$result
    }else{uploaded <- NA}
    if (!is_empty(uploaded)) {

      if (clear) {
        to_del <- uploaded %>% dplyr::rename(cloud_path = .data$name)
        #figure out what's missing
      } else{
        to_del <-
          dplyr::anti_join(uploaded[, "name"], assets[, "cloud_path"], by = c("name" =
                                                                                "cloud_path")) %>%
          dplyr::rename(cloud_path = .data$name)

      }
      #if there's anything to delete, delete it
      if (nrow(to_del) > 0) {
        delete_L <-
          gcs_delete(cloud_path = to_del$cloud_path,
                     bucket = bucket) %>% catch_err(keep_results = TRUE)
        message("the following items deleted from the cloud: \n")
        print(delete_L$result)

        #update uploaded
        uploaded <- gcs_contents(WD = WD, detail = "more")
      }

    }



    if (is_empty(uploaded)) {
      to_upload <- assets

    } else{
      # Prep assets and uploaded for comparing mod dates ------------------------

      uploaded$updated <-
        lubridate::force_tz(uploaded$updated, tz = "UTC")

      #reassign to local timezone
      uploaded$updated <-
        lubridate::with_tz(uploaded$updated, tz = loc_tz)


      merged_mod_times <-
        dplyr::left_join(
          assets[, c("cloud_path", "updated", "path", "key")],
          uploaded[, c("name", "updated")],
          by = c("cloud_path" = "name"),
          suffix = c(".local", ".cloud")
        )


      to_upload <- merged_mod_times %>%
        dplyr::filter(.data$updated.local > .data$updated.cloud |
                        is.na(.data$updated.cloud))

    }


    # UPLOAD STUFF ------------------------------------------------------------

    if (nrow(to_upload) > 0) {
      upload_L <- gcs_add(assets = to_upload,
                          bucket = bucket,
                          key = to_upload$key) %>% catch_err(keep_results = TRUE)
      message("the following missing assets uploaded to the cloud: \n")
      print(upload_L$result)
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
    keys <- unique_sans_na(out$key)

    fm_update_list <- purrr::set_names(keys) %>%
      purrr::map(., \(x) {
        df_x <- out %>% dplyr::filter(.data$key == x)
        df_x$download_url
      })

    test_fm_update <-
      update_fm(WD = WD, change_this = fm_update_list,recompile = FALSE)

    summ <-
      dplyr::tibble(
        success = convert_T_to_check(c(test_uploaded, test_fm_update)),
        task = c(
          "synced assets with the cloud",
          "updated all URLs in front-matter"
        )
      )
    print(summ)
  }#End big else

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
