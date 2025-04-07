#' Make a report about GP's lesson and other outputs
#'
#' Like it says on the tin
#'
#' @return A report about GP's lesson and other outputs
#'
#' @export

report_gp_productivity <- \(){

  df <- batch_get_fm(c("PublicationStatus","numID","LsnCount","ReleaseDate"),"?!")
  unique_units <- df %>%
    dplyr::mutate(num_locales=
                    unlist(purrr::map(.data$numID,~sum(df$numID==.x)))) %>%
    dplyr::distinct(.data$numID,.keep_all = TRUE)


  x%>%  dplyr::group_by(.data$PublicationStatus) %>% dplyr::summarise(num_units=dplyr::n(.data$length(.data$unit)),num_lessons = sum(.data$LsnCount))
  tot_num_lessons <- sum(num_lessons)

  tot_num_units <-x

}
