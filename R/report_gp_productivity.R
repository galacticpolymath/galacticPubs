#' Make a report about GP's lesson and other outputs
#'
#' Like it says on the tin
#'
#' @return A report about GP's lesson and other outputs
#'
#' @export

report_gp_productivity <- \(){

  df <- batch_get_fm(c("PublicationStatus","numID","LsnCount","ReleaseDate"),"?!")
  gp_units <- df %>%
    dplyr::arrange(dplyr::desc(as.Date(.data$ReleaseDate))) %>%
    dplyr::mutate(num_locales=
                    unlist(purrr::map(.data$numID,~sum(df$numID==.x)))) %>%
    dplyr::distinct(.data$numID,.keep_all = TRUE)

  gp_units_published <- gp_units%>%  dplyr::filter(.data$PublicationStatus=="Live") %>% dplyr::summarise(num_units=length(.data$unit),num_lessons = sum(.data$LsnCount),num_multilocale_units=sum(.data$num_locales>1))

  in_development <- df %>% dplyr::filter(.data$PublicationStatus=="Proto")
  message("****Published units: ******")
  message(capture.output(print(as.data.frame(gp_units_published)),type = "message"))
  message("****In development units: ****")
  message(capture.output(print(as.data.frame(in_development)),type = "message"))
  message("******************************")

}
