#' Report information about GP users
#'
#' Provide basic information about users
#'
#' @param verbosity passed to [httr2::req_perform()]; default=1
#' @param view_data logical; do you want to open retrieved user data with [View()]? Default=TRUE
#' @returns Summary tibble
#' @export
#'

report_user_stats <- function(verbosity = 1,view_data=TRUE) {
  # Get the user data
  users <- gp_api_query_users(verbosity = verbosity)
  # Get the number of active users
  users$active_in_30_days <- users$lastSignIn >= Sys.Date() - 30
  #Make NAs be inactive, cuz they would have this value if they'd signed in recently
  users$active_in_30_days <- ifelse(is.na(users$active_in_30_days), FALSE, TRUE)

  # Get the number of users
  num_users <- nrow(users)

  num_active_users <- sum(users$active_in_30_days)

  # Get the number of inactive users
  num_inactive_users <- sum(!users$active_in_30_days)

  # Get the number of users with admin privileges
  num_teacher_users <- sum(users$isTeacher)

  #email subscribers
  num_subscribed <- sum(users$mailingListStatus == "onList", na.rm = TRUE)

  # Estimated reach
  tot_class_size <- sum(users$classroomSize.num, na.rm = TRUE)

  users %>% dplyr::filter(.data$classroomSize.num > 0) %>% dplyr::select(
    "email",
    "classroomSize.num",
    "mailingListStatus",
    "reasonsForSiteVisit.reason-for-visit-0"
  )

  # Create a summary tibble
  summary <- dplyr::tibble(
    total_users = num_users,
    n_subscribed = num_subscribed,
    n_teachers = num_teacher_users,
    class_size = tot_class_size,
    active_users = num_active_users,
    inactive_users = num_inactive_users
  )

  users2 <- users %>%
    dplyr::filter(!is.na(.data$createdAt)) %>%
    dplyr::mutate(createdAt_date = as.Date(.data$createdAt, format = "%d-%b-%Y")) %>%
    dplyr::mutate(Year = lubridate::year(.data$createdAt_date)) %>%
    dplyr::mutate(Month = sprintf("%02d", lubridate::month(.data$createdAt_date))) %>%
    dplyr::mutate(Created = paste0(sprintf(.data$Month, fmt =), "-", .data$Year))

  users2$Created <- factor(users2$Created,
                           levels = rev(unique(users2$Created)),
                           ordered = TRUE)

  #ggplot stuff
  user_growth <- users2 %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$Created)) +
    galacticEdTools::theme_galactic(base.theme = "bw") +
    ggplot2::geom_bar() +
<<<<<<< HEAD
    ggplot2::geom_text(stat = "count",y=1,colour="white",size=7,
                       ggplot2::aes(label =
                                      ggplot2::after_stat(.data$count)), vjust = -0.5) +
=======
    ggplot2::geom_text(stat = "count",y=1,colour="white",size=7, ggplot2::aes(label = ggplot2::after_stat(.data$count)), vjust = -0.5) +
>>>>>>> main
    ggplot2::labs(
      subtitle = "GP User accounts created per month",
      x = "",
      caption = paste0("Tot users=", nrow(users), " as of ", lubridate::today())
    )
  plot(user_growth)

  if(view_data){View(users2)}

  list(data=users2,
       graph=user_growth,
       summary=summary)
}
