#' Report information about GP users
#'
#' Provide basic information about users
#'
#' @returns Summary tibble
#' @export
#'

report_user_stats <- function() {
  # Get the user data
  users <- gp_api_query_users()
  # Get the number of active users
  users$active_in_30_days <- users$lastSignIn >= Sys.Date() - 30
  #Make NAs be inactive, cuz they would have this value if they'd signed in recently
  users$active_in_30_days <- ifelse(is.na(users$active_in_30_days),FALSE,TRUE)

  # Get the number of users
  num_users <- nrow(users)

  num_active_users <- sum(users$active_in_30_days)

  # Get the number of inactive users
  num_inactive_users <- sum(!users$active_in_30_days)

  # Get the number of users with admin privileges
  num_teacher_users <- sum(users$isTeacher)

  #email subscribers
  num_subscribed <- sum(users$mailingListStatus=="onList",na.rm = TRUE)

# Estimated reach
  tot_class_size <- sum(users$classroomSize.num, na.rm = TRUE)

  users %>% dplyr::filter(.data$classroomSize.num>0) %>% dplyr::select("email","classroomSize.num","mailingListStatus", "reasonsForSiteVisit.reason-for-visit-0")

  # Create a summary tibble
  summary <- tibble(
    total_users = num_users,
    n_subscribed= num_subscribed,
    n_teachers= num_teacher_users,
    class_size = tot_class_size,
    active_users = num_active_users,
    inactive_users = num_inactive_users
  )

  return(summary)
}
