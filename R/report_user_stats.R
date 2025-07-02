#' Report information about GP users
#'
#' Provide basic information about users
#'
#' @param verbosity passed to [httr2::req_perform()]; default=1
#' @param view_data logical; do you want to open retrieved user data with [View()]? Default=TRUE
#' @param dev logical; if FALSE (default), gets catalog from the production gp-catalog. Otherwise, from the dev catalog.
#' @returns Summary tibble
#' @export
#'

report_user_stats <- function(verbosity = 1,
                              view_data = TRUE,
                              dev=FALSE) {
  # Get the user data
  users <- gp_api_query_users(verbosity = verbosity, dev=dev)
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
  tot_class_size <- sum(users$classSize, na.rm = TRUE)

  users %>% dplyr::filter(.data$classSize > 0) %>%
    dplyr::select(
    "email",
    "classSize",
    "mailingListStatus",
    "siteVisitReasonsDefault",
    "siteVisitReasonsCustom"
  )


# Add inferred location from zip codes ------------------------------------


  #stupid fix for bad programming; writing my own code
  zip2city <- \(zip){
  zip_code_db <- zipcodeR::zip_code_db
  output <- zip_code_db %>% dplyr::filter(.data$zipcode == zip) %>%
    dplyr::pull(.data$post_office_city)
  if(length(output) == 0){
    output <- NA
  }
  output
  }

  users <- users  %>%
    dplyr::rowwise() %>%
    dplyr::mutate(zip_city=zip2city(.data$zipCode)) %>%
    dplyr::mutate(state=stringr::str_extract(.data$zip_city,", (\\w{2})",1 )) %>%
    dplyr::relocate("zip_city", .after = "mailingListStatus")

  # Set createdAt date to 08-Dec-2024 for accounts added before that key was created
  users$createdAt <- ifelse(is.na(users$createdAt),
                            "08-Dec-2024",
                            users$createdAt)


  # Create a summary tibble
  summary <- dplyr::tibble(
    total_users = num_users,
    n_subscribed = num_subscribed,
    n_teachers = num_teacher_users,
    class_size = tot_class_size,
    n_states = length(unique_sans_na(users$state)),
    active_users = num_active_users,
    inactive_users = num_inactive_users

  )

  users2 <- users %>%
    dplyr::mutate(createdAt_date = as.Date(.data$createdAt, format = "%d-%b-%Y")) %>%
    dplyr::mutate(Year = lubridate::year(.data$createdAt_date)) %>%
    dplyr::mutate(Month = sprintf("%02d", lubridate::month(.data$createdAt_date))) %>%
    dplyr::mutate(Created = paste0(sprintf(.data$Month, fmt = ), "-", .data$Year))

  users2$Created <- factor(users2$Created,
                           levels = rev(unique(users2$Created)),
                           ordered = TRUE)



  #define theme
  report_theme <-
    galacticEdTools::theme_galactic(base.theme = "bw") +ggplot2::theme(
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = -20)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = -10))
    )

  #ggplot stuff
  user_growth <- users2 %>%
    dplyr::filter(!is.na(.data$createdAt_date)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$Created)) +
    report_theme+
    ggplot2::geom_bar(fill=gpColors("dark lightning purple")) +

    ggplot2::geom_text(
      stat = "count",
      y = 0,
      colour = "white",
      size = 5,
      ggplot2::aes(label =
                     ggplot2::after_stat(.data$count)),
      vjust = -0.5
    ) +

    ggplot2::labs(
      subtitle =paste0("GP User accounts created per month (Tot=",nrow(users2),")"),
      x = "",
      caption = paste0(" as of ", lubridate::today())
    )


  # make plot of running total of collective class size


  running_tot_students <- users2 %>%
    dplyr::group_by(.data$Created) %>%
    dplyr::summarise(total_class_size = sum(.data$classSize, na.rm = TRUE)) %>%
    dplyr::mutate(running_total = cumsum(.data$total_class_size))%>%
    #plot
    ggplot2::ggplot(ggplot2::aes(x = .data$Created, y = .data$running_total, group=1)) +
    report_theme+
    ggplot2::geom_line(colour=gpColors("flare fucsia"),linewidth=2) +
    ggplot2::labs(
      subtitle = paste0("Total class size of GP Teacher Users (",scales::comma(sum(users2$classSize,na.rm=T)),
                        ")"),
      x = "",
      y = "Total class size"

    )

  # stacked combined plot
  KPIs <- patchwork::wrap_plots(running_tot_students,user_growth, ncol = 1,nrow=2,heights=c(0.7,0.3)) +
    patchwork::plot_annotation(
      title = "Galactic Polymath Impact Metrics",
      subtitle = "Data from teach.galacticpolymath.com"
    )


  plot(KPIs)

  if (view_data) {
    View(users2)
  }
  # Return a list with the data, graph, and summary
  out <- list(data = users2,
       graph = KPIs,
       summary = summary)
  print(out)
    # summary stats -----------------------------------------------------------
  new_7_days <-sum(users2$account_age<=7,na.rm=T)
  new_30_days <-sum(users2$account_age<=30,na.rm=T)
  total <- nrow(users2)

  message(rep("-",20))
  message("User summary (galacticpolymath.com):\n- TOTAL: ",total,"\n- new in past week: ",new_7_days,"\n- new in past month: ",new_30_days)

  message("- states represented: ",
          length(unique_sans_na(users$state)),
          " (",
          paste0(sort(unique_sans_na(users$state)), collapse = ", "),
          ")")

 invisible(out)
}
