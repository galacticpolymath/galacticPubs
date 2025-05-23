% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_teach_links_lsnHelper.R
\name{update_teach_links_lsnHelper}
\alias{update_teach_links_lsnHelper}
\title{update_teach_links_lsnHelper}
\usage{
update_teach_links_lsnHelper(
  dribble,
  set_grades = NULL,
  set_envir = NULL,
  set_lsn = NULL
)
}
\arguments{
\item{dribble}{the dribble for a "lesson" subfolder of a lesson}

\item{set_grades}{passed to\code{\link[=drive_get_info]{drive_get_info()}}}

\item{set_envir}{passed to \code{\link[=drive_get_info]{drive_get_info()}}
#' @param set_envir passed to \code{\link[=drive_get_info]{drive_get_info()}}}
}
\description{
Helper function for \code{\link[=update_teach_links]{update_teach_links()}} that extracts information for each lesson (if found). Only intended for internal use in update_teach_links.
}
