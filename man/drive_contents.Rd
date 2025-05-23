% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drive_contents.R
\name{drive_contents}
\alias{drive_contents}
\title{drive_contents}
\usage{
drive_contents(dribble, recursive = FALSE, ...)
}
\arguments{
\item{dribble}{a Google drive dribble of a folder (usually the output of \code{\link[=drive_find_path]{drive_find_path()}})}

\item{recursive}{default=FALSE; doesn't really work super well...passed to \code{\link[googledrive:drive_ls]{googledrive::drive_ls()}}.}

\item{...}{other arguments passed to \code{\link[googledrive:drive_find]{googledrive::drive_find()}}}
}
\value{
a dribble showing the contents of the desired folder
}
\description{
Show the contents of a Google drive folder, given a dribble
}
\details{
A shallow wrapper for \code{\link[googledrive:drive_ls]{googledrive::drive_ls()}} that uses just the id of a dribble object and can be piped from \code{\link[=drive_find_path]{drive_find_path()}}
}
\seealso{
Other Google Drive Functions: 
\code{\link{drive_copy_missing_files}()},
\code{\link{drive_cp_dir}()},
\code{\link{drive_find_path}()},
\code{\link{drive_get_info}()},
\code{\link{drive_inSync}()},
\code{\link{drive_new_from_template}()},
\code{\link{drive_open}()},
\code{\link{init_lesson_meta}()},
\code{\link{update_teach_links}()}
}
\concept{Google Drive Functions}
