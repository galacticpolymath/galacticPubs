% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drive_open.R
\name{drive_open}
\alias{drive_open}
\title{drive_open}
\usage{
drive_open(drib)
}
\arguments{
\item{drib}{a Google drive dribble of a folder (usually the output of \code{\link[=drive_find_path]{drive_find_path()}})}
}
\value{
opens the first entry in the dribble in the browser
}
\description{
Open a Google drive file, given a dribble or a path.
}
\details{
If you give it a dribble, it will call \code{\link[googledrive:drive_browse]{googledrive::drive_browse()}}; if you give it a path, it will try to first resolve that path with \code{\link[=drive_find_path]{drive_find_path()}}, then open it in your browser.
}
\seealso{
Other Google Drive Functions: 
\code{\link{drive_contents}()},
\code{\link{drive_copy_missing_files}()},
\code{\link{drive_cp_dir}()},
\code{\link{drive_find_path}()},
\code{\link{drive_get_info}()},
\code{\link{drive_inSync}()},
\code{\link{drive_new_from_template}()},
\code{\link{init_lesson_meta}()},
\code{\link{update_teach_links}()}
}
\concept{Google Drive Functions}
