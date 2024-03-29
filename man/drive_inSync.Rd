% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drive_inSync.R
\name{drive_inSync}
\alias{drive_inSync}
\title{drive_inSync}
\usage{
drive_inSync(path1, path2)
}
\arguments{
\item{path1}{path to file of interest (e.g. that we expect to be newer than path 1, e.g. if it is created from the other paths)
\itemize{
\item paths are resolved by \code{\link[=drive_find_path]{drive_find_path()}}
}}

\item{path2}{path to reference file (expected to be at least slightly older than path 1)}
}
\value{
logical; TRUE if path2>=path1 in age, within one second; otherwise FALSE
}
\description{
Check if two Google Drive objects exist, and whether path 1 (the most derived file) is the same age or newer than path 2- path n (which are used to create path 1). A way of thinking about it is, we're asking is path 1 newer than path 2? If not, return FALSE.
}
\seealso{
Other Google Drive Functions: 
\code{\link{drive_contents}()},
\code{\link{drive_copy_missing_files}()},
\code{\link{drive_cp_dir}()},
\code{\link{drive_find_path}()},
\code{\link{drive_get_info}()},
\code{\link{drive_new_from_template}()},
\code{\link{drive_open}()},
\code{\link{init_lesson_meta}()},
\code{\link{update_teach_links}()}
}
\concept{Google Drive Functions}
