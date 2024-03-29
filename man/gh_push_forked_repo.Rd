% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gh_push_forked_repo.R
\name{gh_push_forked_repo}
\alias{gh_push_forked_repo}
\title{Push new "forked" gh_proj_name to GitHub}
\usage{
gh_push_forked_repo(WD = getwd())
}
\arguments{
\item{WD}{working directory (path to top level folder of the project on virtualized Google Drive for Desktop)}
}
\description{
This is what you do after you've copied a cloned a lesson gh_proj_name in order to make a new language version. See \code{\link[=how_fork_lesson]{how_fork_lesson()}}
}
\details{
This function expects that:
\itemize{
\item You have \href{https://cli.github.com/manual/installation}{gh cli} (github command line) installed
\item You're set up with git in Rstudio
\item You have credentials to write to galacticpolymath github repos
}
}
\seealso{
Other GitHub Functions: 
\code{\link{gh_reset_remote}()}
}
\concept{GitHub Functions}
