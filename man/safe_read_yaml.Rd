% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/safe_read_yaml.R
\name{safe_read_yaml}
\alias{safe_read_yaml}
\title{safe_read_yaml}
\usage{
safe_read_yaml(
  yaml_path = NULL,
  WD = NULL,
  eval.expr = TRUE,
  checkWD = TRUE,
  auto_init = TRUE,
  standardize_NA = FALSE
)
}
\arguments{
\item{yaml_path}{full path to the front-matter.yml file (usually in format '/users/lotsaSubfolders/gp-lessons/front-matter.yml')}

\item{WD}{Google Drive Working Directory. Can use "?" to pass to \code{\link[=parse_wd]{parse_wd()}}}

\item{eval.expr}{boolean; do you want to evaluate expression in yaml prepended with '!expr '? Default=TRUE}

\item{checkWD}{do you want to run \code{\link[=check_wd]{check_wd()}} which will produce warning messages if working directory assumptions aren't met?; default= TRUE}

\item{auto_init}{logical; do you want to automatically create a front-matter.yml file if it's not found? Runs \code{\link[=init_fm]{init_fm()}}; default=TRUE}

\item{standardize_NA}{logical; do you want all "",NULL,list(), etc. to be read in as NA using \code{\link[=is_empty]{is_empty()}}? default=FALSE}
}
\description{
Wrapper for \link[yaml:read_yaml]{read_yaml} that simplifies all null and missing data to NA. Creates front-matter.yml if not found
}
