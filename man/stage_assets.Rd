% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stage_assets.R
\name{stage_assets}
\alias{stage_assets}
\title{stage_assets}
\usage{
stage_assets(
  current_data = NULL,
  WD = getwd(),
  dest_folder = NULL,
  clear = TRUE,
  status = NULL
)
}
\arguments{
\item{current_data}{either data read in from front-matter.yml or made with the Shiny helper function prep_input()}

\item{WD}{what's the project working directory? default: getwd()}

\item{dest_folder}{where you want things to go (defaults to www)}

\item{clear}{do you want to delete everything in the target directory? default: T}

\item{status}{if staging assets for publishing, you can toggle "Draft" or "Live"; default=NULL}
}
\description{
Copy all lesson assets to the desired location to prep for lesson publishing
}
