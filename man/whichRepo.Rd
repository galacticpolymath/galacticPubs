% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/whichRepo.R
\name{whichRepo}
\alias{whichRepo}
\title{whichRepo}
\usage{
whichRepo(WD, fullPath = FALSE)
}
\arguments{
\item{WD}{working directory for the lesson project folder for which you want to find out the associated GitHub repository}

\item{fullPath}{do you want to export the full git path, or just the repo name? Default=F (just name)}
}
\value{
Either a warning or the name of the github repository connected to the current Rstudio project (extracted from \code{rstudioapi::getActiveProject()} and a call to \verb{git remote -v})
}
\description{
Deprecated! Get the name of the GitHub repo associated with a project. No error catching at the moment.
}
\details{
This assumes you have Google Drive for Desktop set up, permissions to access lesson folder, and git installed and authenticated in Rstudio.
}
