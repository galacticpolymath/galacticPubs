% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lesson_go_draft.R
\name{lesson_go_draft}
\alias{lesson_go_draft}
\title{lesson_go_draft}
\usage{
lesson_go_draft(WD = "?")
}
\arguments{
\item{WD}{is working directory of the project; easiest way to supply a different lesson is with "?", which will invoke \link{pick_lesson(shared_drive = "l")}; default is WD=getwd()}
}
\description{
Unstage a lesson (i.e. remove public access and make it editable again). Does the following:
\enumerate{
\item Move lesson project directory from GP-LIVE to GP-Studio (Making it editable to those with access to GP-Studio)
\item Move lesson teaching-materials from GalacticPolymath Shared Drive
\item Rename from "MediumTitle" in public folder back to "teaching-materials"
\item Make the following updates to front-matter:
\itemize{
\item PublicationStatus: 'Draft'
\item GdriveHome: 'GP-Studio'
\item GdrivePublicID: NA
}
}
}
