% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/learningChart.R
\name{learningChart}
\alias{learningChart}
\title{learningChart}
\usage{
learningChart(
  WD = "?",
  caption = NA,
  captionN = FALSE,
  centralText = NA,
  quotedTitle = NA,
  centralTextSize = 3.5,
  saveFile = TRUE,
  destFolder,
  fileName = "GP-Learning-Chart",
  dpi = 200,
  showPlot = TRUE,
  ...
)
}
\arguments{
\item{WD}{working directory; default=getwd(); if "?" supplied, will invoke \code{\link[=pick_lesson]{pick_lesson()}}. The basename of this working directory will then be used to find a match in the gp-lessons git project folder by calling \code{\link[=get_wd_git]{get_wd_git()}}. It's a little roundabout, but is consistent with lookups centering on the Google Drive project working directory.}

\item{caption}{quoted text you want to go at the bottom of the chart}

\item{captionN}{T/F, add the range of the number of standards per grade used to make the plot to caption? default=FALSE}

\item{centralText}{specify grades the chart is for; by default pulls most common gradeBand from compiledAlignment (e.g. "grades\verb{\\\\n}5-6")}

\item{quotedTitle}{the quoted title used to attribute the learning chart (e.g. Knowledge and skills taught by 'quotedTitle')}

\item{centralTextSize}{multiplier for font size of centralText}

\item{saveFile}{T/F, save file or just print to screen?}

\item{destFolder}{where do you want to save the folder; by default in the "assets/learningPlots" folder, 1 level up from the working directory}

\item{fileName}{expects "somefilename" (file extension will be ignored)}

\item{dpi}{what resolution would you like the output in dots per inch? 300 by default}

\item{showPlot}{plot to screen or just save to file? default=T}

\item{...}{other arguments passed to \code{\link[grDevices]{png}}}
}
\value{
the learning chart plot object (grid format); Result can be plotted with grid::grid.draw(result). The chart file is saved to assets/GP_Learning_Chart.png by default
}
\description{
Make a GP Learning Chart. Will also run \code{\link[=upload_assets]{upload_assets()}} when it's done.
}
