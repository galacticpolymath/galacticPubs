% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compile_teach_it.R
\name{compile_teach_it}
\alias{compile_teach_it}
\title{compile_teach_it}
\usage{
compile_teach_it(
  WD = "?",
  teach_it_drib = NULL,
  rename_lessons = TRUE,
  prompt_rename = FALSE
)
}
\arguments{
\item{WD}{is working directory of the project; easiest way to supply a different lesson is with "?", which will invoke \code{\link[=parse_wd]{parse_wd()}}; default is WD=getwd()}

\item{teach_it_drib}{if you already have the teach-it.gsheet dribble looked up from \code{\link[=drive_find_path]{drive_find_path()}}, passing this object can can save some time; default = NULL}

\item{rename_lessons}{logical; do you want to rename lesson folders based on Titles tab? default= T takes about 2sec to check if nothing needs changing; uses helper function \code{\link[=zrename_lessons]{zrename_lessons()}}}

\item{prompt_rename}{logical, do you want to prompt user about whether to rename lessons? default=FALSE}
}
\value{
tibble of the compiled standards data; a JSON is saved to meta/JSON/teaching-materials.json
}
\description{
Compile Teaching Materials from a project's 'teach-it.gsheet'. Also renames folders based on info in the Titles tab and invokes \code{\link[=sweep_teaching_materials]{sweep_teaching_materials()}} to relocate scrap working files.
}
