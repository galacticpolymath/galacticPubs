% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_fm.R
\name{update_fm}
\alias{update_fm}
\title{update_fm (Update Front Matter)}
\usage{
update_fm(
  WD = getwd(),
  save_output = TRUE,
  return_fm = FALSE,
  reorder = TRUE,
  change_this = NULL,
  drive_reconnect = FALSE
)
}
\arguments{
\item{WD}{Working drive; default=getwd()}

\item{save_output}{do you want to save the updated front-matter to WD/meta/front-matter.yml? Default=TRUE}

\item{return_fm}{logical; if TRUE, returns the the updated front-matter; if FALSE (default), returns TRUE/FALSE of success}

\item{reorder}{do you want to reorder the resulting list, based on template order? default=TRUE}

\item{change_this}{A list of values to change in the front matter. Default=NULL. Example: list(RebuildAllMaterials=TRUE,Language="Italian) will trigger a full lesson rebuild when \code{\link[=compile_lesson]{compile_lesson()}} is run and change the Language and locale.}

\item{drive_reconnect}{logical; do you want to re-look-up all \verb{Gdrive*} keys? (might be useful if old files have been replaced instead of updated and \verb{Gdrive*} keys point to a trashed file); default=F}
}
\value{
returns logical of success
}
\description{
Checks for new galacticPubs front-matter_TEMPLATE.yml. If found, it will add new fields,  return the updated object, and write to drive (meta/front-matter.yml) if requested.
}
\details{
If meta/front-matter.yml not found, it is created from the template. Will also combine language and country info to create locale, and add GPCatalogURL if those fields are blank. Attempts to find the lesson on Google Drive in GP-Studio and add GdriveDirID if missing.
}
