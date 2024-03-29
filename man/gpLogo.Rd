% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gpLogo.R
\name{gpLogo}
\alias{gpLogo}
\title{gpLogo}
\usage{
gpLogo(
  ggObj,
  xNPC = 0.9,
  yNPC = 0.9,
  which = "horiz_logoWords_gradWhite",
  size = 0.1,
  cloudinaryString = NULL
)
}
\arguments{
\item{ggObj}{the ggObject you want to add a logo to}

\item{xNPC}{x coordinate (0,1) for start of logo}

\item{yNPC}{y coordinate (0,1) for start of logo}

\item{which}{which logo you want? One of: "grad_logo_gradTrans","grad_logo_gradWhite", "horiz_logoWords_gradTrans", "horiz_logoWords_gradWhite", "horiz_logoWords_whiteAblue","horiz_logoWords_whiteBlack"}

\item{size}{how big should the logo be as a proportion (0,1); 0.1 by default}

\item{cloudinaryString}{any cloudinary processing codes to modify the PNG? \href{https://cloudinary.com/documentation/transformation_reference}{Ref Link}}
}
\description{
add a Galactic Polymath logo to a ggplot2 object
}
