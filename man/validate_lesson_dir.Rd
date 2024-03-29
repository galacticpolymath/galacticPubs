% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_lesson_dir.R
\name{validate_lesson_dir}
\alias{validate_lesson_dir}
\title{validate_lesson_dir}
\usage{
validate_lesson_dir(lesson_dir)
}
\arguments{
\item{lesson_dir}{path (or a vector of paths) to a potential lesson directory; Make sure path starts with leading "/" If it's not relative to the current working dir.}
}
\value{
logical: is this a good lesson directory path or not? Will return a vector as long as lesson_dir input.
}
\description{
Internal function for checking if a directory contains the expected structure of a lesson
}
