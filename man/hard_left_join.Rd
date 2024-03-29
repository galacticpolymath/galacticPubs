% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hard_left_join.R
\name{hard_left_join}
\alias{hard_left_join}
\title{hard_left_join}
\usage{
hard_left_join(df1, df2, by, as_char = FALSE, df1_cols_to_keep = NULL)
}
\arguments{
\item{df1}{the data frame (or tibble) whose structure you want to preserve; If it's an empty tibble, df2 columns matching df1 will be output}

\item{df2}{the data frame with new data you want to crush over df1 (destroying matching cells in df1)}

\item{by}{a single column name you want to use to match df1 and df2 values}

\item{as_char}{logical; do you want to override classes and make everything characters. This will avoid errors of matching incompatible types, but may have unexpected results for some data sets. Default=F}

\item{df1_cols_to_keep}{specify columns of df1 to keep as-is; default=NULL}
}
\value{
A data frame that has the structure of df1, with matching ids and columns replaced with df2 values
}
\description{
Merge one dataframe into another, replacing data frame 1's values with matching values from data frame 2 without a bunch of .x .y column duplication BS. This is a destructive function, so back up your data and make sure you're getting the expected output.
}
\details{
If df2 contains rows not matched in df1, they will get added to the result. Note that even if df2 has NA for a matching id, it will overwrite a value in df1. (That's why it's "hard" left_join).
}
\examples{
(df1<-dplyr::tribble(~id,~b,~c,~d,"Jim",1,2,3,"Bob",4,5,6))
(df2<-dplyr::tribble(~id,~b,~c,"Jim",1.1,2.1,"Bob",NA,5.2))
hard_left_join(df1,df2,by="id")
#df2 values overwrite matching columns; df1 columns not found in df2 are ignored


hard_left_join(df1,df2,by="id",df1_cols_to_keep="b")
#here df1 values for the b column are kept in output
}
