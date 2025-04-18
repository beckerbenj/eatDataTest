% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_data_diff.R
\name{create_diff}
\alias{create_diff}
\title{Create Data and Meta Data Diff.}
\usage{
create_diff(.path = getwd(), name, ID_var)
}
\arguments{
\item{.path}{Path to the \code{eatDaT} repository. Defaults to the current working directory.}

\item{name}{Name of the data set. The file will be named accordingly.}

\item{ID_var}{Name of the id variable in bot data set.}
}
\value{
Creates a .xlsx. Returns \code{NULL}.
}
\description{
Create a data diff that contains a list of differing variables and cross tables
for each differing variable as well as a meta data diff.
}
\examples{
## tbd

}
