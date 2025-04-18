% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data_yaml.R
\name{read_data_yaml}
\alias{read_data_yaml}
\title{Read Data Yaml.}
\usage{
read_data_yaml(.path = getwd(), name)
}
\arguments{
\item{.path}{Path to the \code{eatDaT} repository. Defaults to the current working directory.}

\item{name}{Name of the data set. The file will be named accordingly.}
}
\value{
Returns a named list.
}
\description{
Reads a data yaml that contains paths to the current data version (\code{'release_path'}),
the previous version (\code{'oldrel_path'})
and the current version number (\code{'version'}).
}
\examples{
## tbd

}
