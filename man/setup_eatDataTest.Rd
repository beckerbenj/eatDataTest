% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_eatDataTest.R
\name{setup_eatDataTest}
\alias{setup_eatDataTest}
\title{Setup \code{eatDataTest}.}
\usage{
setup_eatDataTest(.path = getwd())
}
\arguments{
\item{.path}{Path to the \code{eatDataTest} repository. Defaults to the current working directory.}
}
\value{
Creates repository infrastructure. Returns a markdown snippet.
}
\description{
Add the infrastructure for using the directory as a\code{eatDataTest} repository. This includes
\itemize{
\item a \code{data} folder
\item a \code{changelogs} folder
\item a \code{tests} folder
\item a \code{diff} folder
\item a \code{Readme.md} containing the following elements: a table to list all data sets and
instructions on how to use the repository for data contributers.
}
}
\examples{
## tbd

}
