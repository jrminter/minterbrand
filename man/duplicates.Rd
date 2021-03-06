% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr_custom_functions.R
\name{duplicates}
\alias{duplicates}
\title{Extract duplicate rows}
\usage{
duplicates(data, ...)
}
\arguments{
\item{...}{Columns to evaluate for duplication. Works via \code{group_by()}.}
}
\value{
Filtered dataframe with duplicates in given columns
}
\description{
Extract all rows with duplicated values in the given columns
}
\examples{
mtcars \%>\% duplicates(mpg)
}
