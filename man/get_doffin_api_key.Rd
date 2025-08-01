% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_doffin.R
\name{get_doffin_api_key}
\alias{get_doffin_api_key}
\title{Retrieve the Doffin API key}
\usage{
get_doffin_api_key(api_key = NULL)
}
\arguments{
\item{api_key}{Optionally, supply the API key directly. If NULL, will look for the DOFFIN_API_KEY environment variable.}
}
\value{
The API key as a string.
}
\description{
This function retrieves the Doffin API key from the `api_key` argument or the DOFFIN_API_KEY environment variable.
}
\keyword{internal}
