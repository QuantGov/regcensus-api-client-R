% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list.R
\name{list_agencies}
\alias{list_agencies}
\title{Generate a list of Agencies in RegData. Note that the function accepts a vector of multiple jurisdictionIDs, distinct from FIPs or geoCodes.}
\usage{
list_agencies(jurisdictionIDs = NULL)
}
\arguments{
\item{jurisdictionIDs}{An integer or vector of integers.}
}
\value{
List of agencies and their corresponding IDs and geoCodes in the console.
}
\description{
Generate a list of Agencies in RegData. Note that the function accepts a vector of multiple jurisdictionIDs, distinct from FIPs or geoCodes.
}
\examples{
list_agencies()
list_agencies(38)
list_agencies(c(33, 38))
}
