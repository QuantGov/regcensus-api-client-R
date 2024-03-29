% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_frames.R
\name{get_values}
\alias{get_values}
\title{Return dataframe with values of series of interest}
\usage{
get_values(
  jurisdiction,
  series,
<<<<<<< HEAD
  time,
  agency = NA,
  industry = NA,
  dateIsRange = TRUE
)
}
\arguments{
\item{dateIsRange}{}
=======
  date = c(2015, 2020),
  summary = TRUE,
  filtered = TRUE,
  document_type = 3,
  agency = 0,
  industry = "0",
  date_is_range = TRUE,
  document = NA,
  include_metadata = FALSE
)
}
\arguments{
\item{jurisdiction}{Integer - the ID of the jurisdiction of interest}

\item{series}{Series ID}

\item{date}{The date}

\item{summary}{Boolean - Return summary instead of document level data}

\item{filtered}{Boolean - Exclude poorly-performing industry classification results}

\item{document_type}{Integer - ID of document type}

\item{agency}{The agency ID}

\item{industry}{The industry code using the jurisdiction-specific coding system}

\item{date_is_range}{Boolean indicating whether the date parameter is range or should be treated as single data points}

\item{document}{Integer - List of document IDs to return data for. Use when requesting document-level data instead of
summary data.}

\item{include_metadata}{Boolean - Should dataframe include the metadata to make it more readable}
>>>>>>> kampaabeng
}
\value{
dataframe
}
\description{
Return dataframe with values of series of interest
}
\examples{
<<<<<<< HEAD
get_values(jurisdiction = 38, series = c(92), time = c(1990,2000),
=======
get_values(jurisdiction = 38, series = c(92), date = c('1990','2000'),
>>>>>>> kampaabeng
industry = c('111','33'), agency = c(66,111))
}
