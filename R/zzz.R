.onLoad <- function(libname,pkgname){

  #packageStartupMessage("Welcome to RegCensus API - Pulling initial data")
  R.cache::saveCache(object = get_series_period(),
                     key = list(c("seresYearID","summary",
                                  "periodStartDate","periodEndDate",
                                  "periodCode","earliestDate","latestDate",
                                  "earliestPeriodCode","latestPeriodCode",
                                  "recordsAvailable","series.seriesID",
                                  "series.seriesName","series.topic.topicID",
                                  "jurisdiction.jurisdictionID",
                                  "documentSubtype.documentSubtypeID",
                                  "documentSubtype.subtypeName")),
                     compress = T)

  #To Do: write function to get all industries, regardless of jurisdiction
  R.cache::saveCache(object = get_industries(38),
                     key = list(c("industryID","jurisdiction","industryCode",
                                  "industryName","industryDescription",
                                  "industryCodeVersion","industryCodeStandard")),
                     compress = T)
}
.onAttach <- function(libname, pkgname){

}
