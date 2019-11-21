# LIST TOPICS, SERIES, AND JURISDICTIONS

#' Generate a list of Topics in RegData
#'
#' @param topicID An integer.
#' @return List of topics and their corresponding IDs in the console.
#' @examples
#' list_topics()
#' list_topics(1)
list_topics <- function(topicID = NA) {
    json <- regdata_json_request("topics", topicID)
    if (length(json) > 0) {
        topic_text <- paste0(json$topicName, " (ID: ", json$topicID, ")")
    } else {
        topic_text <- "No topic found"
    }
    return(topic_text)
}

#' Generate a list of Jurisdictions in RegData. This function is helpful for identifying the jurisdictions covered and their identifiers used in the RegData system. Note that the function accepts one jurisdictionID, distinct from FIPs or geoCodes.
#'
#' @param jurisdictionID An integer.
#' @return List of jurisdictions and their corresponding IDs and geoCodes in the console.
#' @examples
#' list_jurisdictions()
#' list_jurisdictions(38)
list_jurisdictions <- function(jurisdictionID = NA) {
    json <- regdata_json_request("jurisdictions", jurisdictionID)
    if (length(json) > 0) {
        jurisdiction_text <- paste0(json$jurisdictionName, ", ", json$country,
                                    " (jurisdictionID: ", json$jurisdictionID,
                                    "; geoCode: ", json$geoCode$geoCode,
                                    ")")
    } else {
        jurisdiction_text <- "No jurisdiction"
    }
    return(jurisdiction_text)
}

#' Generate a list of Agencies in RegData. Note that the function accepts a vector of multiple jurisdictionIDs, distinct from FIPs or geoCodes.
#'
#' @param jurisdictionIDs An integer or vector of integers.
#' @return List of agencies and their corresponding IDs and geoCodes in the console.
#' @examples
#' list_agencies()
#' list_agencies(38)
#' list_agencies(c(33, 38))
list_agencies <- function(jurisdictionIDs = NULL) {
    ## Return agencies available in RegData (allowed to pass multiple jurisdiction IDs to get agencies by jurisdiction)
    ## I use NULL rather than NA for this function to deal with case of multiple ids being passed.
    ## if (is.na(c(10,NA))) returns FALSE because it only checks first value.
    if (!is.null(jurisdictionIDs)) {
        id_str <- paste(jurisdictionIDs, collapse=",")
        json <- regdata_json_request(paste0("agencies/jurisdiction?jurisdictions=", id_str), NA)
    } else {
        json <- regdata_json_request("agencies", NA)
    }

    if (length(json) > 0) {
        agency_text <- paste0(json$agencyName, ", "," (agencyID: ", json$agencyID, ")")
    } else {
        agency_text <- "No jurisdiction"
    }
    return(agency_text)

}

#' Generate a list of Series in RegData and their corresponding series IDs and series codes.
#'
#' You can obtain information on series by:
#' \itemize{
#' \item series ID (default). Only one series ID is accepted. Lists by series do not require an id to be defined.
#' \item series code(s). Multiple series codes can be selected by passing a vector of strings as the "id" parameter.
#' \item agency or agencies. The function accepts multiple agency IDs passed as a vector.
#' \item jurisdictions. The function accepts jurisdiction IDs (distinct from geoCodes) as a vector.
#' \item topic. Only one topic ID is allowed.
#' }
#' No "id" is required when selecting series by series. See first example. When selecting series by series codes, agencies, jurisdictions, or topic, the "id" must be specified.
#'
#' @param id An integer or vector of integers. See description above and examples below for when vectors are allowed and when single values are required.
#' @param by A string specifying what type of identifier you are trying to pass in the "id" parameter. Default is "series," and accepted values are "seriesCodes", "agencies", "jurisdictions", and "topic".
#' @return List of series and their corresponding IDs and series codes in the console.
#' @examples
#' list_series()
#' list_series(1)
#' list_series(1, by="series")
#' list_series("1", by="series")
#' list_series(c("RG_RSTR0000002A", "RG_RSTR0000001A", "RG_RSTR0000007A"), by="series")
#' list_series(c(244, 216, 206, 189), by="agencies")
#' list_series(c("06", "10"), by="jurisdictions")
#' list_series(1, by="topic")
list_series <- function(id = NA, by = c("all", "series", "agencies", "industries", "jurisdictions", "topics")) {
    by <- match.arg(by)

    ## check length of id argument to make it sure it is compatible with selected series filter (write error messages later)
    id_len <- length(id)

    if (by == "all") {
        url <- regdata_json_requestURL("series", id)
        json <- regdata_json_request("series", id)
    } else if (by == "series") {
        if(is.na(id))
            stop("You must provide the series in the 'id' parameter.")
        # more than one seriesCode can be passed, so collapse into comma-separated list
        idstr <- paste(id, collapse=",")
        url <- regdata_json_requestURL(paste0("series/seriesCode?series=", idstr), id=NA)
        json <- regdata_json_request(paste0("series/seriesCode?series=", idstr), id=NA)
    } else if (by == "agencies") {
        if(is.na(id))
            stop("You must provide the agency in the 'id' parameter.")
        # more than one agency can be passed, so collapse into comma-separated list
        idstr <- paste(id, collapse=",")
        url <- regdata_json_requestURL(paste0("series/agencies?agencies=", idstr), id=NA)
        json <- regdata_json_request(paste0("series/agencies?agencies=", idstr), id=NA)
    } else if (by == "jurisdictions") {
        # more than one geoCode can be passed, so collapse into comma-separated list
        if(is.na(id))
            stop("You must provide jurisdiction in the 'id' parameter.")
        idstr <- paste(id, collapse=",")
        url <- regdata_json_requestURL(paste0("series/jurisdiction?jurisdictions=", idstr), id=NA)
        json <- regdata_json_request(paste0("series/jurisdiction?jurisdictions=", idstr), id=NA)
    } else {
        if(is.na(id))
            stop("You must provide the topic in the 'id' parameter.")
        url <- regdata_json_requestURL("series/topics", id)
        json <- regdata_json_request("series/topics", id)
    }

    if (length(json) > 0) {
        series_text <- paste0(json$seriesName, " (Series ID: ", json$seriesID, "; Series Code: ", json$seriesCode, ")")
    } else {
        series_text <- "No series found"
    }

    return(series_text)
}

#' Generate a list of Start Years, End Years, and Total Years Available for Series by Jurisdiction in RegData.
#'
#' Each item in the list returns:
#' \itemize{
#' \item Start Year
#' \item End Year
#' \item Number of Years Available
#' \item Series ID
#' \item Series Code
#' \item Jurisdiction ID
#' \item Geo Code
#' }
#'
#' @param jurisdictionID An integer. Only one jurisdiction value may be passed as an argument.
#' @return List of series with information for each series regarding start year, end years, and total years available, by jurisdiction
#' @examples
#' list_seriesyear()
#' list_seriesyear(4)
list_seriesyear <- function(jurisdictionID = NA) {
    if (is.na(jurisdictionID)) {
        json <- regdata_json_request("jurisdictions/available", NA)
    } else {
        json <- regdata_json_request(paste0("jurisdictions/", jurisdictionID, "/available"), NA)
    }

    if (length(json) > 0) {
        seriesyear_text <- paste0(json$series$seriesName, " in ", json$jurisdiction$jurisdictionName,
                                  " (startYear: ", json$startYear,
                                  "; endYear: ", json$endYear,
                                  "; yearsAvailable: ", json$yearsAvailable,
                                  "; seriesID: ", json$series$seriesID,
                                  "; seriesCode: ", json$series$seriesCode,
                                  "; jurisdictionID: ", json$jurisdiction$jurisdictionID,
                                  "; geoCode: ", json$jurisdiction$geoCode$geoCode,
                                  ")")
    } else {
        seriesyear_text <- "No series years found"
    }

    return(seriesyear_text)
}
