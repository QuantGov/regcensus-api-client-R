# RETURN DATA FRAMES FROM USER REQUESTS

get_topics <- function(topicID = NA) {
    topics <- regdata_json_request("topics", topicID)
    topic_df <- as.data.frame(topics)
    return(topic_df)
}

get_jurisdictions <- function(jurisdictionID = NA) {
    jurisdictions <- regdata_json_request("jurisdictions", jurisdictionID)
    jurisdiction_df <- as.data.frame(jurisdictions)
    return(jurisdiction_df)
}

get_agencies <- function(jurisdictionIDs = NULL) {
    if (!is.null(jurisdictionIDs)) {
        id_str <- paste(jurisdictionIDs, collapse=",")
        json <- regdata_json_request(paste0("agencies/jurisdiction?jurisdictions=", id_str), NA)
    } else {
        json <- regdata_json_request("agencies", NA)
    }
    agencies_df <- as.data.frame(json)
    return(agencies_df)
}

get_series <- function(id = NA, by = c("series", "seriesCodes", "agencies", "jurisdictions", "topic")) {
    by <- match.arg(by)

    if (by == "series") {
        json <- regdata_json_request("series", id)
    } else if (by == "seriesCodes") {
        # more than one seriesCode can be passed, so collapse into comma-separated list
        idstr <- paste(id, collapse=",")
        json <- regdata_json_request(paste0("series/seriesCode?seriesCodes=", idstr), id=NA)
    } else if (by == "agencies") {
        # more than one agency can be passed, so collapse into comma-separated list
        idstr <- paste(id, collapse=",")
        json <- regdata_json_request(paste0("series/agencies?agencies=", idstr), id=NA)
    } else if (by == "jurisdictions") {
        # more than one geoCode can be passed, so collapse into comma-separated list
        idstr <- paste(id, collapse=",")
        json <- regdata_json_request(paste0("series/jurisdiction?geoCodes=", idstr), id=NA)
    } else {
        json <- regdata_json_request("series/topics", id)
    }

    series_df <- as.data.frame(json)
    return(series_df)
}

get_seriesyear <- function(jurisdictionID = NA) {
    if (is.na(jurisdictionID)) {
        json <- regdata_json_request("jurisdictions/available", NA)
    } else {
        json <- regdata_json_request(paste0("jurisdictions/", jurisdictionID, "/available"), NA)
    }

    seriesyear_df <- as.data.frame(json)
    return(seriesyear_df)
}

# Values API request requires three parameters
#   1. seriesCode(s)
#   2. time(s)
get_values <- function(jurisdiction, seriesCode, time, agency = NA, industry = NA, dateIsRange = TRUE) {
    # E.g., http://ec2-54-225-4-62.compute-1.amazonaws.com:8080/regdata/values?geo=06&seriesCode=RG_RSTR00000002NA&time=2019
    geoCodestr <- paste(jurisdiction, collapse=",")
    seriesCodestr <- paste(seriesCode, collapse=",")
    timestr <- paste(time, collapse = ",")

    if (dateIsRange) {
        apicall <- paste0(get_baseURL(), "/values?jurisdictions=", geoCodestr, "&seriesCodes=", seriesCodestr, "&time=", timestr, "&dateIsRange=", dateIsRange)
    } else {
        apicall <- paste0(get_baseURL(), "/values?jurisdictions=", geoCodestr, "&seriesCodes=", seriesCodestr, "&time=", timestr)
    }
    json <- jsonlite::fromJSON(apicall, flatten = T) #8/25/2019: Kofi added the flatten option to T, included full fromJSON reference
    values_df <- as.data.frame(json)
    return(values_df)
}
