# RETURN DATA FRAMES FROM USER REQUESTS
get_topics <- function(topicID = NA) {
    topics <- regdata_json_request("topics", topicID)
    topic_df <- as.data.frame(topics)
    names(topic_df) <- snakecase::to_snake_case(names(topic_df))
    return(topic_df)
}

get_jurisdictions <- function(jurisdictionID = NA) {
    jurisdictions <- regdata_json_request("jurisdictions", jurisdictionID)
    jurisdiction_df <- as.data.frame(jurisdictions)
    names(jurisdiction_df) <- snakecase::to_snake_case(names(jurisdiction_df))
    return(jurisdiction_df)
}

get_industries <- function(jurisdictionID=NA){
    if(is.na(jurisdictionID)){
        print("You must specify a jurisdiction. Select from: ")
        print(list_jurisdictions())
        stop()
    }
    industries <- regdata_json_request("industries?jurisdictions=",jurisdictionID)
    industry_df <-as.data.frame(industries)
}

get_agencies <- function(jurisdictionIDs = NULL) {
    if (!is.null(jurisdictionIDs)) {
        id_str <- paste(jurisdictionIDs, collapse=",")
        json <- regdata_json_request(paste0("agencies/jurisdiction?jurisdictions=", id_str), NA)
    } else {
        json <- regdata_json_request("agencies", NA)
    }
    agencies_df <- as.data.frame(json)
    names(agencies_df) <- snakecase::to_snake_case(names(agencies_df))
    return(agencies_df)
}

get_series <- function(id = NA, by = c("all","series", "agencies", "industries", "jurisdictions", "topics")) {
    by <- match.arg(by)

    if (by == "all") {
        json <- regdata_json_request("series", id)
    } else if (by == "series") {
        # more than one seriesCode can be passed, so collapse into comma-separated list
        idstr <- paste(id, collapse=",")
        json <- regdata_json_request(paste0("series/seriesID=", idstr), id=NA)
    } else if (by == "agencies") {
        # more than one agency can be passed, so collapse into comma-separated list
        idstr <- paste(id, collapse=",")
        json <- regdata_json_request(paste0("series/agencies?agencies=", idstr), id=NA)
    } else if (by == "jurisdictions") {
        # more than one geoCode can be passed, so collapse into comma-separated list
        idstr <- paste(id, collapse=",")
        json <- regdata_json_request(paste0("series/jurisdiction?jurisdictions=", idstr), id=NA)
    } else {
        json <- regdata_json_request("series/topics", id)
    }

    series_df <- as.data.frame(json)
    names(series_df) <- snakecase::to_snake_case(names(series_df))
    return(series_df)
}

get_seriesyear <- function(jurisdictionID = NA) {
    if (is.na(jurisdictionID)) {
        json <- regdata_json_request("jurisdictions/available", NA)
    } else {
        json <- regdata_json_request(paste0("jurisdictions/", jurisdictionID, "/available"), NA)
    }

    seriesyear_df <- as.data.frame(json)
    names(seriesyear_df) <- snakecase::to_snake_case(names(seriesyear_df))
    return(seriesyear_df)
}

# Values API request requires three parameters
#   1. seriesCode(s)
#   2. time(s)
get_values <- function(jurisdiction, series, time, agency=NA , industry=NA, dateIsRange = TRUE) {
    # E.g., http://ec2-54-225-4-62.compute-1.amazonaws.com:8080/regdata/values?geo=06&seriesCode=RG_RSTR00000002NA&time=2019
    geoCodestr <- paste(jurisdiction, collapse=",")
    seriesCodestr <- paste(series, collapse=",")
    timeStr <- paste(time, collapse = ",")
    industryStr <-paste(industry,collapse=",")
    agencyStr <-paste(agency,collapse=",")

    if(!is.na(series)){
        url_compose <- paste0(get_baseURL(),"/values?series=",seriesCodestr)
    }else
    {
        print("Valid series ID required. Select from the following list:")
        print(list_series())
        stop("Processing terminated.")

    }
    if(!is.na(jurisdiction))
        url_compose <- paste0(url_compose,"&jurisdiction=",geoCodestr)
    else
        stop("Jurisdiction is required.")

    if(!is.na(agency)){
        url_compose <- paste0(url_compose,"&agency=",agencyStr)
    }
    if(!is.na(industry)){
        url_compose <- paste0(url_compose,"&industry=",industryStr)
    }
    if(!is.na(time)){
        url_compose <- paste0(url_compose,"&time=",timeStr)
    }else{
        stop("Time is required.")
    }

    if (dateIsRange) {
        url_compose <- paste0(url_compose,"&dateIsRange=", dateIsRange)
        #print(url_compose)
    }
    json <- jsonlite::fromJSON(url_compose, flatten = T) #8/25/2019: Kofi added the flatten option to T, included full fromJSON reference
    values_df <- as.data.frame(json)
    names(values_df) <- snakecase::to_snake_case(names(values_df))
    return(values_df)
}
