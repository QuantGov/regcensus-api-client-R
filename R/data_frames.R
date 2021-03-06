# RETURN DATA FRAMES FROM USER REQUESTS



#' Return list of topics in RegCensus
#'
#' @param topicID
#'
#' @return dataframe
#' @export
#'
#' @examples
#' get_topics(1)
#' get_topics()
get_topics <- function(topicID = NA) {
    topics <- regdata_json_request("topics", topicID)
    topic_df <- as.data.frame(topics)
    names(topic_df) <- snakecase::to_snake_case(names(topic_df))
    return(topic_df)
}


#' Generate a list of jurisdictions in RegData
#'
#' @param jurisdiction An integer.
#' @return List of jurisdictions
#' @export
#' @examples
#' get_jurisdictions()
#' get_jurisdictions(1)
get_jurisdictions <- function(jurisdiction = NA) {
    jurisdictions <- regdata_json_request("jurisdictions", jurisdiction)
    jurisdiction_df <- as.data.frame(jurisdictions)
    names(jurisdiction_df) <- snakecase::to_snake_case(names(jurisdiction_df))
    return(jurisdiction_df)
}

#' Return a list of industries with data in RegCensus
#'
#' @param jurisdictionID An integer, the jurisdiction ID
#'
#' @return
#' @export
#'
#' @examples
#' get_industries(38)
get_industries <- function(jurisdictionID=NA){
    if(is.na(jurisdictionID)){
        print("You must specify a jurisdiction. Select from: ")
        print(list_jurisdictions())
        stop()
    }
    industries <- regdata_json_request("industries?jurisdictions=",jurisdictionID)
    industry_df <-as.data.frame(industries)
}

#' Return a dataframe of all agencies in a jurisdiction
#'
#' @param jurisdictionIDs An integer
#'
#' @return dataframe
#' @export
#'
#' @examples
#' get_agencies(38)
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

#' Return a dataframe with the series for the id specified
#'
#' @param id An integer, id for the by parameter
#' @param by
#'
#'
#' @return dataframe
#' @export
#'
#' @examples
#' get_series(by='all')
#' get_series(id=38, by = 'jurisdictions')
#' get_series(id=91, by = 'series')
get_series <- function(id = NA, by = c("all","series", "agencies",
                                       "industries", "jurisdictions", "topics")) {
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

#' Return dataframe with all jurisdictions-series-years of data available
#'
#' @param jurisdictionID
#'
#' @return dataframe
#' @export
#'
#' @examples
#' get_seriesyear()
#' get_seriesyear(38)
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


#' Return dataframe with values of series of interest
#'
#' @param jurisdiction
#' @param series
#' @param time
#' @param agency
#' @param industry
#' @param dateIsRange
#'
#' @return dataframe
#' @export
#'
#' @examples
#' get_values(jurisdiction = 38, series = c(92), time = c(1990,2000),
#' industry = c('111','33'), agency = c(66,111))

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

#' Return values for a list of jurisdictions-series-year combination. It will return data for both
#' national and sub-national (for example, state/province) within the jurisdiction.
#'
#' @param jurisdiction Jurisdiction IDs for the data
#' @param series Series IDs
#' @param years Integers for years
#' @param industries
#' @param dateIsRange A boolean to indicate if the years values is range instead of just single values
#' @param filtered A boolean
#'
#' @return dataframe
#' @export
#'
#' @examples
#' get_country_series(jurisdiction=c(38), series=(1,2), years=c(2010,2011))
#' get_country_series(jurisdiction=c(38), series=(1,2), years=c(2010,2011), industries=c('111','521'))
get_country_values<-function(jurisdiction=c(38,75), series = c(1,2),
                             years = c(2010,2011), industries = NA,
                             dateIsRange=TRUE, filtered=TRUE){
    if(is.na(years)){
      stop("You need to include valid years")
    }else
        yearStr <- paste0(years, collapse=",")

    if(is.na(jurisdiction)){
        print("You need to select at least one of the following jurisdiction IDs")
        print(list_jurisdictions())
        stop()
    }else{
        jurisdictionStr <- paste0(jurisdiction, collapse=",")
        url_compose <- paste0(get_baseURL(),"/values/country?countries=",jurisdictionStr,"&years=",yearStr)
    }

    if(is.na(series)){
        print("You need to select at least one of the following series IDs")
        print(list_series())
        stop()

    }else{
        seriesStr <- paste0(series, collapse=",")
        url_compose <- paste0(url_compose, "&series=",seriesStr)
    }

    if(is.na(industries)){
        industriesStr <- '0'
    }else{
        industriesStr <- paste0(industries, collapse=",")
        url_compose <- paste0(url_compose, "&industries=",industriesStr)
    }

    if(dateIsRange==FALSE)
        url_compose <- paste0(url_compose,"&dateIsRange=", dateIsRange)

    if(filtered==FALSE)
      url_compose <- paste0(url_compose,"&filteredOnly=",filtered)

    print(url_compose)

    json <- jsonlite::fromJSON(url_compose, flatten = T)

    values_df <- as.data.frame(json)
    names(values_df) <- snakecase::to_snake_case(names(values_df))

    return (values_df)

}
