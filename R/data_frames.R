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
<<<<<<< HEAD
get_topics <- function(topicID = NA) {
    topics <- regdata_json_request("topics", topicID)
    topic_df <- as.data.frame(topics)
    names(topic_df) <- snakecase::to_snake_case(names(topic_df))
    return(topic_df)
=======
get_topics <- function(topic = NA) {
  #load all topics
  if (!is.na(topic))
    url_compose <-
      paste0(get_baseURL(), "/topics", paste0(topic, collapse = ","))
  else
    url_compose <- paste0(get_baseURL(), "/topics")

  return(make_api_call(url_compose))
>>>>>>> kampaabeng
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
<<<<<<< HEAD
    jurisdictions <- regdata_json_request("jurisdictions", jurisdiction)
    jurisdiction_df <- as.data.frame(jurisdictions)
    names(jurisdiction_df) <- snakecase::to_snake_case(names(jurisdiction_df))
    return(jurisdiction_df)
=======
  if (!anyNA(jurisdiction)) {
    if(length(jurisdiction) > 0){
      stop("Too many jurisdictions specified.")
    }
    if(is.numeric(jurisdiction)) {
      url_compose <- paste0(get_baseURL(), "/jurisdictions/", jurisdiction)
    } else {
      url_compose <- paste0(get_baseURL(), "/jurisdictions/",
                            find_jurisdiction(jurisdiction))
    }

  }
  else {
    url_compose <- paste0(get_baseURL(), "/jurisdictions")

  }

  return(make_api_call(url_compose, TRUE))
>>>>>>> kampaabeng
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
<<<<<<< HEAD
get_industries <- function(jurisdictionID=NA){
    if(is.na(jurisdictionID)){
        print("You must specify a jurisdiction. Select from: ")
        print(list_jurisdictions())
        stop()
    }
    industries <- regdata_json_request("industries?jurisdictions=",jurisdictionID)
    industry_df <-as.data.frame(industries)
=======
get_industries <- function(jurisdiction = NA) {
  if (is.na(jurisdiction)) {
    print("You must specify a jurisdiction. Select from: ")
    print(get_jurisdictions())
    stop()
  }

  url_compose <-
    paste0(get_baseURL(), "/industries?jurisdiction=", jurisdiction)

  return(make_api_call(url_compose))
>>>>>>> kampaabeng
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
<<<<<<< HEAD
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
=======
get_agencies <- function(jurisdiction = NA) {
  if (!is.na(jurisdiction)) {
    id_str <- paste(jurisdiction, collapse = ",")
    url_compose <-
      paste0(get_baseURL(),
             "/agencies/jurisdiction?jurisdictions=",
             id_str)
  } else {
    url_compose <- paste0(get_baseURL(), "/agencies")
  }


  return(make_api_call(url_compose))
>>>>>>> kampaabeng
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
<<<<<<< HEAD
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
=======
#' get_series(id=1)
get_series <- function(id = NA) {
  if (is.na(id)) {
    url_compose <- paste0(get_baseURL(), "/series")
  } else {
    url_compose <- paste0(get_baseURL(), "/series/", id)
  }
>>>>>>> kampaabeng

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
<<<<<<< HEAD
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
=======
#' get_periods()
#' get_periods(38)
get_series_periods <- function(jurisdiction = NA) {
  #if empty jurisdiction, return data for all
  if (!is.na(jurisdiction)) {
    if (is.character(jurisdiction)) {
      jur_str <- paste0(find_jurisdiction(jurisdiction), collapse = ",")
    }
    else {
      jur_str <- paste0(jurisdiction, collapse = ",")
    }

    url_compose <-
      paste0(get_baseURL(), "/periods", "?jurisdiction=", jur_str)
  } else {
    url_compose <- paste0(get_baseURL(), "/periods/available")
  }


  return(make_api_call(url_compose, TRUE))
>>>>>>> kampaabeng
}


#' Return dataframe with values of series of interest
#'
<<<<<<< HEAD
#' @param jurisdiction
#' @param series
#' @param time
#' @param agency
#' @param industry
#' @param dateIsRange
=======
#' @param jurisdiction Integer - the ID of the jurisdiction of interest
#' @param series Series ID
#' @param date The date
#' @param agency The agency ID
#' @param industry The industry code using the jurisdiction-specific coding system
#' @param date_is_range Boolean indicating whether the date parameter is range or should be treated as single data points
#' @param summary Boolean - Return summary instead of document level data
#' @param filtered Boolean - Exclude poorly-performing industry classification results
#' @param document_type Integer - ID of document type
#' @param document Integer - List of document IDs to return data for. Use when requesting document-level data instead of
#' summary data.
#' @param include_metadata Boolean - Should dataframe include the metadata to make it more readable
>>>>>>> kampaabeng
#'
#' @return dataframe
#' @export
#'
#' @examples
<<<<<<< HEAD
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
=======
#' get_values(jurisdiction = 38, series = c(92), date = c('1990','2000'),
#' industry = c('111','33'), agency = c(66,111))

get_values <-
  function(jurisdiction,
           series,
           date = c(2015, 2020),
           summary = TRUE,
           filtered = TRUE,
           document_type = 1,
           agency = NA,
           industry = '0',
           date_is_range = TRUE,
           document = NA,
           include_metadata = FALSE) {
    if (!summary) {
      print("Request for document-level data could take a while to download. Please be patient.")
    }


    series_str <- paste(series, collapse = ",")

    date_str <- paste(date, collapse = ",")

    industry_str <- paste(industry, collapse = ",")

    agency_str <- paste(agency, collapse = ",")

    if(is.na(document_type)){
      document_type <- 1
    }
>>>>>>> kampaabeng

    # Use document endpoint if summary is false
    if (!summary) {
      url_compose <- paste0(get_baseURL(), "/values/documents?")
    } else {
      url_compose <- paste0(get_baseURL(), "/values?")
    }
    if (length(series) > 0) {
      url_compose <- paste0(url_compose, "&series=", series_str)
    } else {
      print("Valid series ID required. Select from the following list:")
      print(get_series())
      stop("Processing terminated.")
    }
<<<<<<< HEAD
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
=======
    if (length(jurisdiction) > 0) {
      if (!is.numeric(jurisdiction[1])) {
        jur_str <- paste(find_jurisdiction(jurisdiction), collapse = ",")
      } else {
        jur_str <- paste(jurisdiction, collapse = ",")
      }

      url_compose <- paste0(url_compose, "&jurisdiction=", jur_str)
    } else
      stop("Jurisdiction is required.")

    if (length(agency) > 0 & !is.na(agency)) {
      url_compose <- paste0(url_compose, "&agency=", agency_str)
    }

    if (length(industry) > 0) {
      url_compose <- paste0(url_compose, "&industry=", industry_str)
    }

    if (length(date) > 0) {
      url_compose <-
        paste0(url_compose, "&date=", date_str)
    } else {
      stop("Date is required.")
    }

    if (date_is_range) {
      url_compose <- paste0(url_compose, "&dateIsRange=", date_is_range)
    }

    if (filtered) {
      url_compose <- paste0(url_compose, "&filteredOnly=", "true")
    }

    if (summary) {
      url_compose <- paste0(url_compose, "&summary=", "true")
    } else {
      url_compose <- paste0(url_compose, "&summary=", "false")
    }
    url_compose <-
      paste0(url_compose, "&documentType=", document_type)

    return(make_api_call(url_compose, TRUE))
  }
>>>>>>> kampaabeng

#' Return values for a list of jurisdictions-series-year combination. It will return data for both
#' national and sub-national (for example, state/province) within the jurisdiction.
#'
<<<<<<< HEAD
#' @param jurisdiction Jurisdiction IDs for the data
#' @param series Series IDs
#' @param years Integers for years
#' @param industries
#' @param dateIsRange A boolean to indicate if the years values is range instead of just single values
#' @param filtered A boolean
=======
#' @param jurisdiction Integer - Jurisdiction IDs for the data
#' @param series Integer - Series IDs
#' @param date Date string - For period of interest
#' @param industries Text - List of industries to pull data for using the jurisdiction specific coding system
#' @param date_is_range Boolean - Indicate if the years values is range instead of just single values
#' @param document_type Integer - ID of document type
#' @param filtered Boolean - Indicate whether to return filtered data, i.e., the industry-relevant data with poorly classified industries excluded
>>>>>>> kampaabeng
#'
#' @return dataframe
#' @export
#'
<<<<<<< HEAD
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
=======
get_country_values <-
  function(jurisdiction = 38,
           series = c(1, 2),
           date = c(2010, 2011),
           industries = NA,
           date_is_range = TRUE,
           document_type = 1,
           filtered = TRUE) {
    if (length(date) == 0 | is.na(date)) {
      stop("You need to include valid date")
    }

    if (length(jurisdiction) == 0 | is.na(jurisdiction)) {
      print("You need to select at least one of the following jurisdiction IDs")
      print(get_jurisdictions())
      stop()
    } else {
      url_compose <-paste0(get_baseURL(), "/values?documentType=",
          document_type,"&national=true","&date=",paste0(date, collapse = ","),
                 "&jurisdiction=",paste0(jurisdiction, collapse = ","))

    }

    if (length(series) == 0 | is.na(series)) {
      print("You need to select at least one of the following series IDs")
      print(get_series())
      stop()

    } else {
      series_str <- paste0(series, collapse = ",")
      url_compose <- paste0(url_compose, "&series=", series_str)
    }

    if(!is.na(industries)){
      url_compose <- paste0(url_compose, "&industries=", paste0(industries, collapse = ","))
    }


    if (date_is_range == FALSE)
      url_compose <- paste0(url_compose, "&dateIsRange=", "false")
    else {
      url_compose <- paste0(url_compose, "&dateIsRange=", "true")
    }

    if (filtered == FALSE)
      url_compose <- paste0(url_compose, "&filteredOnly=", filtered)
>>>>>>> kampaabeng

    print(url_compose)

<<<<<<< HEAD
    json <- jsonlite::fromJSON(url_compose, flatten = T)

    values_df <- as.data.frame(json)
    names(values_df) <- snakecase::to_snake_case(names(values_df))

    return (values_df)
=======
    return(make_api_call(url_compose, TRUE))

  }


#' Title Return the list of documents for a series-jurisdiction combination
#'
#' @param document_type Integer - ID of document type of interest. Obtain from get_document_types().
#' @param jurisdiction Integer - ID of jurisdiction of interest
#' @return data frame
#' @export
#'
#' @examples
#' get_documents(55,3)
get_documents <- function(jurisdiction, document_type = 3) {
  print("Document-level data could take a while to download. Please be patient.")
  if (!is.null(jurisdiction)) {
    jur_str <- paste(jurisdiction, collapse = ",")
    url_compose <-
      paste0(get_baseURL(),
             "/values/documents?jurisdiction=",
             jur_str)
  } else {
    print("Please select jurisdictions from the list below:")
    get_jurisdictions()
    stop("Jurisdiction is required.")

  }

  if (length(document_type) > 0) {
    url_compose <- paste0(url_compose, "&documentType=", document_type)
  }
  else {
    print(
      "Document type (documentType) is required. Select a document type from the list below:"
    )
    print(get_document_types(NULL))
    stop("Invalid document type specified.")
  }


  documents <- make_api_call(url_compose)
  if (length(documents) > 0) {
    return(documents)
  } else {
    stop(paste0("No data found.", url_compose))
  }

}

#' Title Return the list of document types for a jurisdiction/or all jurisdictions.
#'
#' @param jurisdiction Integer - ID for the jurisdiction of interest. If NA, return all jurisdictions
#' @return data frame of document types
#' @export
#'
#' @examples
#' get_document_types(38)
#' get_document_types()
get_document_types <- function(jurisdiction = NA) {
  if (!is.na(jurisdiction)) {
    id_str <- paste(jurisdiction, collapse = ",")
    url_compose <-
      paste0(get_baseURL(), "/documenttypes?jurisdictions=", id_str)
  } else {
    url_compose <- paste0(get_baseURL(), "/documenttypes")
  }


  json <- make_api_call(url_compose)
  return(json)
}

#' Return series values for a set of industry codes (using NAICS)
#'
#' @param jurisdictions An Integer - jurisdiction(s) of interest. Obtain list of jurisdictions from get_jurisdictions()
#' @param series Integer -  (List of ) Series of interest. Obtain valid list from get_series(id, by)
#' @param date Date (string) - String format of dates. For summary data, just the year is enough. For daily data, use full date format such as
#' '2018-10-12'
#' @param agencies Integer - List of agencies. Obtain from get_agencies()
#' @param industry_type String - List of industry code types desired. Valid values are "all", "2-Digit","3-Digit","4-Digit","5-Digit","6-Digit"
#' @param industries String - List of industry codes to obtain.
#' @param date_is_range Boolean - Date parameter is range
#' @param filtered_only Boolean - For industry values, include only good-performing industry classifications.
#' @param document_type Integer - The type of document. Obtain from get_document_types()
#' @param summary Boolean - Summary data
#'
#' @return Data frame
#' @export
#'

get_industry_values <- function(jurisdictions = c(38),
                                industry_type = NA,
                                industries = NA,
                                series = 1,
                                date = c('2015', '2019'),
                                agencies = NA,
                                date_is_range = TRUE,
                                filtered_only = TRUE,
                                summary = TRUE,
                                document_type = 1) {
  #list of industry options
  industry_types <-
    c("all", "2-Digit", "3-Digit", "4-Digit", "5-Digit", "6-Digit")

  #parse parameters
  series_str <- paste(series, collapse = ",")
  date_str <- paste(date, collapse = ",")
  agency_str <- paste(agencies, collapse = ",")
  document_type_str <- paste(document_type, collapse = ",")

  #if names are used, find the corresponding jurisdiction IDs
  if (length(jurisdictions) > 0 & !is.na(jurisdictions)) {
    if (!is.numeric(jurisdictions)) {
      jur_str <- paste(find_jurisdiction(jurisdictions), collapse = ",")
    } else {
      jur_str <- paste(jurisdictions, collapse = ",")
    }
    url_compose <-
      paste0(get_baseURL(), "/values?jurisdiction=", jur_str)
  } else {
    print("Jurisdiction is required. Select valid jurisdiction IDs from the following:")
    print(get_jurisdictions())
    stop("Invalid jurisdiction specified.")
  }



  #ensure correct industry type is selected
  if (!is.na(industry_type)) {
    # get list of industry types
    industry_list <- get_industries(jurisdiction = jurisdictions) %>%
      dplyr::mutate(ndigits = stringi::stri_length(industryCode))
    #print(industry_list)
    ## 2-digit
    n <- case_when(industry_type == "2-Digit" ~ 2,
                   industry_type == "3-Digit" ~ 3,
                   industry_type == "4-Digit" ~ 4,
                   industry_type == "5-Digit" ~ 5,
                   industry_type == "6-Digit" ~ 6,
                   TRUE ~ 7)


    if(n==7){
      selected_industry_types <- industry_list %>% select(industryCode) %>% distinct()
    } else {
      selected_industry_types <- industry_list %>% dplyr::select(industryCode, ndigits) %>%
        dplyr::filter(ndigits == n) %>%
        dplyr::distinct()
    }
    print(industries)
  } else {
    selected_industry_types <- NA
  }

  if(is.na(industries) && is.na(as.list(selected_industry_types$industryCode))){
    print("Provide valid industry code or industry type (eg. 3-Digit, 2-Digits.)")
    print("Select valid industryType from ")
    print(industry_types)
    stop("Invalid industry type specified.")
  }



  if(!is.na(industries) & !is.na(selected_industry_types)){
    stop("You have to select either industry type or industry code; not both.")
  }


  if (!is.na(industries) & is.na(selected_industry_types))  {
      url_compose <- paste0(url_compose, "&industry=", paste(industries, collapse = ","))
  } else if (!is.na(selected_industry_types) & is.na(industries)){
    url_compose <- paste0(url_compose, "&industry=", paste(selected_industry_types$industryCode, collapse = ","))
  }


  #ensure correct series IDs are selected
  if (length(series) > 0 & !is.na(series)) {
    url_compose <- paste0(url_compose, "&series=", series_str)
  } else {
    print("Provide valid series id. Select valid series IDs from the following:")
    get_series() %>% dplyr::select(seriesID,seriesName)
    stop("Invalid series ID specified.")
  }

  #ensure the correct dates are selected
  if (length(date) > 0 & !is.na(date)) {
    url_compose <- paste0(url_compose, "&date=", date_str)
  } else {
    stop("Invalid dates specified")
  }

  if (date_is_range) {
    url_compose <- paste0(url_compose, "&dateIsRange=true")
  }
  else {
    url_compose <- paste0(url_compose, "&dateIsRange=false")
  }

  if (summary) {
    url_compose <- paste0(url_compose, "&summary=true")
  } else {
    url_compose <- paste0(url_compose, "&summary=false")
  }

  if (filtered_only) {
    url_compose <- paste0(url_compose, "&filteredOnly=true")
  } else {
    url_compose <- paste0(url_compose, "&filteredOnly=false")
    print("Unfiltered results are not reliable. Use at your own discretion.")
  }

  if (length(document_type) > 0 & !is.na(document_type)) {
    #dt <- get_document_types()
    url_compose <-
      paste0(url_compose, "&documentType=", document_type_str)


  } else {
    print("Select valid Document Type (documentType) from the following list:")
    get_document_types() %>% dplyr::select(document_type=documentSubtypeID,document_name = subtypeName)
  }

  if (!is.na(agencies) & length(agencies) > 0) {
    url_compose <- paste0(url_compose, "&agencies=", agency_str)
  }

  return(make_api_call(url_compose, TRUE))
>>>>>>> kampaabeng

}
