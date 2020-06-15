# RETURN DATA FRAMES FROM USER REQUESTS



#' Return list of topics in RegCensus
#'
#' @param topic Integer - the ID of the topic of interest
#'
#' @return dataframe
#' @export
#'
#' @examples
#' get_topics(1)
#' get_topics()
get_topics <- function(topic = NA) {

  #load all topics
  if (!is.na(topic))
    url_compose <- paste0(get_baseURL(),"/topics",paste0(topic,collapse = ","))
  else
    url_compose <- paste0(get_baseURL(),"/topics")

  return(make_api_call(url_compose))
}



#' Generate a list of jurisdictions in RegData
#'
#' @param jurisdiction Integer - the ID of the jurisdiction of interest
#'
#' @return List of jurisdictions
#' @export
#' @examples
#' get_jurisdictions()
#' get_jurisdictions(38)

get_jurisdictions <- function(jurisdiction = NA) {
  if (!is.na(jurisdiction)) {
    url_compose <- paste0(get_baseURL(),"/jurisdictions", jurisdiction)
  }
    else {
      url_compose <- paste0(get_baseURL(),"/jurisdictions")

    }

    return(make_api_call(url_compose))
}

#' Return a list of industries with data in RegCensus
#'
#' @param jurisdiction An integer, the jurisdiction ID
#'
#' @return data frame with industries
#' @export
#'
#' @examples
#' get_industries(38)
get_industries <- function(jurisdiction=NA){

    if (is.na(jurisdiction)) {
        print("You must specify a jurisdiction. Select from: ")
        print(list_jurisdictions())
        stop()
    }

  url_compose <- paste0(get_baseURL(),"/industries?jurisdiction=",jurisdiction)


  return(make_api_call(url_compose))
}

#' Return a dataframe of all agencies in a jurisdiction
#'
#' @param jurisdiction Integer - the IDs of the jurisdiction of interest
#'
#' @return dataframe
#' @export
#'
#' @examples
#' get_agencies(38)
get_agencies <- function(jurisdiction = NA) {
    if (!is.na(jurisdiction)) {
        id_str <- paste(jurisdiction, collapse = ",")
        url_compose <- paste0(get_baseURL(),"/agencies/jurisdiction?jurisdictions=",id_str)
      } else {
      url_compose <- paste0(get_baseURL(),"/agencies")
    }


    return(make_api_call(url_compose))
}

#' Return a dataframe with the series for the id specified
#'
#' @param id Integer - ID for the by parameter
#'
#' @return dataframe
#' @export
#'
#' @examples
#' get_series(bidy=1)
get_series <- function(id = NA) {
  if(is.na(id)){
    url_compose <- paste0(get_baseURL(),"/series")
  } else {
    url_compose <- paste0(get_baseURL(),"/series/",id)
  }

  return(make_api_call(url_compose))
}

#' Return dataframe with all jurisdictions-series-years of data available
#'
#' @param jurisdiction Integer - the ID of the jurisdiction of interest
#'
#' @return dataframe
#' @export
#'
#' @examples
#' get_series_period()
#' get_series_period(38)
get_series_period <- function(jurisdiction=NA) {

  #if empty jurisdiction, return data for all
    if (!is.na(jurisdiction)) {
      jur_str <- paste0(find_jurisdiction(jurisdiction), collapse = ",")
      url_compose <- paste0(get_baseURL(),"/periods","?jurisdiction=",jur_str)
    } else {
      url_compose <- paste0(get_baseURL(),"/periods/available")
    }


    return(make_api_call(url_compose))
}


#' Return dataframe with values of series of interest
#'
#' @param jurisdiction Integer - the ID of the jurisdiction of interest
#' @param series Series ID
#' @param time The time
#' @param agency The agency ID
#' @param industry The industry code using the jurisdiction-specific coding system
#' @param date_is_range Boolean indicating whether the time parameter is range or should be treated as single data points
#' @param summary Boolean - Return summary instead of document level data
#' @param filtered Boolean - Exclude poorly-performing industry classification results
#' @param documentType Integer - ID of document
#' @param document Integer - List of document IDs to return data for. Use when requesting document-level data instead of
#' summary data.
#' @param include_metadata Boolean - Should dataframe include the metadata to make it more readable
#'
#' @return dataframe
#' @export
#'
#' @examples
#' get_values(jurisdiction = 38, series = c(92), time = c('1990','2000'),
#' industry = c('111','33'), agency = c(66,111))

get_values <- function(jurisdiction, series, time=c(2015,2019), summary=TRUE,
                       filtered=TRUE, documentType=3, agency=0, industry='0',
                       date_is_range = TRUE, document=NA, include_metadata=FALSE) {

    if (!summary) {
      print("Request for document-level data could take a while to download. Please be patient.")
    }


    series_str <- paste(series, collapse = ",")

    time_str <- paste(time, collapse = ",")

    industry_str <- paste(industry,collapse = ",")

    agency_str <- paste(agency,collapse = ",")

    if (length(series) > 0) {
        url_compose <- paste0(get_baseURL(),"/values?series=",series_str)
    }else
    {
        print("Valid series ID required. Select from the following list:")
        print(list_series())
        stop("Processing terminated.")

    }
    if (length(jurisdiction) > 0) {
      if (!is.numeric(jurisdiction[1])) {
        jur_str <- paste(find_jurisdiction(jurisdiction),collapse = ",")
      }else{
        jur_str <- paste(jurisdiction, collapse = ",")
      }

      url_compose <- paste0(url_compose,"&jurisdiction=",jur_str)
    }else
        stop("Jurisdiction is required.")

    if (length(agency) > 0) {
        url_compose <- paste0(url_compose,"&agency=",agency_str)
    }

    if (length(industry) > 0) {
        url_compose <- paste0(url_compose,"&industry=",industry_str)
    }

    if (length(time) > 0) {
        url_compose <- paste0(url_compose,"&time=",time_str)
    }else{
        stop("Time is required.")
    }

    if (date_is_range) {
        url_compose <- paste0(url_compose,"&dateIsRange=", date_is_range)
    }

    if (filtered) {
      url_compose <- paste0(url_compose,"&filteredOnly=","true")
    }

    url_compose <- paste0(url_compose,"&documentType=",documentType)

    return(make_api_call(url_compose, TRUE))
}

#' Return values for a list of jurisdictions-series-year combination. It will return data for both
#' national and sub-national (for example, state/province) within the jurisdiction.
#'
#' @param jurisdiction Integer - Jurisdiction IDs for the data
#' @param series Integer - Series IDs
#' @param time Date string - For period of interest
#' @param industries Text - List of industries to pull data for using the jurisdiction specific coding system
#' @param date_is_range Boolean - Indicate if the years values is range instead of just single values
#' @param filtered Boolean - Indicate whether to return filtered data, i.e., the industry-relevant data with poorly classified industries excluded
#'
#' @return dataframe
#' @export
#'
get_country_values <- function(jurisdiction = c(38,75), series = c(1,2),
                             time = c(2010,2011), industries = NA,
                             date_is_range=TRUE, filtered=TRUE){
    if (length(time) == 0) {
      stop("You need to include valid time period")
    }else
        time_str <- paste0(time, collapse = ",")

    if (length(jurisdiction) == 0) {
        print("You need to select at least one of the following jurisdiction IDs")
        print(list_jurisdictions())
        stop()
    }else{
        jurisdiction_str <- paste0(jurisdiction, collapse = ",")
        url_compose <- paste0(get_baseURL(),"/values/country?countries=",jurisdiction_str,"&years=",time_str)
    }

    if (length(series) == 0) {
        print("You need to select at least one of the following series IDs")
        print(list_series())
        stop()

    }else{
        series_str <- paste0(series, collapse = ",")
        url_compose <- paste0(url_compose, "&series=",series_str)
    }

    if (length(industries) == 0) {
        industries_str <- '0'
    }else{
      industries_str <- paste0(industries, collapse = ",")
        url_compose <- paste0(url_compose, "&industries=",industries_str)
    }

    if (date_is_range == FALSE)
        url_compose <- paste0(url_compose,"&dateIsRange=", "false")
    else{
      url_compose <- paste0(url_compose,"&dateIsRange=", "true")
    }

    if (filtered == FALSE)
      url_compose <- paste0(url_compose,"&filteredOnly=",filtered)


    return(make_api_call(url_compose))

}


#' Title Return the list of documents for a series-jurisdiction combination
#'
#' @param document_type Integer - ID of document type of interest. Obtain from get_document_types().
#' @param jurisdiction Integer - ID of jurisdiction of interest
#'
#' @return data frame
#' @export
#'
#' @examples
#' get_documents(55,3)
get_documents <- function(jurisdiction, document_type=3){
  print("Document-level data could take a while to download. Please be patient.")
  if (!is.null(jurisdiction)) {
    jur_str <- paste(jurisdiction, collapse = ",")
    url_compose <- paste0(get_baseURL(),"/documents?jurisdiction=", jur_str)
  }else{

    print("Please select jurisdictions from the list below:")
    list_jurisdictions()
    stop("Jurisdiction is required.")

  }

  if (length(document_type) > 0) {
    url_compose <- paste0(url_compose, "&documentType=",document_type)
  }
  else {
    print("Document type (documentType) is required. Select a document type from the list below:")
    print(get_document_types(NULL))
    stop("Invalid document type specified.")
  }


   documents <- make_api_call(url_compose)
   if (length(documents) > 0) {
     return (documents)
  }else{
    stop(paste0("No data found.",url_compose))
  }

}

#' Title Return the list of document types for a jurisdiction/or all jurisdictions.
#'
#' @param jurisdiction Integer - ID for the jurisdiction of interest. If NA, return all jurisdictions
#'
#' @return data frame of document types
#' @export
#'
#' @examples
#' get_document_types(38)
#' get_document_types()
get_document_types <- function(jurisdiction=NA){

  if (!is.na(jurisdiction)) {
    id_str <- paste(jurisdiction, collapse = ",")
    url_compose <- paste0(get_baseURL(),"/documenttypes?jurisdictions=",id_str)
  } else {
    url_compose <- paste0(get_baseURL(),"/documenttypes")
  }


  json <- make_api_call(url_compose)
  return(json)
}

#' Return series values for a set of industry codes (using NAICS)
#'
#' @param jurisdiction An Integer - jurisdiction(s) of interest. Obtain list of jurisdictions from list_jurisdictions()
#' @param series Integer -  (List of ) Series of interest. Obtain valid list from list_series(id, by)
#' @param time Date (string) - String format of dates. For summary data, just the year is enough. For daily data, use full date format such as
#' '2018-10-12'
#' @param agency Integer - List of agencies. Obtain from get_agencies()
#' @param industry_type String - List of industry code types desired. Valid values are "all", "2-Digit","3-Digit","4-Digit","5-Digit","6-Digit"
#' @param date_is_range Boolean - Date parameter is range
#' @param filtered_only Boolean - For industry values, include only good-performing industry classifications.
#' @param document_type Integer - The type of document. Obtain from list_document_types()
#' @param summary Boolean - Summary data
#'
#' @return Data frame
#' @export
#'

get_industry_values <- function(jurisdiction=c(38),
                                industry_type=c("2-Digit","3-Digit",
                                               "4-Digit","5-Digit","6-Digit"),
                                series=c(1,9),
                                time=c('2015','2019'),
                                agency=c(0),
                                date_is_range=TRUE,
                                filtered_only=TRUE,
                                summary=TRUE,
                                document_type=3) {
  #list of industry options
  industry_types = c("all","2-Digit","3-Digit","4-Digit","5-Digit","6-Digit")

  #parse parameters
  series_str <- paste(series, collapse = ",")
  time_str <- paste(time, collapse = ",")
  industry_str <- paste(industry_type,collapse = ",")
  agency_str <- paste(agency,collapse = ",")
  document_type_str = paste(document_type,collapse = ",")

  #if names are used, find the corresponding jurisdiction IDs
  if (length(jurisdiction) > 0) {
    if (!is.numeric(jurisdiction[1])) {
      jur_str <- paste(find_jurisdiction(jurisdiction),collapse = ",")
    }else {
      jur_str <- paste(jurisdiction, collapse = ",")
    }
    url_compose <- paste0(get_baseURL(),"/values/industryTypes?jurisdictions=",jur_str)
  }else{
    print("Jurisdiction is required. Select valid jurisdiction IDs from the following:")
    print(list_jurisdictions())
    stop("Invalid jurisdiction specified.")
  }

  #ensure correct industry type is selected
  if (length(industry_type) > 0) {
    url_compose <- paste0(url_compose,"&industryType=",industry_type)
  }else{
    print("Provide valid industry code type.")
    print(paste("Select valid industryType from :", industry_types))
    stop("Invalid industry type specified.")
  }


  #ensure correct series IDs are selected
  if (length(series) > 0) {
    url_compose <- paste0(url_compose,"&series=",series_str)
  }else{
    print("Provide valid series id. Select valid series IDs from the following:")
    print(get_series(by = "all"))
    stop("Invalid series ID specified.")
  }

  #ensure the correct dates are selected
  if (length(date) > 0) {
    url_compose <- paste0(url_compose,"&date=",time_str)
  } else {
    stop("Invalid dates specified")
  }

  if (date_is_range) {
      url_compose <- paste0(url_compose,"&dateIsRange=true")
  }
  else {
      url_compose <- paste0(url_compose,"&dateIsRange=false")
    }

  if (summary) {
    url_compose <- paste0(url_compose,"&summary=true")
  } else {
    url_compose <- paste0(url_compose,"&summary=false")
  }

  if (filtered_only) {
    url_compose <- paste0(url_compose,"&filteredOnly=true")
  } else {
    url_compose <- paste0(url_compose,"&filteredOnly=false")
    print("Unfiltered results are not reliable. Use at your own discretion.")
  }

  if (length(document_type) > 0) {
     #dt <- get_document_types()
       url_compose <- paste0(url_compose,"&documentType=",document_type_str)


  } else {
    print("Select valid Document Type (documentType) from the following list:")
    list_document_types()
  }

  if (length(agency) > 0) {
     url_compose <- paste0(url_compose,"&agencies=",agency_str)
  }

  return(make_api_call(url_compose))

}
