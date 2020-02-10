# HELPER FUNCTIONS

#' Given a flattened json file, produce R friendly column names.
#'
#' @param json - JSON file
#'
#' @return data frame
#'

clean_column_names <- function(json){

}

## Get base url
#' Base URL for the RegCensus API
#'
#' @return String
#'
#'
#' get_baseURL()

get_baseURL <- function() {
    return("https://api.quantgov.org")
}

## Generate API call from controller and requested ID
#' Return the basic URL for an API call.
#'
#' @param controller The name of the controller
#' @param id ID List of IDs to pass to API call
#'
#' @return String
#'

regdata_json_request <- function(controller, id) {
    base_url <- get_baseURL()

    if (length(id) == 1) {
        apicall <- paste0(base_url, "/", controller, "/", id)

    } else {
        apicall <- paste0(base_url, "/", controller)

    }

      return(make_api_call(apicall))
}

## Return URL of API call
#' Return URL of API Call from list of IDs
#'
#' @param controller  short name of controller
#' @param id List of IDs
#'
#' @return String with URL
#'
regdata_json_requestURL <- function(controller, id) {
    base_url <- get_baseURL()

    if (!is.na(id)) {
        apicall <- paste0(base_url, "/", controller, "/", id)
    } else {
        apicall <- paste0(base_url, "/", controller)
    }

    return(apicall)
}

## Manage API Calls
#' Manage API call, including error messages
#'
#' @param url - Text - URL with data.
#' @param verbose - Boolean - if true, show URL calls to API
#'
#' @return json with results
#'


make_api_call <- function(url,verbose=FALSE){

    out <- tryCatch(
        {
          if (verbose)
            message(paste0("API call: ", url))

            #call the api and display progress in text
            pbapply::pbapply(as.data.frame(url),
                             FUN = jsonlite::fromJSON,
                             MARGIN = 1,
                             flatten = TRUE)
            ##jsonlite::fromJSON(txt = url,flatten = TRUE)


        },

        error = function(cond){
          if (verbose) {
            message(paste0("URL is not correctly formed: ", url))
            message("Original error message: ")
          }
          message(cond)
            return(NA)
        },

        warning = function(cond){
          if (verbose)
            message(paste0("URL caused a warning: ", url))
          message(cond)
        },

        finally = {
          if (verbose)
            message(paste0("API call to ", url, " complete."))
        }
    )

    out <- as.data.frame(out)


}

#' Return list of jurisdiction IDs based on the names
#'
#' @param name List of names of the jurisdiction
#'
#' @return List with the jurisdiction IDs of the names specified



find_jurisdiction <- function(name=NA){

    if (length(name) == 0)
        stop("Invalid names specified.")



    #find jurisdiction IDs by name
    jurs <- get_jurisdictions()

    name_list0 = name[1]

    if (length(name) > 1) {
        name_list0 = paste0(name_list0, "|")
        for (i in 2:length(name)) {
            name_list0 = paste0(name_list0, name[i])
            if (i < length(name) )
                name_list0 = paste0(name_list0, "|")
        }
    }

    jurs <- dplyr::filter(jurs,grepl(name_list0,jurs$jurisdictionName,ignore.case = TRUE))

    jurs <- dplyr::select(jurs,jurisdictionID)

    return(jurs$jurisdictionID)

}
