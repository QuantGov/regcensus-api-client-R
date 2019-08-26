# HELPER FUNCTIONS
## Get base url
get_baseURL <- function() {
    return("http://ec2-52-87-213-8.compute-1.amazonaws.com:8080/regdata")
}

## Generate API call from controller and requested ID
regdata_json_request <- function(controller, id) {
    base_url <- get_baseURL()

    if (!is.na(id)) {
        apicall <- paste0(base_url, "/", controller, "/", id)
    } else {
        apicall <- paste0(base_url, "/", controller)
    }
    json <- fromJSON(apicall)
    return(json)
}

## Return URL of API call
regdata_json_requestURL <- function(controller, id) {
    base_url <- get_baseURL()

    if (!is.na(id)) {
        apicall <- paste0(base_url, "/", controller, "/", id)
    } else {
        apicall <- paste0(base_url, "/", controller)
    }

    return(apicall)
}
