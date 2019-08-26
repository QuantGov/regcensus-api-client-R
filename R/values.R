# RETURN VALUES FROM USER REQUESTS

get_geoCodes <- function(jurisdictionIDs) {
    geoCodes <- c()
    i <- 1
    for (jid in jurisdictionIDs) {
        jurisdiction <- get_jurisdictions(jid)
        geoCodes[i] <- paste0(jurisdiction$geoCode)
        i <- i + 1
    }

    return(geoCodes)
}

get_seriesCodes <- function(seriesIDs) {
    seriesCodes <- c()
    i <- 1
    for (sid in seriesIDs) {
        ## Note get_series does not work correctly right now
        series <- get_series(sid)
        seriesCodes[i] <- paste0(series$x)
        i <- i + 1
    }

    return(seriesCodes)
}

get_jurisdictionID <- function(geoCodes) {
    # this function can be expanded to return vector of jids for state names, state abbr, and countries
    jurisdictions <- get_jurisdictions()
    jids <- c()
    i <- 1
    for (geoCode in geoCodes) {
        jid <- unique(jurisdictions$jurisdictionID[jurisdictions$geoCode$geoCode == geoCode])
        jids[i] <- paste(jid, collapse=",")
        i <- i + 1
    }

    return(jids)
}

get_years <- function(seriesCode) {
    # Idea is to return vector of available years for provided seriesCode and jurisdiction, if provided

}
