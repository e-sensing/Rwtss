#' @title Send a request to WTSS server
#' @name .wtss_send_request
#'
#' @description Sends a request to the WTSS server and gets a response
#'
#' @param request   valid request according to the WTSS protocol
#' @return  response from the server
.wtss_send_request <- function(request) {
    
    # check if URL exists and perform the request
    tryCatch(response <- RCurl::getURL(request), 
             error = function(e) {
                 e$message <- paste("HTTP request failed. 
                              The URL server may be incorrect or the service 
                              may be temporarily unavailable.")
                 return(NULL)
             })
    
    return(response)
}
#' @title Parse a JSON response from the WTSS server
#' @name .wtss_parse_json
#'
#' @description Parse a JSON response from the WTSS service
#'
#' @param response   valid JSON response from the WTSS service
#' @return  parsed JSON document
.wtss_parse_json <- function(response) {
    
    # validate json
    if (jsonlite::validate(response)) {
        json_response <- jsonlite::fromJSON(response)
        
        if ("exception" %in% names(json_response))
            json_response <- NULL
    }
    else
        json_response <- NULL
    
    return(json_response)
}