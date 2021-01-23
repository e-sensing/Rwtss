#' @title Send a request to WTSS server
#' @name .wtss_send_request
#'
#' @description Sends a request to the WTSS server and times out after 10 tries
#'
#' @param request   valid request according to the WTSS protocol
#' @return  response from the server
.wtss_send_request <- function(request) {
    response <- NULL 
    ce <- 0
    # try 10 times (avoid time out connection)
    while (purrr::is_null(response) & ce < 10) {
        response <- .wtss_get_response(request)
        ce <- ce + 1
    }
    
    return(response)
}

#' @title Get a response to the WTSS server
#' @name .wtss_get_response 
#'
#' @description Sends a request to the WTSS server and gets a response
#'
#' @param request   valid request according to the WTSS protocol
#' @return  response from the server
.wtss_get_response <- function(request) {
    # check if URL exists and perform the request
    response <- NULL
    
    tryCatch({
        response <- httr::GET(request)
        httr::stop_for_status(response)
    }, 
    error = function(e) {
        return(NULL)
    })
    
    if (!purrr::is_null(response))
        response <- httr::content(response, "text", encoding = "UTF-8")
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
    if (!purrr::is_null(response) && jsonlite::validate(response)) {
        json_response <- jsonlite::fromJSON(response)
        
        if ("exception" %in% names(json_response))
            json_response <- NULL
    }
    else
        json_response <- NULL
        
    return(json_response)
}
#' @title Process a request to the WTSS server
#' @name .wtss_process_request
#'
#' @description Process a request 
#'
#' @param request   valid request to the WTSS service
#' @return  parsed JSON document
.wtss_process_request <- function(request){
    ce <- 0
    result <-  NULL
    # avoid time out connection 
    while (purrr::is_null(result) & ce < 10) {
        result <- .wtss_parse_json(.wtss_send_request(request))
        ce <- ce + 1
    }
    return (result)
}
