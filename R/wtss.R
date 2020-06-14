#' @title Creates a WTSS object
#' @name WTSS
#'
#' @description Creates a connection to a WTSS server.
#'
#' @param URL        URL of the service provider. The default value is
#' "http://www.esensing.dpi.inpe.br/wtss".
#' @param .show_msg  Show connection message
#' @return  R WTSS object associated to the service.
#' @examples
#' wtss <-  wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss")
#' @export
WTSS <- function(URL = "http://www.esensing.dpi.inpe.br/wtss", 
                 .show_msg = TRUE) {
    # remove trailing dash
    URL <- .wtss_remove_trailing_dash(URL)
    
    # create a list to store the metadata
    wtss.obj <- list(url = character(), coverages = character(), 
                   description = list(), valid = logical())
    # store the URL
    wtss.obj$url <- URL
    # try to retrieve the coverage list
    wtss.obj$coverages <- .wtss_list_coverages(wtss.obj)
  
    # if the coverage list is NULL, the wtss.obj is invalid
    if (purrr::is_null(wtss.obj$coverages))
        wtss.obj$valid <- FALSE
    else
        wtss.obj$valid <- TRUE

    # assign the object class
    class(wtss.obj) <- append(class(wtss.obj), "wtss", after = 0)
    # is the object valid? If yes then inform the connection
    if (.show_msg && wtss.obj$valid)
        message(paste0("Connected to WTSS server at ", URL))
    if (!wtss.obj$valid)
        message(paste0("WTSS server at ", wtss.obj$url, 
                     " not responding - please check URL"))
      
return(wtss.obj)
}

#' @title List the coverages available in the WTSS service
#' @name list_coverages
#'
#' @description Lists coverages available in the WTSS service 
#'
#' @param wtss.obj       WTSS object
#' @return               NULL if fails, TRUE if works
#' @examples
#' wtss_service <-  WTSS("http://www.esensing.dpi.inpe.br/wtss")
#' list_coverages(wtss_service)
#' @export
list_coverages <- function(wtss.obj) {
    
    # is the object already a WTSS object or just a URL?
    # if it is an URL, try to create a WTSS object
    if (!("wtss" %in% class(wtss.obj))) {
        URL <- wtss.obj # the parameter should be a URL
        URL <- .wtss_remove_trailing_dash(URL)
        wtss.obj <- wtss::WTSS(URL, .show_msg = FALSE)
    }
    # is the wtss object valid?
    if (!wtss.obj$valid) {
        message(paste0("WTSS server at URL ", wtss.obj$URL, 
                       "not responding - please check URL"))
        return(wtss.obj)
    }
    else {
    
      # print coverages
      cat("Coverages: ")
      cat(paste(wtss.obj$coverages), " ")
    } 
  
    return(invisible(wtss.obj))
}

#' @title Retrieves the list of cubes from the URL server
#' @name  describe_coverage
#'
#' @description Contacts the WTSS server to describe one  coverages
#' @param wtss.obj    A WTSS object
#' @param name        A character vector of coverage names
#' @param .print      Print the coverage description
#' @return            NULL if fails, TRUE if works
#' 
#' @examples
#' wtss_service  <-  WTSS("http://www.esensing.dpi.inpe.br/wtss")
#' describe_coverage(wtss_service, "MOD13Q1")
#' @export
describe_coverage <- function(wtss.obj, name, .print = TRUE) {
  
    # is the object already a WTSS object or just a URL?
    # if it is an URL, try to create a WTSS object
    if (!("wtss" %in% class(wtss.obj))) {
        URL <- wtss.obj # the parameter should be a URL
        URL <- .wtss_remove_trailing_dash(URL)
        wtss.obj <- wtss::WTSS(URL, .show_msg = FALSE)
    }
    # is the wtss object valid?
    if (!wtss.obj$valid) {
      message(paste0("WTSS server not responding - please check URL"))
      return(invisible(wtss.obj))
    }
  
    assertthat::assert_that(length(name) == 1, 
                msg = "WTSS - select only one coverage to describe")
    result <- NULL
    
    # build a "describe_coverage" request 
    request <- paste(wtss.obj$url,"/describe_coverage?name=", name, sep = "")
    ce <- 0
    
    # avoid time out connection 
    while (purrr::is_null(result) & ce < 10) {
        result <- .wtss_parse_json(.wtss_send_request(request))
        ce <- ce + 1
        # if the server does not answer any item, return NULL
        if (purrr::is_null(result)) {
            message("WTSS - coverage information not available")
            return(wtss.obj)
        }
    }
    # convert the coverage description into a tibble

    cov.tb <- .wtss_coverage_description(wtss.obj, result)
    
    # print the content of the coverage
    if (.print)
      .wtss_print_coverage(cov.tb)

    # check if the description is already associated to the WTSS object
    if (length(wtss.obj$description) != 0) {
        if (!(name %in% wtss.obj$description$name)) {
            # add the coverage description
            cov.tb       <- dplyr::bind_rows(wtss.obj$description, cov.tb)
            # export the description
            eval.parent(substitute(wtss.obj$description <- cov.tb))  
        }
    }
    else
        # description list empty 
        # export the description
        eval.parent(substitute(wtss.obj$description <- cov.tb))  
      
    # inform uses that WTSS object has the description
    if (.print)
      message("Coverage description saved in WTSS object")
    
    return(invisible(wtss.obj))
}
