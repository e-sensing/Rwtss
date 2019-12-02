#' @title Creates a WTSS object
#' @name WTSS
#'
#' @description Creates a connection to a WTSS server.
#'
#' @param URL        URL of the service provider.
#' @return  R WTSS object associated to the service.
#' @examples {
#' wtss <-  wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")
#' }
#' @export
WTSS <- function(URL) {
    
  # create a list to store the metadata
  wtss.obj <- list(url = character(), coverages = character(), 
                   description = list())
  
  wtss.obj$url <- URL
  
  wtss.obj$coverages <- .wtss_list_coverages(wtss.obj)
  
  if (purrr::is_null(wtss.obj$coverages)) {
    message("WTSS server not responding - please check URL")
    return(NULL)
  }
  class(wtss.obj) <- append(class(wtss.obj), "wtss", after = 0)
  message(paste0("Connected to WTSS server at ", URL))
  return(wtss.obj)
}

#' @title List the coverages available in the WTSS service
#' @name list_coverages
#'
#' @description Lists coverages available in the WTSS service 
#'
#' @param wtss.obj       WTSS object
#' @return               NULL if fails, TRUE if works
#' @examples {
#' wtss <-  WTSS("http://www.esensing.dpi.inpe.br/wtss/")
#' list_coverages(wtss)
#' }
#' @export
list_coverages <- function(wtss.obj) {
    if (purrr::is_null(wtss.obj) || purrr::is_null(wtss.obj$coverages)) {
        message("WTSS - server URL not working") 
        return(NULL)
    }
    else {
      # initial message
      cat(paste("Object of Class WTSS\n"))
    
      # print serverUrl
      cat(paste("server-url: ",paste(wtss.obj$url),  "\n"))
    
      # print coverages
      cat("Coverages: ")
      cat(paste(wtss.obj$coverages), " ")
    } 
  
    return(invisible(TRUE))
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
#' wtss  <-  WTSS("http://www.esensing.dpi.inpe.br/wtss/")
#' describe_coverage(wtss, wtss$coverages[1])
#' @export
describe_coverage <- function(wtss.obj, name, .print = TRUE) {
    assertthat::assert_that(length(name) == 1, 
                msg = "WTSS - select only one coverage to describe")
    result <- NULL
    # concat describe_coverage according to a name into the service URL 
    request <- paste(wtss.obj$url,"/describe_coverage?name=", name, sep = "")
    ce <- 0
    # avoid time out connection 
    while (purrr::is_null(result) & ce < 10) {
        result <- .wtss_parse_json(.wtss_send_request(request))
        ce <- ce + 1
        # if the server does not answer any item, return NULL
        if (purrr::is_null(result)) {
            message("WTSS - coverage information not available")
            return(NULL)
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
    
    return(invisible(TRUE))
}






