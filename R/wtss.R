#' @title Creates a WTSS object
#' @name WTSS
#'
#' @description Uses the WTSS services to print information and save metadata about a
#' chosen cube.
#'
#' @param URL        URL of the service provider.
#' @return  R WTSS object associated to the service.
#' @examples {
#' wtss <-  wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")
#' }
#' @export
WTSS <- function(URL) {
    
  # create a list to store the metadata
  wtss.obj <- list(url = character(), coverages = character(), description = list())
  
  wtss.obj$url <- URL
  
  wtss.obj$coverages <- .wtss_list_coverages(wtss.obj)
  
  if (purrr::is_null(wtss.obj$coverages)) {
    message("WTSS server not responding - please check URL")
    return(NULL)
  }
  class(wtss.obj) <- append(class(wtss.obj), "wtss", after = 0)
  return(wtss.obj)
}

#' @title List the coverages available in the WTSS service
#' @name list_coverages
#'
#' @description Displays information about coverages available in the WTSS service 
#'
#' @param wtss.obj       WTSS object
#' @return               NULL if fails, TRUE if works
#' @examples {
#' wtss <-  WTSS("http://www.dpi.inpe.br/tws/wtss")
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
#' @description Contacts the WTSS server to request information about one or more coverages
#' @param wtss.obj    A WTSS object
#' @param coverages   A character vector of coverage names
#' @param .print      Print the coverage description
#' @return            NULL if fails, TRUE if works
#' 
#' @examples
#' wtss  <-  WTSS("http://www.esensing.dpi.inpe.br/wtss/")
#' describe_coverage(wtss, wtss$coverages[1])
#' @export
describe_coverage <- function(wtss.obj, coverages, .print = TRUE) {
  
    result <- purrr::map(coverages, function(cov) {
        items <- NULL
        # concat describe_coverage according to a name into the service URL 
        request <- paste(wtss.obj$url,"/describe_coverage?name=", cov, sep = "")
        ce <- 0
        # avoid time out connection 
        while (purrr::is_null(items) & ce < 10) {
           items <- .wtss_parse_json(.wtss_send_request(request))
           ce <- ce + 1
        }
        
        # if the server does not answer any item, return NULL
        if (purrr::is_null(items)) {
            message("WTSS - coverage information not available")
            return(NULL)
        }
        return(items)
    })
    # convert the coverage description into a list of tibbles
    # print information for each coverage
    cov.lst <- purrr::map(result, function(res) {
               cov.tb <- .wtss_coverage_description(wtss.obj, res)
               if (.print)
                  .wtss_print_coverage(cov.tb)
               return(cov.tb)
              })
    
    # fold the list of tibbles into a single tibble
    description.tb <- dplyr::bind_rows(cov.lst)
    
    # export the description
    eval.parent(substitute(wtss.obj$description <- description.tb))
    # inform uses that WTSS object has the description
    if (.print)
      message("Coverage description saved in WTSS object")
    
    return(invisible(TRUE))
}






