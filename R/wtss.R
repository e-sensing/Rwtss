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
    message("WTSS not working properly - please check URL")
    return(NULL)
  }
  class(wtss.obj) <- append(class(wtss.obj), "wtss", after = 0)
  return(wtss.obj)
}

#' @title Shows a WTSS object
#' @name show
#'
#' @description Displays information about the WTSS service 
#'
#' @param wtss.obj       WTSS object
#' @examples {
#' wtss <-  WTSS("http://www.esensing.dpi.inpe.br/wtss/")
#' wtss
#' }
#' @export
show <- function(wtss.obj){
    if (!purrr::is_null(wtss.obj))
        message("Invalid WTSS object")
    else {
        # initial message
        cat(paste("Object of Class WTSS\n\n"))
      
        # print serverUrl
        cat(paste("server-url: ",paste(wtss.obj$url),  "\n"))
      
        # print coverages
        cat("Coverages: ")
        cat(paste(wtss.obj$coverages), " ")
    }
    return(invisible(TRUE))  
}

#' @title List the coverages available in the WTSS service
#' @name list_coverages
#'
#' @description Displays information about coverages available in the WTSS service 
#'
#' @param wtss.obj       WTSS object
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
    else
       show(wtss.obj) 
  
    return(wtss.obj)
}

#' @title Retrieves the list of cubes from the URL server
#' @name  describe_coverage
#'
#' @description Contacts the WTSS server to request information about one or more coverages
#' @param wtss.obj    A WTSS object
#' @param coverages   A character vector of coverage names
#' @return            A WTSS object with with coverage information
#' 
#' @examples
#' wtss  <-  WTSS("http://www.esensing.dpi.inpe.br/wtss/")
#' wtss  <- describe_coverage(wtss, wtss$coverages[1])
#' @export
describe_coverage <- function(wtss.obj, coverages) {
  
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
        
        # if the server does not answer any item
        if (purrr::is_null(items))
          return(NULL)
        
        return(items)
    })
    
    cov.lst <- purrr::map(result, function(res) {
               cov.tb <- .wtss_coverage_description(wtss.obj, res)
               return(cov.tb)
              })
    description.tb <- dplyr::bind_rows(cov.lst)
    
    wtss.obj$description <- description.tb
    
    return(wtss.obj)
  
}
#' @title Retrieves the list of cubes from the URL server
#' @name .wtss_list_coverages
#'
#' @description Uses the WTSS services to print information and save metadata about a
#' chosen cube.
#'
#' @param wtss.obj      WTSS object associated to the service
#' @return              updated WTSS object.
.wtss_list_coverages <- function(wtss.obj) {
    
    items <- NULL
    ce <- 0
    
    url <- wtss.obj$url
    
    # concat list_coverages to the service URL 
    request <- paste(url,"/list_coverages",sep = "")
    
    # try only 10 times (avoid time out connection)
    while (purrr::is_null(items) & ce < 10) {
        items <- .wtss_parse_json(.wtss_send_request(request))
        ce <- ce + 1
    }
    
    # if the server does not answer any item
    if (purrr::is_null(items))
        return(NULL)
    
    # if the server answers correctly
    return(items$coverages)
}

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
                                   may be temporarily unavailable."); 
                return(NULL);
            })
  
    return(response)
}
#' @title Parse a JSON response from the WTSS server
#' @name .wtss_parse_json
#'
#' @description Uses the jsonlite package to parse a JSON response from the WTSS service
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

#' @title Decodes the description from a WTSS coverage
#' @name  .wtss_coverage_description
#' 
#' @description creates a tibble to store the description of the WTSS coverage
#' @param wtss.obj  valid WTSS object
#' @param cov       coverage response provided by WTSS service
#' 
.wtss_coverage_description <- function(wtss.obj, cov){
    
    name <- cov$name
    
    # retrieve the satellite associated to the cube
    satellite <- .wtss_config_satellite(name)
    # retrieve the sensor associated to the cube
    sensor    <- .wtss_config_sensor(name)
    # temporal extent
    timeline <- lubridate::as_date(cov$timeline)
    
    # retrieve information about the bands
    band_info <- cov$attributes
    
    attr <- tibble::as.tibble(band_info)
    bands <- attr$name
    
    t <- dplyr::select(dplyr::filter(attr, name %in% bands), 
                       name, missing_value, scale_factor, valid_range)
    missing_values        <- t$missing_value
    names(missing_values) <- t$name
    
    scale_factors         <- t$scale_factor
    names(scale_factors)  <- t$name
    
    minimum_values        <- t$valid_range$min
    names(minimum_values) <- t$name
    
    maximum_values        <- t$valid_range$max
    names(maximum_values) <- t$name
    
    # Spatial extent
    xmin <- cov$spatial_extent$xmin
    ymin <- cov$spatial_extent$ymin
    xmax <- cov$spatial_extent$xmax
    ymax <- cov$spatial_extent$ymax
    
    # Spatial resolution
    xres <- cov$spatial_resolution$x
    yres <- cov$spatial_resolution$y
    
    # Size (rows and cols)
    nrows <- cov$dimension$y$max_idx - cov$dimensions$y$min_idx + 1
    ncols <- cov$dimension$x$max_idx - cov$dimensions$x$min_idx + 1
    
    # Projection CRS
    crs <- cov$crs$proj4
    
    # create a tibble to store the metadata
    cov.tb <- tibble::tibble(URL            = wtss.obj$url,
                             satellite      = satellite,
                             sensor         = sensor,
                             name           = name,
                             bands          = list(bands),
                             scale_factors  = list(scale_factors),
                             missing_values = list(missing_values),
                             minimum_values = list(minimum_values),
                             maximum_values = list(maximum_values),
                             timeline       = list(timeline),
                             nrows          = nrows,
                             ncols          = ncols,
                             xmin           = xmin,
                             xmax           = xmax,
                             ymin           = ymin,
                             ymax           = ymax,
                             xres           = xres,
                             yres           = yres,
                             crs            = crs)
    
    class(cov.tb) <- append(class(cov.tb), "coverage", after = 0)

    return(cov.tb)
}

