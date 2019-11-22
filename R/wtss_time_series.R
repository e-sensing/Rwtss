#' @title Get time series
#' @name time_series
#' @author  Gilberto Camara
#' @description This function retrieves the time series for a pair of coordinates 
#' 
#' @param wtss.obj      Either a WTSS object or a server URL
#' @param name          Coverage name.
#' @param attributes    Vector of band names.
#' @param longitude     Longitude in WGS84 coordinate system.
#' @param latitude      Latitude in WGS84 coordinate system.
#' @param start_date    Start date in the format yyyy-mm-dd or yyyy-mm depending on the coverage.
#' @param end_date      End date in the format yyyy-mm-dd or yyyy-mm depending on the coverage.
#' @return              time series in a tibble format
#' @examples {
#' wtss <- wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")
#' ts   <- wtss::time_series(wtss, "MOD13Q1", c("ndvi","evi"), 
#'                 longitude = -45.00, latitude  = -12.00,
#'                 start_date = "2000-02-18", end_date = "2016-12-18")
#' }
#'@export 

time_series <- function(wtss.obj,
                        name,
                        attributes,
                        longitude,
                        latitude,
                        start_date,
                        end_date) {
    
    # is the object already a WTSS object or just a URL?
    # 
    # if it is an URL, try to create a WTSS object
    # 
    if (!("wtss" %in% class(wtss.obj))) {
        URL <- wtss.obj   # the parameter should be a URL if it is not a wtss object
        wtss.obj <- wtss::WTSS(URL)
        ensurer::ensure_that(wtss.obj, !purrr::is_null(.),
            err_desc = "WTSS - invalid URL for WTSS server")

    }
    # is there a coverage with this name in the WTSS service?
    # 
    ensurer::ensure_that(name, (.) %in% wtss.obj$coverages,
        err_desc = paste0("WTSS - coverage", name, "not available in the WTSS server"))
    
    # have we described the coverage before?
    # if not, get the coverage description
    # if fails, return NULL
    if (length(wtss.obj$description) == 0) {
        wtss.obj <- wtss::describe_coverage(wtss.obj, name)
        ensurer::ensure_that(wtss.obj, !purrr::is_null(.),
            err_desc = paste0("WTSS - could not retrieve description of coverage ",
                           name, " from WTSS server"))
    }

    # get coverage description
    desc <- dplyr::filter(wtss.obj$description, name == name)
    
    # check if the selected attributes are available
    cov_bands <- desc$bands[[1]]
    ensurer::ensure_that(attributes, all((.) %in% cov_bands),
                         err_desc = "WTSS - attributes not available.")
    
    # check bounds for latitude and longitude
    ensurer::ensure_that(longitude, (.) > desc$xmin && (.) < desc$xmax,
                         err_desc = "WTSS - invalid longitude value")
    
    ensurer::ensure_that(latitude, (.) > desc$ymin && (.) < desc$ymax,
                         err_desc = "WTSS - invalid latitude value")
    
    # check start and end date
    timeline <- desc$timeline[[1]]
    n_dates  <- length(timeline)
    
    # test is start date is valid
    ensurer::ensure_that(start_date, lubridate::as_date(.) >= lubridate::as_date(timeline[1]) &&
                                     lubridate::as_date(.) < lubridate::as_date(timeline[n_dates]),
                         err_desc = "WTSS - invalid start date")
    
    # test if end date is valid
    ensurer::ensure_that(end_date, lubridate::as_date(.) >  lubridate::as_date(timeline[1]) &&
                                   lubridate::as_date(.) <= lubridate::as_date(timeline[n_dates]) &&
                                   lubridate::as_date(.) >  lubridate::as_date(start_date),
                         err_desc = "WTSS - invalid end date")
    
    items <- NULL
    ce <- 0
    
    URL <- wtss.obj$url
    # try to retrive the time series 
    request <- paste(URL,"/time_series?coverage=", name, "&attributes=", 
                     paste(attributes, collapse = ","),
                     "&longitude=", longitude, "&latitude=", latitude,
                     "&start_date=", start_date, "&end_date=", end_date, sep = "")
                
    # try only 10 times (avoid time out connection)
    while (purrr::is_null(items) && ce < 10) {
        items <- .wtss_parse_json(.wtss_send_request(request))
        ce <- ce + 1
    }
                
    # if the server does not answer any item
    ensurer::ensure_that(items, !purrr::is_null(items),
                     err_desc = "Server connection timeout. Verify the URL or try again later.")
                
    result <- list(.wtss_time_series_processing(items))
                
    names(result) <- name
    
    ts.tb <- .wtss_to_tibble(result, name, attributes, longitude, latitude, start_date, end_date, desc)
                
    return(ts.tb)
}

#' @title Processing a Time Series Result from WTSS
#' @name .wtss_time_series_processing
#' 
#' @param items  Items retrieved from WTSS server
#' @return tibble with a time series 
#' 

.wtss_time_series_processing <- function(items) {
    
    attr_list <- list(items$result$attributes)
    
    
    attr.processed <- purrr::map(attr_list, function(subdataset) {
        # assign attribute values 
        value <- subdataset$values
        
        # assign values to dataframe
        value <- data.frame(value, stringsAsFactors = FALSE)
        
        # dataset names to the values vectors 
        names(value) <- subdataset$attribute
        
        return(value)
        
    })
    
    attr.processed <- data.frame(attr.processed, stringsAsFactors = FALSE)
    
    # convert string into date format
    timeline <- unlist(strsplit(items$result$timeline, split = " "))
    
    # check date format
    format <- lubridate::guess_formats(timeline[1], c("%Y-%m-%d", "%Y-%m"))
    
    # if monthly date
    if (any(format == "%Y-%m"))
        timeline = as.Date(zoo::as.yearmon(timeline))
    else # if weekly or daily date
        if (any(format == "%Y-%m-%d"))
            timeline = as.Date(timeline, format)
    
    return(list(center_coordinate = 
                    data.frame(longitude = items$result$coordinates$longitude, 
                               latitude  = items$result$coordinates$latitude), 
                attributes = zoo::zoo(attr.processed, timeline)))
    
}

