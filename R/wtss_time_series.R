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
#' # connect to a WTSS server
#' wtss <- wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")
#' # retrieve a time series
#' ts   <- wtss::time_series(wtss, "MOD13Q1", c("ndvi","evi"), 
#'                 longitude = -45.00, latitude  = -12.00,
#'                 start_date = "2000-02-18", end_date = "2016-12-18")
#' }
#'@export 

time_series <- function(wtss.obj,
                        name,
                        attributes = NULL,
                        longitude,
                        latitude,
                        start_date = NULL,
                        end_date   = NULL) {
    
    # is the object already a WTSS object or just a URL?
    # 
    # if it is an URL, try to create a WTSS object
    # 
    if (!("wtss" %in% class(wtss.obj))) {
        URL <- wtss.obj   # the parameter should be a URL if it is not a wtss object
        wtss.obj <- wtss::WTSS(URL)
        assertthat::assert_that(!purrr::is_null(wtss.obj),
            msg = "WTSS - invalid URL for WTSS server")

    }
    # is there a coverage with this name in the WTSS service?
    # 
    assertthat::assert_that(name %in% wtss.obj$coverages,
        msg = paste0("WTSS - coverage", name, "not available in the WTSS server"))
    
    # have we described the coverage before?
    # if not, get the coverage description
    # if fails, return NULL
    if (length(wtss.obj$description) == 0) {
        wtss::describe_coverage(wtss.obj, name, .print = FALSE)
        assertthat::assert_that(!purrr::is_null(wtss.obj),
            msg = paste0("WTSS - could not retrieve description of coverage ",
                           name, " from WTSS server"))
    }

    # get coverage description
    desc <- dplyr::filter(wtss.obj$description, name == name)
    
    # check if the selected attributes are available
    cov_bands <- desc$bands[[1]]
    if (purrr::is_null(attributes))
        attributes <- cov_bands
    
    assertthat::assert_that(all(attributes %in% cov_bands),
                            msg = "WTSS - attributes not available.")
    
    # check bounds for latitude and longitude
    assertthat::assert_that(longitude > desc$xmin && longitude < desc$xmax,
                            msg = "WTSS - invalid longitude value")
    
    assertthat::assert_that(latitude > desc$ymin && latitude < desc$ymax,
                            msg = "WTSS - invalid latitude value")
    
    # check start and end date
    timeline <- desc$timeline[[1]]
    n_dates  <- length(timeline)
    
    if (purrr::is_null(start_date))
        start_date <-  lubridate::as_date(timeline[1])
    if (purrr::is_null(end_date))
        end_date <-  lubridate::as_date(timeline[n_dates])
    # test is start date is valid
    assertthat::assert_that(lubridate::as_date(start_date) >= lubridate::as_date(timeline[1]) &&
                            lubridate::as_date(start_date) < lubridate::as_date(timeline[n_dates]),
                            msg = "WTSS - invalid start date")
    
    # test if end date is valid
    assertthat::assert_that(lubridate::as_date(end_date) >  lubridate::as_date(timeline[1]) &&
                            lubridate::as_date(end_date) <= lubridate::as_date(timeline[n_dates]) &&
                            lubridate::as_date(end_date) >  lubridate::as_date(start_date),
                            msg = "WTSS - invalid end date")
    
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
    assertthat::assert_that(!purrr::is_null(items),
                            msg = "Server connection timeout. Verify the URL or try again later.")
                
    result <- list(.wtss_time_series_processing(items))
                
    names(result) <- name
    
    ts.tb <- .wtss_to_tibble(result, name, attributes, longitude, latitude, start_date, end_date, desc)
                
    class(ts.tb) <- append(class(ts.tb), "wtss", after = 0)
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
#' @title Export data to be used to the zoo format
#' @name wtss_to_zoo
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from a tibble to a list of a zoo series.
#'
#' @param  data       A tibble with time series.
#' @param  band       Name of the band to be exported (if NULL all bands are exported).
#' @return            List of time series in zoo format.
#' @examples
#' # connect to a WTSS server
#' wtss <- wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")
#' # retrieve a time series
#' ts_wtss  <- wtss::time_series(wtss, "MOD13Q1", c("ndvi","evi"), 
#'                 longitude = -45.00, latitude  = -12.00,
#'                 start_date = "2000-02-18", end_date = "2016-12-18")
#' # convert to zoo
#' zoo.lst <- wtss::wtss_to_zoo(ts_wtss)
#'          
#' @export
wtss_to_zoo <- function(data, band = NULL){
    zoo.lst <- data$time_series %>%
        purrr::map(function(ts) {
            if (purrr::is_null(band))
                band <-  colnames(ts[-1:0])
            # transform each sits time series to the zoo format
            zoo.ts <- zoo::zoo(ts[, band, drop = FALSE], ts$Index)
            return(zoo.ts)
        })
    
    return(zoo.lst)
}

#' @title Export data to be used to the ts format
#' @name wtss_to_ts
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from a sits tibble to a list of a zoo series.
#'
#' @param  data       A sits tibble with time series.
#' @param  band       Name of the band to be exported
#' @return            A time series in the ts format.
#' @examples
#' # read a tibble with 400 samples of Cerrado and 346 samples of Pasture
#' # connect to a WTSS server
#' wtss <- wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")
#' # retrieve a time series
#' ts_wtss  <- wtss::time_series(wtss, "MOD13Q1", c("ndvi","evi"), 
#'                 longitude = -45.00, latitude  = -12.00,
#'                 start_date = "2000-02-18", end_date = "2016-12-18")
#' # convert to zoo
#' ts <- wtss::wtss_to_ts(ts_wtss, band = "ndvi")
#' @export
wtss_to_ts <- function(data, band){
    # only convert one row at a time
    assertthat::assert_that(nrow(data) == 1,
                    msg = "WTSS - Convertion to ts only accepts one time series at a time.")
    # only univariate time series are accept
    assertthat::assert_that(length(band) == 1,
                    msg = "WTSS - Convertion to ts only accepts one band at a time.")
    
    # retrive the time series
    ts_wtss <- tibble::as_tibble(data$time_series[[1]])
    # transform the time series to the zoo format
    zoo <- zoo::zoo(ts_wtss[, band, drop = FALSE], ts_wtss$Index)
    ts  <- TSstudio::zoo_to_ts(zoo) 
    return(ts)
}
