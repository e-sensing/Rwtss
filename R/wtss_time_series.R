#' @title Get time series
#' @name time_series
#' @author  Gilberto Camara
#' @description Retrieves the time series for a pair of coordinates 
#' 
#' @param wtss.obj      Either a WTSS object or a server URL
#' @param name          Coverage name.
#' @param attributes    Vector of band names.
#' @param longitude     Longitude in WGS84 coordinate system.
#' @param latitude      Latitude in WGS84 coordinate system.
#' @param start_date    Start date in the format yyyy-mm-dd or yyyy-mm 
#'                      depending on the coverage.
#' @param end_date      End date in the format yyyy-mm-dd or yyyy-mm 
#'                      depending on the coverage.
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
        URL <- wtss.obj # the parameter should be a URL or a wtss object
        wtss.obj <- wtss::WTSS(URL)
        assertthat::assert_that(!purrr::is_null(wtss.obj),
            msg = "WTSS - invalid URL for WTSS server")

    }
    # is there a coverage with this name in the WTSS service?
    # 
    assertthat::assert_that(name %in% wtss.obj$coverages,
        msg = paste0("WTSS - coverage", name, 
                     "not available in the WTSS server"))
    
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
    assertthat::assert_that(lubridate::as_date(start_date) >= 
                                lubridate::as_date(timeline[1]) &&
                            lubridate::as_date(start_date) < 
                                lubridate::as_date(timeline[n_dates]),
                            msg = "WTSS - invalid start date")
    
    # test if end date is valid
    assertthat::assert_that(lubridate::as_date(end_date) >  
                                lubridate::as_date(timeline[1]) &&
                            lubridate::as_date(end_date) <= 
                                lubridate::as_date(timeline[n_dates]) &&
                            lubridate::as_date(end_date) >  
                                lubridate::as_date(start_date),
                            msg = "WTSS - invalid end date")
    
    items <- NULL
    ce <- 0
    
    URL <- wtss.obj$url
    # try to retrive the time series 
    request <- paste(URL,"/time_series?coverage=", name, "&attributes=", 
                     paste(attributes, collapse = ","),
                     "&longitude=", longitude, "&latitude=", latitude,
                     "&start_date=", start_date, 
                     "&end_date=", end_date, sep = "")
                
    # try only 10 times (avoid time out connection)
    while (purrr::is_null(items) && ce < 10) {
        items <- .wtss_parse_json(.wtss_send_request(request))
        ce <- ce + 1
    }
                
    # if the server does not answer any item
    assertthat::assert_that(!purrr::is_null(items),
                            msg = "Server connection timeout. 
                            Verify the URL or try again later.")
                
    result <- list(.wtss_time_series_processing(items))
                
    names(result) <- name
    
    ts.tb <- .wtss_to_tibble(result, name, attributes, longitude, latitude, 
                             start_date, end_date, desc)
                
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
        timeline <- as.Date(zoo::as.yearmon(timeline))
    else # if weekly or daily date
        if (any(format == "%Y-%m-%d"))
            timeline <- as.Date(timeline, format)
    
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
#' @param  band       Name of the band to be exported 
#'                    (if NULL all bands are exported).
#' @return            List of time series in zoo format.
#' @examples {
#' # connect to a WTSS server
#' wtss <- wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")
#' # retrieve a time series
#' ts_wtss  <- wtss::time_series(wtss, "MOD13Q1", c("ndvi","evi"), 
#'                 longitude = -45.00, latitude  = -12.00,
#'                 start_date = "2000-02-18", end_date = "2016-12-18")
#' # convert to zoo
#' zoo.lst <- wtss::wtss_to_zoo(ts_wtss)
#' }
#' @export
wtss_to_zoo <- function(data, band = NULL){
    # only convert one row at a time
    assertthat::assert_that(nrow(data) == 1,
                            msg = "WTSS - Convertion to ts only accepts 
                            one time series at a time.")
    ts <- data$time_series[[1]]
    if (purrr::is_null(band))
        band <-  colnames(ts[-1:0])
    # transform a time series to the zoo format
    zoo.ts <- zoo::zoo(ts[, band, drop = FALSE], ts$Index)
    
    return(zoo.ts)
}

#' @title Export data to be used to the ts format
#' @name wtss_to_ts
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from a wtss tibble to a time series "ts".
#' A WTSS tibble contains data retrieved from a WTSS server. 
#' These data sets are time series with irregular intervals. Given that
#' of many functions that use the R "ts" format, this function converts 
#' a WTSS time series (a tibble with data and metadata) to the "ts" format. 
#' Since  "ts" requires regular time series, it interpolates 
#' the original irregular time series to a regular time series. To do this, the 
#' user needs to specify a period which is recognised by the "ts" format. 
#' This period can be either {"year", "quarter", "month", "week", "day"}, 
#' {"years", "quarters", "months", "weeks", "days"} or
#' {1, 4, 12, 52}. This function creates a new time series with the required 
#' frequency and intepolates the missing values using spline interpolation 
#' from the "zoo" package (zoo::na.spline).
#'
#' @param  data          A sits tibble with time series.
#' @param  band          Name of the band to be exported 
#'                       (optional if series has only one band)
#' @param  period        One of c("year", "quarter", "month", "week", "day"), 
#'                       c("years", "quarters", "months", "weeks", "days") or
#'                       c(1, 4, 12, 52)
#' @return               A time series in the ts format.
#' @examples {
#' # connect to a WTSS server
#' wtss <- wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")
#' # retrieve a time series
#' ts_wtss  <- wtss::time_series(wtss, "MOD13Q1", c("ndvi","evi"), 
#'                 longitude = -45.00, latitude  = -12.00,
#'                 start_date = "2000-02-18", end_date = "2016-12-18")
#' # convert to ts
#' ts <- wtss::wtss_to_ts(ts_wtss, band = "ndvi")
#' }
#' @export
wtss_to_ts <- function(data, band  = NULL, period = "week"){
    # only convert one row at a time
    assertthat::assert_that(nrow(data) == 1,
                    msg = "WTSS - Convertion to ts only accepts 
                    one time series at a time.")

    # retrieve the time series
    ts_wtss <- tibble::as_tibble(data$time_series[[1]])
    # no band informed?
    if (purrr::is_null(band)) {
        # only univariate time series are accepted
        assertthat::assert_that(ncol(ts_wtss) == 2,
                                msg = "WTSS - Convertion to ts only accepts 
                                one band at a time.")
        band <- names(ts_wtss[,2])
    }
    # check valid periods
    valid_periods_1 <- c("year", "quarter", "month", "week", "day")
    valid_periods_2 <- c("years", "quarters", "months", "weeks", "days")
    valid_frequencies <- c(1, 4, 12, 52, 365)
    names(valid_frequencies) <- c("year", "quarter", "month", "week", "day")
    names(valid_periods_2)   <- c("year", "quarter", "month", "week", "day")
    
    assertthat::assert_that(period %in% valid_periods_1 ||
                            period %in% valid_periods_2 ||
                            period %in% valid_frequencies,
                            msg = "WTSS - Invalid period for convertion to ts")
    # is the period in c("year", "quarter", "month", "day")?
    if (period %in% valid_periods_1) {
        zoo_frequency <- period
        ts_frequency  <- valid_frequencies[period]
    }
    # is the period in c("years", "quarters", "months", "days")?
    else if (period %in% valid_periods_2) {
        zoo_frequency <- names(valid_periods_2[period])
        ts_frequency  <- valid_frequencies[zoo_frequency]
    }
    # is the period in c(1, 4, 12, 52)?
    else if (period %in% valid_frequencies) {
        zoo_frequency <- names(valid_frequencies[period])
        ts_frequency  <- period
    }
    else {
        message("WTSS - Invalid period ")
        return(NULL)
    }
    
    # get the start and end date of the series
    start_date <- lubridate::as_date(data$start_date)
    end_date   <- lubridate::as_date(data$end_date)
    
    # convert to zoo
    ts_zoo <-  wtss::wtss_to_zoo(data)
    
    # create a regular zoo time series 
    # create a timeline with regular interval
    timeline_reg <- seq(start_date, end_date, by = zoo_frequency)
    # create a zoo time series with regular intervals filled with NA
    ts_zoo_reg <- zoo::zoo(x = NA, order.by = timeline_reg)
    
    # merge the two time series (regular and irregular)
    ts_zoo_merged <- zoo::merge.zoo(ts_zoo, ts_zoo_reg)[,band]
    
    # interpolated zoo series
    ts_zoo_interp <- zoo::na.spline(ts_zoo_merged)
    #get regular time series
    ts_zoo_reg2 <- ts_zoo_interp[zoo::index(ts_zoo_reg)]
    
    ts_start <- c(as.numeric(lubridate::year(start_date)), 
                  as.numeric(lubridate::week(start_date)))
    ts_end   <- c(as.numeric(lubridate::year(end_date)), 
                  as.numeric(lubridate::week(end_date)))
    
    ts_ts <- stats::ts(data = ts_zoo_reg2, start = ts_start, end = ts_end,
                       frequency = ts_frequency)
    
    return(ts_ts)
}

