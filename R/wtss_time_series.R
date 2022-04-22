

#' @title Processing a Time Series Result from WTSS
#' @name .wtss_time_series_processing
#' 
#' @param items  Items retrieved from WTSS server
#' @return tibble with a time series 
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
    
    timeline <- lubridate::as_date(timeline)
    
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
#' @examples
#' \donttest{
#' # retrieve a time series
#' ts_wtss  <- Rwtss::time_series("http://www.esensing.dpi.inpe.br/wtss", 
#'                 "MOD13Q1", c("ndvi","evi"), 
#'                 longitude = -45.00, latitude  = -12.00,
#'                 start_date = "2000-02-18", end_date = "2016-12-18")
#' # convert to zoo
#' zoo.lst <- Rwtss::wtss_to_zoo(ts_wtss)
#' }
#' @export
wtss_to_zoo <- function(data, band = NULL){
    # only convert one row at a time
    if (nrow(data) > 1) {
        message("Conversion to ts only accepts one time series at a time.")  
        data <- data[1,]
    }
    
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
#' a time series (a tibble with data and metadata) to the "ts" format. 
#' Since  "ts" requires regular time series, it interpolates 
#' the original irregular time series to a regular time series. To do this, the 
#' user needs to specify a period which is recognised by the "ts" format. 
#' This period can be either {"month", "week", "day"}, 
#' {"months", "weeks", "days"} or
#' {12, 52, 365}. This function creates a new time series with the required 
#' frequency and intepolates the missing values using spline interpolation 
#' from the "zoo" package (zoo::na.spline).
#'
#' @param  data          A sits tibble with time series.
#' @param  band          Name of the band to be exported 
#'                       (optional if series has only one band)
#' @param  period        One of c("month", "week", "day"), 
#'                       c("months", "weeks", "days") or
#'                       c(12, 52, 365)
#' @return               A time series in the ts format.
#' @examples
#' \dontrun{
#' # connect to a WTSS server
#' wtss <- "https://brazildatacube.dpi.inpe.br/wtss/"
#' # retrieve a time series
#' ts_wtss  <- Rwtss::time_series(wtss, "MOD13Q1-6", c("ndvi","evi"), 
#'                 longitude = -45.00, latitude  = -12.00,
#'                 start_date = "2000-02-18", end_date = "2016-12-18")
#' # convert to ts
#' ts <- Rwtss::wtss_to_ts(ts_wtss, band = "ndvi")
#' }
#' @export
wtss_to_ts <- function(data, band  = NULL, period = "week"){
    # only convert one row at a time
    if (nrow(data) > 1) {
        message("Conversion to ts only accepts one time series at a time.")
        data <- data[1,]
    }
    # retrieve the time series
    ts_wtss <- tibble::as_tibble(data$time_series[[1]])
    # no band informed?
    if (purrr::is_null(band)) {
        # only univariate time series are accepted
        if (ncol(ts_wtss > 2)) {
            message("WTSS - Conversion to ts only accepts one band at a time.")
            message("using the first available band")
        }
        band <- names(ts_wtss[,2])
    }
    # more than one band?
    if (length(band) > 1) {
        message("WTSS - Conversion to ts only accepts one band at a time.")
        message("using the first available band")
        band <- band[1]
    }
    # check valid periods
    valid_periods <- c("month", "week", "day")
    names(valid_periods) <- c("months", "weeks", "days")
    valid_frequencies <- c(12, 52, 365)
    names(valid_frequencies) <- c("month", "week", "day")
    freq_to_period <- valid_periods 
    names(freq_to_period) <- c(12, 52, 365)
    # is the period in c("week", "month", "day")?
    if (period %in% valid_periods) {
        zoo_frequency <- period
        ts_frequency  <- valid_frequencies[period]
    }
    # is the period in c("weeks", "months", "days")?
    else if (period %in% names(valid_periods)) {
        zoo_frequency <- valid_periods[period]
        ts_frequency  <- valid_frequencies[zoo_frequency]
    }
    # is the period in c(12, 52, 365)?
    else if (period %in% valid_frequencies) {
        zoo_frequency <- freq_to_period[as.character(period)]
        ts_frequency  <- period
    }
    else {
        message("Rwtss - Invalid period for conversion to ts")
        return(NULL)
    }
    
    # get the start and end date of the series
    start_date <- lubridate::as_date(data$start_date)
    end_date   <- lubridate::as_date(data$end_date)
    
    # convert to zoo
    ts_zoo <-  Rwtss::wtss_to_zoo(data)
    
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
