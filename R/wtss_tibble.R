#' @title Create a sits tibble to store the time series information
#' @name .wtss_tibble
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function returns an empty tibble that
#' contains the satellite image time series and its metadata. The columns are
#' <longitude, latitude, start_date, end_date, label, cube, time_series>.
#' WTSS functions produce a tibble as output.
#' 
#' @return A tibble.
.wtss_tibble <- function() {
    result <- tibble::tibble(longitude   = double(),
                             latitude    = double(),
                             start_date  = as.Date(character()),
                             end_date    = as.Date(character()),
                             coverage    = character(),
                             time_series = list()
    )
    class(result) <- append(class(result), "sits", after = 0)
    return(result)
}

#' @title Import time series in the zoo format to a tibble
#' @name .wtss_to_tibble
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts data from an instance of a zoo series to a sits tibble.
#'
#' @param ts            list of time series retrieved by WTSS
#' @param name          Name of the coverage where data comes from.
#' @param bands         Bands to be retrieved from the time series.
#' @param longitude     Longitude of the chosen location.
#' @param latitude      Latitude of the chosen location.
#' @param start_date    Starting date of the time series
#' @param end_date      End date of the time series
#' @param cov_desc      Description of the WTSS coverage
#' @return Time series in sits tibble format.
#'
.wtss_to_tibble <- function(ts, name, bands, longitude, latitude, 
                            start_date, end_date, cov_desc) {
    # retrieve the time series information
    time_series <- ts[[name]]$attributes
    
    # determine the missing value for each band
    missing_values <- cov_desc$missing_values[[1]]
    
    # update missing values to NA
    bands %>%
        purrr::map(function(b) {
            time_series[, b][time_series[, b] == missing_values[b]] <<- NA
        })
    
    # interpolate missing values
    time_series[, bands] <- zoo::na.spline(time_series[, bands])
    
    # scale the time series
    scale_factors <- cov_desc$scale_factors[[1]]
    bands %>%
        purrr::map(function(b) {
            time_series[, b] <<- time_series[, b]*scale_factors[b]
        })
    
    # convert the series to a tibble
    ts.tb <- tibble::as_tibble(zoo::fortify.zoo(time_series))
    
    # create a list to store the time series coming from the WTSS service
    ts.lst <- list()
    ts.lst[[1]] <- ts.tb
    
    # create a tibble to store the WTSS data
    data <- .wtss_tibble()
    # add one row to the tibble
    data <- tibble::add_row(data,
                               longitude,
                               latitude,
                               start_date  = start_date,
                               end_date    = end_date,
                               coverage    = name,
                               time_series = ts.lst)
    
    # return the tibble with the time series
    return(tibble::as_tibble(data))
}
