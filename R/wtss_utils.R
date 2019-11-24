#' @title retrieve the bands associated to a time series
#' @name .wtss_bands
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param ts         Tibble with time series
#'
#' @return List of bands

.wtss_bands <- function(ts) {
    bands <- ts$time_series[[1]] %>% colnames() %>% .[2:length(.)]
    return(bands)
}

#' @title Aligns dates of time series to a reference date
#' @name .wtss_align_dates
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Converts the time indexes of a set of tibble to a single reference year.
#' This function is useful to join many time series from different years to a single year,
#' which is required by methods that combine many time series, such as clustering methods.
#' The reference year is taken from the date of the start of the time series
#' available in the data cube.
#'
#' @param  data          Input tibble (useful for chaining functions).
#' @param  ref_dates     Dates to align the time series.
#' @return               A tibble with the converted dates (useful for chaining functions).
.wtss_align_dates <- function(data, ref_dates) {
    
    # function to shift a time series in time
    shift_ts <- function(d, k) dplyr::bind_rows(utils::tail(d,k), utils::head(d,-k))
    
    # get the reference date
    start_date <- lubridate::as_date(ref_dates[1])
    
    data1.lst <- purrr::pmap(list(data$longitude, data$latitude, data$coverage, data$time_series),
                function(long, lat, cov, ts) {
                    
                    # only rows that match the number of reference dates are kept
                    if (length(ref_dates) == nrow(ts)) {
                        # in what direction do we need to shift the time series?
                        sense <- lubridate::yday(lubridate::as_date(ts[1,]$Index)) - lubridate::yday(lubridate::as_date(start_date))
                        # find the date of minimum distance to the reference date
                        idx <- which.min(abs((lubridate::as_date(ts$Index) - lubridate::as_date(start_date))/lubridate::ddays(1)))
                        # do we shift time up or down?
                        if (sense < 0) shift <- -(idx - 1) else shift <- (idx - 1)
                        # shift the time series to match dates
                        if (idx != 1) ts <- shift_ts(ts, -(idx - 1))
                        # convert the time index to a reference year
                        first_date <- lubridate::as_date(ts[1,]$Index)
                        # change the dates to the reference dates
                        ts1 <- dplyr::mutate(ts, Index = ref_dates)
                        
                        # save the resulting row in the output tibble
                        row <- tibble::tibble(longitude   = long,
                                              latitude    = lat,
                                              start_date  = lubridate::as_date(ref_dates[1]),
                                              end_date    = ref_dates[length(ref_dates)],
                                              coverage    = cov,
                                              time_series = list(ts1))
                    }
                    return(row)
                })
    data1.tb <- dplyr::bind_rows(data1.lst)
    return(data1.tb)
}