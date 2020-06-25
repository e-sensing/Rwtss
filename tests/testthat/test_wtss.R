context("WTSS service")
library(testthat)

test_that("List coverages", {
    wtss2 <-  "http://www.esensing.dpi.inpe.br/wtss/"

    output <- capture.output(wtss::list_coverages(wtss2))
    if (!purrr::is_null(output)) {
        expect_true(as.logical(grep("Coverages", output[1])))
        expect_true(as.logical(grep("MOD13Q1", output[1])))        
    }
})

test_that("Describe coverage", {
    wtss3 <-  "http://www.esensing.dpi.inpe.br/wtss/"

    output <- capture.output(wtss::describe_coverage(wtss3, "MOD13Q1"))
    expect_true(as.logical(grep("satellite", output[5])))
    expect_true(as.logical(grep("minimum_values", output[15])))
    expect_true(as.logical(grep("maximum_values", output[20])))
    expect_true(as.logical(grep("Timeline", output[29])))
})

test_that("Time Series", {
    wtss5 <- "http://www.esensing.dpi.inpe.br/wtss/"

    ts    <- wtss::time_series(wtss5, "MOD13Q1", c("ndvi","evi"), 
                     longitude = -45.00, latitude  = -12.00,
                     start_date = "2000-02-18", end_date = "2016-12-18")
    expect_true(nrow(ts$time_series[[1]]) == 388)
    expect_true(ncol(ts$time_series[[1]]) == 3)
})

test_that("Time Series 2", {
    wtss6 <- "http://www.esensing.dpi.inpe.br/wtss/"
    
    ts    <- wtss::time_series(wtss6, "MOD13Q1", 
                               longitude = -45.00, latitude  = -12.00)
    expect_true(ncol(ts$time_series[[1]]) == 7)
    expect_true(all(c("Index", "mir", "blue", "nir", "red", "evi", "ndvi") 
                    %in% names(ts$time_series[[1]])))
    expect_true(nrow(ts$time_series[[1]]) >= 452)
})

test_that("Plot",{
    wtssg <- "http://www.esensing.dpi.inpe.br/wtss/"
    
    tsg    <- wtss::time_series(wtssg, "MOD13Q1", 
                               longitude = -45.00, latitude  = -12.00)
    g <- plot(tsg)
    expect_true(unname(summary(g)[1,2]) == "gg")
})
test_that("Time Series - errors", {
    wtss7 <- "http://www.esensing.dpi.inpe.br/wtss/"
    
    expect_message(ts <- wtss::time_series(wtss7, "MOD13Q1", 
                                         longitude = 45.00, latitude  = -12.00))
    expect_true(purrr::is_null(ts))
    
    expect_message(ts <- wtss::time_series(wtss7, "MOD13Q1", 
                                         longitude = -45.00, latitude  = 12.00))
    expect_true(purrr::is_null(ts))
    
    expect_message(ts <- wtss::time_series(wtss7, "MOD13Q1", c("swir", "ndvi"),
                                        longitude = -45.00, latitude  = -12.00))
    expect_true(purrr::is_null(ts))
    
    expect_message(ts <- wtss::time_series(wtss7, "MOD13Q1",
                                         longitude = -45.00, latitude  = -12.00,
                                         start_date = "1999-01-01"))
    expect_true(purrr::is_null(ts))
    
    expect_message(ts <- wtss::time_series(wtss7, "MOD13Q1",
                                         longitude = -45.00, latitude  = -12.00,
                                         end_date = "2045-01-01"))
    expect_true(purrr::is_null(ts))
    
    expect_message(ts <- wtss::time_series(wtss7, "MOD13Q1",
                                         longitude = -45.00, latitude  = -12.00,
                                         start_date = "2010-01-01", 
                                         end_date   = "2005-01-01"))
    expect_true(purrr::is_null(ts))
    
    expect_message(ts <- wtss::time_series(wtss7, "MOD13Q12",
                                        longitude = -45.00, latitude  = -12.00))
    
    expect_true(!purrr::is_null(ts))
})

test_that("Time Series - conversion to ts and zoo", {
    wtss8 <- "http://www.esensing.dpi.inpe.br/wtss/"

    ts    <- wtss::time_series(wtss8, "MOD13Q1", 
                               longitude = -45.00, latitude  = -12.00)
    
    ts_start <- c(as.numeric(lubridate::year(ts$start_date)), 
                  as.numeric(lubridate::week(ts$start_date)))
    ts_end   <- c(as.numeric(lubridate::year(ts$end_date)), 
                  as.numeric(lubridate::week(ts$end_date)))
    
    ts_zoo <- wtss::wtss_to_zoo(ts)
    
    expect_true(nrow(ts_zoo) == nrow(ts$time_series[[1]]))
    expect_true(ncol(ts_zoo) == (ncol(ts$time_series[[1]]) - 1))
    expect_true(all(as.vector(ts_zoo[,1]) == 
                        dplyr::pull(ts$time_series[[1]][,2])))
    
    ts1    <- wtss::time_series(wtss8, "MOD13Q1", 
                               longitude = -45.00, latitude  = -12.50)
    
    ts2    <- dplyr::bind_rows(ts,ts1)
    expect_message(wtss::wtss_to_zoo(ts2))
    

    ts_week <- wtss::wtss_to_ts(ts, band = "ndvi", period = "week")
    ts_weeks <- wtss::wtss_to_ts(ts, band = "ndvi", period = "weeks")
    ts_week_52 <- wtss::wtss_to_ts(ts, band = "ndvi", period = 52)
    
    expect_true(all(stats::start(ts_week) == ts_start))
    expect_true(ts_week[1] == as.numeric(ts_zoo[1, "ndvi"]))
    expect_true(all(ts_week == ts_weeks))
    expect_true(all(ts_week == ts_week_52))
    
    ts_day <- wtss::wtss_to_ts(ts, band = "ndvi", period = "day")
    ts_days <- wtss::wtss_to_ts(ts, band = "ndvi", period = "days")
    ts_day_1 <- wtss::wtss_to_ts(ts, band = "ndvi", period = 365)
    
    expect_true(all(stats::start(ts_day) == ts_start))
    expect_true(ts_day[1] == as.numeric(ts_zoo[1, "ndvi"]))
    expect_true(all(ts_day == ts_days))
    expect_true(all(ts_day == ts_day_1))
    
    ts_month  <- wtss::wtss_to_ts(ts, band = "ndvi", period = "month")
    ts_months <- wtss::wtss_to_ts(ts, band = "ndvi", period = "months")
    ts_month_12 <- wtss::wtss_to_ts(ts, band = "ndvi", period = 12)
    expect_true(all(ts_month == ts_months))
    expect_true(all(ts_month == ts_month_12))
    
    expect_true(all(stats::start(ts_month) == ts_start))
    expect_true(ts_month[1] == as.numeric(ts_zoo[1, "ndvi"]))
    
    expect_message(wtss::wtss_to_ts(ts, band = "ndvi", period = "year"))
    
    expect_message(wtss::wtss_to_ts(ts, band = c("ndvi", "evi")))
    expect_message(wtss::wtss_to_ts(ts))
    
    expect_message(ts2_ts <- wtss::wtss_to_ts(ts2, band = "ndvi"))
    
    
})

test_that("Bad connection to a WTSS service", {
    wtss10 <- "http://www.dpi.inpe.br2/wtss/"
    
    output <- suppressMessages(wtss::list_coverages(wtss10))
    expect_true(purrr::is_null(output))
    
    output <- suppressMessages(wtss::describe_coverage(wtss10, "MOD"))
    expect_true(purrr::is_null(output))
    
    ts <- suppressMessages(wtss::time_series(wtss10, "MOD13Q1", 
                longitude = 45.00, latitude  = -12.00))
    expect_true(purrr::is_null(ts))
})


test_that("Guess satellite",{
    sat_sensor <- wtss:::.wtss_guess_satellite(0.002)
    expect_equal(unname(sat_sensor[1]), "TERRA")
    expect_equal(unname(sat_sensor[2]), "MODIS")
    
    sat_sensor <- wtss:::.wtss_guess_satellite(10)
    expect_equal(unname(sat_sensor[1]), "SENTINEL-2")
    expect_equal(unname(sat_sensor[2]), "MSI")
    
    sat_sensor <- wtss:::.wtss_guess_satellite(70)
    expect_equal(unname(sat_sensor[1]), "CBERS")
    expect_equal(unname(sat_sensor[2]), "AWFI")
    
    sat_sensor <- wtss:::.wtss_guess_satellite(30)
    expect_equal(unname(sat_sensor[1]), "LANDSAT")
    expect_equal(unname(sat_sensor[2]), "OLI")
    
    sat_sensor <- wtss:::.wtss_guess_satellite(5)
    expect_equal(unname(sat_sensor[1]), "UNKNOWN")
    expect_equal(unname(sat_sensor[2]), "UNKNOWN")
})
