context("WTSS service")
test_that("Connection to a WTSS service", {
    URL <- "http://www.esensing.dpi.inpe.br/wtss/"
    wtss1 <-  wtss::WTSS(URL) 
    expect_true(wtss1$url == URL)
    coverages <- wtss1$coverages
    expect_true("MOD13Q1" %in% coverages)
})
test_that("Bad connection to a WTSS service", {
    URL <- "http://www.dpi.inpe.br/wtss/"
    expect_message(wtss1 <- wtss::WTSS(URL), "WTSS server not responding - please check URL")
    expect_true(purrr::is_null(wtss1))
})

test_that("List coverages", {
    URL   <- "http://www.esensing.dpi.inpe.br/wtss/"
    wtss2 <-  wtss::WTSS(URL) 
    output <- capture.output(wtss::list_coverages(wtss2))
    expect_true(as.logical(grep(wtss2$url, output[2])))
    expect_true(as.logical(grep(wtss2$coverages[1], output[3])))
})

test_that("Describe coverage", {
    URL   <- "http://www.esensing.dpi.inpe.br/wtss/"
    wtss3 <-  wtss::WTSS(URL) 
    output <- capture.output(wtss::describe_coverage(wtss3, wtss3$coverages[1]))
    expect_true(as.logical(grep("satellite", output[5])))
    expect_true(as.logical(grep("minimum_values", output[15])))
    expect_true(as.logical(grep("maximum_values", output[20])))
    expect_true(as.logical(grep("Timeline", output[29])))
})

test_that("Time Series", {
    wtss4 <- wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")
    ts    <- wtss::time_series(wtss4, "MOD13Q1", c("ndvi","evi"), 
                     longitude = -45.00, latitude  = -12.00,
                     start_date = "2000-02-18", end_date = "2016-12-18")
    expect_true(nrow(ts$time_series[[1]]) == 388)
    expect_true(ncol(ts$time_series[[1]]) == 3)
})

test_that("Time Series 2", {
    wtss5 <- wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")
    ts    <- wtss::time_series(wtss5, "MOD13Q1", 
                               longitude = -45.00, latitude  = -12.00)
    expect_true(nrow(ts$time_series[[1]]) == 452)
    expect_true(ncol(ts$time_series[[1]]) == 7)
})

test_that("Time Series", {
    wtss6 <- wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")
    ts    <- wtss::time_series(wtss6, "MOD13Q1", longitude = 45.00, latitude  = -12.00)
}