context("WTSS service")
test_that("Connection to a WTSS service", {
    URL <- "http://www.esensing.dpi.inpe.br/wtss/"
    wtss1 <-  wtss::WTSS(URL) 
    expect_true(wtss1$url == URL)
    coverages <- wtss1$coverages
    expect_true("MOD13Q1" %in% coverages)
})
