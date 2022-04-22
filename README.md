Rwtss - R interface to Web Time Series Service
================

[![CRAN/METACRAN
Version](https://www.r-pkg.org/badges/version/Rwtss)](https://CRAN.R-project.org/package=Rwtss)
[![CRAN/METACRAN Total
downloads](http://cranlogs.r-pkg.org/badges/grand-total/Rwtss?color=blue)](https://CRAN.R-project.org/package=Rwtss)
[![Build
Status](https://cloud.drone.io/api/badges/e-sensing/Rwtss/status.svg)](https://cloud.drone.io/e-sensing/Rwtss)
[![codecov](https://codecov.io/gh/e-sensing/Rwtss/branch/master/graph/badge.svg?token=gDsvUHhLmI)](https://codecov.io/gh/e-sensing/Rwtss)

## About the package

The Rwtss package is a front-end to the Web Time Series Service (WTSS)
that offers time series of remote sensing data using a simple API. A
WTSS server takes as input an Earth observation data cube, that has a
spatial and a temporal dimension and can be multidimensional in terms of
its attributes. The WTSS API has three commands, which are are (a)
*list\_coverages*, that returns a list of coverages available in the
server; (b) *describe\_coverage*, that that returns the metadata for a
given coverage; (c) *time\_series*, that returns a time series for a
spatio-temporal location.

The R interface to WTSS services considers that “coverages” are
equivalent to “data cubes”. Data cubes rely on the fact that Earth
observation satellites revisit the same place at regular intervals. Thus
measures can be calibrated so that observations of the same place in
different times are comparable. These calibrated observations can be
organised in regular intervals, so that each measure from sensor is
mapped into a three dimensional multivariate array in space-time.

The first step towards using the service is connecting to a server that
supports the WTSS protocol. Currently, Brazil’s National Institute for
Space Research (INPE) runs such a WTSS service in the address below.

``` r
# Connect to the WTSS server at INPE Brazil
wtss_inpe <-  "https://brazildatacube.dpi.inpe.br/wtss/"
```

## Listing coverages available at the WTSS server

This operation allows clients to retrieve the capabilities provided by
any server that implements WTSS. It returns a list of coverage names
available in a server instance.

``` r
# Connect to the WTSS server at INPE Brazil
Rwtss::list_coverages(wtss_inpe)
```

    #> [1] "MOD13Q1-6"               "MYD13Q1-6"              
    #> [3] "S2-SEN2COR_10_16D_STK-1" "LC8_30_16D_STK-1"       
    #> [5] "CB4MUX_20_1M_STK-1"      "LC8_30_6M_MEDSTK-1"     
    #> [7] "CB4_64_16D_STK-1"        "LANDSAT-MOZ_30_1M_STK-1"

## Describing a coverage from the WTSS server

This operation returns the metadata for a given coverage identified by
its name. It includes its range in the spatial and temporal dimensions.

``` r
# Connect to the WTSS server at INPE Brazil
desc <- Rwtss::describe_coverage(wtss_inpe, name = "MOD13Q1-6")
```

    #> ---------------------------------------------------------------------
    #> WTSS server URL = https://brazildatacube.dpi.inpe.br/wtss
    #> Cube (coverage) = MOD13Q1-6
    #> Timeline - 508 time steps
    #> start_date: 2000-02-18 end_date: 2022-03-22
    #> ---------------------------------------------------------------------

## Obtaining a time series

This operation requests the time series of values of a coverage
attribute at a given location. Its parameters are: (a) *wtss.obj*:
either a WTSS object (created by the operation wtss::WTSS as shown
above) or a valid WTSS server URL; (b) *name*: Cube (coverage) name; (c)
*attributes*: vector of band names (optional). If omitted, all bands are
retrieved; (d) *longitude*: longitude in WGS84 coordinate system;
(e)*latitude*: Latitude in WGS84 coordinate system; (f)*start\_date*
(optional): Start date in the format yyyy-mm-dd or yyyy-mm depending on
the coverage. If omitted, the first date on the timeline is used; (g)
*end\_date*(optional): End date in the format yyyy-mm-dd or yyyy-mm
depending on the coverage. If omitted, the last date of the timeline is
used; (h) To access the BDC time series it is necessary to provide a
token, provide the token through the `token` parameter. To create a new
BDC token, please see this
[tutorial](https://brazil-data-cube.github.io/applications/dc_explorer/token-module.html).

``` r
# Request a time series from the "MOD13Q1" coverage
ndvi_ts   <- Rwtss::time_series(wtss_inpe, 
                                name = "MOD13Q1-6", 
                                attributes = c("NDVI", "EVI"), 
                                longitude = -45.00, 
                                latitude  = -12.00,
                                start_date = "2000-02-18", 
                                end_date = "2016-12-18",
                                token = "YOUR-BDC-TOKEN")
```

``` r
ndvi_ts
#> # A tibble: 1 × 7
#>   longitude latitude start_date end_date   label   cube      time_series       
#>       <dbl>    <dbl> <date>     <date>     <chr>   <chr>     <list>            
#> 1       -45      -12 2000-02-18 2016-12-18 NoClass MOD13Q1-6 <tibble [388 × 3]>
```

The result of the operation is a `tibble` which contains data and
metadata. The first six columns contain the metadata: satellite, sensor,
spatial and temporal information, and the coverage from where the data
has been extracted. The spatial location is given in longitude and
latitude coordinates for the “WGS84” ellipsoid. The `time_series` column
contains the time series data for each spatiotemporal location. This
data is also organized as a tibble, with a column with the dates and the
other columns with the values for each spectral band.

For compatibility with the **sits** suite of packages for satellite
image time series analysis, the R interface to the WTSS packages uses
the term “cube” to refer to the contents of the coverage. The time
series retrieved from WTSS also include a “label” column, to be use for
assigning labels to time series samples that are used to train
classifiers.

``` r
# Showing the contents of a time series
ndvi_ts$time_series[[1]]
#> # A tibble: 388 × 3
#>    Index       NDVI   EVI
#>    <date>     <dbl> <dbl>
#>  1 2000-02-18 0.374 0.302
#>  2 2000-03-05 0.820 0.497
#>  3 2000-03-21 0.802 0.481
#>  4 2000-04-06 0.809 0.432
#>  5 2000-04-22 0.749 0.409
#>  6 2000-05-08 0.727 0.394
#>  7 2000-05-24 0.698 0.374
#>  8 2000-06-09 0.654 0.325
#>  9 2000-06-25 0.608 0.310
#> 10 2000-07-11 0.583 0.291
#> # … with 378 more rows
```
