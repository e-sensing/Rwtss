Rwtss - R interface to Web Time Series Service
================

[![CRAN/METACRAN
Version](https://www.r-pkg.org/badges/version/wtss)](https://CRAN.R-project.org/package=wtss)
[![CRAN/METACRAN Total
downloads](http://cranlogs.r-pkg.org/badges/grand-total/wtss?color=blue)](https://CRAN.R-project.org/package=wtss)
[![Travis-CI Build
Status](https://travis-ci.com/e-sensing/wtss.svg?branch=master)](https://travis-ci.com/e-sensing/wtss)
[![Codecov test
coverage](https://codecov.io/gh/e-sensing/wtss/branch/master/graph/badge.svg)](https://codecov.io/gh/e-sensing/wtss?branch=master)

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
supports the WTSS protocol. Currenlty, Brazil’s National Insitute for
Space Research (INPE) runs such a WTSS service in the address below.

``` r
# Connect to the WTSS server at INPE Brazil
wtss_inpe <-  "http://www.esensing.dpi.inpe.br/wtss"
```

## Listing coverages available at the WTSS server

This operation allows clients to retrieve the capabilities provided by
any server that implements WTSS. It returns a list of coverage names
available in a server instance.

``` r
# Connect to the WTSS server at INPE Brazil
Rwtss::list_coverages(wtss_inpe)
```

    #> [1] "MOD13Q1"   "MOD13Q1_M"

## Describing a coverage from the WTSS server

This operation returns the metadata for a given coverage identified by
its name. It includes its range in the spatial and temporal dimensions.

``` r
# Connect to the WTSS server at INPE Brazil
desc <- Rwtss::describe_coverage(wtss_inpe, name = "MOD13Q1")
```

    #> ---------------------------------------------------------------------
    #> WTSS server URL = http://www.esensing.dpi.inpe.br/wtss
    #> Cube (coverage) = MOD13Q1
    #> Timeline - 452 time steps
    #> start_date: 2000-02-18 end_date: 2019-09-30
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
used.

``` r
# Request a time series from the "MOD13Q1" coverage
ndvi_ts   <- Rwtss::time_series(wtss_inpe, name = "MOD13Q1", 
        attributes = c("ndvi","evi"), longitude = -45.00, latitude  = -12.00,
        start_date = "2000-02-18", end_date = "2016-12-18")
```

``` r
ndvi_ts
#> # A tibble: 1 x 7
#>   longitude latitude start_date end_date   label   cube    time_series       
#>       <dbl>    <dbl> <date>     <date>     <chr>   <chr>   <list>            
#> 1     -53.5    -10.4 2000-02-18 2019-09-30 NoClass MOD13Q1 <tibble [452 × 2]>
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
#> # A tibble: 452 x 2
#>    Index       ndvi
#>    <date>     <dbl>
#>  1 2000-02-18 0.884
#>  2 2000-03-05 0.691
#>  3 2000-03-21 0.853
#>  4 2000-04-06 0.854
#>  5 2000-04-22 0.879
#>  6 2000-05-08 0.861
#>  7 2000-05-24 0.853
#>  8 2000-06-09 0.864
#>  9 2000-06-25 0.880
#> 10 2000-07-11 0.870
#> # … with 442 more rows
```
