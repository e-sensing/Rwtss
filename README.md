WTSS - R interface to Web Time Series Service
================

## About the package

The WTSS-R package is a front-end to the Web Time Series Service (WTSS)
that offers time series of remote sensing data using a simple API. A
WTSS server takes as input an Earth observation data cube, that has a
spatial and a temporal dimension and can be multidimensional in terms of
its attributes. The WTSS API has three commands, which are are
*list\_coverages*, that returns a list of coverages available in the
server; (b) *describe\_coverage*, that that returns the metadata for a
given coverage; (c) *time\_series*, that returns a time series for a
spatio-temporal location.

Data cubes rely on the fact that Earth observation satellites revisit
the same place at regular intervals. Thus measures can be calibrated so
that observations of the same place in different times are comparable.
These calibrated observations can be organised in regular intervals, so
that each measure from sensor is mapped into a three dimensional
multivariate array in
space-time.

<img src="/Users/gilberto/Library/R/3.6/library/wtss/extdata/markdown/figures/3D-Arrays.png" title="A Normalized Difference Vegetation Index (NDVI) time series" alt="A Normalized Difference Vegetation Index (NDVI) time series" style="display: block; margin: auto;" />

## Connecting to a WTSS server

The first step towards using the service is connecting to a server that
supports the WTSS protocol. Currenlty, Brazil’s National Insitute for
Space Research (INPE) runs such a service. In the package, the
connection is enabled by using the URL of the service. The package
informs if the connection has been correctly made.

``` r
# Connect to the WTSS server at INPE Brazil
wtss_inpe <-  wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")
```

    ## Connected to WTSS server at http://www.esensing.dpi.inpe.br/wtss/

## Listing coverages available at the WTSS server

This operation allows clients to retrieve the capabilities provided by
any server that implements WTSS. It returns a list of coverage names
available in a server instance.

``` r
# Connect to the WTSS server at INPE Brazil
wtss::list_coverages(wtss_inpe)
```

    ## Object of Class WTSS
    ## server-url:  http://www.esensing.dpi.inpe.br/wtss/ 
    ## Coverages: MOD13Q1 MOD13Q1_M

## Describing a coverage from the WTSS server

This operation returns the metadata for a given coverage identified by
its name. It includes its range in the spatial and temporal dimensions.

``` r
# Connect to the WTSS server at INPE Brazil
wtss::describe_coverage(wtss_inpe, name = "MOD13Q1")
```

    ## ----------------------------------------------------------------------------------
    ## WTSS server URL = http://www.esensing.dpi.inpe.br/wtss/
    ## Coverage = MOD13Q1
    ## 
    ## satellite  sensor  bands                                        
    ## ---------  ------  ---------------------------------------------
    ## TERRA      MODIS   c("mir", "blue", "nir", "red", "evi", "ndvi")
    ## 
    ## 
    ## |scale_factors                                                                    |
    ## |:--------------------------------------------------------------------------------|
    ## |c(mir = 1e-04, blue = 1e-04, nir = 1e-04, red = 1e-04, evi = 1e-04, ndvi = 1e-04)|
    ## 
    ## 
    ## |minimum_values                                                   |
    ## |:----------------------------------------------------------------|
    ## |c(mir = 0, blue = 0, nir = 0, red = 0, evi = -2000, ndvi = -2000)|
    ## 
    ## 
    ## |maximum_values                                                                   |
    ## |:--------------------------------------------------------------------------------|
    ## |c(mir = 10000, blue = 10000, nir = 10000, red = 10000, evi = 10000, ndvi = 10000)|
    ## 
    ## 
    ## nrows  ncols       xmin  xmax  ymin  ymax      xres      yres  crs                                
    ## -----  -----  ---------  ----  ----  ----  --------  --------  -----------------------------------
    ## 24000  24000  -81.23413   -30   -40    10  0.002087  0.002087  +proj=longlat +datum=WGS84 +no_defs
    ## 
    ## Timeline - 452 time steps
    ## start_date: 2000-02-18 end_date: 2019-09-30
    ## ----------------------------------------------------------------------------------

    ## Coverage description saved in WTSS object

The coverage description is saved as a tibble in the wtss object, to be
used whenever required.

``` r
# Coverage description available in the wtss object
wtss_inpe$description
```

    ## # A tibble: 1 x 19
    ##   URL   satellite sensor name  bands scale_factors missing_values
    ##   <chr> <chr>     <chr>  <chr> <lis> <list>        <list>        
    ## 1 http… TERRA     MODIS  MOD1… <chr… <dbl [6]>     <dbl [6]>     
    ## # … with 12 more variables: minimum_values <list>, maximum_values <list>,
    ## #   timeline <list>, nrows <dbl>, ncols <dbl>, xmin <dbl>, xmax <dbl>,
    ## #   ymin <dbl>, ymax <dbl>, xres <dbl>, yres <dbl>, crs <chr>

## Obtaining a time series

This operation requests the time series of values of a coverage
attribute at a given location. Its parameters are: (a) *wtss.obj*:
either a WTSS object (created by the operation wtss::WTSS as shown
above) or a valid WTSS server URL; (b) *name*: Coverage name; (c)
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
ts   <- wtss::time_series(wtss_inpe, name = "MOD13Q1", attributes = c("ndvi","evi"), 
                          longitude = -45.00, latitude  = -12.00,
                          start_date = "2000-02-18", end_date = "2016-12-18")

ts
```

    ## # A tibble: 1 x 6
    ##   longitude latitude start_date end_date   coverage time_series       
    ##       <dbl>    <dbl> <date>     <date>     <chr>    <list>            
    ## 1       -45      -12 2000-02-18 2016-12-18 MOD13Q1  <tibble [388 × 3]>

The result of the operation is a `tibble` which contains data and
metadata. The first six columns contain the metadata: satellite, sensor,
spatial and temporal information, and the coverage from where the data
has been extracted. The spatial location is given in longitude and
latitude coordinates for the “WGS84” ellipsoid. The `time_series` column
contains the time series data for each spatiotemporal location. This
data is also organized as a tibble, with a column with the dates and the
other columns with the values for each spectral band.

``` r
# Showing the contents of a time series
ts$time_series[[1]]
```

    ## # A tibble: 388 x 3
    ##    Index       ndvi   evi
    ##    <date>     <dbl> <dbl>
    ##  1 2000-02-18 0.374 0.302
    ##  2 2000-03-05 0.820 0.497
    ##  3 2000-03-21 0.802 0.481
    ##  4 2000-04-06 0.809 0.432
    ##  5 2000-04-22 0.749 0.409
    ##  6 2000-05-08 0.727 0.394
    ##  7 2000-05-24 0.698 0.374
    ##  8 2000-06-09 0.654 0.325
    ##  9 2000-06-25 0.608 0.310
    ## 10 2000-07-11 0.583 0.291
    ## # … with 378 more rows

## Plotting the time series

For convenience, the **WTSS** package provides a convenience funtion for
plotting the time series.

``` r
# Plotting the contents of a time series
plot(ts[1,])
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Conversion to “zoo” and “ts” formats

Since many time series analysis functions in R require data to be made
available in the “zoo” and “ts” formats, the *wtss* package provides two
convenience functions: *wtss\_to\_zoo* and “*wtss\_to\_ts*. The example
below shows the detection of trends in a series converted to the”ts"
format using the BFAST package \[@Verbesselt2010\].

``` r
library(bfast)
```

    ## Registered S3 method overwritten by 'xts':
    ##   method     from
    ##   as.zoo.xts zoo

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## Registered S3 methods overwritten by 'forecast':
    ##   method             from    
    ##   fitted.fracdiff    fracdiff
    ##   residuals.fracdiff fracdiff

``` r
# create a connection using a serverUrl
server <-  wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")
```

    ## Connected to WTSS server at http://www.esensing.dpi.inpe.br/wtss/

``` r
# get a time series for the "ndvi" attribute
ndvi_wtss <- wtss::time_series(server, "MOD13Q1", attributes = c("ndvi"), 
                         latitude = -10.408, longitude = -53.495, 
                         start = "2000-02-18", end = "2016-01-01")

# convert to ts
ndvi_ts <- wtss::wtss_to_ts(ndvi_wtss, band = "ndvi")

# detect trends
bf <- bfast::bfast01(ndvi_ts)
# plot the result
plot(bf)
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
