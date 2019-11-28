# Example of the WTSS package
#
# Retrieve an MOD13Q1 NDVI time series for a location in Brazilian Amazonia
# 
# Apply the BFAST package to detect the breaks
#

# installing and loading packages
library(bfast)
library(wtss)
library(zoo)

# create a connection using a serverUrl
server <-  wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")

# Get the list of coverages provided by the service
coverages <-  wtss::list_coverages(server)

# Get the description of the third coverage
cv <- wtss::describe_coverage(server,c("MOD13Q1"))

# get a time series for the "ndvi" attribute
ndvi_wtss <- wtss::time_series(server, "MOD13Q1", attributes = c("ndvi"), 
                         latitude = -10.408, longitude = -53.495, 
                         start = "2000-02-18", end = "2016-01-01")

# plot the time-series
plot(ndvi_wtss)

# transform time series to the TS format
ndvi_ts <- wtss::wtss_to_ts(ndvi_wtss)

# use BFAST01 for checking for one major break in the time series
one_break <-  bfast::bfast01(ndvi_ts)

# plot BFAST result
plot(one_break)

start_date <- lubridate::as_date("2008-02-18")

# time series in a ts object with part of the original values
ndvi_part_ts <- wtss::wtss_to_ts(ndvi_wtss, start_date = "2000-02-18", end_date = "2012-06-30")

# using bfastmonitor for monitoring disturbances in time series in near real-time
plot(bfast::bfastmonitor(ndvi_part_ts, start = lubridate::decimal_date("2000-02-18")))
