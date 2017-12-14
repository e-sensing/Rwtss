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
server <-  wtss::WTSS("http://www.dpi.inpe.br/tws/wtss")

# Get the list of coverages provided by the service
coverages <-  wtss::listCoverages(server)

# Get the description of the third coverage
cv <- wtss::describeCoverage(server,c("mod13q1_512"))

# get a time series for the "ndvi" attribute
ndvi <- wtss::timeSeries(server, "mod13q1_512", attributes=c("ndvi"), 
                         latitude=-10.408, longitude=-53.495, 
                         start="2000-02-18", end="2016-01-01")

# plot the time-series
plot(ndvi$mod13q1_512$attributes[,1])

# transform time series to the TS format
interval <- as.numeric(difftime(zoo::index(ndvi$mod13q1_512$attributes[2]),index(ndvi$mod13q1_512$attributes[1]),units = "days"))
start_date <- lubridate::decimal_date(lubridate::ymd(index(ndvi$mod13q1_512$attributes[1])))

ndvi_ts <- ts(zoo::coredata(ndvi$mod13q1_512$attributes[,"ndvi"]), freq=365.25/interval, start= start_date)

# use BFAST for checking for one major break in the time series
breaks_ts = bfast::bfast01(ndvi_ts)

# plot BFAST result
plot(breaks_ts)

# time series in a ts object with part of the original values
ndvi_part = ts(zoo::coredata(ndvi$mod13q1_512$attributes[,1])[1:270],freq=365.25/interval, start= start_date)

# using bfastmonitor for monitoring disturbances in time series in near real-time
plot(bfast::bfastmonitor(ndvi_part, start=time(ndvi_part)[228], history=time(ndvi_part)[1]))
