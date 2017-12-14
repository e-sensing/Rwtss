# Example of the WTSS package
#
# Retrieve an MOD13Q1 NDVI time series for a location in Brazilian Amazonia
# 
# Apply the Breakout Detection library from Twitter
#

# installing and loading packages
library(strucchange)
library(zoo)
library(lubridate)
library(wtss)

# create a connection using a serverUrl
server <-  wtss::WTSS("http://www.dpi.inpe.br/tws/wtss")

# get the list of coverages provided by the service
coverages <- wtss::listCoverages(server)

# get the description of the MOD13Q1 coverage
cv <-  wtss::describeCoverage(server, "mod13q1_512")

# get a time series for the "ndvi" attribute
ndvi <- wtss::timeSeries(server, "mod13q1_512", attributes=c("ndvi"), 
                         latitude=-10.408, longitude=-53.495, 
                         start="2000-02-18", end="2016-01-01")

# transform time series to the TS format
interval <- as.numeric(difftime(zoo::index(ndvi$mod13q1_512$attributes[2]),index(ndvi$mod13q1_512$attributes[1]),units = "days"))
start_date <- lubridate::decimal_date(lubridate::ymd(index(ndvi$mod13q1_512$attributes[1])))

ndvi_ts <- ts(zoo::coredata(ndvi$mod13q1_512$attributes[,"ndvi"]), freq=365.25/interval, start= start_date)

# analysis using strucchange
fs.ndvi <- strucchange::Fstats(ndvi_ts ~ 1)

# plot the F statistics
plot(fs.ndvi)

# breakpoints
bk <- strucchange::breakpoints(fs.ndvi)

# plot breakpoints
lines(bk)
