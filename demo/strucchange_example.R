# Example of the WTSS package
#
# Retrieve an MOD13Q1 NDVI time series for a location in Brazilian Amazonia
# 
# Apply the Breakout Detection library from Twitter
#

# installing and loading packages
library(strucchange)
library(wtss)

# create a connection using a serverUrl
server <-  wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")

# get the list of coverages provided by the service
coverages <- wtss::list_coverages(server)

# get the description of the MOD13Q1 coverage
cv <-  wtss::describe_coverage(server, "MOD13Q1")

# get a time series for the "ndvi" attribute
ndvi <- wtss::time_series(server, "MOD13Q1", attributes = c("ndvi"), 
                         latitude = -10.408, longitude = -53.495, 
                         start = "2000-02-18", end = "2016-01-01")

# plot the time series
plot(ndvi)

# transform time series to the TS format
ndvi_ts <- wtss::wtss_to_ts(ndvi)

# analysis using strucchange
fs.ndvi <- strucchange::Fstats(ndvi_ts ~ 1)

# plot the F statistics
plot(fs.ndvi)

# breakpoints
bk <- strucchange::breakpoints(fs.ndvi)

# plot breakpoints
lines(bk)

