# Example of the WTSS package
#
# Retrieve an MOD13Q1 NDVI time series for a location in Brazilian Amazonia
# 
# Apply the Changepoint package to dectect breaks
#

# installing and loading packages
library(changepoint)
library(wtss)

# create a connection using a serverUrl
server <-  wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")

# get the list of coverages provided by the service
coverages <- wtss::list_coverages(server)

# get the description of the MOD13Q1 coverage
cv  <- describe_coverage(server, "MOD13Q1")

# Get a time seriesfor the "ndvi" attribute
ndvi <- wtss::time_series(server, "MOD13Q1", attributes=c("ndvi"), 
                         latitude=-10.408, longitude=-53.495, 
                         start="2000-02-18", end="2016-01-01")

# transform time series to the TS format
ndvi_ts <- wtss::wtss_to_ts(ndvi)

# break point detection analysis 
cpt_meanvar = changepoint::cpt.meanvar(ndvi_ts)

# break point in a line plot
plot(cpt_meanvar)
