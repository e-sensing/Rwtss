# Example of the WTSS package
#
# Retrieve an MOD13Q1 NDVI time series for a location in Brazilian Amazonia
# 
# Apply the Changepoint package to dectect breaks
#

# installing and loading packages
library(changepoint)
library(Rwtss)

# choose a WTSS service
server <-  "https://brazildatacube.dpi.inpe.br/wtss/"

# get the list of coverages provided by the service
coverages <- Rwtss::list_coverages(server)

# get the description of the MOD13Q1 coverage
cv  <- Rwtss::describe_coverage(server, "MOD13Q1-6")

token <- Sys.getenv("BDC_ACCESS_KEY")

# Get a time seriesfor the "ndvi" attribute
# To create a token in BDC, please see: https://brazil-data-cube.github.io/applications/dc_explorer/token-module.html
ndvi <- Rwtss::time_series(
    URL = server, 
    name = "MOD13Q1-6", 
    attributes = "NDVI",
    latitude = -10.408, 
    longitude = -53.495,
    start_date = "2000-02-18", 
    end_date = "2016-01-01",
    token = "YOUR-BDC-TOKEN"
)

# transform time series to the TS format
ndvi_ts <- Rwtss::wtss_to_ts(ndvi)

# break point detection analysis 
cpt_meanvar = changepoint::cpt.meanvar(ndvi_ts)

# break point in a line plot
plot(cpt_meanvar)
