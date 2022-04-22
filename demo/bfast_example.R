# Example of the WTSS package
#
# Retrieve an MOD13Q1 NDVI time series for a location in Brazilian Amazonia
# 
# Apply the BFAST package to detect the breaks
#

# installing and loading packages
library(bfast)
library(Rwtss)

# choose a WTSS server
server <-  "https://brazildatacube.dpi.inpe.br/wtss/"

# Get the list of coverages provided by the service
coverages <-  Rwtss::list_coverages(server)

# Get the description of the third coverage
cv <- Rwtss::describe_coverage(server, "MOD13Q1-6")

# get a time series for the "ndvi" attribute
# To create a token in BDC, please see: https://brazil-data-cube.github.io/applications/dc_explorer/token-module.html
ndvi_wtss <- Rwtss::time_series(
    URL = server, 
    name = "MOD13Q1-6", 
    attributes = "NDVI",
    latitude = -10.408, 
    longitude = -53.495,
    token = "YOUR-BDC-TOKEN"
)

# plot the time-series
plot(ndvi_wtss)

# transform time series to the TS format
ndvi_ts <- Rwtss::wtss_to_ts(ndvi_wtss)

# use BFAST01 for checking for one major break in the time series
one_break <-  bfast::bfast01(ndvi_ts)

# plot BFAST result
plot(one_break)
