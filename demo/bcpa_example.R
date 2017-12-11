# installing and loading packages
library(bcpa)
library(wtss)
library(zoo)
library(lubridate)

# create a connection using a serverUrl
server = WTSS("http://www.dpi.inpe.br/tws/wtss")

# get the list of coverages provided by the service
coverages = listCoverages(server)

# get the description of the third coverage
cv = describeCoverage(server, coverages[4])

# Get a time series
spatio_temporal = timeSeries(server, names(cv), attributes=cv[[names(cv)]]$attributes$name[1], latitude=-10.408, longitude=-53.495, start="2000-02-18", end="2016-01-01")

# time series in a ts object with part of the original values
time_series_ts1 = ts(zoo::coredata(spatio_temporal[[names(cv)]]$attributes[,1]), 
                     freq=365.25/(as.numeric(difftime(index(spatio_temporal[[names(cv)]]$attributes[2]),
                                                      index(spatio_temporal[[names(cv)]]$attributes[1]),units = "days"))), 
                     start=lubridate::decimal_date(lubridate::ymd(index(spatio_temporal[[names(cv)]]$attributes[1]))))

# plot the time series
plot(time_series_ts1)

# break point detection analysis 
get_bb <- bcpa::GetBestBreak(zoo::coredata(time_series_ts1),index(time_series_ts1), tau=FALSE)

# break point in a line plot
abline(v = get_bb[2], col=2)
