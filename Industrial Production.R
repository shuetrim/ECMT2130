#-------------------------------------------------------------------------------------
# ECMT2130 Financial Econometrics
# Written by Geoff Shuetrim
# Industrial Production: Unit root testing
#
#-------------------------------------------------------------------------------------

library(readxl)
library(xts)
library(urca)

data <- read_excel("industrial_production.xlsx")
dates <- as.Date(data$date)
ip <- xts(data[,2], order.by=dates)

long_timespan <- '19800101/20191231'

short_timespan <- '20000101/20191231'

plot(ip[long_timespan])
plot(ip[short_timespan])

log_ip = log(ip)
plot(log_ip[long_timespan])
plot(log_ip[short_timespan])


log_ip_1d = diff(log_ip, differences = 1)
plot(log_ip_1d[long_timespan])
plot(log_ip_1d[short_timespan])

# Generic augmented D-F test command
help("ur.df")
summary(ur.df(log_ip[short_timespan], type = c("none"), lags=4, selectlags = c("Fixed")))
summary(ur.df(log_ip[short_timespan], type = c("drift"), lags=4, selectlags = c("Fixed")))
summary(ur.df(log_ip[short_timespan], type = c("trend"), lags=4, selectlags = c("Fixed")))

help(kpss.test)
kpss.test(log_ip[short_timespan], null = c("Level"))
kpss.test(log_ip[short_timespan], null = c("Trend"))
