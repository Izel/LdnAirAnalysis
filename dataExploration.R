######################################
# Data Exploration
######################################

library(openair)
library(RODBC)

# Preparing connection and driver for mysql queries
driver = "MySQL ODBC 8.0 Unicode Driver" 
db = "LDNAir"
host = "127.0.0.1"
port = "3306"
user = "gloria"
pwd = "Asly1130"

conn = paste("DRIVER=",driver, ";Database=",db,";Server=",host, ";Port=",port,
             ";PROTOCOL=TCPIP",";UID=", user,";PWD=",pwd, sep="")
ODBCconn = odbcDriverConnect(conn)

sql = "select measure_date, code, site, no, no2, nox, pm25, pm10, benzene 
      from ST_AURN_MEASURES_2013 
      union select measure_date, code, site, no, no2, nox, pm25, pm10, benzene  
      from ST_AURN_MEASURES_2014 
      union select measure_date, code, site, no, no2, nox, pm25, pm10, benzene  
      from ST_AURN_MEASURES_2015 
      union select measure_date, code, site, no, no2, nox, pm25, pm10, benzene   
      from ST_AURN_MEASURES_2016
      union select measure_date, code, site, no, no2, nox, pm25, pm10, benzene 
      from ST_AURN_MEASURES_2017
      union select measure_date, code, site, no, no2, nox, pm25, pm10, benzene  
      from ST_AURN_MEASURES_2018"
pollutants = sqlQuery(ODBCconn, sql)

# Renaming the measure_date column to date. It facilitates the use of openair 
# functions for AURN networks data.
names(pollutants)[names(pollutants) == "measure_date"] <- "date"

# Transforming the date values
pollutants$date = as.POSIXct(pollutants$date)

###
# Data exploration for Nitrogen Oxid (no)
pollutant = pollutants[1:4]
# Data overview
summary(pollutant)

# Plotting the no yearly measures between 2013 - 2018 
plot(pollutant$date, pollutants$nox, type = "l", xlab = "Year",
     ylab = "Nitrogen oxides (ppb)")

# Wide view of NO pollutant by station 
summaryPlot(pollutant)

# Calculating the mean value for the 2013-2018 period
mean(pollutant$no, na.rm = TRUE)

# Yearly mean for NO between 2013-2018
means = aggregate(pollutant$no, format(pollutant["date"],"%Y"), mean, 
                  na.rm = TRUE)
# Plotting the NO yearly mean measures between 2013-2018
plot(means$date, means$x, type = "l", ylab = "Concentration level (ppb)",
     xlab = "Year", main = "Yearly mean no (bootstrap uncertainties)")

# Yearly mean for NO between 2013-2018
means = aggregate(pollutant$no, format(pollutant["date"],"%m"), mean, 
                  na.rm = TRUE)

# Plotting the NO yearly mean measures between 2013-2018
#means$date = seq(from=1, by=1, length = nrow(means))
plot(means$x, type="l", xaxt = "n",  col = "cadetblue",  xlab = "Month",
     ylab = "Concentration level (ppb)", lwd = 2,
     main = "NO Monthly mean (bootstrap uncertainties) 2013-2018")

# adding tick marks at each month interval
months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
axis(1, at=1:12, labels=months)
abline(v = 1:12, col = "grey80")


pollutant = pollutants[1:4]
means = aggregate(pollutant$no, format(pollutant["date"],"%w-%H"), mean, na.rm=TRUE)
head(means)

plot(seq(1, nrow(means), 1), means$x, type="l", xaxt = "n", col = "cadetblue", lwd = 2,
     xlab = "day of week", ylab = "nitrogen oxid (ppb)",
     main = "Nitrogen oxid mean at London by day of the week between 2013-2018")
# add some tick marks at 24 hr intervals
axis(1, at = seq(1, nrow(means), 24), labels = FALSE)
# add some labels to x-axis
days = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
loc.days = seq(13, 157, 24) # location of labels on x-axis
# write text in margin
mtext(days, side = 1, line = 1, at = loc.days)
# add some grid lines
abline(v = seq(1, 169, 24), col = "grey85")


# Trend for NO
smoothTrend(pollutant, pollutant = "no", simulate = TRUE, ylab = "Concentration (ppb)",
            main = "Monthly mean nox (bootstrap uncertainties)")

smoothTrend(pollutant, pollutant ="no", type = "season", simulate = TRUE,
            ylab = "Concentration (ppb)", main = "Monthly mean nox (bootstrap uncertainties)")




# Montly means for NOx in 2013
means2013 = aggregate(pollutants2013["nox"], format(pollutants2013["date"],"%m"),
                      mean, na.rm = TRUE)
means2013$date = seq(from=1, by=1, length = nrow(means2013))
plot(means2013$date, means2013$nox, type = "l", ylab = "concentration (ppb)",
     ylab = "Year", main = "Yearly mean no (bootstrap uncertainties)")

# Trend for NOx
smoothTrend(pollutants, pollutant = "no", simulate = TRUE, ylab = "concentration (ppb)",
            main = "monthly mean nox (bootstrap uncertainties)")

# Trend of NOx deseasonalized
smoothTrend(pollutants2013, pollutant = "nox", deseason = TRUE, simulate =TRUE,
            ylab = "concentration (ppb)",
            main = "monthly mean deseasonalised no2 (bootstrap uncertainties)")

# Trend for Nox using the wind direction
smoothTrend(pollutants2013, pollutant = "nox", deseason = TRUE, type = "wd")

smoothTrend(pollutants2013, pollutant = c("no", "no2", "nox"), type = "season",
            date.breaks = 1, lty = 0)


#Exploration for Nitrogens
hist(pollutants2013$no, main = "Histogram of nitrogen oxide - NO",
     xlab = "Nitrogen oxide NO (ppb)", col="lightblue", freq=FALSE)

hist(pollutants2013$no2, main = "Histogram of nitrogen dioxide - NO2",
     xlab = "Nitrogen dioxide NO2 (ppb)", col="deepskyblue4", freq=FALSE)

hist(pollutants2013$no, main = "Histogram of nitrogen dioxide as  NOx",
     xlab = "Nitrogen oxide as NOx (ppb)", col="darkblue", freq=FALSE)

#Exploration for Particulates
hist(pollutants2013$pm10, main = "Histogram of Particulate PM10",
     xlab = "Particulated material PM10 (ppb)", col="khaki3", freq=FALSE)

hist(pollutants2013$pm10, main = "Histogram of Particulate PM25",
     xlab = "Particulated material PM25 (ppb)", col="khaki4", freq=FALSE)


hist(pollutants2013$`123tmb`, main = "Histogram of 123tmb",
     xlab = "Particulated material PM25 (ppb)", col="khaki4", freq=FALSE)
hist(pollutants2013$`135tmb`, main = "Histogram of 135tmb",
     xlab = "Particulated material PM25 (ppb)", col="khaki4", freq=FALSE)
hist(pollutants2013$`124tmb`, main = "Histogram of 124tmb",
     xlab = "Particulated material PM25 (ppb)", col="khaki4", freq=FALSE)

plot(pollutants2013$date, pollutants2013$`123tmb`, type = "l", xlab = "month",
     ylab = "123tmb")

smoothTrend(pollutants2013, pollutant = "123tmb", simulate = TRUE, ylab = "concentration (ppb)",
            main = "monthly mean 123tmb (bootstrap uncertainties)")
