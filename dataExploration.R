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

sql = "select measure_date, code, site, no, no2, nox, pm25, pm10, so2, o3, ws, wd, temp  
      from ST_AURN_MEASURES_2013 
      union select measure_date, code, site, no, no2, nox, pm25, pm10, so2, o3, ws, wd, temp 
      from ST_AURN_MEASURES_2014 
      union select measure_date, code, site, no, no2, nox, pm25, pm10, so2, o3, ws, wd, temp   
      from ST_AURN_MEASURES_2015 
      union select measure_date, code, site, no, no2, nox, pm25, pm10, so2, o3, ws, wd, temp     
      from ST_AURN_MEASURES_2016
      union select measure_date, code, site, no, no2, nox, pm25, pm10, so2, o3, ws, wd, temp  
      from ST_AURN_MEASURES_2017
      union select measure_date, code, site, no, no2, nox, pm25, pm10, so2, o3, ws, wd, temp   
      from ST_AURN_MEASURES_2018"
pollutants = sqlQuery(ODBCconn, sql)

head(pollutants)

# Renaming the measure_date column to date. It facilitates the use of openair 
# functions for AURN networks data.
names(pollutants)[names(pollutants) == "measure_date"] <- "date"

# Transforming the date values
pollutants$date = as.POSIXct(pollutants$date)

# General view of the measures(mean)
png("images/NoxGeneral.png", width = 12 * 600, height = 5 * 600, res = 600)
timePlot(pollutants, pollutant = c("no", "no2", "nox"),
         avg.time = "year", normalise = "1/1/2013", lwd = 3, lty = 1,
         group = TRUE, ylim = c(0, 800), type = "site", 
         main = "General view (mean) of nitrogens between 2013-2018")
dev.off()
png("images/particulatesGeneral.png", width = 12 * 1200, height = 5 * 1200, res = 1200)
timePlot(pollutants, pollutant = c("pm10", "pm25"),
         avg.time = "year", normalise = "1/1/2013", lwd = 3, lty = 1,
         group = TRUE, ylim = c(0, 200), type = "site",
         main = "General view of particulates between 2013-2018")
dev.off()

# Trend for pollutants
smoothTrend(pollutants, pollutant = c("no2", "pm25", "pm10", "so2", "o3"), 
            deseason = TRUE, simulate =TRUE, ylab = "concentration (ppb)",
            main = "Monthly mean non-deseasonalised trend")

smoothTrend(pollutants, pollutant = c("no2", "pm25", "pm10", "so2", "o3"), 
            deseason = FALSE, simulate =TRUE, ylab = "concentration (ppb)",
            main = "Monthly mean deseasonalised trend")
head(pollutants)
# Variables relation
corPlot(pollutants, dendrogram = TRUE)

# Trend N02 percentiles
smoothTrend(pollutants, pollutant = "no2", simulate = TRUE, ylab = "concentration (ppb)",
            main = "Monthly trend of NO2 by percentiles (5, 50, 95)", statistic = "percentile",
            percentile = c(5, 50, 95))
smoothTrend(pollutants, pollutant = "o3", simulate = TRUE, ylab = "concentration (ppb)",
            main = "Monthly trend of O3 by percentiles (5, 50, 95)", statistic = "percentile",
            deseason = TRUE, percentile = c(5, 50, 95))

# Trend for No2, temperature, wd and ws
smoothTrend(pollutants, pollutant = "wd", 
            deseason = TRUE, simulate =TRUE, ylab = "concentration (ppb)",
            main = "Monthly mean non-deseasonalised trend")

smoothTrend(pollutants, pollutant = c("no2", "ws", "temp"),
            deseason = TRUE, simulate =TRUE, ylab = "concentration (ppb)",
            main = "Monthly mean non-deseasonalised trend")
#--------------------------------------------
# Data exploration for Nitrogen Oxid (no)
#--------------------------------------------
pollutant = pollutants[1:4]
# Data overview
summary(pollutants)

# Plotting the no yearly measures between 2013 - 2018 
plot(pollutants$date, pollutants$no, type = "l", xlab = "Year",
     ylab = "Nitrogen oxides (ppb)")

# Wide view of NO pollutant by station 
summaryPlot(pollutant)

# Yearly mean for NO between 2013-2018
means = aggregate(pollutant$no, format(pollutant["date"],"%Y"), mean, 
                  na.rm = TRUE)

# Plotting the NO yearly mean measures between 2013-2018
plot(means$date, means$x, type = "l", ylab = "Concentration level (u/m3)", lwd = 2,
     xlab = "Year", main = "Yearly mean no (bootstrap uncertainties)", col = "cadetblue")

# Maxims
maxs = aggregate(pollutant$no, format(pollutant["date"],"%Y"), max, 
                  na.rm = TRUE)
# Plotting the NO yearly mean measures between 2013-2018
plot(maxs$date, maxs$x, type = "l", ylab = "Concentration level (u/m3)", lwd = 2,
     xlab = "Year", main = "Yearly maxims for NO (bootstrap uncertainties)", 
     col = "cadetblue")

# Monthly mean for NO between 2013-2018
means = aggregate(pollutant$no, format(pollutant["date"],"%m"), mean, 
                  na.rm = TRUE)

# Plotting the NO yearly mean measures between 2013-2018
plot(means$x, type="l", xaxt = "n",  col = "cadetblue",  xlab = "Month",
     ylab = "Concentration level (ppb)", lwd = 2,
     main = "NO Monthly mean (bootstrap uncertainties) 2013-2018")

# Adding tick marks at each month interval
months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
axis(1, at=1:12, labels=months)
abline(v = 1:12, col = "grey80")

# Day of the week
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

# adding grid lines
abline(v = seq(1, 169, 24), col = "grey85")

# Daily mean  of no in 2013
calendarPlot(pollutant, pollutant="no", year = 2013, annotate = "value", 
             lim =50, col.lim = c("black", "red"), layout = c(4, 3), 
             breaks = c(0, 50, 100, 150, 1000), cols = "increment", 
             labels = c("Very low", "Low", "High", "Very High"),
             statistic = "max")

#--------------------------------------------
# Data exploration for Nitrogen Dioxide (no2)
#--------------------------------------------
head(pollutants)
pollutant = pollutants[,c(1,2,3,5,11,12,13)]
# Data overview
summary(pollutant)

# Plotting the no2 yearly measures between 2013 - 2018 
plot(pollutants$date, pollutants$no2, type = "l", xlab = "Year",
     ylab = "Nitrogen dioxide (ug/m3)", col = "blue",
     main = "Nitrogen dioxide measures between 2013 - 2018")

# Wide view of NO pollutant by station 
summaryPlot(pollutant)

hist(pollutants$no2, main = "Histogram for nitrogen dioxide - NO2",
     xlab = "Nitrogen dioxide NO2 (ugm-3)", col="deepskyblue4", freq=FALSE)

# Yearly mean for NO2 between 2013-2018
means = aggregate(pollutant$no2, format(pollutant["date"],"%Y"), mean, 
                  na.rm = TRUE)
# Plotting the NO2 yearly mean measures between 2013-2018
plot(means$date, means$x, type = "l", ylab = "Concentration level (ppb)", lwd = 2,
     xlab = "Year", main = "Yearly mean for nitrogen dioxide", col = "blue")

# Yearly maxims for NO2 between 2013-2018
maxs = aggregate(pollutant$no2, format(pollutant["date"],"%Y"), max, 
                  na.rm = TRUE)

# Plotting the NO2 yearly mean measures between 2013-2018
plot(maxs$date, maxs$x, type = "l", ylab = "Concentration level (ppb)", lwd = 2,
     xlab = "Year", main = "Yearly mean for nitrogen dioxide no2 (u/m3)", col = "blue")

# Monthly mean for NO2 between 2013-2018
means = aggregate(pollutant$no, format(pollutant["date"],"%Y-%m"), mean, 
                  na.rm = TRUE)

# Plotting the NO yearly mean measures between 2013-2018
plot(means$x, type="l", xaxt = "n",  col = "blue",  xlab = "Month",
     ylab = "Concentration level (ppb)", lwd = 2,
     main = "Monthly mean for NO2 (u/m3) between 2013-2018")

# adding tick marks at each month interval
months = means$date
axis(1, at=1:nrow(means), labels=months)
abline(v = 1:nrow(means), col = "grey80")

# Daily maxims of NO2 in 2013
calendarPlot(pollutant, pollutant="no2", year = 2013, breaks = c(0, 200, 400, 600, 1000),
             annotate = "value", lim = 200, col.lim = c("black", "red"), cols = "Purples",
             layout = c(4,3), statistic = "max",
             main="Daily maximum concentration of NO2 (u/m3) in 2013") 

calendarPlot(pollutant, pollutant="no2", year = 2014, breaks = c(0, 200, 400, 600, 1000),
             annotate = "value", lim = 400, col.lim = c("black", "red"), cols = "Purples",
             layout = c(4,3), statistic = "max",
             main="Daily maximum concentration of NO2 (u/m3) in 2014")

calendarPlot(pollutant, pollutant="no2", year = 2015, breaks = c(0, 200, 400, 600, 1000),
             annotate = "value", lim = 400, col.lim = c("black", "red"), cols = "Purples",
             layout = c(4,3), statistic = "max",
             main="Daily maximum concentration of NO2 (u/m3) in 2015") 

calendarPlot(pollutant, pollutant="no2", year = 2016, breaks = c(0, 200, 400, 600, 1000),
             annotate = "value", lim = 400, col.lim = c("black", "red"), cols = "Purples",
             layout = c(4,3), statistic = "max",
             main="Daily maximum concentration of NO2 (u/m3) in 2016")

calendarPlot(pollutant, pollutant="no2", year = 2017, breaks = c(0, 200, 400, 600, 1000),
             annotate = "value", lim = 400, col.lim = c("black", "red"), cols = "Purples",
             layout = c(4,3), statistic = "max",
             main="Daily maximum concentration of NO2 (u/m3) in 2017")

calendarPlot(pollutant, pollutant="no2", year = 2018, breaks = c(0, 200, 400, 600, 1000),
             annotate = "value", lim = 400, col.lim = c("black", "red"), cols = "Purples",
             layout = c(4,3), statistic = "max",
             main="Daily maximum concentration of NO2 (u/m3) in 2018")

plot(as.factor(format(pollutant$date, "%Y-%m")), pollutant$no, col = "cadetblue")




smoothTrend(pollutants, pollutant = "no2", simulate = TRUE, ylab = "Concentration (ppb)",
            main = "Monthly mean no2 (bootstrap uncertainties)")

smoothTrend(pollutant, pollutant ="no2", type = "season", simulate = TRUE,
            ylab = "Concentration (ppb)", main = "Monthly mean no2 (bootstrap uncertainties)")



#--------------------------------------------
# Data exploration for PM2.5
#--------------------------------------------
pollutant = pollutants[,c(1,2,3,7)]
# Data overview
summary(pollutant)

# Plotting the no yearly measures between 2013 - 2018 
plot(pollutants$date, pollutants$pm25, type = "l", xlab = "Year",
     ylab = "Nitrogen dioxides (ppb)", col = "blue")

# Wide view of PM2.5 pollutant by station 
summaryPlot(pollutant)

# Yearly mean for PM2.5 between 2013-2018
means = aggregate(pollutant$pm25, format(pollutant["date"],"%Y"), mean, 
                  na.rm = TRUE)
# Plotting the NO2 yearly mean measures between 2013-2018
plot(means$date, means$x, type = "l", ylab = "Concentration level (ppb)", lwd = 2,
     xlab = "Year", main = "Yearly mean for PM2.5 (u/m3)", col = "darkred")

# Monthly mean for NO2 between 2013-2018
means = aggregate(pollutant$no, format(pollutant["date"],"%Y-%m"), mean, 
                  na.rm = TRUE)

# Plotting the NO yearly mean measures between 2013-2018
plot(means$x, type="l", xaxt = "n",  col = "darkred",  xlab = "Month",
     ylab = "Concentration level (ppb)", lwd = 2,
     main = "Monthly mean for NO2 (u/m3) between 2013-2018")

# adding tick marks at each month interval
months = means$date
axis(1, at=1:nrow(means), labels=months)
abline(v = 1:nrow(means), col = "grey80")

# Daily maxims of PM2.5 in 2013
calendarPlot(pollutant, pollutant="pm25", year = 2013, 
             breaks = cc(0, 12, 24, 35, 42, 47, 53, 59, 65, 70, 1000),
             annotate = "value", lim = 54, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM2.5 (u/m3) in 2013") 

# Daily maxims of PM2.5 in 2014
calendarPlot(pollutant, pollutant="pm25", year = 2014, 
             breaks = cc(0, 12, 24, 35, 42, 47, 53, 59, 65, 70, 1000),
             annotate = "value", lim = 54, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM2.5 (u/m3) in 2014") 

# Daily maxims of PM2.5 in 2015
calendarPlot(pollutant, pollutant="pm25", year = 2015, 
             breaks = cc(0, 12, 24, 35, 42, 47, 53, 59, 65, 70, 1000),
             annotate = "value", lim = 54, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM2.5 (u/m3) in 2015") 

# Daily maxims of PM2.5 in 2016
calendarPlot(pollutant, pollutant="pm25", year = 2016, 
             breaks = cc(0, 12, 24, 35, 42, 47, 53, 59, 65, 70, 1000),
             annotate = "value", lim = 54, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM2.5 (u/m3) in 2016") 

# Daily maxims of PM2.5 in 2017
calendarPlot(pollutant, pollutant="pm25", year = 2017, 
             breaks = cc(0, 12, 24, 35, 42, 47, 53, 59, 65, 70, 1000),
             annotate = "value", lim = 54, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM2.5 (u/m3) in 2017") 

# Daily maxims of PM2.5 in 2018
calendarPlot(pollutant, pollutant="pm25", year = 2018, 
             breaks = cc(0, 12, 24, 35, 42, 47, 53, 59, 65, 70, 1000),
             annotate = "value", lim = 54, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM2.5 (u/m3) in 2018") 



#--------------------------------------------
# Data exploration for PM10
#--------------------------------------------
pollutant = pollutants[,c(1,2,3,8)]
head(pollutant)
# Data overview
summary(pollutant)

# Plotting the no yearly measures between 2013 - 2018 
plot(pollutants$date, pollutants$pm10, type = "l", xlab = "Year",
     ylab = "concentratons (ug/m3)", main=" between 2013 -2018", col = "orange")

# Wide view of PM10 pollutant by station 
summaryPlot(pollutant)

# Yearly mean for PM10 between 2013-2018
means = aggregate(pollutant$pm10, format(pollutant["date"],"%Y"), mean, 
                  na.rm = TRUE)
# Plotting the PM10 yearly mean measures between 2013-2018
plot(means$date, means$x, type = "l", ylab = "Concentration level (ppb)", lwd = 2,
     xlab = "Year", main = "Yearly mean for PM10 (u/m3)", col = "orange")

# Monthly mean for PM10 between 2013-2018
means = aggregate(pollutant$pm10, format(pollutant["date"],"%Y-%m"), mean, 
                  na.rm = TRUE)

# Plotting the PM10 yearly mean measures between 2013-2018
plot(means$x, type="l", xaxt = "n",  col = "orange",  xlab = "Month",
     ylab = "Concentration level (ppb)", lwd = 2,
     main = "Monthly mean for PM10 (ug/m3) between 2013-2018")

# adding tick marks at each month interval
months = means$date
axis(1, at=1:nrow(means), labels=months)
abline(v = 1:nrow(means), col = "grey80")

# Daily maxims of PM2.5 in 2013
calendarPlot(pollutant, pollutant="pm10", year = 2013, 
             breaks = c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000),
             annotate = "value", lim = 67, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM10 (ug/m3) in 2013") 

# Daily maxims of PM10 in 2014
calendarPlot(pollutant, pollutant="pm10", year = 2014, 
             breaks = c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000),
             annotate = "value", lim = 67, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM10 (u/m3) in 2014") 

# Daily maxims of PM2.5 in 2015
calendarPlot(pollutant, pollutant="pm10", year = 2015, 
             breaks = c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000),
             annotate = "value", lim = 67, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM10 (u/m3) in 2015") 

# Daily maxims of PM2.5 in 2016
calendarPlot(pollutant, pollutant="pm10", year = 2016, 
             breaks = c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000),
             annotate = "value", lim = 67, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM10 (u/m3) in 2016") 

# Daily maxims of PM2.5 in 2017
calendarPlot(pollutant, pollutant="pm10", year = 2017, 
             breaks = c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000),
             annotate = "value", lim = 67, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM10 (u/m3) in 2017") 

# Daily maxims of PM10 in 2018
calendarPlot(pollutant, pollutant="pm10", year = 2018, 
             breaks = c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000),
             annotate = "value", lim = 67, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM10 (u/m3) in 2018") 

#--------------------------------------------
# Data exploration for O3
#--------------------------------------------
pollutant = pollutants[,c(1,2,3,10)]
head(pollutant)
# Data overview
summary(pollutant)

# Plotting the so3 yearly measures between 2013 - 2018 
plot(pollutants$date, pollutants$o3, type = "l", xlab = "Year",
     ylab = "concentratons (ug/m3)", main=" between 2013 -2018", col = "darkgreen")

# Wide view of PM10 pollutant by station 
summaryPlot(pollutant)

hist(pollutants$no, main = "Histogram of O3",
     xlab = "Ozone ug/m3", col="darkgreen", freq=FALSE)

pollutant <- rollingMean(pollutant, pollutant = "o3", hours = 8, data.thresh = 75, na.rm=TRUE)
pollutant = na.omit(pollutant)
head(pollutant)
o3.breaks <-c(0, 34, 66, 100, 121, 141, 160, 188, 214, 240, 500)
labels <- c("1 - Low", "2 - Low", "3 - Low", "4 - Moderate", "5 - Moderate",
            "6 - Moderate", "7 - High", "8 - High", "9 - High", "10 - Very High")

calendarPlot(pollutant, year = 2013, pollutant = "rolling8o3", labels = labels,
             breaks = o3.breaks, statistic = "max", cols = "jet", lim = 141,
             annotate = "value")

pollutantLimit = subset(pollutant, pollutant["rolling8o3"] >= 141) 
aggregate(pollutantLimit["rolling8o3"], format(pollutantLimit["date"],"%Y-%m-%d"), 
          mean, na.rm = TRUE)
# Plotting the o3 between 2013 - 2018 
png("images/numO3Warnings.png", width = 4 * 400, height = 4.5 * 300, res = 300)
plot(pollutantLimit$date, pollutantLimit$rolling8o3, type = "p", xlab = "Year",
     ylab = "concentrations (ug/m3)", main=" Number of o3 warning alarms between 2013-2018", col = "darkgreen")
dev.off()










# Yearly mean for PM10 between 2013-2018
means = aggregate(pollutant["rolling8o3"], format(pollutant["date"],"%Y-%m-%d"), 
                  mean, na.rm = TRUE)
# Plotting the PM10 yearly mean measures between 2013-2018
plot(means$date, means$x, type = "l", ylab = "Concentration level (ppb)", lwd = 2,
     xlab = "Year", main = "Yearly mean for SO3 (u/m3)", col = "orange")

# Monthly mean for PM10 between 2013-2018
means = aggregate(pollutant$pm10, format(pollutant["date"],"%Y-%m"), mean, 
                  na.rm = TRUE)

# Plotting the PM10 yearly mean measures between 2013-2018
plot(means$x, type="l", xaxt = "n",  col = "orange",  xlab = "Month",
     ylab = "Concentration level (ppb)", lwd = 2,
     main = "Monthly mean for PM10 (ug/m3) between 2013-2018")

# adding tick marks at each month interval
months = means$date
axis(1, at=1:nrow(means), labels=months)
abline(v = 1:nrow(means), col = "grey80")

# Daily maxims of PM2.5 in 2013
calendarPlot(pollutant, pollutant="pm10", year = 2013, 
             breaks = c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000),
             annotate = "value", lim = 67, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM10 (ug/m3) in 2013") 

# Daily maxims of PM10 in 2014
calendarPlot(pollutant, pollutant="pm10", year = 2014, 
             breaks = c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000),
             annotate = "value", lim = 67, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM10 (u/m3) in 2014") 

# Daily maxims of PM2.5 in 2015
calendarPlot(pollutant, pollutant="pm10", year = 2015, 
             breaks = c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000),
             annotate = "value", lim = 67, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM10 (u/m3) in 2015") 

# Daily maxims of PM2.5 in 2016
calendarPlot(pollutant, pollutant="pm10", year = 2016, 
             breaks = c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000),
             annotate = "value", lim = 67, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM10 (u/m3) in 2016") 

# Daily maxims of PM2.5 in 2017
calendarPlot(pollutant, pollutant="pm10", year = 2017, 
             breaks = c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000),
             annotate = "value", lim = 67, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM10 (u/m3) in 2017") 

# Daily maxims of PM10 in 2018
calendarPlot(pollutant, pollutant="pm10", year = 2018, 
             breaks = c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000),
             annotate = "value", lim = 67, col.lim = c("black", "red"), 
             cols = "increment", layout = c(4,3), statistic = "mean",
             main="Daily mean concentration of PM10 (u/m3) in 2018") 















# Montly means for NOx in 2013
means2013 = aggregate(pollutants2013["nox"], format(pollutants2013["date"],"%m"),
                      mean, na.rm = TRUE)
means2013$date = seq(from=1, by=1, length = nrow(means2013))
plot(means2013$date, means2013$nox, type = "l", ylab = "concentration (ppb)",
     ylab = "Year", main = "Yearly mean no (bootstrap uncertainties)")



# Trend of NOx deseasonalized
smoothTrend(pollutants, pollutant = c("no2", "pm25", "pm10"), deseason = TRUE, simulate =TRUE,
            ylab = "concentration (ppb)",
            main = "monthly mean deseasonalised no2 (bootstrap uncertainties)")

# Trend for Nox using the wind direction
smoothTrend(pollutants2013, pollutant = "nox", deseason = TRUE, type = "wd")

smoothTrend(pollutants2013, pollutant = c("no", "no2", "nox"), type = "season",
            date.breaks = 1, lty = 0)



#Exploration for Nitrogens
hist(pollutants2013$no, main = "Histogram of nitrogen oxide - NO",
     xlab = "Nitrogen oxide NO (ppb)", col="lightblue", freq=FALSE)

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


