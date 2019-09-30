######################################
# Data Exploration 
######################################
library(openair)
library(ggplot2)
library(RODBC)
library(dplyr)
library(zoo)

# Preparing connection and driver for mysql queries
driver = "MySQL ODBC 8.0 Unicode Driver" 
db = "YOUR DB"
host = "127.0.0.1"
port = "3306"
user = "YOUR_USER"
pwd = "YOUR_PASSWORD"

conn = paste("DRIVER=",driver, ";Database=",db,";Server=",host, ";Port=",port,
             ";PROTOCOL=TCPIP",";UID=", user,";PWD=",pwd, sep="")
ODBCconn = odbcDriverConnect(conn)

sql = "select measure_date, code, site, no2, pm25, pm10, o3, ws, wd, temp  
      from ST_AURN_MEASURES_2013 
      union select measure_date, code, site, no2, pm25, pm10, o3, ws, wd, temp 
      from ST_AURN_MEASURES_2014 
      union select measure_date, code, site, no2, pm25, pm10, o3, ws, wd, temp   
      from ST_AURN_MEASURES_2015 
      union select measure_date, code, site, no2, pm25, pm10, o3, ws, wd, temp     
      from ST_AURN_MEASURES_2016
      union select measure_date, code, site, no2, pm25, pm10, o3, ws, wd, temp  
      from ST_AURN_MEASURES_2017
      union select measure_date, code, site, no2, pm25, pm10, o3, ws, wd, temp   
      from ST_AURN_MEASURES_2018"

sql = "select measure_date, code, site, no2, pm25, pm10, o3, ws, wd, temp   
      from ST_AURN_MEASURES_2019"
pollutants = sqlQuery(ODBCconn, sql)

head(pollutants)

# Renaming the measure_date column to date. It facilitates the use of openair 
# functions for AURN networks data.
names(pollutants)[names(pollutants) == "measure_date"] <- "date"

# Transforming the date values
pollutants$date = as.POSIXct(pollutants$date)

# Replacing negative values to zero
pollutants$no2[pollutants$no2 < 0] <- 0
pollutants$pm25[pollutants$pm25 < 0] <- 0
pollutants$pm10[pollutants$pm10 < 0] <- 0
pollutants$o3[pollutants$o3 < 0] <- 0

head(pollutants)

# Data overview
summary(pollutants)

# Dataset normalization
#norm_pollutants = normalize(pollutants[4:13], method="Z-score")

# Dataset normalization
#pollutants[4:13] = scale(pollutants[4:13])


# General view of the measures(mean)
png("images/No2General.png", width = 12 * 600, height = 5 * 600, res = 600)
timePlot(pollutants, pollutant = "no2",
         avg.time = "hour", normalise = "1/1/2013", lwd = 3, lty = 1,
         group = TRUE, ylim = c(0, 800), type = "site", 
         main = "General view (mean) of nitrogen dioxide between 2013-2018")
dev.off()
png("images/particulatesGeneral.png", width = 12 * 1200, height = 5 * 1200, res = 1200)
timePlot(pollutants, pollutant = c("pm10", "pm25"),
         avg.time = "year", normalise = "1/1/2013", lwd = 3, lty = 1,
         group = TRUE, ylim = c(0, 200), type = "site",
         main = "General view of particulates between 2013-2018")
dev.off()
png("images/ozoneGeneral.png", width = 12 * 1200, height = 5 * 1200, res = 1200)
timePlot(pollutants, pollutant = "o3",
         avg.time = "year", normalise = "1/1/2013", lwd = 3, lty = 1,
         group = TRUE, ylim = c(0, 400), type = "site",
         main = "General view of sodiun dioxide and ozone between 2013-2018")
dev.off()

summaryPlot(pollutants, pollutant="o3", main="Ozone data overview")
summaryPlot(pollutants, pollutant="no2", main="Nitrogen dioxide data overview")
summaryPlot(pollutants, pollutant="pm25", main="PM2.5 data overview")
summaryPlot(pollutants, pollutant="pm10", main="PM10 data overview")

# Trend for pollutants
png("images/nonDeseasonalisedTrend.png", width = 12 * 600, height = 5 * 600, res = 600)
smoothTrend(pollutants, pollutant = c("no2", "pm25", "pm10", "o3"), 
            deseason = TRUE, simulate =TRUE, ylab = "concentration (ppb)",
            main = "Monthly mean non-deseasonalised trend")
dev.off()
png("images/deseasonalisedTrend.png", width = 12 * 600, height = 5 * 600, res = 600)
smoothTrend(pollutants, pollutant = c("no2", "pm25", "pm10", "o3"), 
            deseason = FALSE, simulate =TRUE, ylab = "concentration (ppb)",
            main = "Monthly mean deseasonalised trend")
dev.off()

# Maxims levels of concentrations according to EEA (Year mean) for regulated 
# pollutants between 2013-2018
no2YearMean = aggregate(pollutants$no2, format(pollutants["date"],"%y"), mean,
                       na.rm = TRUE)
pm25YearMean = aggregate(pollutants$pm25, format(pollutants["date"],"%y"), mean,
                       na.rm = TRUE)
pm10YearMean = aggregate(pollutants$pm10, format(pollutants["date"],"%y"), mean,
                         na.rm = TRUE)

# No2 yearly mean between 2013-2018
names(no2YearMean)[names(no2YearMean) == "x"] <- "No2 yearly mean"
# PM2.5 yearly mean between 2013-2018
names(pm25YearMean)[names(pm25YearMean) == "x"] <- "PM2.5 yearly mean"
# PM10 yearly mean between 2013-2018
names(pm10YearMean)[names(pm10YearMean) == "x"] <- "PM10 yearly mean"

no2YearMean
pm25YearMean
pm10YearMean

#------------------------------------------------------------------------------
# Data exploration for O3
#------------------------------------------------------------------------------
# Maximun level concentrations for O3 per Hour

pollutantO3 = data.frame("date"=pollutants$date, "o3"=pollutants$o3)
ggplot(pollutantO3, aes(date, o3))+
   geom_point(color="forestgreen")+
   labs(x="Year", y="Ozone concentration (u/m3)")+
   scale_y_continuous(breaks = c(0,50,100,150,180,200)) +
   geom_hline(yintercept=180, size=1, color="darkred") +
   ggtitle("Hourly ozone concentrations between 2013 -2018")
theme_update(plot.title = element_text(hjust=0.5))

# Filtering by values over 180 u/m3
pollutantO3$o3[pollutantO3$o3 < 180] <- NA
pollutantO3 = na.omit(pollutantO3)

#number of values above 180 u/m3
nrow(pollutantO3)

plot(pollutantO3$date,pollutantO3$o3, type = "p", xlab = "Year",
     ylab = "Ozone concentration (ug/m3)", pch = 20,
     main="Hourly concentrations above 180 u/m3 between 2013 -2018", 
     col = "forestgreen")

# Max level concentratons for O3 each 8 hours
pollutants <- rollingMean(pollutants, pollutant = "o3", hours = 8, data.thresh = 75, na.rm=TRUE)
pollutant8O3 = data.frame("date"=pollutants$date, "x8h"=pollutants$rolling8o3)
pollutant8O3 = na.omit(pollutant8O3)

hist(pollutant8O3$x8h, main = "Histogram for ozone (8 hours)",
     xlab = "Ozone (ugm-3)", col="darkgreen", freq=FALSE)

ggplot(pollutant8O3, aes(date, x8h))+
   geom_point(color="darkolivegreen3")+
   labs(x="Year", y="Ozone concentrations (u/m3)")+
   scale_y_continuous(breaks = c(0,50,100,120,150,200)) +
   geom_hline(yintercept=120, size=1, color="darkred") +
   ggtitle("8 hours concentrations (u/m3) between 2013 -2018")
theme_update(plot.title = element_text(hjust=0.5))
   
pollutant8O3$x8h[pollutant8O3$x8h < 120] <- NA
pollutant8O3 = na.omit(pollutant8O3)

# Number of concentrations over limit (120 u/m3)
nrow(pollutant8O3)

# Plotting the O3 by 8 hours measures between 2013 - 2018 
plot(pollutant8O3$date,pollutant8O3$x8h, type = "p", xlab = "Year",
     ylab = "Ozone concentration (ug/m3)", pch = 5,
     main="8 hours concentrations above 120 u/m3 between 2013 -2018", 
     col = "darkolivegreen3")


# o3 over limit per year between 2013-2018
o3OverDay = pollutant8O3

#o3OverDay = distinct(o3OverDay, date, .keep_all= TRUE)
o3OverDay = aggregate(o3OverDay$x8h, format(o3OverDay["date"],"%Y"), FUN = length)
names(o3OverDay)[names(o3OverDay) == "x"] <- "o3"
head(pollutant8O3)


plot(o3OverDay$date,o3OverDay$o3, type = "b", xlab = "Year",
     ylab = "Number of days", pch = 15,
     main="Number of days above Ozone limits per year between 2013 -2018", 
     col = "darkolivegreen3")

head(pollutant8O3)
o3OverDay$date = as.POSIXct(o3OverDay$date)
d = data.frame("date" = as.POSIXct(o3OverDay$date), "o3"= o3OverDay$o3) 

 

o3Over = aggregate(d$o3,format(d["date"],"%Y"), FUN = length)
names(o3Over)[names(o3Over) == "x"] <- "O3 over limit"
View(o3Over)

plot(pollutant8O3$date,pollutant8O3$x8h, type = "p", xlab = "Year",
     ylab = "Number of days", pch = 15,
     main="Ozone 8 hours concentrations above limit 2013-2018", 
     col = "darkolivegreen3")

o3.breaks <-c(0, 66, 120, 160, 214, 500)
labels <- c("Low", "Moderate", "High", " Very High", "Harmful")

# second highest
png("images/o3-2013.png", width = 7 * 600, height = 4 * 600, res = 600)
calendarPlot(pollutant8O3, year = 2013, pollutant = "x8h", labels = labels,
             breaks = o3.breaks, statistic = "max", cols = c("lightblue","blue","orange","red","brown"), lim = 120,
             annotate = "value", main="Ozone 8 hours concentrations above limit in 2013")
dev.off()

# Lowest
png("images/o3-2014.png", width = 7 * 600, height = 4 * 600, res = 600)
calendarPlot(pollutant8O3, year = 2014, pollutant = "x8h", labels = labels,
             breaks = o3.breaks, statistic = "max", cols = c("lightblue","blue","orange","red","brown"), lim = 120,
             annotate = "value")
dev.off()
png("images/o3-2015.png", width = 7 * 600, height = 4 * 600, res = 600)
calendarPlot(pollutant8O3, year = 2015, pollutant = "x8h", labels = labels,
             breaks = o3.breaks, statistic = "max", cols = c("lightblue","blue","orange","red","brown"), lim = 120,
             annotate = "value", main="Ozone 8 hours concentrations above limit in 2015")
dev.off()
png("images/o3-2016.png", width = 7 * 600, height = 4 * 600, res = 600)
calendarPlot(pollutant8O3, year = 2016, pollutant = "x8h", labels = labels,
             breaks = o3.breaks, statistic = "max", cols = c("lightblue","blue","orange","red","brown"), lim = 120,
             annotate = "value", main="Ozone 8 hours concentrations above limit in 2016")
dev.off()
png("images/o3-2017.png", width = 7 * 600, height = 4 * 600, res = 600)
calendarPlot(pollutant8O3, year = 2017, pollutant = "x8h", labels = labels,
             breaks = o3.breaks, statistic = "max", cols = c("lightblue","blue","orange","red","brown"), lim = 120,
             annotate = "value", main="Ozone 8 hours concentrations above limit in 2017")
dev.off()

# Highest
png("images/o3-2018.png", width = 7 * 600, height = 4 * 600, res = 600)
calendarPlot(pollutant8O3, year = 2018, pollutant = "x8h", labels = labels,
             breaks = o3.breaks, statistic = "max", cols = c("lightblue","blue","orange","red","brown"), lim = 120,
             annotate = "value", main="Ozone 8 hours concentrations above limit in 2018")
dev.off()
# Variables corelation
corPlot(pollutants, dendrogram = TRUE)

# Trend N02 percentiles
smoothTrend(pollutants, pollutant = "no2", simulate = TRUE, ylab = "concentration (ppb)",
            main = "Monthly trend of NO2 by percentiles (5, 50, 95)", statistic = "percentile",
            percentile = c(5, 50, 95))

smoothTrend(pollutants, pollutant = "rolling8o3", simulate = TRUE, ylab = "concentration (ppb)",
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
# Data exploration for Nitrogen Dioxide (no2)
#--------------------------------------------
pollutant = pollutants[,c(1,4,8,9,10)]
head(pollutant)
# Data overview
summary(pollutant)
no2median = median(pollutant$no2)
pollutant$no2[is.na(pollutant$no2)] = no2median
#pollutant$no2 <- na.approx(pollutant$no2)

no2HourlyMean = aggregate(pollutant[-1], format(pollutant["date"],"%Y-%m-%d-%H"), mean,
                          na.rm = TRUE)
write.csv(no2HourlyMean,"/home/gloria/PycharmProjects/pollution/data/no2Hourly.csv")

no2DaylyMean = aggregate(pollutant[-1], format(pollutant["date"],"%Y-%m-%d"), mean,
                          na.rm = TRUE)
head(no2DaylyMean)
write.csv(no2DaylyMean,"/home/gloria/PycharmProjects/pollution/data/no2Dayly-Dataset.csv")

# Plotting the no2 yearly measures between 2013 - 2018 
png("images/no2Concentrations.png", width = 6 * 600, height = 5 * 600, res = 600)
plot(pollutants$date, pollutants$no2, type = "l", xlab = "Year",
     ylab = "Nitrogen dioxide (ug/m3)", col = "slateblue",
     main = "Nitrogen dioxide concentrations between 2013 - 2018")
dev.off()

pollutantNo2 = data.frame("date"=pollutants$date, "no2"=pollutants$no2)
png("images/no2HourlyConcentration.png", width = 6 * 600, height = 5 * 600, res = 600)
ggplot(pollutantNo2, aes(date, no2))+
   geom_point(color="slateblue1")+
   labs(x="Year", y="NO2 concentration (u/m3)")+
   scale_y_continuous(breaks = c(0, 75,150, 200, 400)) +
   geom_hline(yintercept=200, size=1, color="darkred") +
   ggtitle("Hourly nitrogen dioxide concentrations between 2013 -2018")
theme_update(plot.title = element_text(hjust=0.5))
dev.off()

# Keeping just the values over 200 u/m3
pollutantNo2$no2[pollutantNo2$no2 < 200] <- NA
pollutantNo2 = na.omit(pollutantNo2)
nrow(pollutantNo2)
png("images/no2Above.png", width = 6 * 600, height = 5 * 600, res = 600)
plot(pollutantNo2$date,pollutantNo2$no2, type = "p", xlab = "Year",
     ylab = "Nitrogen dioxide concentration (ug/m3)", pch = 20,
     main="Hourly concentrations above 200 u/m3 between 2013 -2018", 
     col = "slateblue3")
dev.off()

# Wide view of NO pollutant by station 
summaryPlot(pollutant)

hist(pollutants$no2, main = "Histogram for nitrogen dioxide - NO2",
     xlab = "Nitrogen dioxide NO2 (ugm-3)", col="deepskyblue4", freq=FALSE)

# NO2 over limit per year between 2013-2018
no2Over = aggregate(pollutantNo2$no2, format(pollutantNo2["date"],"%Y"), FUN = length)
names(no2Over)[names(no2Over) == "x"] <- "no2 over limit"

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
