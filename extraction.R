#!/usr/bin/env Rscript
###############################################################################
# Data Extraction
# Extracting data of Stations (Metadata) from AURN network and its measures
###############################################################################

library(openair)
library(stringr)
library(dplyr)

# Obtaining the stations in U.K
UKStations = importMeta(source = "AURN", all=TRUE)

# Obtaining the stations located in Greater London and save the info in a 
# csv file.
GLStations = subset(UKStations, grepl("London", UKStations$site))

write.csv(GLStations,paste("data/", "LdnMetaStations", ".csv", sep = "",
                           collapse = NULL))

# Obtaining the data from stations located in Greater London area since 2013 
# and 2019. For this project, data from 2013-2018 

stationsCode = distinct(GLStations)

keep <- c("date","no2", "o3", "pm2.5", "pm25", "pm10", "ws", "wd", "site", 
          "code")

ldnPollutants = importAURN(site=stationsCode$code, pollutant= "all", 
                             year=2013:2019, hc=TRUE, meta=FALSE, verbose=FALSE)
  
finalLdnPollutants = ldnPollutants[ , (names(ldnPollutants) %in% keep)]
head(finalLdnPollutants)
write.csv(finalLdnPollutants, paste("data/", "ldnAir_dataset", ".csv", 
                                    sep = "", collapse = NULL))









