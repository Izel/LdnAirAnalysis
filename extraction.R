###############################################################################
# Data Extraction
# Extracting data of Stations (Metadata) from AURN network and its measures
###############################################################################

library(openair)
library(stringr)

# Obtaining the stations in U.K
UKStations = importMeta(source = "AURN", all=TRUE)

# Obtaining the stations located in Greater London and save the info in a 
# csv file.
GLStations = subset(UKStations, zone_id == "A01")
write.csv(GLStations,paste("data/", "LdnMetaStations", ".csv", sep = "",
                           collapse = NULL))

# Obtaining the data from stations located in Greater London area since 2013 
# and 2019. For this project, data from 2013-2018 will be used to train, test
# and validate the model. Data for 2019 will be used to predictions after the
# model construction
for(i in 2013:2019){
  ldnPollutants = importAURN(site= stationsCode$code, pollutant= "all", 
                             year=i, hc=TRUE, meta=FALSE, verbose=FALSE)
  write.csv(ldnPollutants,paste("data/", i, ".csv", sep = "", 
                                collapse = NULL))
}




