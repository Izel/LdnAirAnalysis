# LDN Air Analysis
This analysis shows the air quality in greather London area. An **average** measure of each pollutanat were calculated in order to measure their impact in the air quality.  Currently, The UK has a lot AURN statiosn to detect pollutant matrials and report their concentrations, but this analysis will include all  AURN networks located in the Great London area, wich includes 39 stations that monitor 57 pollutants. 

<p align="center">
  <img src="plots/stations.png"  width="50%" height="50%" />
</p>

Although all of them cause effect in humans, animals, structures and vegetation, some of them are considered highly harmful and due to their effects in human health.  These pollutants are the particulates **PM2.5** and **PM10**,  nitrogen dioxide **NO2** and ozone  **O3** which are regulated by the [European Environmental Agency EEA](https://www.eea.europa.eu/themes/air/air-quality-standards).

## Tools:
 - R
 - [Open Air](www.openair-project.org) (R Library for air analysis)
 - ggplot2 for R
 - MySQL database.
  
## Dataset
The datasets used for this analysis can be accessed through [UK Air web site](https://uk-air.defra.gov.uk/data), which is part of the [Department for Environment Food and Rural Affairs of U.K](https://www.gov.uk/government/organisations/department-for-environment-food-rural-affairs).
There is two datasets: **The stations** and **the measures**. The stations dataset contains metadata related to the station like location, name, time of use, time off, parameters for calibration, etc.  Despite the stations are phisycally located in different areas of London and each station measures a set of pollutants, not all the stations measures the same.

<p align="center">
  <img src="plots/stationsMeta.png" />
</p>

The pollutnats concentrations are measured per hour and stored in the station. After a considerable time, the information is extracted from the stations al=nd published in the **UK Air Website**. The interval of time used in this analysis were 6 years (2013-2018). 

<p align="center">
  <img src="plots/stationsDataset.png" />
</p>

## How to run
1. In Rstudio, open the file `LdnAirAnalysis.Rproj`
2. Install dependencies: openair and dplyr
3. Create folders **data** and **images** in the projet root
3. Run the `extraction.R`. It will generate a  `.csv` file in the folder `data/`
5. Execute `dataExploration.R`

## Pollutant selection criteria
The European Union (EU) through the European Environment Agency has developed an extensive legislation which establishes standards and objectives for a number of pollutants in air. The selection criteria for the pollutant to analyze will be that one that has exceeded this quality index limit more frequently. 

<p align="center">
  <img src="plots/AQI.JPG" />
</p>


## Output
A couple of `.csv` file that contains the average measures of the most significant pollutant in London per hour and per day.

## Some Plots
Correlation matrix between pollutants and variables monitored by the sensors in the stations.
<p align="center">  
 <img src="plots/correation.png" width="50%" height="50%"/>
</p>
Summary for O3
<p align="center"><img src="plots/o3summary.png" width="50%" height="50%"/></p>
Summary for No2
<p align="center"> <img src="plots/no2summary.png" width="50%" height="50%"/></p>
Data trend non deseasonalized
<p align="center"><img src="plots/nonDeseasonalisedTrend.png" width="50%" height="50%"/></p>
Data trend deseasonalized
<p align="center"><img src="plots/deseasonalisedTrend.png" width="50%" height="50%"/></p>

