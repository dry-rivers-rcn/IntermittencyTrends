## -----------
## Timeseries Exploration
## Kendra Kaiser
## Date Created: 04-20-2020
## Goal: Explore the metrics of interest to understand why some metrics might not have many sites with significant trends over time


source(file.path("code", "paths+packages.R"))
library(gclus)
library(ggplot2)
library(gridExtra)

# --- load trends data ----

gage_annual <- 
  file.path(dir_data, 
            "results/00_SelectGagesForAnalysis_GageSampleAnnual.csv") %>% 
  readr::read_csv()

# --- assess first no flow data change over time in locations with significant trends

sig<- fnf_sig$gage_ID

ggplot(gage_annual, aes(x=currentwyear[gage_ID ==6827500], y=firstnoflowcaly[gage_ID ==6827500])) + 
  geom_point()
for (i in 1:21){ 
plot(gage_annual$currentwyear[gage_annual$gage_ID ==sig[i]], gage_annual$firstnoflowcaly[gage_annual$gage_ID ==sig[i]])
}
