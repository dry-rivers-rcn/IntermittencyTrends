## 00_SelectGagesForAnalysis.R
# This script will select gages for analysis based on the following characteristics:
#  -At least 1 no-flow day in at least 10 of the 30 years

source(file.path("code", "paths+packages.R"))

## load mean annual long-term stats for each gage from John
gages_mean <-
  file.path(dir_data, "mean_annual_no_flow_and_climate_metrics_110419.csv") %>% 
  readr::read_csv()

# subset to gages meeting threshold
noflowfraction_min_threshold <- 15/365   # Eng et al used 15 days
noflowfraction_max_threshold <- 350/365  # also at least 15 no-flow days/yr
year_threshold <- 30

# sites to sample
gage_sample <- gages_mean[gages_mean$annualfractionnoflow > noflowfraction_min_threshold &
                            gages_mean$annualfractionnoflow < noflowfraction_max_threshold &
                            gages_mean$years_data > year_threshold, ]

sum(gage_sample$CLASS == "Ref")
sum(gage_sample$CLASS == "Non-ref")

## subset annual stats for sampled gages
# load data from john
gages_annual_summary <- 
  file.path(dir_data, 
            "annual_no_flow_and_climate_metrics_020720_trends.csv") %>% 
  readr::read_csv()

gage_sample_annual <- 
  gages_annual_summary %>% 
  subset(gage_ID %in% gage_sample$gage_ID)

## save data to repository
gage_sample %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv"))

gage_sample_annual %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv"))
