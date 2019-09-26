## 00_SelectGagesForAnalysis.R
# This script will select gages for analysis based on the following characteristics:
#  -At least 1 no-flow day in at least 10 of the 30 years

source(file.path("code", "paths+packages.R"))

## load John's annual summary statistics
gages_annual_summary <- 
  file.path(dir_DataAnalysis, "data", 
            "annual_no_flow_and_climate_metric_means_for_no_flow_sites_081919.csv") %>% 
  readr::read_csv()

## locations of stream gages
sf_gages <- 
  file.path("data", "USGS_GageLocations.gpkg") %>% 
  sf::st_read()

# kansas is FIPS 20
gages_westKS <- subset(sf_gages, state_cd == 20 & dec_long_v < -100)$site

## variables we care about:
#  -totalnoflowperwyear = number of days with no flow per water year
# (variable descriptions: dir_DataAnalysis/code/Variable_explanations.xlsx)

# count number of years with >= 1 no-flow day per site
gages_allyears_summary <- 
  gages_annual_summary %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarize(total_flow_years = n(), 
                   no_flow_years = sum(totalnoflowperwyear >= 1)) %>% 
  dplyr::left_join(sf_gages, by = c("site" = "site_no"))

## subset to gages meeting criteria and in western kansas
gages_analysis <- gages_allyears_summary[gages_allyears_summary$no_flow_years >= 10 &
                                           gages_allyears_summary$state_cd == 20 &
                                           gages_allyears_summary$dec_long_v < -100, ] %>% 
  na.omit()

# save file
gages_analysis %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis.csv"))
