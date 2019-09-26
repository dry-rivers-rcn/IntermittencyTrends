## 00_SelectGagesForAnalysis.R
# This script will select gages for analysis based on the following characteristics:
#  -At least 1 no-flow day in at least 10 of the 30 years

source(file.path("code", "paths+packages.R"))

## load John's annual summary statistics
gages_annual_summary <- 
  file.path(dir_DataAnalysis, "data", 
            "annual_no_flow_and_climate_metric_means_for_no_flow_sites_081919.csv") %>% 
  readr::read_csv()

## variables we care about:
#  -totalnoflowperwyear = number of days with no flow per water year
# (variable descriptions: dir_DataAnalysis/code/Variable_explanations.xlsx)

# count number of years with >= 1 no-flow day per site
gages_allyears_summary <- 
  gages_annual_summary %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarize(total_flow_years = n(), 
                   no_flow_years = sum(totalnoflowperwyear >= 1))

## subset to gages meeting criteria
gages_analysis <- gages_allyears_summary[gages_allyears_summary$no_flow_years >= 10, ]

# save file
gages_analysis %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis.csv"))
