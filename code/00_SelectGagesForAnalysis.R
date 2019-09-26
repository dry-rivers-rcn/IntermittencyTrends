## 00_SelectGagesForAnalysis.R
# This script will select gages for analysis based on the following characteristics:
#  -At least 1 no-flow day in at least 10 of the 30 years

source(file.path("code", "paths+packages.R"))

## load John's annual summary statistics
gages_annual_summary <- 
  file.path(dir_DataAnalysis, "data", 
            "annual_no_flow_and_climate_metric_means_for_no_flow_sites_081919.csv") %>% 
  readr::read_csv()

## overall geomorphic/setting summary statistics
gages_summary <- 
  file.path(dir_DataAnalysis, "data", 
            "mean_annual_no_flow_and_climate_metric_coefvars_for_no_flow_sites_081919_with_info.csv") %>% 
  readr::read_csv()

## locations of stream gages
sf_gages <- 
  file.path("data", "USGS_GageLocations.gpkg") %>% 
  sf::st_read()

# read in HPA boundary shapefile for preliminary analysis
sf_HPA <- 
  sf::st_read("C:/Users/samzipper/OneDrive - The University of Kansas/GIS_GeneralFiles/HPA_Boundary/hp_bound2010.shp") %>% 
  sf::st_transform(sf::st_crs(sf_gages)) %>% 
  subset(AQUIFER == "High Plains aquifer")

# subset to gages within high plains aquifer
HPA_gages <- sf::st_within(sf_gages, sf_HPA, sparse = F)
sf_HPA_gages <- sf_gages[HPA_gages[,1], ]

## variables we care about:
#  -totalnoflowperwyear = number of days with no flow per water year
#  -totalnoflowperiods = number of discrete no flow periods per year
#  -maxlengthnoflow = maximum no flow length
# (variable descriptions: dir_DataAnalysis/code/Variable_explanations.xlsx)

# count number of years with >= 1 no-flow day per site
gages_allyears_summary <- 
  gages_annual_summary %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarize(total_flow_years = n(), 
                   no_flow_years = sum(totalnoflowperwyear >= 1)) %>% 
  dplyr::left_join(sf_gages, by = c("site" = "site_no")) %>% 
  dplyr::left_join(
    gages_summary[,c("site", "dec_lat_va", "dec_long_va", "DRAIN_SQKM", "STATE", "CLASS", "SNOW_PCT_PRECIP", 
                     "GEOL_REEDBUSH_DOM", "GEOL_REEDBUSH_SITE", "GEOL_HUNT_DOM_DESC", "FRESHW_WITHDRAWAL", 
                     "PCT_IRRIG_AG", "POWER_NUM_PTS", "POWER_SUM_MW", "DEVNLCD06", 
                     "FORESTNLCD06", "PLANTNLCD06", "WATERNLCD06", "SNOWICENLCD06", 
                     "IMPNLCD06", "ELEV_MEAN_M_BASIN", "SLOPE_PCT", "AWCAVE", "PERMAVE", 
                     "CLAYAVE", "SILTAVE", "SANDAVE", "ECO3_SITE", "ECO3")],
    by = "site")

## subset to gages meeting criteria and in western kansas
gages_analysis <- 
  gages_allyears_summary %>% 
  subset(site %in% sf_HPA_gages$site) %>% 
  na.omit()

# save file
gages_analysis %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis.csv"))
