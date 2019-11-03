## 00_SelectGagesForAnalysis.R
# This script will select gages for analysis based on the following characteristics:
#  -At least 1 no-flow day in at least 10 of the 30 years

source(file.path("code", "paths+packages.R"))

## load John's annual summary statistics
gages_annual_summary <- 
  file.path(dir_data, 
            "annual_no_flow_and_climate_metrics_102719.csv") %>% 
  readr::read_csv()

## read in HPA boundary shapefile for preliminary analysis
sf_HPA <- 
  sf::st_read("C:/Users/samzipper/OneDrive - The University of Kansas/GIS_GeneralFiles/HPA_Boundary/hp_bound2010.shp") %>% 
  subset(AQUIFER == "High Plains aquifer")

## locations of stream gages
sf_gages <- 
  file.path("data", "USGS_GageLocations.gpkg") %>% 
  sf::st_read() %>% 
  sf::st_transform(proj_crs)

# subset to gages within high plains aquifer or within a chosen distance of edge (to get streams just off edge)
HPA_gages <- sf::st_is_within_distance(sf_gages, sf_HPA, dist = 50*1000, sparse = F)  # distance units are [m]
sf_HPA_gages <- sf_gages[HPA_gages[,1], ]

## variables we care about:
#  -totalnoflowperwyear = number of days with no flow per water year
#  -totalnoflowperiods = number of discrete no flow periods per year
#  -maxlengthnoflow = maximum no flow length
# (variable descriptions: dir_DataAnalysis/code/Variable_explanations.xlsx)

# count number of years with >= 1 no-flow day per site
gages_allyears_summary <- 
  gages_annual_summary %>% 
  dplyr::group_by(site, CLASS) %>% 
  dplyr::summarize(total_flow_years = n(), 
                   no_flow_years = sum(totalnoflowperwyear >= 1))

## subset to gages meeting criteria and in HPA
gages_analysis <- 
  gages_allyears_summary %>% 
  subset(site %in% sf_HPA_gages$site) %>% 
  na.omit()

# save file
gages_analysis %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis.csv"))
