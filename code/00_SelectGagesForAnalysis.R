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

## subset trends data for sampled gages
gage_trends <- 
  file.path(dir_data, "trends_in_no_flow_and_climate_for_no_flow_sites_34_plus_years_020720.csv") %>% 
  readr::read_csv() %>% 
  # remove first column - it is just the row number
  dplyr::select(-X1) %>% 
  # rename X - it is the metric for each column. also rename site to gage_ID to match other files
  dplyr::rename(metric = X, gage_ID = site) %>% 
  # subset to sites in sample
  subset(gage_ID %in% gage_sample$gage_ID) %>% 
  # remove 'currentwyear'
  subset(metric != "currentwyear")

## define regions - for now, use the NA_L1NAME column as a preliminary start and refine it to fewer groups
gage_sample$region <- NA
gage_sample$region[gage_sample$NA_L1NAME == "GREAT PLAINS" & gage_sample$dec_lat_va > 43] <- "North Great Plains"
gage_sample$region[gage_sample$NA_L1NAME == "GREAT PLAINS" & gage_sample$dec_lat_va <= 43] <- "South Great Plains"
gage_sample$region[gage_sample$NA_L1NAME %in% c("MEDITERRANEAN CALIFORNIA", "MARINE WEST COAST FOREST")] <- "Mediterranean California"
gage_sample$region[gage_sample$NA_L1NAME %in% c("SOUTHERN SEMI-ARID HIGHLANDS", "NORTH AMERICAN DESERTS")] <- "Western Desert"
gage_sample$region[gage_sample$NA_L1NAME %in% c("NORTHWESTERN FORESTED MOUNTAINS", "TEMPERATE SIERRAS")] <- "Western Mountains"
gage_sample$region[gage_sample$NA_L1NAME == "EASTERN TEMPERATE FORESTS"] <- "Eastern Temperate"

## save data to repository
gage_sample %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv"))

gage_sample_annual %>% 
  dplyr::left_join(gage_sample[ , c("gage_ID", "region")], by = "gage_ID") %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv"))

gage_trends %>% 
  dplyr::left_join(gage_sample[ , c("gage_ID", "region")], by = "gage_ID") %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis_GageSampleTrends.csv"))

## plot
# load state map
states <- map_data("state")

ggplot() + 
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = NA, color = col.gray) +
  geom_point(data = gage_sample, aes(x = dec_long_va, y = dec_lat_va, color = region, shape = CLASS)) +
  scale_x_continuous(name = "Longitude") +
  scale_y_continuous(name = "Latitude") +
  scale_color_discrete(name = "Region") +
  scale_shape_discrete(name = "Class") +
  labs(title = "Gages Selected for Analysis", subtitle = "Tentative grouping by regions from NA_L1NAME") +
  coord_map() +
  ggsave(file.path("results", "00_SelectGagesForAnalysis_Map.png"),
         width = 190, height = 90, units= "mm")
