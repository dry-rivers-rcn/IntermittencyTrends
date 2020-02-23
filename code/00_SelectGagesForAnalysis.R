## 00_SelectGagesForAnalysis.R
# This script will select gages for analysis based on the following characteristics:
#  -At least 1 no-flow day in at least 10 of the 30 years

source(file.path("code", "paths+packages.R"))

## load data from John - mean annual data
# mean annual long-term stats for each gage
gages_mean_new <-
  file.path(dir_data, "data_for_spearman_rank_correlations.csv") %>% 
  readr::read_csv() %>% 
  dplyr::select(-X1)

# old version of gages_mean, which has ref/nonref info
gages_mean_old <-
  file.path(dir_data, "mean_annual_no_flow_and_climate_metrics_110419.csv") %>% 
  readr::read_csv() 

# drop flow metrics - the ones we care about are in gages_mean_new
gages_mean_old <- gages_mean_old[,-(56:99)]

# drop columns from gages_mean_old that are contained in gages_mean_new
same_cols <- names(gages_mean_old)[names(gages_mean_old) %in% names(gages_mean_new)]
same_cols <- same_cols[-1]  # keep 'gage_ID'
gages_mean_old <- 
  gages_mean_old %>% 
  dplyr::select(-all_of(same_cols), -(`p/pet`), -(`swe/P`))

# combine gages_mean_old and gages_mean_new
gages_mean <- dplyr::left_join(gages_mean_new, gages_mean_old, by = c("gage_ID"))

## subset to gages meeting threshold
noflowfraction_min_threshold <- 15/365   # Eng et al used 15 days
noflowfraction_max_threshold <- 350/365  # also at least 15 no-flow days/yr
year_threshold <- 30

# sites to sample
gage_sample <- gages_mean[gages_mean$annualfractionnoflow > noflowfraction_min_threshold &
                            gages_mean$annualfractionnoflow < noflowfraction_max_threshold &
                            gages_mean$years_data > year_threshold, ]

sum(gage_sample$CLASS == "Ref")
sum(gage_sample$CLASS == "Non-ref")

## load annual stats for each gage
# annual stats for each gage - climate and flow metrics, but only extract flow
gages_annual_flow <- 
  file.path(dir_data, 
            "annual_no_flow_and_climate_metrics_020720_trends.csv") %>% 
  readr::read_csv() %>% 
  dplyr::select(gage_ID, currentwyear, annualfractionnoflow, totalnoflowperiods, 
                firstnoflowcaly, zeroflowcentroiddate, peak2z_length) %>% 
  subset(gage_ID %in% gage_sample$gage_ID)

# annual stats for each gage - climate only (these should replace climate stats from gages_annual_summary)
gages_annual_climate <- 
  file.path(dir_data, "annual_climate_metrics_for_CONUS_USGS_101719.csv") %>% 
  readr::read_csv() %>% 
  dplyr::rename(gage_ID = sitewith0) %>% 
  subset(gage_ID %in% gage_sample$gage_ID)

# combine climate and flow stats
gages_annual_summary <-
  dplyr::right_join(gages_annual_flow, gages_annual_climate, by = c("gage_ID", "currentwyear"))

## calculate trends - this is modified from John's script, CalculateTrends_020720.R
fulllengthwyears <- tibble::tibble(currentwyear = c(1980:2018))
sites <- unique(gages_annual_summary$gage_ID)

for(i in seq_along(sites)){
  current <- subset(gages_annual_summary, gage_ID == sites[i])
  current[current==-Inf] <- NA
  current[current==Inf] <- NA
  
  # which columns to calculate trends?
  cols_trend <- which(!names(gages_annual_summary) %in% c("gage_ID", "currentwyear"))
  
  results <- tibble::tibble(metric = colnames(gages_annual_summary[cols_trend]),
                            tau = NA, 
                            pval = NA, 
                            slope = NA)
  
  for(col in cols_trend){
    currentcolumnname <- colnames(current)[col]
    currentcolumn <- tibble::tibble(variable = dplyr::pull(current, col))
    currentcolumn$currentwyear <- current$currentwyear
    currentcolumn <- dplyr::right_join(currentcolumn, fulllengthwyears, by = "currentwyear")
    years_data <-  sum(is.finite(currentcolumn$variable))
    
    # only calculate trend if at least 30 years of data
    if(years_data >= 30){
      manken <- Kendall::MannKendall(currentcolumn$variable)
      sen <- zyp::zyp.sen(variable ~ currentwyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      output <- c(tau, pval, sen$coefficients[2])
      results[results$metric == currentcolumnname, c("tau", "pval", "slope")] <- output
    } 
  }
  
  results$gage_ID <- sites[i]
  
  if (i == 1){
    gage_trends <- results
  } else {
    gage_trends <- dplyr::bind_rows(gage_trends, results)
  }
  
  # status update
  print(paste0("Site ", i, " complete"))
}

## define regions - for now, use the NA_L1NAME column as a preliminary start and refine it to fewer groups
# (eventually these will be replaced with output from John's analysis of spatial patterns)
gage_sample$region <- NA
gage_sample$region[gage_sample$NA_L1NAME == "GREAT PLAINS" & gage_sample$dec_lat_va > 43] <- "North Great Plains"
gage_sample$region[gage_sample$NA_L1NAME == "GREAT PLAINS" & gage_sample$dec_lat_va <= 43] <- "South Great Plains"
gage_sample$region[gage_sample$NA_L1NAME %in% c("MEDITERRANEAN CALIFORNIA", "MARINE WEST COAST FOREST")] <- "Mediterranean California"
gage_sample$region[gage_sample$NA_L1NAME %in% c("SOUTHERN SEMI-ARID HIGHLANDS", "NORTH AMERICAN DESERTS")] <- "Western Desert"
gage_sample$region[gage_sample$NA_L1NAME %in% c("NORTHWESTERN FORESTED MOUNTAINS", "TEMPERATE SIERRAS")] <- "Western Mountains"
gage_sample$region[gage_sample$NA_L1NAME == "EASTERN TEMPERATE FORESTS"] <- "Eastern Temperate"

table(gage_sample$region, gage_sample$CLASS)

## save data to repository
gage_sample %>% 
  readr::write_csv(path = file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv"))

gages_annual_summary %>% 
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
