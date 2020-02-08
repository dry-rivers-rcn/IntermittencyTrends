## 02_SeparateDrivers.R
# This script calculates the relative impacts of climate and anthropogenic impacts through time.

source(file.path("code", "paths+packages.R"))

## load list of gages to study
gages_analysis <- 
  file.path("results", "00_SelectGagesForAnalysis.csv") %>% 
  readr::read_csv()

## load John's annual summary statistics
gages_annual_summary <- 
  file.path(dir_data, 
            "annual_no_flow_and_climate_metrics_102719.csv") %>% 
  readr::read_csv() %>% 
  subset(site %in% gages_analysis$site)

## save for joanna
gages_annual_summary %>% 
  readr::write_csv("C:/Users/samzipper/Desktop/DryRivers_AnnualData_HPAgages.csv")

gages_ref <- gages_analysis$site[gages_analysis$CLASS == "Ref"]
gages_nonref <- gages_analysis$site[gages_analysis$CLASS == "Non-ref"]

## load shapefiles
sf_gages <- 
  file.path("data", "USGS_GageLocations.gpkg") %>% 
  sf::st_read() %>% 
  sf::st_transform(proj_crs)

sf_gages_ref <- 
  sf_gages %>% 
  subset(site_no %in% gages_ref)

## for each gage: calculate sen's slope
for (s in 1:length(gages_nonref)){
  # grab site data
  df_site <- 
    gages_annual_summary %>% 
    subset(site %in% gages_nonref[s])
 
  # metric
  metric <- "totalnoflowperwyear"
  
  # for each site, collect predictors:
  #  -annual precip
  #  -annual PET
  #  -annual P-PET
  #  -metric value at reference gage(s)
  predictors <- 
    tibble::tibble(wyear = df_site$wyear,
                   metric = dplyr::pull(df_site, metric),
                   p_mm_wy = df_site$p_mm_wy,
                   pet_mm_wy = df_site$pet_mm_wy) %>% 
    dplyr::mutate(defc_mm_wy = pet_mm_wy - p_mm_wy)
  
  ## get only closest reference gage as predictor
  # find closest reference gage
  ref_closest <- sf_gages_ref$site_no[which.min(sf::st_distance(subset(sf_gages, site_no == gages_nonref[s]),
                                                                sf_gages_ref))]
  df_r <-
    gages_annual_summary %>%
    subset(site == ref_closest & wyear %in% predictors$wyear) %>%
    dplyr::select(wyear, totalnoflowperwyear)
  predictors <-
    dplyr::left_join(predictors, df_r, by = "wyear")
  colnames(predictors)[dim(predictors)[2]] <- "ref_closest"
  
  # ## grab all references gages as predictors
  # for (r in 1:length(gages_ref)){
  #   df_r <- 
  #     gages_annual_summary %>% 
  #     subset(site == gages_ref[r] & wyear %in% predictors$wyear) %>% 
  #     dplyr::select(wyear, totalnoflowperwyear)
  #   predictors <- 
  #     dplyr::left_join(predictors, df_r, by = "wyear")
  #   colnames(predictors)[dim(predictors)[2]] <- paste0("ref", gages_ref[r])
  # }
  
  # set up output file
  df_fit_s <- 
    tibble::tibble(site = gages_nonref[s],
                   wyear = predictors$wyear,
                   metric = metric,
                   observed = predictors$metric,
                   predicted = NaN)
  
  ## regression
  # get rid of water year
  predictors$wyear <- NULL
  pls_fit <- pls::plsr(metric ~ ., data = predictors)
  
  # get predicted and observed
  df_fit_s$predicted[as.numeric(row.names(predict(pls_fit, ncomp = 3)))] <- predict(pls_fit, ncomp = 3)
  
  #plot(pls_fit, ncomp = 3, line = T)
  
  if (s == 1){
    df_fit_all <- df_fit_s
  } else {
    df_fit_all <- dplyr::bind_rows(df_fit_all, df_fit_s)
  }
  
}

## plot
ggplot(df_fit_all, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "No Flow Days/Year, all non-reference gages") +
  scale_x_continuous(name = "Observed") +
  scale_y_continuous(name = "Predicted") +
  ggsave(file.path("results", "02_SeparateDrivers_ScatterFit.png"))
