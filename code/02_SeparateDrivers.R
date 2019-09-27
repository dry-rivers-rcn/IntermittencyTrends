## 02_SeparateDrivers.R
# This script calculates the relative impacts of climate and anthropogenic impacts through time.

source(file.path("code", "paths+packages.R"))

## load list of gages to study
gages_analysis <- 
  file.path("results", "00_SelectGagesForAnalysis.csv") %>% 
  readr::read_csv()

## load John's annual summary statistics
gages_annual_summary <- 
  file.path(dir_DataAnalysis, "data", 
            "annual_no_flow_and_climate_metric_means_for_no_flow_sites_081919.csv") %>% 
  readr::read_csv() %>% 
  subset(site %in% gages_analysis$site)

gages_ref <- gages_analysis$site[gages_analysis$type == "ref"]
gages_nonref <- gages_analysis$site[gages_analysis$type == "nonref"]

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
  
  for (r in 1:length(gages_ref)){
    df_r <- 
      gages_annual_summary %>% 
      subset(site == gages_ref[r] & wyear %in% predictors$wyear) %>% 
      dplyr::select(wyear, totalnoflowperwyear)
    predictors <- 
      dplyr::left_join(predictors, df_r, by = "wyear")
    colnames(predictors)[dim(predictors)[2]] <- paste0("ref", gages_ref[r])
  }
  
  ## regression
  # get rid of water year
  predictors$wyear <- NULL
  pls_fit <- pls::plsr(metric ~ ., data = predictors)
  
  plot(pls_fit, ncomp = 3, line = T)
  
}
