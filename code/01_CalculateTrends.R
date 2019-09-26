## 01_CalculateTrends.R

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

## for each gage: calculate sen's slope
for (s in 1:dim(gages_analysis)[1]){
  # grab site data
  df_site <- 
    gages_annual_summary %>% 
    subset(site %in% gages_analysis$site[s])
  
  all_years <- seq(min(df_site$wyear), max(df_site$wyear))
  missing_years <- all_years[!(all_years %in% df_site$wyear)]
  if (length(missing_years) == 0){
    df_site <- 
      df_site %>% 
      dplyr::arrange(wyear)
  }
  
  # create ts objects
  ts_noflowdays <- ts(as.numeric(df_site$totalnoflowperwyear), start = df_site$wyear[1], frequency = 1)
  ts_noflowperiods <- ts(as.numeric(df_site$totalnoflowperiods), start = df_site$wyear[1], frequency = 1)
  ts_noflowlength <- ts(as.numeric(df_site$maxlengthnoflow), start = df_site$wyear[1], frequency = 1)
  
  # if there are 0 no-flow periods, no-flow length returns a NaN but should be 0
  ts_noflowlength[ts_noflowperiods==0] <- 0
  
  # calculate slope
  sen_noflowdays <- trend::sens.slope(ts_noflowdays)
  sen_noflowperiods <- trend::sens.slope(ts_noflowperiods)
  sen_noflowlength <- trend::sens.slope(ts_noflowlength)
  
  # summarize data
  df_site_out <- 
    tibble::tibble(metric = c("noflowdays", "noflowperiods", "noflowlength"),
                   slope = c(sen_noflowdays$estimates, sen_noflowperiods$estimates, sen_noflowlength$estimates),
                   p_value = c(sen_noflowdays$p.value, sen_noflowperiods$p.value, sen_noflowlength$p.value))
  
  # combine
  if (s == 1){
    site_trends <- df_site_out
  } else {
    site_trends <- dplyr::bind_rows(site_trends, df_site_out)
  }
}

# save file
site_trends %>% 
  readr::write_csv(path = file.path("results", "01_CalculateTrends.csv"))
