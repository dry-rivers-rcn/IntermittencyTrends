## SeparateDrivers-National.R
# This script calculates the relative impacts of climate and anthropogenic impacts through time.
# General goal is to build statistical relationships for reference gages and extend to nonref gages.

source(file.path("code", "paths+packages.R"))

## load data - gage mean properties and annual values
gage_sample <- 
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv"))

gage_regions <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv"))

gage_sample_annual <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv")) %>% 
  dplyr::left_join(gage_regions, by = "gage_ID") %>% 
  # add some derived variables
  dplyr::mutate(p.pet_wy = p_mm_wy/pet_mm_wy,
                swe.p_wy = swe_mm_wy/p_mm_wy)

gage_trends <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleTrends.csv")) %>% 
  dplyr::left_join(gage_regions, by = "gage_ID")

## clean up data and get ready to fit statistical model
# predictors and metrics to retain; to get full list of options: 
#   dput(names(gage_sample_annual))
#   dput(names(gage_sample))
predictors_annual_all <- c("p_mm_wy", "p_mm_jja", "p_mm_son", "p_mm_djf", "p_mm_mam", "pet_mm_wy", 
                           "pet_mm_jja", "pet_mm_son", "pet_mm_djf", "pet_mm_mam", "T_max_c_wy", 
                           "T_max_c_jja", "T_max_c_son", "T_max_c_djf", "T_max_c_mam", "T_min_c_wy", 
                           "T_min_c_jja", "T_min_c_son", "T_min_c_djf", "T_min_c_mam", "pcumdist10days", 
                           "pcumdist50days", "pcumdist90days", "swe_mm_wy", "swe_mm_jja", 
                           "swe_mm_son", "swe_mm_djf", "swe_mm_mam", "srad_wm2_wy", "srad_wm2_jja", 
                           "srad_wm2_son", "srad_wm2_djf", "srad_wm2_mam", "pdsi_wy", "pdsi_jja", 
                           "pdsi_son", "pdsi_djf", "pdsi_mam", "p.pet_wy", "swe.p_wy")

predictors_static_all <- c("DRAIN_SQKM", "accumulated_NID_storage", "POWER_SUM_MW", 
                           "FORESTNLCD06", "PLANTNLCD06", "WATERNLCD06", "SNOWICENLCD06", 
                           "IMPNLCD06", "ELEV_MEAN_M_BASIN", "SLOPE_PCT", "AWCAVE", "PERMAVE", 
                           "TOPWET", "depth_bedrock_m", "porosity", "storage_m",  
                           #"dec_lat_va", "dec_long_va", 
                           "GEOL_REEDBUSH_DOM", "GEOL_REEDBUSH_SITE", "GEOL_HUNT_DOM_DESC", 
                           "FRESHW_WITHDRAWAL", "PCT_IRRIG_AG", "POWER_NUM_PTS", "DEVNLCD06", 
                           "CLAYAVE", "SILTAVE", "SANDAVE", "log_k", "log_q0")


## second: develop statistical models
#predictors_annual <- c("p.pet_wy", "swe.p_wy", "p_mm_wy", "pet_mm_wy", "swe_mm_wy", "srad_wm2_wy", "pdsi_wy")  # change year to year
#predictors_static <- c("DRAIN_SQKM", "ELEV_MEAN_M_BASIN", "SLOPE_PCT")  # don't change year to year
predictors_annual <- predictors_annual_all
predictors_static <- predictors_static_all

metrics <- c("annualfractionnoflow", "firstnoflowcaly", "peak2z_length")


fit_data_in <- 
  gage_sample_annual %>% 
  # subset to fewer columns - metrics and predictors
  dplyr::select(c("gage_ID", "currentwyear", all_of(metrics), all_of(predictors_annual))) %>% 
  # join with static predictors
  dplyr::left_join(gage_sample[ , c("gage_ID", "CLASS", predictors_static)], by = "gage_ID")

# choose a metric
start_flag <- T
for (metric in metrics){
  # subset to complete cases and references gages only
  fit_ref_data_in <- 
    fit_data_in %>% 
    dplyr::select(-all_of(metrics[metrics != metric])) %>%  # drop metrics you aren't interested in
    subset(CLASS == "Ref") %>% 
    subset(complete.cases(.))
  
  # rename metric columln
  names(fit_ref_data_in)[names(fit_ref_data_in) == metric] <- "observed"
  
  # build formula
  fit_formula <- as.formula(paste0("observed ~ ", paste(c(predictors_annual, predictors_static), collapse = "+")))
  
  # fit random forest model
  set.seed(1)
  fit_rf <- randomForest::randomForest(fit_formula,
                                       data = fit_ref_data_in,
                                       ntree = 500,
                                       importance = T)

  # predict with random forest
  fit_ref_data_in$predicted <- predict(fit_rf, fit_ref_data_in)
  
  # extract variable importance
  fit_rf_imp <- tibble::tibble(predictor = rownames(fit_rf$importance),
                               IncNodePurity = fit_rf$importance[,'IncNodePurity'],
                               IncMSE = fit_rf$importance[,'%IncMSE'],
                               metric = metric)
  
  # add grouping columns
  fit_ref_data_in$metric <- metric
  
  # combine into one data frame
  if (start_flag){
    fit_results <- fit_ref_data_in
    fit_importance <- fit_rf_imp
    start_flag <- F
  } else {
    fit_results <- dplyr::bind_rows(fit_results, fit_ref_data_in)
    fit_importance <- dplyr::bind_rows(fit_importance, fit_rf_imp)
  }
  
  print(paste0(metric, " complete"))
}

# add region back in
fit_results <- dplyr::left_join(fit_results, gage_sample[,c("gage_ID", "region")], by = "gage_ID")

## make plots
for (m in metrics){
  if (m == "annualfractionnoflow") axes_limits <- c(0,1)
  if (m == "firstnoflowcaly") axes_limits <- c(0,366)
  if (m == "peak2z_length") axes_limits <- c(0,150)
  ggplot(subset(fit_results, metric == m), aes(x = observed, y = predicted, color = region)) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(shape = 21) +
    facet_wrap(region ~ ., scales = "free", ncol = 3) +
    scale_x_continuous(name = "Observed", limits = axes_limits, expand = c(0,0)) +
    scale_y_continuous(name = "Predicted", limits = axes_limits, expand = c(0,0)) +
    labs(title = paste0("Predicted vs. observed ", m),
         subtitle = "National random forest model, reference gages") +
    stat_smooth(method = "lm") +
    theme(legend.position = "bottom") +
    ggsave(file.path("results", paste0("SeparateDrivers-NationalByRegions_RandomForest_", m, ".png")),
           width = 160, height = 120, units = "mm")
  
  ggplot(subset(fit_results, metric == m), aes(x = observed, y = predicted)) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(aes(color = region), shape = 21) +
    scale_x_continuous(name = "Observed", limits = axes_limits, expand = c(0,0)) +
    scale_y_continuous(name = "Predicted", limits = axes_limits, expand = c(0,0)) +
    labs(title = paste0("Predicted vs. observed ", m),
         subtitle = "National random forest model, reference gages") +
    stat_smooth(method = "lm") +
    theme(legend.position = "bottom") +
    ggsave(file.path("results", paste0("SeparateDrivers-National_RandomForest_", m, ".png")),
           width = 125, height = 140, units = "mm")
}

# variable importance
# rank based on annualfractionnoflow
rank_importance <- 
  fit_importance %>% 
  subset(metric == "annualfractionnoflow") %>% 
  dplyr::arrange(IncMSE) %>% 
  dplyr::pull(predictor)
fit_importance$predictor <- factor(fit_importance$predictor, levels = rank_importance)

ggplot(fit_importance, aes(x = IncMSE, y = predictor)) + 
  geom_vline(xintercept = 0, color = col.gray) +
  geom_point() +
  facet_wrap(~metric, scales = "free_x") +
  scale_x_continuous(name = "Increase in MSE caused by predictor variability\n[Higher Value = more important variable]") +
  scale_y_discrete(name = "Predictor") +
  labs(title = "Predictor importance, national random forest models") +
  ggsave(file.path("results", "SeparateDrivers-National_RandomForest_VariableImportance.png"),
         width = 160, height = 120, units = "mm")

## fit statistics
# fit by region
fit_stats <- 
  fit_results %>% 
  dplyr::group_by(metric, region) %>% 
  dplyr::summarize(RMSE = hydroGOF::rmse(predicted, observed),
                   NRMSE = hydroGOF::nrmse(predicted, observed, norm = "maxmin"),
                   R2 = R2(predicted, observed))

# overall fit
fit_results %>% 
  dplyr::group_by(metric) %>% 
  dplyr::summarize(RMSE = hydroGOF::rmse(predicted, observed),
                   NRMSE = hydroGOF::nrmse(predicted, observed, norm = "maxmin"),
                   R2 = R2(predicted, observed))
