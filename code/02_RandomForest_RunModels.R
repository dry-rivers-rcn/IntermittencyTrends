## 02_RandomForest_RunModels.R
#' This script is intended to train random forest models on reference gages and apply them to all gages.
#' 
#' Input variables will be selected using the output from 01_RandomForest_PreliminaryVariableImportance.R
#' 

source(file.path("code", "paths+packages.R"))

## load data - gage mean properties and annual values
gage_sample <- 
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv")) %>% 
  dplyr::mutate(gage_ID = as.numeric(gage_ID))

gage_regions <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv"))

gage_sample_annual <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv")) %>% 
  dplyr::left_join(gage_regions, by = "gage_ID") %>% 
  # add some derived variables
  dplyr::mutate(p.pet_wy = p_mm_wy/pet_mm_wy,
                swe.p_wy = swe_mm_wy/p_mm_wy,
                p.pet_djf = p_mm_djf/pet_mm_djf,
                swe.p_djf = swe_mm_djf/p_mm_djf,
                p.pet_mam = p_mm_mam/pet_mm_mam,
                swe.p_mam = swe_mm_mam/p_mm_mam,
                p.pet_jja = p_mm_jja/pet_mm_jja,
                swe.p_jja = swe_mm_jja/p_mm_jja,
                p.pet_son = p_mm_son/pet_mm_son,
                swe.p_son = swe_mm_son/p_mm_son)

rf_var_importance <- 
  file.path("results", "01_RandomForest_PreliminaryVariableImportance.csv") %>% 
  readr::read_csv() %>% 
  dplyr::group_by(metric, region, predictor) %>% 
  dplyr::summarize(IncMSE_mean = mean(IncMSE)) %>% 
  dplyr::ungroup()

# metrics and regions to predict
metrics <- c("annualfractionnoflow", "firstnoflowcaly", "peak2z_length")
regions <- c("National", unique(gage_sample$region))

# all possible predictors
predictors_annual <- c("p_mm_wy", "p_mm_jja", "p_mm_son", "p_mm_djf", "p_mm_mam", "pet_mm_wy", 
                       "pet_mm_jja", "pet_mm_son", "pet_mm_djf", "pet_mm_mam", "T_max_c_wy", 
                       "T_max_c_jja", "T_max_c_son", "T_max_c_djf", "T_max_c_mam", "T_min_c_wy", 
                       "T_min_c_jja", "T_min_c_son", "T_min_c_djf", "T_min_c_mam", "pcumdist10days", 
                       "pcumdist50days", "pcumdist90days", "swe_mm_wy", "swe_mm_jja", 
                       "swe_mm_son", "swe_mm_djf", "swe_mm_mam", "srad_wm2_wy", "srad_wm2_jja", 
                       "srad_wm2_son", "srad_wm2_djf", "srad_wm2_mam", "pdsi_wy", "pdsi_jja", 
                       "pdsi_son", "pdsi_djf", "pdsi_mam", "p.pet_wy", "swe.p_wy", "p.pet_djf", "swe.p_djf",
                       "p.pet_mam", "swe.p_mam", "p.pet_jja", "swe.p_jja", "p.pet_son", "swe.p_son")

predictors_static <- c("DRAIN_SQKM", "accumulated_NID_storage", 
                       "ELEV_MEAN_M_BASIN", "SLOPE_PCT", "AWCAVE", "PERMAVE", 
                       "TOPWET", "depth_bedrock_m", 
                       "CLAYAVE", "SILTAVE", "SANDAVE")

# combine into one data frame
fit_data_in <- 
  gage_sample_annual %>% 
  # subset to fewer columns - metrics and predictors
  dplyr::select(c("gage_ID", "currentwyear", all_of(metrics), all_of(predictors_annual))) %>% 
  # join with static predictors
  dplyr::left_join(gage_sample[ , c("gage_ID", "CLASS", "region", predictors_static)], by = "gage_ID")

## test number of predictors to use in final models
for (n_pred in 5:20){
  # get predictor variables
  rf_var_predtest <-
    rf_var_importance %>% 
    subset(metric == "annualfractionnoflow" & region == "National") %>% 
    dplyr::top_n(n = n_pred, wt = IncMSE_mean)
  
  fit_formula <- as.formula(paste0("observed ~ ", paste(unique(rf_var_predtest$predictor), collapse = "+")))
  
  # subset
  fit_data_predtest <- 
    fit_data_in %>% 
    dplyr::select(-all_of(metrics[metrics != m])) %>%  # drop metrics you aren't interested in
    subset(complete.cases(.))
  
  # rename metric column
  names(fit_data_predtest)[names(fit_data_predtest) == m] <- "observed"
  
  # fit model
  set.seed(1)
  fit_rf <- randomForest::randomForest(fit_formula,
                                       data = subset(fit_data_predtest, CLASS == "Ref"),
                                       ntree = 500,
                                       importance = T)
  
  if (n_pred == 5){
    fit_predtest <- tibble::tibble(n = n_pred,
                                   TotalMSE = fit_rf$mse[length(fit_rf$mse)],
                                   TotalR2 = fit_rf$rsq[length(fit_rf$rsq)])
  } else {
    fit_predtest <- dplyr::bind_rows(fit_predtest,
                                     tibble::tibble(n = n_pred,
                                                    TotalMSE = fit_rf$mse[length(fit_rf$mse)],
                                                    TotalR2 = fit_rf$rsq[length(fit_rf$rsq)]))
  }
  
  print(paste0(n_pred, " complete"))
}

# plot and look for elbow
ggplot(fit_predtest, aes(x = n, y = TotalMSE)) + 
  geom_point() + geom_line() +
  scale_x_continuous(name = "Number of Predictors", breaks = seq(5,20)) +
  scale_y_continuous(name = "Model MSE") +
  labs(title = "Random forest MSE as a function of number of predictors",
       subtitle = "Predicting annualfractionnoflow for national model") +
  ggsave(file.path("figures_manuscript", "RandomForest_MSEvNumberPredictors.png"),
         width = 120, height = 95, units = "mm")

# choose n_pred
n_pred <- 16

## loop through metrics and regions
for (m in metrics){
  # subset to complete cases and references gages only
  fit_data_m <- 
    fit_data_in %>% 
    dplyr::select(-all_of(metrics[metrics != m])) %>%  # drop metrics you aren't interested in
    subset(complete.cases(.))
  
  # rename metric column
  names(fit_data_m)[names(fit_data_m) == m] <- "observed"
  
  for (r in regions){
    if (r == "National") {
      fit_data_r <- fit_data_m
    } else {
      fit_data_r <- subset(fit_data_m, region == r)
    }
    
    # get predictor variables
    rf_var_m_r <-
      rf_var_importance %>% 
      subset(metric == m & region == r) %>% 
      dplyr::top_n(n = n_pred, wt = IncMSE_mean)
    
    fit_formula <- as.formula(paste0("observed ~ ", paste(unique(rf_var_m_r$predictor), collapse = "+")))
    
    # fit model
    set.seed(1)
    fit_rf <- randomForest::randomForest(fit_formula,
                                         data = subset(fit_data_r, CLASS == "Ref"),
                                         ntree = 500,
                                         importance = T)
    
    # run model
    fit_data_r$predicted <- predict(fit_rf, fit_data_r)
    fit_data_r$metric <- m
    fit_data_r$region_rf <- r
    fit_data_i <- 
      fit_data_r %>% 
      dplyr::select(gage_ID, CLASS, currentwyear, observed, predicted, metric, region, region_rf)
    
    # extract variable importance
    fit_rf_imp_i <- tibble::tibble(predictor = rownames(fit_rf$importance),
                                   VarPrcIncMSE = fit_rf$importance[,'%IncMSE'],
                                   TotalMSE = fit_rf$mse[length(fit_rf$mse)],
                                   TotalR2 = fit_rf$rsq[length(fit_rf$rsq)],
                                   metric = m,
                                   region_rf = r)
    
    if (m == metrics[1] & r == regions[1]){
      fit_data_out <- fit_data_i
      fit_rf_imp <- fit_rf_imp_i
    } else {
      fit_data_out <- dplyr::bind_rows(fit_data_out, fit_data_i)
      fit_rf_imp <- dplyr::bind_rows(fit_rf_imp, fit_rf_imp_i)
    }
    
    # status update
    print(paste0(m, " ", r, " complete, ", Sys.time()))
  }
}

ggplot(subset(fit_data_out, metric == "annualfractionnoflow" & CLASS == "Ref"), 
       aes(x = predicted, y = observed, color = region)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf)

ggplot(subset(fit_data_out, metric == "annualfractionnoflow" & CLASS == "Ref"), 
       aes(x = currentwyear, y = (predicted - observed), color = region)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_point() +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf)

# save data
fit_data_out %>% 
  readr::write_csv(file.path("results", "02_RandomForest_RunModels_Predictions.csv"))

fit_rf_imp %>% 
  readr::write_csv(file.path("results", "02_RandomForest_RunModels_VariableImportance.csv"))
