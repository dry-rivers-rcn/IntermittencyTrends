## 04_RandomForest_RunModels.R
#' This script is intended to train and run random forest models.
#' 
#' Input variables will be selected using the output from 01_RandomForest_PreliminaryVariableImportance.R
#' and 02_RandomForest_FigureOutNumPredictors.R. Hyperparameters will be based on the output of 
#' 03_RandomForest_TuneHyperparameters.R
#' 

source(file.path("code", "paths+packages.R"))
library(tidymodels)

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
  dplyr::mutate(p.pet_cy = p_mm_cy/pet_mm_cy,
                swe.p_cy = swe_mm_cy/p_mm_cy,
                p.pet_jfm = p_mm_jfm/pet_mm_jfm,
                swe.p_jfm = swe_mm_jfm/p_mm_jfm,
                p.pet_amj = p_mm_amj/pet_mm_amj,
                swe.p_amj = swe_mm_amj/p_mm_amj,
                p.pet_jas = p_mm_jas/pet_mm_jas,
                swe.p_jas = swe_mm_jas/p_mm_jas,
                p.pet_ond = p_mm_ond/pet_mm_ond,
                swe.p_ond = swe_mm_ond/p_mm_ond)

## set up predictions
# metrics and regions to predict
metrics <- c("annualnoflowdays", "zeroflowfirst", "peak2z_length")
regions <- c("National", unique(gage_sample$region))

# all possible predictors
predictors_climate <- c("p_mm_cy", "p_mm_jas", "p_mm_ond", "p_mm_jfm", "p_mm_amj", "pet_mm_cy", 
                        "pet_mm_jas", "pet_mm_ond", "pet_mm_jfm", "pet_mm_amj", "T_max_c_cy", 
                        "T_max_c_jas", "T_max_c_ond", "T_max_c_jfm", "T_max_c_amj",
                        "swe_mm_cy", "swe_mm_jas", 
                        "swe_mm_ond", "swe_mm_jfm", "swe_mm_amj", "p.pet_cy", "swe.p_cy", "p.pet_jfm", "swe.p_jfm",
                        "p.pet_amj", "swe.p_amj", "p.pet_jas", "swe.p_jas", "p.pet_ond", "swe.p_ond")

predictors_human <- c("dams_n", "maxstorage_af", "normstorage_af", "majordams_n", 
                      "wuse_mm", "irrig_prc", "lulc_water_prc", "lulc_dev_prc", "lulc_wetland_prc",
                      "lulc_forest_prc", "lulc_barren_prc", "lulc_grass_prc", "lulc_ag_prc")

predictors_static <- c("drain_sqkm", "elev_mean_m_basin", "slope_pct", 
                       "awcave", "permave", "topwet", "depth_bedrock_m", 
                       "porosity", "storage_m", "clayave", "siltave", "sandave")

# previous year predictors will be calculated further down
predictors_climate_with_previous <- 
  c(predictors_climate, c("p_mm_cy.previous", "p_mm_jas.previous", "p_mm_ond.previous", 
                          "p_mm_jfm.previous", "p_mm_amj.previous", "pet_mm_cy.previous", 
                          "pet_mm_jas.previous", "pet_mm_ond.previous", "pet_mm_jfm.previous", 
                          "pet_mm_amj.previous", "T_max_c_cy.previous", "T_max_c_jas.previous", 
                          "T_max_c_ond.previous", "T_max_c_jfm.previous", "T_max_c_amj.previous",
                          "swe_mm_cy.previous", "swe_mm_jas.previous", "swe_mm_ond.previous", 
                          "swe_mm_jfm.previous", "swe_mm_amj.previous", "p.pet_cy.previous", 
                          "swe.p_cy.previous", "p.pet_jfm.previous", "swe.p_jfm.previous", 
                          "p.pet_amj.previous", "swe.p_amj.previous", "p.pet_jas.previous", 
                          "swe.p_jas.previous", "p.pet_ond.previous", "swe.p_ond.previous"))

predictors_all <- c(predictors_human, predictors_static, predictors_climate_with_previous)

## calculate previous water year climate metrics
gage_sample_prevyear <- 
  gage_sample_annual[,c("gage_ID", "currentclimyear", predictors_climate)] %>% 
  dplyr::mutate(wyearjoin = currentclimyear + 1) %>% 
  dplyr::select(-currentclimyear)

## combine into one data frame
fit_data_in <- 
  gage_sample_annual %>% 
  # subset to fewer columns - metrics and predictors
  dplyr::select(c("gage_ID", "currentclimyear", "Sample", all_of(metrics), 
                  all_of(predictors_climate), all_of(predictors_human))) %>% 
  # join with previous water year
  dplyr::left_join(gage_sample_prevyear, 
                   by = c("gage_ID", "currentclimyear"="wyearjoin"), 
                   suffix = c("", ".previous")) %>% 
  # join with static predictors
  dplyr::left_join(gage_sample[ , c("gage_ID", "CLASS", "region", predictors_static)], by = "gage_ID")

## load hyperparameter tuning results
tune_res_all <- readr::read_csv(file.path("results", "03_RandomForest_TuneHyperparameters.csv"))

## loop through metrics and regions
# choose number of predictors - based on script 02_RandomForest_FigureOutNumPredictors.R
npred_final <- tibble::tibble(metric = c("annualnoflowdays", "zeroflowfirst", "peak2z_length"),
                              npred = c(22, 27, 27)) 
for (m in metrics){
  
  # determine number of predictors
  n_pred <- npred_final$npred[npred_final$metric == m]
  
  for (r in regions){
    # get predictor variables
    rf_var_m_r <-
      file.path("results", paste0("01_RandomForest_PreliminaryVariableImportance_", m, "_", gsub(" ", "", r, fixed = TRUE), ".csv")) %>% 
      readr::read_csv() %>% 
      dplyr::slice_max(order_by = ImpCondPerm, n = n_pred)
    
    if (r == "National") {
      fit_data_r <- 
        fit_data_in %>% 
        dplyr::select(gage_ID, currentclimyear, region, Sample, all_of(m), all_of(rf_var_m_r$predictor)) %>% 
        subset(complete.cases(.))
    } else {
      fit_data_r <- 
        fit_data_in %>% 
        subset(region == r) %>% 
        dplyr::select(gage_ID, currentclimyear, region, Sample, all_of(m), all_of(rf_var_m_r$predictor)) %>% 
        subset(complete.cases(.))
    }
    
    names(fit_data_r)[names(fit_data_r)==m] <- "observed"
    
    # split into training/testing
    fit_data_train <- 
      fit_data_r %>% 
      subset(Sample == "Train")
    fit_data_test <- 
      fit_data_r %>% 
      subset(Sample == "Test")
    
    # set up model engine
    tune_res <-
      tune_res_all %>% 
      subset(metric == m & region_rf == r & .metric == "mae") %>% 
      dplyr::filter(mean == min(mean))
    
    rf_engine <- 
      rand_forest(trees = tune_res$trees[1], 
                  mtry = tune_res$mtry[1], 
                  min_n = tune_res$min_n[1]) %>% 
      set_engine("ranger", 
                 num.threads = (parallel::detectCores() - 1),
                 importance = "permutation") %>% 
      set_mode("regression")
    
    # set up recipe
    rf_recipe <-
      fit_data_train %>% 
      recipe(observed ~ .) %>%
      update_role(gage_ID, currentclimyear, region, Sample, new_role = "ID") %>% 
      step_normalize(all_predictors(), -all_outcomes())
    
    # set up workflow
    rf_workflow <-
      workflow() %>% 
      add_model(rf_engine) %>% 
      add_recipe(rf_recipe)
    
    # fit model
    rf_fit <- 
      rf_workflow %>% 
      fit(data = fit_data_train)
    
    # predict
    fit_data_train$predicted <- predict(rf_fit, fit_data_train)$.pred
    fit_data_test$predicted <-  predict(rf_fit, fit_data_test)$.pred
    
    # combine training and test output
    fit_data_i <- 
      dplyr::bind_rows(fit_data_train, fit_data_test) %>% 
      dplyr::select(gage_ID, currentclimyear, observed, predicted) %>% 
      dplyr::mutate(region_rf = r,
                    metric = m) %>% 
      dplyr::left_join(dplyr::select(fit_data_in,
                                     gage_ID, currentclimyear, region, Sample, all_of(predictors_all)), 
                                     by = c("gage_ID", "currentclimyear"))
    
    # extract variable importance
    fit_rf_imp_i <- tibble::tibble(predictor = names(pull_workflow_fit(rf_fit)$fit$variable.importance),
                                   IncMSE = pull_workflow_fit(rf_fit)$fit$variable.importance,
                                   oobMSE = pull_workflow_fit(rf_fit)$fit$prediction.error,
                                   metric = m,
                                   region_rf = r)
    
    # partial dependence plots for all variables
    ranger_fit <- ranger::ranger(observed ~ ., 
                                 data = dplyr::bind_cols(rf_fit$pre$mold$outcomes, 
                                                         rf_fit$pre$mold$predictors),
                                 num.trees = tune_res$trees[1],
                                 mtry = tune_res$mtry[1], 
                                 min.node.size = tune_res$min_n[1],
                                 num.threads = (parallel::detectCores() - 1))
    
    
    for (v in 1:n_pred){
      var <- rf_var_m_r$predictor[v]
      df_pdp_var <- 
        pdp::partial(ranger_fit, 
                     pred.var = var,
                     mtry = tune_res$mtry[1], 
                     min.node.size = tune_res$min_n[1],
                     num.threads = (parallel::detectCores() - 1),
                     parallel = T) %>% 
        magrittr::set_colnames(c("value", "yhat"))
      df_pdp_var$predictor <- var
      df_pdp_var$metric <- m
      df_pdp_var$region <- r
      class(df_pdp_var) <- "data.frame"
      
      if (v == 1){
        df_pdp <- df_pdp_var
      } else {
        df_pdp <- dplyr::bind_rows(df_pdp, df_pdp_var)
      }
    }
    
    # combine
    if (m == metrics[1] & r == regions[1]){
      fit_data_out <- fit_data_i
      fit_rf_imp <- fit_rf_imp_i
      fit_pdp_out <- df_pdp
    } else {
      fit_data_out <- dplyr::bind_rows(fit_data_out, fit_data_i)
      fit_rf_imp <- dplyr::bind_rows(fit_rf_imp, fit_rf_imp_i)
      fit_pdp_out <- dplyr::bind_rows(fit_pdp_out, df_pdp)
    }
    
    # status update
    print(paste0(m, " ", r, " complete, ", Sys.time()))
    
  }
}

# save data
fit_data_out %>% 
  dplyr::select(gage_ID, currentclimyear, observed, predicted, region_rf, metric) %>% 
  readr::write_csv(file.path("results", "04_RandomForest_RunModels_Predictions.csv"))

fit_rf_imp %>% 
  readr::write_csv(file.path("results", "04_RandomForest_RunModels_VariableImportance.csv"))

fit_pdp_out %>% 
  readr::write_csv(file.path("results", "04_RandomForest_RunModels_PartialDependence.csv"))

# plots
min(subset(fit_data_out, metric == "annualnoflowdays")$predicted)

ggplot(subset(fit_data_out, metric == "annualnoflowdays" & Sample == "Test"), 
       aes(x = predicted, y = observed, color = region)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf)

ggplot(subset(fit_data_out, metric == "annualnoflowdays"), 
       aes(x = predicted, y = observed, color = region)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf)

ggplot(subset(fit_data_out, metric == "annualnoflowdays" & Sample == "Test"), 
       aes(x = currentclimyear, y = (predicted - observed), color = region)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_point() +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf)