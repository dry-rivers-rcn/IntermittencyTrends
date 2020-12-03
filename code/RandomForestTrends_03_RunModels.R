## RandomForestTrends_03_RunModels.R
#' This script is intended to train and run random forest models.
#' 
#' Input variables will be selected using the output from RandomForestTrends_01_VariableImportance+NumPredictors.R.
#' Hyperparameters will be based on the output of RandomForestTrends_02_TuneHyperparameters.R
#' 

source(file.path("code", "paths+packages.R"))
library(tidymodels)

## load data
# RF input data
fit_data_in <- 
  readr::read_csv(file = file.path("results", "RandomForestTrends_01_RFinputData.csv")) 

# variable importance
rf_var <- 
  readr::read_csv(file = file.path("results", "RandomForestTrends_02_PreliminaryVariableImportance.csv"))

# metrics to predict
metrics <- c("annualnoflowdays", "zeroflowfirst", "peak2z_length")

## load hyperparameter tuning results
tune_res_all <- readr::read_csv(file.path("results", "RandomForestTrends_02_TuneHyperparameters.csv"))

## loop through metrics
# choose number of predictors - based on script RandomForestTrends_01_VariableImportance+NumPredictors.R
npred_final <- tibble::tibble(metric = c("tau_annualnoflowdays", "tau_zeroflowfirst", "tau_peak2z_length"),
                              npred = c(27, 23, 16))
for (m in metrics){
  # name for trend
  tau_m <- paste0("tau_", m)
  
  # determine number of predictors
  n_pred <- npred_final$npred[npred_final$metric == tau_m]
  
  # get predictor variables
  rf_var_m_r <-
    rf_var %>% 
    subset(metric == tau_m) %>% 
    dplyr::slice_max(order_by = ImpCondPerm, n = n_pred)
  
  # get data for this metric
  fit_data_r <- 
    fit_data_in %>% 
    dplyr::select(gage_ID, region, Sample, all_of(tau_m), all_of(rf_var_m_r$predictor)) %>% 
    subset(complete.cases(.))
  
  # rename metric column
  names(fit_data_r)[names(fit_data_r) == tau_m] <- "observed"
  
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
    subset(metric == m & .metric == "mae") %>% 
    dplyr::filter(mean == min(mean))
  
  rf_engine <- 
    rand_forest(trees = tune_res$trees[1], 
                mtry = tune_res$mtry[1], 
                min_n = tune_res$min_n[1]) %>% 
    set_engine("ranger", 
               num.threads = ncores,
               importance = "permutation") %>% 
    set_mode("regression")
  
  # set up recipe
  rf_recipe <-
    fit_data_train %>% 
    recipe(observed ~ .) %>%
    update_role(gage_ID, region, Sample, new_role = "ID") %>% 
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
    dplyr::select(gage_ID, observed, predicted) %>% 
    dplyr::mutate(metric = m) %>% 
    dplyr::left_join(dplyr::select(fit_data_in,
                                   gage_ID, region, Sample, all_of(rf_var_m_r$predictor)), 
                     by = c("gage_ID"))
  
  # extract variable importance
  fit_rf_imp_i <- tibble::tibble(predictor = names(pull_workflow_fit(rf_fit)$fit$variable.importance),
                                 IncMSE = pull_workflow_fit(rf_fit)$fit$variable.importance,
                                 oobMSE = pull_workflow_fit(rf_fit)$fit$prediction.error,
                                 metric = m)
  
  # partial dependence plots for all variables
  ranger_fit <- ranger::ranger(observed ~ ., 
                               data = dplyr::bind_cols(rf_fit$pre$mold$outcomes, 
                                                       rf_fit$pre$mold$predictors),
                               num.trees = tune_res$trees[1],
                               mtry = tune_res$mtry[1], 
                               min.node.size = tune_res$min_n[1],
                               num.threads = ncores)
  
  
  for (v in 1:n_pred){
    var <- rf_var_m_r$predictor[v]
    df_pdp_var <- 
      pdp::partial(ranger_fit, 
                   pred.var = var,
                   mtry = tune_res$mtry[1], 
                   min.node.size = tune_res$min_n[1],
                   num.threads = ncores,
                   parallel = T) %>% 
      magrittr::set_colnames(c("value", "yhat"))
    df_pdp_var$predictor <- var
    df_pdp_var$metric <- m
    class(df_pdp_var) <- "data.frame"
    
    if (v == 1){
      df_pdp <- df_pdp_var
    } else {
      df_pdp <- dplyr::bind_rows(df_pdp, df_pdp_var)
    }
  }
  
  # combine
  if (m == metrics[1]){
    fit_data_out <- fit_data_i
    fit_rf_imp <- fit_rf_imp_i
    fit_pdp_out <- df_pdp
  } else {
    fit_data_out <- dplyr::bind_rows(fit_data_out, fit_data_i)
    fit_rf_imp <- dplyr::bind_rows(fit_rf_imp, fit_rf_imp_i)
    fit_pdp_out <- dplyr::bind_rows(fit_pdp_out, df_pdp)
  }
  
  # status update
  print(paste0(m, " complete, ", Sys.time()))
  
}

# save data
fit_data_out %>% 
  dplyr::select(gage_ID, observed, predicted, metric) %>% 
  readr::write_csv(file.path("results", "RandomForestTrends_03_RunModels_Predictions.csv"))

fit_rf_imp %>% 
  readr::write_csv(file.path("results", "RandomForestTrends_03_RunModels_VariableImportance.csv"))

fit_pdp_out %>% 
  readr::write_csv(file.path("results", "RandomForestTrends_03_RunModels_PartialDependence.csv"))
