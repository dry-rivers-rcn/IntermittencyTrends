## RandomForestTrends_02_TuneHyperparameters.R
#' This script is intended to tune random forest hyperparameters:
#'  - mtry
#'  - ntree
#'  - min_n
#' 
#' Input variables will be selected using the output from RandomForestTrends_01_VariableImportance+NumPredictors.R
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

## set up tuning parameter space
# set up model engine
rf_tune <- 
  rand_forest(trees = tune(), 
              mtry = tune(), 
              min_n = tune()) %>% 
  set_engine("ranger", num.threads = ncores) %>% 
  set_mode("regression")

# create grid of parameters for tuning
rf_tune_grid <- grid_regular(trees(range = c(250, 1650)),
                             mtry(range = c(1, 10)),
                             min_n(range = c(3, 25)),
                             levels = 8)

## loop through metrics
# choose number of predictors - based on script RandomForestTrends_01_VariableImportance+NumPredictors.R
npred_final <- tibble::tibble(metric = c("tau_annualnoflowdays", "tau_zeroflowfirst", "tau_peak2z_length"),
                              npred = c(27, 23, 16))
n_folds <- 5 # choose number of folds for cross-val

for (m in metrics){
  # name for trend
  tau_m <- paste0("tau_", m)
  
  # subset to training data
  fit_data_m <- 
    fit_data_in %>% 
    subset(Sample == "Train")
  
  # rename metric column
  names(fit_data_m)[names(fit_data_m) == tau_m] <- "observed"
  
  # determine number of predictors
  n_pred <- npred_final$npred[npred_final$metric == tau_m]
  
  # get predictor variables
  rf_var_m_r <-
    rf_var %>% 
    subset(metric == tau_m) %>% 
    dplyr::slice_max(order_by = ImpCondPerm, n = n_pred)
  
  fit_data_r <- 
    fit_data_m %>% 
    dplyr::select(gage_ID, observed, region, all_of(rf_var_m_r$predictor)) %>% 
    subset(complete.cases(.))
  
  # set up folds
  tune_folds <- vfold_cv(fit_data_r, v = 5, strata = region)
  
  # set up recipe
  tune_recipe <-
    fit_data_r %>% 
    recipe(observed ~ .) %>%
    update_role(gage_ID, region, new_role = "ID") %>% 
    step_normalize(all_predictors(), -all_outcomes())
  
  # build tuning workflow
  tune_wf <-
    workflow() %>% 
    add_model(rf_tune) %>% 
    add_recipe(tune_recipe)
  
  # run tuning
  tune_res <-
    tune_wf %>% 
    tune_grid(
      resamples = tune_folds,
      grid = rf_tune_grid,
      metrics = metric_set(mae, rmse, rsq)
    )
  
  # collect results
  tune_res_m <-
    tune_res %>% 
    collect_metrics() %>% 
    dplyr::mutate(metric = m)
  
  if (m == metrics[1]){
    tune_res_all <- tune_res_m
  } else {
    tune_res_all <- dplyr::bind_rows(tune_res_all, tune_res_m)
  }
  
  # status update
  print(paste0(m, " complete, ", Sys.time()))
  
}

# save data
tune_res_all %>% 
  readr::write_csv(file.path("results", "RandomForestTrends_02_TuneHyperparameters.csv"))

ggplot(subset(tune_res_all, .metric = "mae"), aes(x = mtry, y = mean, color = min_n)) +
  geom_point() +
  facet_wrap(~metric)

ggplot(subset(tune_res_all, .metric = "mae"), aes(x = trees, y = mean, color = min_n)) +
  geom_point() +
  facet_wrap(~metric)

ggplot(subset(tune_res_all, .metric = "mae"), aes(x = min_n, y = mean, color = trees)) +
  geom_point() +
  facet_wrap(~metric)
