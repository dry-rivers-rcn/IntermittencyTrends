## 03_RandomForest_TuneHyperparameters.R
#' This script is intended to tune random forest hyperparameters:
#'  - mtry
#'  - ntree
#'  - min_n
#' 
#' Input variables will be selected using the output from 01_RandomForest_PreliminaryVariableImportance.R
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

## set up tuning parameter space
# set up model engine
rf_tune <- 
  rand_forest(trees = tune(), 
              mtry = tune(), 
              min_n = tune()) %>% 
  set_engine("ranger", num.threads = (parallel::detectCores() - 1)) %>% 
  set_mode("regression")

# create grid of parameters for tuning
rf_tune_grid <- grid_regular(trees(range = c(250, 1650)),
                             mtry(range = c(1, 10)),
                             min_n(range = c(3, 25)),
                             levels = 8)

## loop through metrics and regions
# choose number of predictors - based on script 02_RandomForest_FigureOutNumPredictors.R
npred_final <- tibble::tibble(metric = c("annualnoflowdays", "zeroflowfirst", "peak2z_length"),
                              npred = c(22, 27, 27)) 
n_folds <- 5 # choose number of folds for cross-val

for (m in metrics){
  # subset to training data, predictors
  fit_data_m <- 
    fit_data_in %>% 
    subset(Sample == "Train")
  
  # rename metric column
  names(fit_data_m)[names(fit_data_m) == m] <- "observed"
  
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
        fit_data_m %>% 
        dplyr::select(gage_ID, CLASS, currentclimyear, observed, region, all_of(rf_var_m_r$predictor)) %>% 
        subset(complete.cases(.))
    } else {
      fit_data_r <- 
        fit_data_m %>% 
        subset(region == r) %>% 
        dplyr::select(gage_ID, CLASS, currentclimyear, observed, region, all_of(rf_var_m_r$predictor)) %>% 
        subset(complete.cases(.))
    }
    
    # set up folds
    tune_folds <- vfold_cv(fit_data_r, v = 5, strata = region)
    
    # set up recipe
    tune_recipe <-
      fit_data_r %>% 
      recipe(observed ~ .) %>%
      update_role(gage_ID, currentclimyear, region, CLASS, new_role = "ID") %>% 
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
    tune_res_m_r <-
      tune_res %>% 
      collect_metrics() %>% 
      dplyr::mutate(metric = m, 
                    region_rf = r)
    
    if (m == metrics[1] & r == regions[1]){
      tune_res_all <- tune_res_m_r
    } else {
      tune_res_all <- dplyr::bind_rows(tune_res_all, tune_res_m_r)
    }
    
    # status update
    print(paste0(m, " ", r, " complete, ", Sys.time()))
    
  }
}

# save data
tune_res_all %>% 
  readr::write_csv(file.path("results", "03_RandomForest_TuneHyperparameters.csv"))