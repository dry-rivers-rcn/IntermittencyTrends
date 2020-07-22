## RandomForest_TuneModels.R
#' This script is intended to tune random forest models by:
#'   - eliminate redundant (highly correlated) variables and variables with near-zero variance
#'   - determining the most important variables using default RF parameters
#'   - with most important variables, tune hyperparameters

source(file.path("code", "paths+packages.R"))
library(tidymodels)
library(ranger)
library(Boruta)
library(VSURF)

####
#### prep data
####

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
metrics <- c("annualfractionnoflow", "zeroflowfirst", "peak2z_length")
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
  dplyr::select(c("gage_ID", "currentclimyear", all_of(metrics), 
                  all_of(predictors_climate), all_of(predictors_human))) %>% 
  # join with previous water year
  dplyr::left_join(gage_sample_prevyear, 
                   by = c("gage_ID", "currentclimyear"="wyearjoin"), 
                   suffix = c("", ".previous")) %>% 
  # join with static predictors
  dplyr::left_join(gage_sample[ , c("gage_ID", "CLASS", "Sample", "region", predictors_static)], by = "gage_ID")

###
### filter out predictor variables
###
check_cor <- 
  cor(fit_data_in[fit_data_in$Sample == "Train",predictors_all], use = "pairwise.complete.obs", method = "pearson")

cor_high <-
  check_cor %>% 
  reshape2::melt() %>% 
  subset(Var1 != Var2 & is.finite(value)) %>% 
  subset(abs(value) > 0.9) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(Var1, Var2)
#cor_high <- subset(cor_high, !(Var1 %in% predictors_drop | Var2 %in% predictors_drop))

# there are 51 highly-correlated pairs of predictor variables
# (pearson r > 0.9, which is used in step_corr function)
# we want to drop: 
#  - seasonal variables when correlated with annual value
#  - previous year variables when correlated with current year value
predictors_drop <-
  # highly correlated variables
  c("T_max_c_jfm", "T_max_c_ond", "T_max_c_amj", "T_max_c_cy.previous",
    "T_max_c_jfm.previous", "T_max_c_ond.previous", "T_max_c_jas.previous",
    "T_max_c_amj.previous", "sandave", "storage_m", 'maxstorage_af', "swe_mm_cy.previous",
    "swe_mm_jfm","p_mm_cy", "p_mm_jas", "p_mm_jfm", "p_mm_amj",
    "p_mm_cy.previous", "p_mm_jas.previous", "p_mm_jfm.previous", "p_mm_amj.previous",
    "pet_mm_cy.previous", "pet_mm_ond.previous", "pet_mm_ond",
    # near-zero variance
    "swe_mm_jas", "swe_mm_amj", "swe.p_jas", "normstorage_af", "swe_mm_jas.previous",
    "swe_mm_amj.previous", "swe.p_jas.previous")

###
### begin loop through metrics and regions
###

# for testing
#m <- "annualfractionnoflow"
#r <- "National"
#fit_data_play <- 
#  fit_data_in %>% 
#  dplyr::sample_frac(0.1)

for (m in metrics){
  
  fit_data_m <- 
    fit_data_in %>% 
    subset(Sample == "Train") %>% 
    dplyr::select(gage_ID, currentclimyear, region, all_of(m), all_of(predictors_all))
  
  for (r in regions){
    
    if (r == "National") {
      fit_data_r <- 
        fit_data_m %>% 
        subset(complete.cases(.))
    } else {
      fit_data_r <- 
        subset(fit_data_m, region == r) %>% 
        subset(complete.cases(.))
    }
    
    # variable selection; this study compared approaches: https://academic.oup.com/bib/article/20/2/492/4554516
    #  - vita and varSelRF are only for classification RFs
    #  - Boruta had high performance but slow computationally
    
    ## boruta
    br <- Boruta(x = fit_data_r[ , predictors_all], y = pull(fit_data_r, m), 
                 doTrace = 0, ntree = 500)
    
    br_imp_median <- tibble(predictor = colnames(br$ImpHistory),
                            boruta_imp_median = apply(br$ImpHistory, 2, median))
    
    br_vars <- 
      tibble::tibble(predictor = names(br$finalDecision),
                     boruta_decision = br$finalDecision) %>% 
      dplyr::left_join(br_imp_median, by = "predictor") %>% 
      dplyr::mutate(metric = m, 
                    region_rf = r)
    
    ## VSURF
    vs <- VSURF(x = fit_data_r[ , predictors_all], y = pull(fit_data_r, m),
                parallel = T, ntree = 500,
                RFimplem = "ranger",
                verbose = F)
    
    vs_vars <-   
      dplyr::bind_rows(
        tibble::tibble(predictor = predictors_all[vs$varselect.pred],
                       vsurf_group = "Thresholding"),
        tibble::tibble(predictor = predictors_all[vs$varselect.pred],
                       vsurf_group = "Interpretation"),
        tibble::tibble(predictor = predictors_all[vs$varselect.pred],
                       vsurf_group = "Prediction")) %>% 
      dplyr::mutate(metric = m, 
                    region_rf = r)
    
    ## collect results
    if (m == metrics[1] $ r == regions[1]){
      br_all <- br_vars
      vs_all <- vs_vars
    } else {
      br_all <- dplyr::bind_rows(br_all, br_vars)
      vs_all <- dplyr::bind_rows(vs_all, vs_vars)
    }
    
    print(paste0(m, " ", r, " complete, ", Sys.time()))
  }
}

## write output
br_all %>% 
  readr::write_csv(path = file.path("results", "RandomForest_VariableSelection-Boruta.csv"))
vs_all %>% 
  readr::write_csv(path = file.path("results", "RandomForest_VariableSelection-VSURF.csv"))



####
#### build model with tidymodels framework
####



## loop through metrics will go here



# rename metric column

## loop through regions will go here


# split into training/testing
fit_data_train <- 
  fit_data_r %>% 
  subset(Sample == "Train") %>% 
  dplyr::select(gage_ID, currentclimyear, region, observed, all_of(predictors_final))
fit_data_test <- 
  fit_data_r %>% 
  subset(Sample == "Test") %>% 
  dplyr::select(gage_ID, currentclimyear, region, observed, all_of(predictors_final))

# set up recipe
tune_recipe <-
  fit_data_train %>% 
  recipe(observed ~ .) %>%
  update_role(gage_ID, currentclimyear, region, new_role = "ID") %>% 
  step_normalize(all_predictors(), -all_outcomes()) %>% 
  prep()

# bake the testing data too
fit_data_test_baked <- 
  tune_recipe %>%
  bake(fit_data_test) 

# set up folds
tune_folds <- vfold_cv(fit_data_train, v = 5, strata = "region")

# set up tuning model
rf_tune <- 
  rand_forest(trees = tune(), 
              mtry = tune(), 
              min_n = tune()) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("regression")

# create grid of parameters for tuning
rf_tune_grid <- grid_regular(trees(range = c(100, 500)),
                             mtry(range = c(1, 10)),
                             min_n(range = c(3, 100)),
                             levels = 2)

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

# plot results
tune_res %>%
  collect_metrics() %>%
  subset(.metric == "mae") %>% 
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(x = mtry, y = mean, color = min_n)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(trees ~ ., scales = "free") +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

# look at best trees
tune_res %>%
  show_best("mae")

# choose best tree for final workflow
final_wf <- 
  tune_wf %>% 
  finalize_workflow(best_tree)

# fit with k-folds
rf_fit_folds <-
  final_wf %>% 
  fit_resamples(tune_folds,
                metrics = metric_set(mae, rmse, rsq))
collect_metrics(rf_fit_folds)


### figure out how to apply to test data
rf_testing_pred <- 
  predict(rf_fit_folds, fit_data_test_baked)


# fit best model to training data
rf_fit_best <-
  final_wf %>% 
  fit(fit_data_test_baked)

rf_fit_test

# use workflow to predict
fit_data_test_baked$predicted <- predict(rf_fit_test, fit_data_test_baked)$.pred

# check fit
fit_data_test %>%
  metrics(truth = observed, estimate = predicted)

ggplot(fit_data_test, aes(x = predicted, y = observed)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1))




Boruta::Boruta(fit_formula, data = fit_data_r, holdHistory = F) -> BorutaTest
Boruta::Boruta(fit_formula, data = fit_data_r, getImp = getImpFerns, holdHistory = F) -> BorutaTestFerns
