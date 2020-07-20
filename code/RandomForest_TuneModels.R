## RandomForest_TuneModels.R
#' This script is intended to tune random forest models by:
#'   - determining the most important variables
#'     - this study compared approaches: https://academic.oup.com/bib/article/20/2/492/4554516
#'   - determining the best mtry and ntree

source(file.path("code", "paths+packages.R"))
library(tidymodels)
library(ranger)

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

predictors_all <- c(predictors_climate, predictors_human, predictors_static, predictors_climate_with_previous)

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
  cor(fit_data_in[,predictors_all], use = "pairwise.complete.obs", method = "pearson")
#check_cor[lower.tri(check_cor, diag = T)] <- NA

cor_high <-
  check_cor %>% 
  reshape2::melt() %>% 
  subset(Var1 != Var2 & is.finite(value)) %>% 
  subset(abs(value) > 0.9) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(Var1, Var2)

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

#cor_high <- subset(cor_high, !(Var1 %in% predictors_drop | Var2 %in% predictors_drop))

## reduce to a subset of data for developing approach
fit_data_play <- 
  fit_data_in %>% 
  dplyr::sample_frac(0.1)

####
#### build model with tidymodels framework
####

m <- "annualfractionnoflow"
r <- "National"
predictors_final <- predictors_all[!(predictors_all %in% predictors_drop)]


## loop through metrics

# get rid of unneeded metrics
fit_data_m <- 
  fit_data_play %>% 
  dplyr::select(-all_of(metrics[metrics != m]))

# rename metric column
names(fit_data_m)[names(fit_data_m) == m] <- "observed"

## loop through regions

# subset to region
fit_data_r <- 
  fit_data_m %>% 
  subset(complete.cases(.))

# split into training/testing
fit_data_train <- subset(fit_data_r, Sample == "Train")
fit_data_test <- subset(fit_data_r, Sample == "Test")

# set up recipe
tune_recipe <-
  fit_data_train %>% 
  recipe(as.formula(paste0("observed ~ ", paste(predictors_final, collapse = "+")))) %>%
  step_normalize(all_predictors(), -all_outcomes()) %>%

#### next step: integrate recipe into workflow below



# set up folds
tune_folds <- vfold_cv(rf_train, v = 10)

# set up tuning model
rf_tune <- 
  rand_forest(trees = tune(), 
              mtry = tune(), 
              min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

# create grid of parameters for tuning; reasonable values selected using functions from dials package
rf_tune_grid <- grid_regular(trees(range = c(200, 400)),
                             mtry(range = c(1, 10)),
                             min_n(range = c(3, 100)),
                             levels = 2)

# build tuning workflow
tune_wf <-
  workflow() %>% 
  add_model(rf_tune) %>% 
  add_formula(as.formula(paste0("observed ~ ", paste(predictors_all, collapse = "+"))))

# run tuning
tune_res <-
  tune_wf %>% 
  tune_grid(
    resamples = tune_folds,
    grid = rf_tune_grid
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
  subset(trees == 150) %>% 
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(x = mtry, y = mean, color = min_n)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

# look at best trees
tune_res %>%
  show_best("rmse")

# choose best tree for final workflow
best_tree <-
  tune_res %>% 
  select_best("rmse")

final_wf <- 
  tune_wf %>% 
  finalize_workflow(best_tree)

# fit with k-folds
rf_fit_folds <-
  final_wf %>% 
  fit_resamples(rf_folds)

collect_metrics(rf_fit_folds)

# fit best model to test data
final_fit <-
  final_wf %>% 
  last_fit(rf_split)

collect_metrics(final_fit)

# build model
rf_ranger <- 
  rand_forest(trees = 100, mode = "regression") %>% 
  set_engine("ranger")

# set up workflow
rf_wflow <- 
  workflow() %>% 
  add_model(rf_ranger) %>% 
  add_formula(observed ~ .)

# fit model
rf_fit <-
  rf_wflow %>% 
  fit(data = rf_train)

# fit with k-folds
rf_fit_folds <-
  rf_wflow %>% 
  fit_resamples(rf_folds)
collect_metrics(rf_fit_folds)

# use workflow to predict
predict(rf_fit, rf_test)

# transform test data
rf_testing <-
  rf_recipe %>% 
  bake(testing(rf_split))

# prep training data
rf_training <- juice(rf_recipe)



# check fit
rf_ranger %>%
  predict(rf_testing) %>%
  bind_cols(rf_testing) %>%
  metrics(truth = observed, estimate = .pred)





<<<<<<< HEAD
# set up recipe (formula)
fit_formula <- as.formula(paste0("observed ~ ", paste(predictors_all, collapse = "+")))
rf_recipe <- 
  rf_train %>% 
  recipe(fit_formula) %>% 
  step_zv(all_predictors()) %>% 
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()



fit_formula <- as.formula(paste0("observed ~ ", paste(predictors_all, collapse = "+")))
=======
fit_formula <- as.formula(paste0("observed ~ ", paste(predictors_final, collapse = "+")))
>>>>>>> 6ae9f35de94cb09c5b6ab7a9925c8fde47256c27
system.time(fit_rf <- randomForest::randomForest(fit_formula,
                                                 data = fit_data_r,
                                                 ntree = 500))
system.time(fit_rg <- ranger::ranger(fit_formula,
                                     data = fit_data_r,
                                     num.trees = 500,
                                     importance = "permutation",
                                     seed = 1))
fit_rg
fit_rf

ranger::importance(fit_rg)

fit_ct <-
  train(fit_formula,
        data = fit_data_r,
        method = "ranger",
        trControl = trainControl(method="cv", number = 5, verboseIter = T),
        tuneGrid = expand.grid(
          mtry = 2:4,
          splitrule = "gini",
          min.node.size),
        num.trees = 100,
        importance = "permutation")
varImp(fit_ct)

Boruta::Boruta(fit_formula, data = fit_data_r, holdHistory = F) -> BorutaTest
Boruta::Boruta(fit_formula, data = fit_data_r, getImp = getImpFerns, holdHistory = F) -> BorutaTestFerns
