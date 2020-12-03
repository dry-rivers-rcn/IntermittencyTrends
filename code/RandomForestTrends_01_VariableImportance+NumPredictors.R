## RandomForestTrends_01_VariableImportance+NumPredictors.R

source(file.path("code", "paths+packages.R"))
library(tidymodels)
library(partykit)
library(future.apply)

## metrics we want to plot, in the order they should be plotted
metrics <- c("annualnoflowdays", "peak2z_length", "zeroflowfirst")

## load data
gage_regions <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv"))

gage_mean <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv"))

gage_trends <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleTrends.csv")) %>% 
  dplyr::select(gage_ID, metric, mk_tau, mk_p)

gage_sample_annual <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv")) %>% 
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

## variables of interest
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

predictors_all <- c(predictors_human, predictors_static, predictors_climate)

# predictors to exclude due to high correlation, minimal variable (script: 01_RandomForest_PreliminaryVariableImportance.R)
predictors_drop <-
  # highly correlated variables
  c("T_max_c_jfm", "T_max_c_ond", "T_max_c_amj", "T_max_c_cy.previous",
    "T_max_c_jfm.previous", "T_max_c_ond.previous", "T_max_c_jas.previous",
    "T_max_c_amj.previous", "swe_mm_cy.previous", "swe_mm_jfm",
    "p_mm_cy", "p_mm_jas", "p_mm_amj", "p_mm_cy.previous", "p.pet_jas.previous","p.pet_amj.previous",
    "pet_mm_cy.previous", "pet_mm_jfm", "pet_mm_jfm.previous",
    # static variables
    "sandave", "storage_m", 'maxstorage_af', 
    # near-zero variance
    "swe_mm_jas", "swe_mm_amj", "swe.p_jas", "normstorage_af", "swe_mm_jas.previous",
    "swe_mm_amj.previous", "swe.p_jas.previous")

predictors_trimmed <- predictors_all[!(predictors_all %in% predictors_drop)]

## grab trend data - one row per gage
df_trends_tau <- 
  gage_trends %>% 
  subset(metric %in% c(metrics, predictors_trimmed)) %>% 
  tidyr::pivot_wider(id_cols = c("gage_ID"), names_from = "metric", values_from = "mk_tau", names_prefix = "tau_")
df_trends_p <- 
  gage_trends %>% 
  subset(metric %in% c(metrics, predictors_trimmed)) %>% 
  tidyr::pivot_wider(id_cols = c("gage_ID"), names_from = "metric", values_from = "mk_p", names_prefix = "p_")

## collect data for random forest: mean values and trends for all predictors
data_all <-
  gage_sample_annual %>% 
  dplyr::select(gage_ID, any_of(predictors_climate), any_of(predictors_human)) %>% 
  dplyr::group_by(gage_ID) %>% 
  dplyr::summarize_all(mean, na.rm = T) %>% 
  # add static variables
  dplyr::left_join(gage_mean[,c("gage_ID", predictors_static)], by = "gage_ID") %>% 
  # get rid of highly correlated variables
  dplyr::select(gage_ID, any_of(predictors_trimmed)) %>% 
  # add in trends
  dplyr::left_join(df_trends_tau, by = "gage_ID") %>% 
  # add region and class for sampling
  dplyr::left_join(gage_mean[,c("gage_ID", "region", "CLASS")], by = "gage_ID")

## set up train and test sample
# choose fraction of gages to use as validation
frac_test <- 0.2

# need to take sample distributed across regions and ref/nonref
set.seed(1)
test <- 
  data_all %>% 
  dplyr::group_by(region, CLASS) %>% 
  dplyr::sample_frac(frac_test) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Sample = "Test")
train <- 
  data_all %>% 
  dplyr::mutate(Sample = "Train") %>% 
  subset(!(gage_ID %in% test$gage_ID))

fit_data_in <-
  dplyr::bind_rows(test, train) %>% 
  dplyr::select(-CLASS)

# all candidate predictor variables
predictors_final <- 
  fit_data_in %>% 
  dplyr::select(-gage_ID, -Sample, -region, -all_of(paste0("tau_", metrics))) %>% 
  names()

## run random forests
for (m in metrics){
  
  # name for trend
  tau_m <- paste0("tau_", m)
  
  # since you only have one data point for gage, regional models not feasible due to small sample size
  r <- "National"
  
  # collect data
  fit_data_r <- 
    fit_data_in %>% 
    dplyr::select(gage_ID, region, Sample, all_of(tau_m), all_of(predictors_final)) %>% 
    subset(complete.cases(.))
  
  names(fit_data_r)[names(fit_data_r)==tau_m] <- "observed"
  
  # split into training/testing
  fit_data_train <- 
    fit_data_r %>% 
    subset(Sample == "Train")
  fit_data_test <- 
    fit_data_r %>% 
    subset(Sample == "Test")
  
  ## step 1: variable importance
  fit_varimp <- partykit::cforest(
    observed ~ .,
    data = dplyr::select(fit_data_train, -gage_ID, -region, -Sample),
    control = ctree_control(mincriterion = 0.95),
    ntree = 500, 
    applyfun = apply_seeded,
    cores = ncores
  )
  vi <- partykit::varimp(fit_varimp, 
                         conditional = T, 
                         applyfun = apply_seeded,
                         cores = ncores)
  
  fit_varimp_i <- tibble::tibble(predictor = names(vi),
                                 ImpCondPerm = vi) %>% 
    dplyr::arrange(-ImpCondPerm) %>% 
    dplyr::mutate(metric = tau_m)
  
  ## step 2: test number of predictors
  pred_test_range <- 5:length(fit_varimp_i$predictor)
  for (n_pred in pred_test_range){
    
    # get predictor variables
    rf_var_predtest <-
      fit_varimp_i %>% 
      dplyr::top_n(n = n_pred, wt = ImpCondPerm)
    
    # subset
    fit_data_predtest <- 
      fit_data_train %>% 
      dplyr::select(observed, all_of(rf_var_predtest$predictor)) %>%  # drop metrics you aren't interested in
      subset(complete.cases(.))
    
    # fit model
    set.seed(1)
    
    rf_recipe <-
      recipe(observed ~ ., data = fit_data_predtest) %>%
      step_normalize(all_predictors(), -all_outcomes())
    
    rf_engine <- 
      rand_forest() %>% 
      set_engine("ranger", num.threads = ncores) %>% 
      set_mode("regression")
    
    rf_wflow <-
      workflow() %>% 
      add_model(rf_engine) %>% 
      add_recipe(rf_recipe)
    
    rf_fit <- 
      rf_wflow %>% 
      parsnip::fit(data = fit_data_predtest)
    
    if (n_pred == pred_test_range[1]){
      fit_predtest_i <- tibble::tibble(metric = tau_m,
                                       n = n_pred,
                                       OOBmse = pull_workflow_fit(rf_fit)$fit$prediction.error,
                                       OOBr2 = pull_workflow_fit(rf_fit)$fit$r.squared)
    } else {
      fit_predtest_i <- dplyr::bind_rows(fit_predtest_i,
                                         tibble::tibble(metric = tau_m,
                                                        n = n_pred,
                                                        OOBmse = pull_workflow_fit(rf_fit)$fit$prediction.error,
                                                        OOBr2 = pull_workflow_fit(rf_fit)$fit$r.squared))
    }
    
    print(paste0(tau_m, " ", n_pred, " complete"))
  }
  
  # combine
  if (m == metrics[1]){
    fit_varimp_all <- fit_varimp_i
    fit_predtest <- fit_predtest_i
  } else {
    fit_varimp_all <- dplyr::bind_rows(fit_varimp_all, fit_varimp_i)
    fit_predtest <- dplyr::bind_rows(fit_predtest, fit_predtest_i)
  }
  
  # status update
  print(paste0(m, " complete, ", Sys.time()))
  
}

## save data frame used for building RF models
readr::write_csv(fit_data_in, file = file.path("results", "RandomForestTrends_01_RFinputData.csv"))

## save output
readr::write_csv(fit_varimp_all, file = file.path("results", "RandomForestTrends_02_PreliminaryVariableImportance.csv"))
readr::write_csv(fit_predtest, file = file.path("results", "RandomForestTrends_02_NumPredictors.csv"))

# find optimal
fit_predtest %>% 
  dplyr::group_by(metric) %>% 
  dplyr::filter(OOBmse == min(OOBmse))

fit_predtest %>% 
  dplyr::group_by(metric) %>% 
  dplyr::filter(OOBr2 == max(OOBr2))

# final number of predictors
npred_final <- tibble::tibble(metric = c("tau_annualnoflowdays", "tau_zeroflowfirst", "tau_peak2z_length"),
                              npred = c(27, 23, 16))

# plot and look for elbow
ggplot(fit_predtest, aes(x = n, y = OOBmse)) + 
  geom_vline(data = npred_final, aes(xintercept = npred), color = "red") +
  geom_point() + geom_line() +
  scale_x_continuous(name = "Number of Predictors") +
  scale_y_continuous(name = "Random Forest OOB MSE [training gages only]") +
  facet_grid(metric~., scales = "free_y",
             labeller = as_labeller(c("tau_annualnoflowdays" = "\u03c4, Annual\nNo-Flow Days", 
                                      "tau_peak2z_length" = "\u03c4, Days from Peak\nto No Flow",
                                      "tau_zeroflowfirst" = "\u03c4, First\nNo-Flow Day"))) +
  labs(title = "Random forest trend MSE as a function of number of predictors",
       subtitle = "National model for each metric") +
  ggsave(file.path("figures_manuscript", "RandomForestTrends_MSEvNumberPredictors.png"),
         width = 150, height = 150, units = "mm")
