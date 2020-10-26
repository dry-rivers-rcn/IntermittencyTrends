## 02_RandomForest_FigureOutNumPredictors.R
#' This script is intended to figure out the parsimonious number of predictors to use
#' in random forest models.
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

## test number of predictors to use in final models
pred_test_range <- 10:54  # 54 total predictors possible

# set up model engine
rf_engine <- 
  rand_forest() %>% 
  set_engine("ranger", num.threads = (parallel::detectCores() - 1)) %>% 
  set_mode("regression")

for (m in metrics){
  rf_var_importance <- 
    file.path("results", paste0("01_RandomForest_PreliminaryVariableImportance_", m, "_National.csv")) %>% 
    readr::read_csv()
  
  for (n_pred in pred_test_range){
    
    # get predictor variables
    rf_var_predtest <-
      rf_var_importance %>% 
      dplyr::top_n(n = n_pred, wt = ImpCondPerm)
    
    # subset
    fit_data_predtest <- 
      fit_data_in %>% 
      subset(Sample == "Train") %>% 
      dplyr::select(all_of(m), all_of(rf_var_predtest$predictor)) %>%  # drop metrics you aren't interested in
      subset(complete.cases(.))
    
    # rename metric column
    names(fit_data_predtest)[names(fit_data_predtest) == m] <- "observed"
    
    # fit model
    set.seed(1)
    
    rf_recipe <-
      recipe(observed ~ ., data = fit_data_predtest) %>%
      step_normalize(all_predictors(), -all_outcomes())
    
    rf_wflow <-
      workflow() %>% 
      add_model(rf_engine) %>% 
      add_recipe(rf_recipe)
    
    rf_fit <- 
      rf_wflow %>% 
      parsnip::fit(data = fit_data_predtest)
    
    if (n_pred == pred_test_range[1] & m == metrics[1]){
      fit_predtest <- tibble::tibble(metric = m,
                                     n = n_pred,
                                     OOBmse = pull_workflow_fit(rf_fit)$fit$prediction.error,
                                     OOBr2 = pull_workflow_fit(rf_fit)$fit$r.squared)
    } else {
      fit_predtest <- dplyr::bind_rows(fit_predtest,
                                       tibble::tibble(metric = m,
                                                      n = n_pred,
                                                      OOBmse = pull_workflow_fit(rf_fit)$fit$prediction.error,
                                                      OOBr2 = pull_workflow_fit(rf_fit)$fit$r.squared))
    }
    
    print(paste0(m, " ", n_pred, " complete"))
  }
}

# save results
fit_predtest %>% 
  readr::write_csv(path = file.path("results", "02_RandomForest_FigureOutNumPredictors.csv"))

# find optimal
fit_predtest %>% 
  dplyr::group_by(metric) %>% 
  dplyr::filter(OOBmse == min(OOBmse))

fit_predtest %>% 
  dplyr::group_by(metric) %>% 
  dplyr::filter(OOBr2 == max(OOBr2))

# final number of predictors
npred_final <- tibble::tibble(metric = c("annualnoflowdays", "zeroflowfirst", "peak2z_length"),
                              npred = c(22, 27, 27))

# plot and look for elbow
ggplot(fit_predtest, aes(x = n, y = OOBmse)) + 
  geom_vline(data = npred_table, aes(xintercept = npred), color = "red") +
  geom_point() + geom_line() +
  scale_x_continuous(name = "Number of Predictors") +
  scale_y_continuous(name = "Random Forest OOB MSE [training gages only]") +
  facet_grid(metric~., scales = "free_y",
             labeller = as_labeller(c("annualnoflowdays" = "Annual No-Flow\nDays", 
                                      "peak2z_length" = "Days from Peak\nto No Flow",
                                      "zeroflowfirst" = "First No-Flow Day\n[climate year]"))) +
  labs(title = "Random forest MSE as a function of number of predictors",
       subtitle = "National model for each metric") +
  ggsave(file.path("figures_manuscript", "RandomForest_MSEvNumberPredictors.png"),
         width = 150, height = 150, units = "mm")

ggplot(fit_predtest, aes(x = n, y = OOBr2)) + 
  geom_point() + geom_line() +
  scale_x_continuous(name = "Number of Predictors") +
  scale_y_continuous(name = "Random Forest OOB R2 [training gages only]") +
  facet_grid(metric~., scales = "free_y",
             labeller = as_labeller(c("annualfractionnoflow" = "Annual Fraction\nZero Flow [-]", 
                                      "peak2z_length" = "Peak-to-Zero\nLength [days]",
                                      "zeroflowfirst" = "First Zero Flow\nDay [climate year]"))) +
  labs(title = "Random forest R2 as a function of number of predictors",
       subtitle = "National model for each metric")

ggplot(fit_predtest, aes(x = OOBmse, y = OOBr2)) + 
  geom_point() +
  scale_x_continuous(name = "Random Forest OOB MSE [training gages only]") +
  scale_y_continuous(name = "Random Forest OOB R2 [training gages only]") +
  facet_wrap(metric~., scales = "free",
             labeller = as_labeller(c("annualfractionnoflow" = "Annual Fraction\nZero Flow [-]", 
                                      "peak2z_length" = "Peak-to-Zero\nLength [days]",
                                      "zeroflowfirst" = "First Zero Flow\nDay [climate year]")))
