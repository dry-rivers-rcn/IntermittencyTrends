## 02_RandomForest_RunModels.R
#' This script is intended to train and run random forest models.
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

rf_var_importance <- 
  file.path("results", "01_RandomForest_PreliminaryVariableImportance.csv") %>% 
  readr::read_csv() %>% 
  dplyr::group_by(metric, region, predictor) %>% 
  dplyr::summarize(PrcIncMSE_mean = mean(PrcIncMSE)) %>% 
  dplyr::ungroup()

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

predictors_human <- c("dams_n", "maxstorage_af", 
                      "normstorage_af", "majordams_n", "wuse_mm", "irrig_prc", 
                      "lulc_water_prc", "lulc_dev_prc", "lulc_forest_prc", "lulc_barren_prc", 
                      "lulc_grass_prc", "lulc_ag_prc", "lulc_wetland_prc")

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
  dplyr::select(c("gage_ID", "currentclimyear", all_of(metrics), 
                  all_of(predictors_climate), all_of(predictors_human))) %>% 
  # join with previous water year
  dplyr::left_join(gage_sample_prevyear, 
                   by = c("gage_ID", "currentclimyear"="wyearjoin"), 
                   suffix = c("", ".previous")) %>% 
  # join with static predictors
  dplyr::left_join(gage_sample[ , c("gage_ID", "CLASS", "Sample", "region", predictors_static)], by = "gage_ID")

## loop through metrics and regions
n_pred <- 10 # choose number of predictors - based on script 02_RandomForest_FigureOutNumPredictors.R
n_folds <- length(unique(gage_sample$Sample)) # choose number of folds for cross-val

for (m in metrics){
  # get rid of unneeded metrics
  fit_data_m <- 
    fit_data_in %>% 
    dplyr::select(-all_of(metrics[metrics != m]))
  
  # rename metric column
  names(fit_data_m)[names(fit_data_m) == m] <- "observed"
  
  for (r in regions){
    # get predictor variables
    rf_var_m_r <-
      rf_var_importance %>% 
      subset(metric == m & region == r) %>% 
      dplyr::top_n(n = n_pred, wt = PrcIncMSE_mean)
    
    if (r == "National") {
      fit_data_r <- 
        fit_data_m %>% 
        dplyr::select(gage_ID, CLASS, Sample, currentclimyear, observed, region, all_of(rf_var_m_r$predictor)) %>% 
        subset(complete.cases(.))
    } else {
      fit_data_r <- 
        fit_data_m %>% 
        subset(region == r) %>% 
        dplyr::select(gage_ID, CLASS, Sample, currentclimyear, observed, region, all_of(rf_var_m_r$predictor)) %>% 
        subset(complete.cases(.))
    }
    
    fit_formula <- as.formula(paste0("observed ~ ", paste(unique(rf_var_m_r$predictor), collapse = "+")))
    
    ## k-folds cross-validation and prediction
    set.seed(1)
    for (k in 1:n_folds){
      ## train model
      fit_rf <- randomForest::randomForest(fit_formula,
                                           data = subset(fit_data_r, Sample != paste0("Test", k)),
                                           ntree = 500,
                                           localImp = T)
      
      # run model
      fit_data_r$predicted <- predict(fit_rf, fit_data_r)
      fit_data_r$metric <- m
      fit_data_r$region_rf <- r
      fit_data_i <- 
        fit_data_r %>% 
        dplyr::select(gage_ID, CLASS, currentclimyear, observed, predicted, metric, region_rf) %>% 
        dplyr::mutate(kfold = k)
      
      # extract variable importance
      fit_rf_imp_i <- tibble::tibble(predictor = rownames(randomForest::importance(fit_rf, type = 1)),
                                     VarPrcIncMSE = randomForest::importance(fit_rf, type = 1)[,'%IncMSE'],
                                     metric = m,
                                     region_rf = r,
                                     kfold = k)
      
      if (m == metrics[1] & r == regions[1] & k == 1){
        fit_data_out <- fit_data_i
        fit_rf_imp <- fit_rf_imp_i
      } else {
        fit_data_out <- dplyr::bind_rows(fit_data_out, fit_data_i)
        fit_rf_imp <- dplyr::bind_rows(fit_rf_imp, fit_rf_imp_i)
      }
      
      # status update
      print(paste0(m, " ", r, " k", k, " complete, ", Sys.time()))
      
    }
  }
}

# save data
fit_data_out %>% 
  readr::write_csv(file.path("results", "03_RandomForest_RunModels_Predictions.csv"))

fit_rf_imp %>% 
  readr::write_csv(file.path("results", "03_RandomForest_RunModels_VariableImportance.csv"))

# plots
min(subset(fit_data_out, metric == "annualfractionnoflow")$predicted)

ggplot(subset(fit_data_out, metric == "annualfractionnoflow" & Sample == "Test"), 
       aes(x = predicted, y = observed, color = region)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf)

ggplot(subset(fit_data_out, metric == "annualfractionnoflow"), 
       aes(x = predicted, y = observed, color = region)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf)

ggplot(subset(fit_data_out, metric == "annualfractionnoflow" & Sample == "Test"), 
       aes(x = currentclimyear, y = (predicted - observed), color = region)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_point() +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf)

ggplot(subset(fit_rf_imp, metric == "annualfractionnoflow"), 
       aes(x = predictor, y = VarPrcIncMSE, color = region_rf)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_point() +
  facet_wrap(~region_rf)
