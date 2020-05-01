## 01_RandomForest_PreliminaryVariableImportance.R
#' This script selects the variables that will be used for each random forest model
#' with the following approach:
#'   - Build model with all predictor variables
#'   - Repeat 25x using 80% of reference gages in each sample
#' This variable importance will be used to build models.
#' 
#' A total of 21 models will be built: (6 regions + National) * (3 metrics) = 21 models

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

## clean up data and get ready to fit statistical model
# predictors and metrics to retain; to get full list of options: 
#   dput(names(gage_sample_annual))
#   dput(names(gage_sample))
predictors_annual <- c("p_mm_wy", "p_mm_jja", "p_mm_son", "p_mm_djf", "p_mm_mam", "pet_mm_wy", 
                       "pet_mm_jja", "pet_mm_son", "pet_mm_djf", "pet_mm_mam", "T_max_c_wy", 
                       "T_max_c_jja", "T_max_c_son", "T_max_c_djf", "T_max_c_mam", "T_min_c_wy", 
                       "T_min_c_jja", "T_min_c_son", "T_min_c_djf", "T_min_c_mam", "pcumdist10days", 
                       "pcumdist50days", "pcumdist90days", "swe_mm_wy", "swe_mm_jja", 
                       "swe_mm_son", "swe_mm_djf", "swe_mm_mam", "srad_wm2_wy", "srad_wm2_jja", 
                       "srad_wm2_son", "srad_wm2_djf", "srad_wm2_mam", "pdsi_wy", "pdsi_jja", 
                       "pdsi_son", "pdsi_djf", "pdsi_mam", "p.pet_wy", "swe.p_wy", "p.pet_djf", "swe.p_djf",
                       "p.pet_mam", "swe.p_mam", "p.pet_jja", "swe.p_jja", "p.pet_son", "swe.p_son")

predictors_static <- c("DRAIN_SQKM", "ELEV_MEAN_M_BASIN", "SLOPE_PCT", "AWCAVE", "PERMAVE", 
                       "TOPWET", "depth_bedrock_m", "CLAYAVE", "SILTAVE", "SANDAVE")

# metrics and regions to predict
metrics <- c("annualfractionnoflow", "firstnoflowcaly", "peak2z_length")
regions <- c("National", unique(gage_sample$region))

# combine into one data frame
fit_data_in <- 
  gage_sample_annual %>% 
  # subset to fewer columns - metrics and predictors
  dplyr::select(c("gage_ID", "currentwyear", all_of(metrics), all_of(predictors_annual))) %>% 
  # join with static predictors
  dplyr::left_join(gage_sample[ , c("gage_ID", "CLASS", "region", predictors_static)], by = "gage_ID")

# number of iterations and percent of gages to sample each iteration
n_iter <- 25
prc_sample <- 0.8

## loop through metrics and regions
for (m in metrics){
  # subset to complete cases and references gages only
  fit_data_m <- 
    fit_data_in %>% 
    dplyr::select(-all_of(metrics[metrics != m])) %>%  # drop metrics you aren't interested in
    subset(CLASS == "Ref") %>% 
    subset(complete.cases(.))
  
  # rename metric column
  names(fit_data_m)[names(fit_data_m) == m] <- "observed"
  
  # build formula
  fit_formula <- as.formula(paste0("observed ~ ", paste(c(predictors_annual, predictors_static), collapse = "+")))
  
  for (r in regions){
    if (r == "National") {
      fit_data_r <- fit_data_m
    } else {
      fit_data_r <- subset(fit_data_m, region == r)
    }
    
    gages_r <- unique(fit_data_r$gage_ID)
    
    set.seed(1)
    for (iter in 1:n_iter){
      # select gages
      gages_i <- sample(gages_r, size = floor(length(gages_r)*prc_sample))
      
      # fit model
      fit_rf <- randomForest::randomForest(fit_formula,
                                           data = subset(fit_data_r, gage_ID %in% gages_i),
                                           ntree = 500,
                                           importance = T)
      
      # extract variable importance
      fit_rf_imp_i <- tibble::tibble(predictor = rownames(fit_rf$importance),
                                     IncMSE = fit_rf$importance[,'%IncMSE'],
                                     metric = m,
                                     region = r,
                                     iteration = iter)
      if (iter == 1 & m == metrics[1] & r == regions[1]){
        fit_rf_imp <- fit_rf_imp_i
      } else {
        fit_rf_imp <- dplyr::bind_rows(fit_rf_imp, fit_rf_imp_i)
      }
      
      # status update
      print(paste0(m, " ", r, " ", iter, " complete, ", Sys.time()))
    }
  }
}

# write csv file
fit_rf_imp %>% 
  readr::write_csv(path = file.path("results", "01_RandomForest_PreliminaryVariableImportance.csv"))
