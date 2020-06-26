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

## clean up data and get ready to fit statistical model
# predictors and metrics to retain; to get full list of options: 
#   dput(names(gage_sample_annual))
#   dput(names(gage_sample))
predictors_annual <- c("p_mm_cy", "p_mm_jas", "p_mm_ond", "p_mm_jfm", "p_mm_amj", "pet_mm_cy", 
                       "pet_mm_jas", "pet_mm_ond", "pet_mm_jfm", "pet_mm_amj", "T_max_c_cy", 
                       "T_max_c_jas", "T_max_c_ond", "T_max_c_jfm", "T_max_c_amj", "T_min_c_cy", 
                       "T_min_c_jas", "T_min_c_ond", "T_min_c_jfm", "T_min_c_amj", "pcumdist10days", 
                       "pcumdist50days", "pcumdist90days", "swe_mm_cy", "swe_mm_jas", 
                       "swe_mm_ond", "swe_mm_jfm", "swe_mm_amj", "srad_wm2_cy", "srad_wm2_jas", 
                       "srad_wm2_ond", "srad_wm2_jfm", "srad_wm2_amj", "pdsi_cy", "pdsi_jas", 
                       "pdsi_ond", "pdsi_jfm", "pdsi_amj", "p.pet_cy", "swe.p_cy", "p.pet_jfm", "swe.p_jfm",
                       "p.pet_amj", "swe.p_amj", "p.pet_jas", "swe.p_jas", "p.pet_ond", "swe.p_ond",
                       "dams_n", "maxstorage_af", 
                       "normstorage_af", "majordams_n", "wuse_mm", "irrig_prc", "harvcrop_prc", 
                       "lulc_water_prc", "lulc_dev_prc", "lulc_forest_prc", "lulc_barren_prc", 
                       "lulc_grass_prc", "lulc_ag_prc", "lulc_wetland_prc")

predictors_static <- c("drain_sqkm", "elev_mean_m_basin", "slope_pct", 
                       "awcave", "permave", "topwet", "depth_bedrock_m", 
                       "porosity", "storage_m", "clayave", "siltave", "sandave")

# previous year predictors will be calculated further down
predictors_annual_with_previous <- 
  c(predictors_annual, c("p_mm_cy.previous", "p_mm_jas.previous", "p_mm_ond.previous", 
                         "p_mm_jfm.previous", "p_mm_amj.previous", "pet_mm_cy.previous", 
                         "pet_mm_jas.previous", "pet_mm_ond.previous", "pet_mm_jfm.previous", 
                         "pet_mm_amj.previous", "T_max_c_cy.previous", "T_max_c_jas.previous", 
                         "T_max_c_ond.previous", "T_max_c_jfm.previous", "T_max_c_amj.previous", 
                         "T_min_c_cy.previous", "T_min_c_jas.previous", "T_min_c_ond.previous", 
                         "T_min_c_jfm.previous", "T_min_c_amj.previous", "pcumdist10days.previous", 
                         "pcumdist50days.previous", "pcumdist90days.previous", "swe_mm_cy.previous", 
                         "swe_mm_jas.previous", "swe_mm_ond.previous", "swe_mm_jfm.previous", 
                         "swe_mm_amj.previous", "srad_wm2_cy.previous", "srad_wm2_jas.previous", 
                         "srad_wm2_ond.previous", "srad_wm2_jfm.previous", "srad_wm2_amj.previous", 
                         "pdsi_cy.previous", "pdsi_jas.previous", "pdsi_ond.previous", 
                         "pdsi_jfm.previous", "pdsi_amj.previous", "p.pet_cy.previous", 
                         "swe.p_cy.previous", "p.pet_jfm.previous", "swe.p_jfm.previous", 
                         "p.pet_amj.previous", "swe.p_amj.previous", "p.pet_jas.previous", 
                         "swe.p_jas.previous", "p.pet_ond.previous", "swe.p_ond.previous"))

# metrics and regions to predict
metrics <- c("annualfractionnoflow", "zeroflowfirst", "peak2z_length")
regions <- c("National", unique(gage_sample$region))

# get previous water year climate metrics
gage_sample_prevyear <- 
  gage_sample_annual[,c("gage_ID", "currentclimyear", predictors_annual)] %>% 
  dplyr::mutate(wyearjoin = currentclimyear + 1) %>% 
  dplyr::select(-currentclimyear)

# combine into one data frame
fit_data_in <- 
  gage_sample_annual %>% 
  # subset to fewer columns - metrics and predictors
  dplyr::select(c("gage_ID", "currentclimyear", all_of(metrics), all_of(predictors_annual))) %>% 
  # join with previous water year
  dplyr::left_join(gage_sample_prevyear, 
                   by = c("gage_ID", "currentclimyear"="wyearjoin"), 
                   suffix = c("", ".previous")) %>% 
  # join with static predictors
  dplyr::left_join(gage_sample[ , c("gage_ID", "CLASS", "region", predictors_static)], by = "gage_ID")

# number of iterations and percent of gages to sample each iteration
n_iter <- 50
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
  fit_formula <- as.formula(paste0("observed ~ ", paste(c(predictors_annual_with_previous, predictors_static), collapse = "+")))
  
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
                                           localImp = T)
      
      # extract variable importance
      fit_rf_imp_i <- tibble::tibble(predictor = rownames(randomForest::importance(fit_rf, type = 1)),
                                     PrcIncMSE = randomForest::importance(fit_rf, type = 1)[,'%IncMSE'],
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
