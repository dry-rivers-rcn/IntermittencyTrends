## 01_RandomForest_PreliminaryVariableImportance.R
#' This script selects the variables that will be used for each random forest model
#' with the following approach:
#'   - Build model with all predictor variables
#'   - Repeat using 80% of reference gages in each sample
#' This variable importance will be used to build models.
#' 
#' A total of 21 models will be built: (6 regions + National) * (3 metrics) = 21 models

source(file.path("code", "paths+packages.R"))
library(tidymodels)
library(ranger)
library(partykit)
library(future.apply)

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
  dplyr::select(c("gage_ID", "currentclimyear", "Sample", all_of(metrics), 
                  all_of(predictors_climate), all_of(predictors_human))) %>% 
  # join with previous water year
  dplyr::left_join(gage_sample_prevyear, 
                   by = c("gage_ID", "currentclimyear"="wyearjoin"), 
                   suffix = c("", ".previous")) %>% 
  # join with static predictors
  dplyr::left_join(gage_sample[ , c("gage_ID", "CLASS", "region", predictors_static)], by = "gage_ID")

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
    "T_max_c_amj.previous", "swe_mm_cy.previous", "swe_mm_jfm",
    "p_mm_cy", "p_mm_jas", "p_mm_amj", "p_mm_cy.previous", "p.pet_jas.previous","p.pet_amj.previous",
    "pet_mm_cy.previous", "pet_mm_jfm", "pet_mm_jfm.previous",
    # static variables
    "sandave", "storage_m", 'maxstorage_af', 
    # near-zero variance
    "swe_mm_jas", "swe_mm_amj", "swe.p_jas", "normstorage_af", "swe_mm_jas.previous",
    "swe_mm_amj.previous", "swe.p_jas.previous")

###
### begin loop through metrics and regions
###

# trimmed predictors list to use in models
predictors_trimmed <- predictors_all[!(predictors_all %in% predictors_drop)]

# number of cores to use
ncores <- (parallel::detectCores() - 1)

## loop through metrics and regions
set.seed(1)
for (m in metrics){
  # subset to complete cases
  fit_data_m <- 
    fit_data_in %>% 
    subset(Sample == "Train") %>% 
    dplyr::select(gage_ID, currentclimyear, region, all_of(m), all_of(predictors_trimmed)) %>% 
    subset(complete.cases(.))
  
  # rename metric column
  names(fit_data_m)[names(fit_data_m) == m] <- "observed"
  
  for (r in regions){
    if (r == "National") {
      fit_data_r <- fit_data_m
    } else {
      fit_data_r <- subset(fit_data_m, region == r)
    }
    
    # extract variable importance - conditional variable importance from party package
    #  useful blog post: https://www.r-bloggers.com/be-aware-of-bias-in-rf-variable-importance-metrics/
    #   - suggests permutation importance as more robust
    #   - this accounts for highly correlated predictor variables, but is super slow
    #  useful slides by authors of party package: https://www.statistik.uni-dortmund.de/useR-2008/slides/Strobl+Zeileis.pdf
    
    fit_rf <- partykit::cforest(
      observed ~ .,
      data = dplyr::select(fit_data_r, -gage_ID, -currentclimyear, -region),
      control = ctree_control(mincriterion = 0.95),
      ntree = 500, 
      applyfun = apply_seeded,
      cores = ncores
    )
    vi <- partykit::varimp(fit_rf, 
                           conditional = T, 
                           applyfun = apply_seeded,
                           cores = ncores)
    
    fit_rf_imp_i <- tibble::tibble(predictor = names(vi),
                                   ImpCondPerm = vi)
    
    # write csv file separately for each metric and region
    fit_rf_imp_i %>% 
      readr::write_csv(file = file.path("results", paste0("01_RandomForest_PreliminaryVariableImportance_", m, "_", gsub(" ", "", r, fixed = TRUE), ".csv")))
    
    # status update
    print(paste0(m, " ", r, " complete, ", Sys.time()))
    
  }
}
