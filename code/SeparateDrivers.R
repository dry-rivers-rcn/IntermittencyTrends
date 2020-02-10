## SeparateDrivers.R
# This script calculates the relative impacts of climate and anthropogenic impacts through time.
# General goal is to build statistical relationships for reference gages and extend to nonref gages.

source(file.path("code", "paths+packages.R"))

## load data - gage mean properties and annual values
gage_sample <- 
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv"))

gage_sample_annual <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv")) %>% 
  dplyr::rename(p_pet = `p/pet`)  # need to rename to remove slash

gage_trends <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleTrends.csv"))

## test on a single region
r <- "South Great Plains"

# subset data to region
gage_sample_r <- subset(gage_sample, region == r)
gage_sample_annual_r <- subset(gage_sample_annual, region == r)

## clean up data and get ready to fit statistical model
# predictors and metrics to retain; to get full list of options: 
#   dput(names(gage_sample_annual_r))
#   dput(names(gage_sample_r))
predictors_annual <- c("p_pet", "p_mm_wy", "pet_mm_wy", "swe_mm_wy", "srad_wm2_wy", "pdsi_wy")  # change year to year
predictors_static <- c("dec_lat_va", "dec_long_va", "DRAIN_SQKM")  # don't change year to year
metrics <- c("annualfractionnoflow", "zeroflowcentroiddate", "totalnoflowperiods")

fit_data_in <- 
  gage_sample_annual_r %>% 
  # subset to fewer columns - metrics and predictors
  dplyr::select(c("gage_ID", "currentwyear", all_of(metrics), all_of(predictors_annual))) %>% 
  # join with static predictors
  dplyr::left_join(gage_sample_r[ , c("gage_ID", "CLASS", predictors_static)], by = "gage_ID") %>% 
  # there are some NAs in the SWE data - remove for now
  subset(is.finite(swe_mm_wy))

# subset to reference gages
fit_ref_data_in <- 
  fit_data_in %>% 
  subset(CLASS == "Ref")

# select sample
fit_sample <- sample(1:nrow(fit_ref_data_in), round(nrow(fit_ref_data_in)*0.75))

# build formula
fit_formula <- as.formula(paste0(metrics[1], " ~ ", paste(c(predictors_annual, predictors_static), collapse = "+")))

# fit random forest model
fit_rf <- randomForest::randomForest(fit_formula,
                                     data = fit_ref_data_in,
                                     subset = fit_sample)

# predict with random forest
fit_ref_data_in$rf_prediction <- predict(fit_rf, fit_ref_data_in)
fit_ref_data_in$train <- FALSE
fit_ref_data_in$train[fit_sample] <- TRUE

ggplot(fit_ref_data_in, aes(x = annualfractionnoflow, y = rf_prediction, color = train)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  stat_smooth(method = "lm")