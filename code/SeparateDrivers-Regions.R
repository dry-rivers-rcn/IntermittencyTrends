## SeparateDrivers-Regions.R
# This script calculates the relative impacts of climate and anthropogenic impacts through time.
# General goal is to build statistical relationships for reference gages and extend to nonref gages.

source(file.path("code", "paths+packages.R"))

## load data - gage mean properties and annual values
gage_sample <- 
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv"))

gage_sample_annual <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv")) %>% 
  # add some derived variables
  dplyr::mutate(p.pet_wy = p_mm_wy/pet_mm_wy,
                swe.p_wy = swe_mm_wy/p_mm_wy)

gage_trends <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleTrends.csv"))

## loop through regions
regions <- unique(gage_sample$region)
start_flag <- T
for (r in regions){
  
  # subset data to region
  gage_sample_r <- subset(gage_sample, region == r)
  gage_sample_annual_r <- subset(gage_sample_annual, region == r)
  
  ## clean up data and get ready to fit statistical model
  # predictors and metrics to retain; to get full list of options: 
  #   dput(names(gage_sample_annual_r))
  #   dput(names(gage_sample_r))
  predictors_annual <- c("p.pet_wy", "swe.p_wy", "p_mm_wy", "pet_mm_wy", "swe_mm_wy", "srad_wm2_wy", "pdsi_wy")  # change year to year
  predictors_static <- c("dec_lat_va", "dec_long_va", "DRAIN_SQKM")  # don't change year to year
  metrics <- c("annualfractionnoflow", "zeroflowcentroiddate", "totalnoflowperiods")
  
  fit_data_in <- 
    gage_sample_annual_r %>% 
    # subset to fewer columns - metrics and predictors
    dplyr::select(c("gage_ID", "currentwyear", all_of(metrics), all_of(predictors_annual))) %>% 
    # join with static predictors
    dplyr::left_join(gage_sample_r[ , c("gage_ID", "CLASS", predictors_static)], by = "gage_ID")
  
  # choose a metric
  for (metric in metrics){
    # subset to complete cases and references gages only
    fit_ref_data_in <- 
      fit_data_in %>% 
      dplyr::select(-all_of(metrics[metrics != metric])) %>%  # drop metrics you aren't interested in
      subset(CLASS == "Ref") %>% 
      subset(complete.cases(.))
    
    # rename metric columln
    names(fit_ref_data_in)[names(fit_ref_data_in) == metric] <- "observed"
    
    # select sample
    set.seed(1)
    fit_sample <- sample(1:nrow(fit_ref_data_in), round(nrow(fit_ref_data_in)*0.75))
    
    # build formula
    fit_formula <- as.formula(paste0("observed ~ ", paste(c(predictors_annual, predictors_static), collapse = "+")))
    
    # fit random forest model
    fit_rf <- randomForest::randomForest(fit_formula,
                                         data = fit_ref_data_in,
                                         subset = fit_sample,
                                         ntree = 250)
    
    # predict with random forest
    fit_ref_data_in$predicted <- predict(fit_rf, fit_ref_data_in)
    fit_ref_data_in$sample <- "Test"
    fit_ref_data_in$sample[fit_sample] <- "Train"
    
    # add grouping columns
    fit_ref_data_in$region <- r
    fit_ref_data_in$metric <- metric
    
    # combine into one data frame
    if (start_flag){
      fit_results <- fit_ref_data_in
      start_flag <- F
    } else {
      fit_results <- dplyr::bind_rows(fit_results, fit_ref_data_in)
    }
    
    print(paste0(r, " - ", metric, " complete"))
  }
}

## make plots
for (m in metrics){
  if (m == "annualfractionnoflow") axes_limits <- c(0,1)
  if (m == "zeroflowcentroiddate") axes_limits <- c(0,366)
  if (m == "totalnoflowperiods") axes_limits <- c(0,30)
  ggplot(subset(fit_results, metric == m), aes(x = observed, y = predicted, color = sample)) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(shape = 21) +
    facet_wrap(region ~ ., scales = "free", ncol = 3) +
    scale_x_continuous(name = "Observed", limits = axes_limits, expand = c(0,0)) +
    scale_y_continuous(name = "Predicted", limits = axes_limits, expand = c(0,0)) +
    labs(title = paste0("Predicted vs. observed ", m), 
         subtitle = "Regional random forest models, reference gages") +
    stat_smooth(method = "lm") +
    theme(legend.position = "bottom") +
    ggsave(file.path("results", paste0("SeparateDrivers-Regions_RandomForest_", m, ".png")),
           width = 160, height = 120, units = "mm")
}

## fit statistics
# fit by region
fit_stats <- 
  fit_results %>% 
  subset(sample == "Test") %>% 
  dplyr::group_by(metric, region) %>% 
  dplyr::summarize(RMSE = hydroGOF::rmse(predicted, observed),
                   NRMSE = hydroGOF::nrmse(predicted, observed, norm = "maxmin"),
                   R2 = R2(predicted, observed))

# overall fit
fit_results %>% 
  subset(sample == "Test") %>% 
  dplyr::group_by(metric) %>% 
  dplyr::summarize(RMSE = hydroGOF::rmse(predicted, observed),
                   NRMSE = hydroGOF::nrmse(predicted, observed, norm = "maxmin"),
                   R2 = R2(predicted, observed))
