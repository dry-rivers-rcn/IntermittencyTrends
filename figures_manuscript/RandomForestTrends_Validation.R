## RandomForestTrends_Validation.R
# This script will produce a figure showing the validation for the random forest
# model (scatterplot of sim vs obs) and a table with computed fit statistics.
# This model is for predicting trends, not annual values, which is how it differs
#  from the script RandomForest_Validation.R

source(file.path("code", "paths+packages.R"))

## load data
# RF input data
fit_data_in <- 
  readr::read_csv(file = file.path("results", "RandomForestTrends_01_RFinputData.csv")) 

# random forest predictions
rf_all <-
  readr::read_csv(file.path("results", "RandomForestTrends_03_RunModels_Predictions.csv")) %>% 
  dplyr::mutate(residual = predicted - observed) %>% 
  dplyr::left_join(fit_data_in, by = "gage_ID")

## calculate fit statistics
rf_fit <- 
  rf_all %>% 
  subset(Sample == "Test") %>% 
  dplyr::group_by(metric) %>% 
  dplyr::summarize(MAE = round(hydroGOF::mae(predicted, observed), 3),
                   Rsq = round(R2(predicted, observed), 2),
                   RMSE = round(hydroGOF::rmse(predicted, observed), 3),
                   NRMSE = hydroGOF::nrmse(predicted, observed, norm = "maxmin"),
                   KGE = round(hydroGOF::KGE(predicted, observed, method = "2012"), 3)) %>% 
  dplyr::ungroup()

rf_fit$metric[rf_fit$metric == "tau_annualnoflowdays"] <- "Tau, No-Flow Days"
rf_fit$metric[rf_fit$metric == "tau_peak2z_length"] <- "Tau, Days from Peak to No-Flow"
rf_fit$metric[rf_fit$metric == "tau_zeroflowfirst"] <- "Tau, First No-Flow Day"

rf_fit %>% 
  readr::write_csv(file.path("figures_manuscript", "RandomForestTrends_Validation-FitTable.csv"))

## scatterplot
# set factor order
rf_all$Sample <- factor(rf_all$Sample, levels = c("Train", "Test"))

ggplot(rf_all, aes(x = predicted, y = observed)) + 
  geom_hline(yintercept = 0, color = col.gray) +
  geom_vline(xintercept = 0, color = col.gray) +
  geom_point(shape = 1, aes(color = region)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  facet_grid(metric ~ Sample, 
             labeller = 
               as_labeller(c("annualnoflowdays" = "Kendall \u03c4,\nAnnual No-Flow Days",
                             "peak2z_length" = "Kendall \u03c4,\nDays from Peak to No-Flow",
                             "zeroflowfirst" = "Kendall \u03c4,\nFirst No-Flow Day",
                             "Train" = "Train",
                             "Test" = "Test"))) +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_x_continuous(name = "Predicted [Kendall \u03c4]") +
  scale_y_continuous(name = "Observed [Kendall \u03c4]") +
  #coord_equal() +
  theme(legend.position = "bottom") +
  ggsave(file.path("figures_manuscript", "RandomForestTrends_Validation.png"),
         width = 190, height = 180, units = "mm")
  NULL