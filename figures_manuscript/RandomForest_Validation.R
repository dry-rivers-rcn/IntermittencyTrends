## Figure+Table_RandomForest_Validation.R
# This script will produce a figure showing the validation for the random forest
# model (scatterplot of sim vs obs) and a table with computed fit statistics.

source(file.path("code", "paths+packages.R"))

## load data
# annual metrics to extract test or train
gage_sample_annual <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv")) %>% 
  dplyr::select(gage_ID, currentclimyear, Sample)

# regions
gage_regions <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv")) %>% 
  dplyr::select(gage_ID, region)

# random forest predictions
rf_all <-
  readr::read_csv(file.path("results", "04_RandomForest_RunModels_Predictions.csv")) %>% 
  dplyr::mutate(residual = predicted - observed) %>% 
  dplyr::left_join(gage_sample_annual, by = c("gage_ID", "currentclimyear")) %>% 
  dplyr::left_join(gage_regions, by = "gage_ID")

rf_all$region_rf[rf_all$region_rf != "National"] <- "Regional"

## calculate fit statistics
rf_fit_regional <-
  rf_all %>% 
  subset(Sample == "Test") %>% 
  dplyr::group_by(metric, region_rf, region) %>% 
  dplyr::summarize(MAE = round(hydroGOF::mae(predicted, observed), 3),
                   Rsq = round(R2(predicted, observed), 2),
                   RMSE = round(hydroGOF::rmse(predicted, observed), 3),
                   NRMSE = hydroGOF::nrmse(predicted, observed, norm = "maxmin"),
                   KGE = round(hydroGOF::KGE(predicted, observed, method = "2012"), 3)) %>% 
  dplyr::ungroup()

rf_fit_national <- 
  rf_all %>% 
  subset(Sample == "Test") %>% 
  dplyr::group_by(metric, region_rf) %>% 
  dplyr::summarize(MAE = round(hydroGOF::mae(predicted, observed), 3),
                   Rsq = round(R2(predicted, observed), 2),
                   RMSE = round(hydroGOF::rmse(predicted, observed), 3),
                   NRMSE = hydroGOF::nrmse(predicted, observed, norm = "maxmin"),
                   KGE = round(hydroGOF::KGE(predicted, observed, method = "2012"), 3)) %>% 
  dplyr::ungroup()

rf_fit <- 
  dplyr::bind_rows(rf_fit_regional, rf_fit_national) %>% 
  dplyr::arrange(metric, region_rf, region)

rf_fit$metric[rf_fit$metric == "annualnoflowdays"] <- "Annual No-Flow Days"
rf_fit$metric[rf_fit$metric == "peak2z_length"] <- "Days from Peak to No-Flow"
rf_fit$metric[rf_fit$metric == "zeroflowfirst"] <- "First No-Flow Day"

rf_fit %>% 
  readr::write_csv(file.path("figures_manuscript", "RandomForest_Validation-FitTable.csv"))

# plot model error: national vs regional models
rf_fit_wide <-
  rf_fit %>% 
  dplyr::select(metric, region_rf, region, RMSE) %>% 
  tidyr::pivot_wider(id_cols = c("metric", "region"),
                     names_from = "region_rf",
                     values_from = "RMSE")

ggplot(rf_fit_wide, aes(x = National, y = Regional, color = region)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  facet_wrap(~metric, scales = "free") +
  scale_x_continuous(name = "National Model") +
  scale_y_continuous(name = "Regional Model") +
  scale_color_manual(name = "Region", values = pal_regions) +
  labs(title = "Validation RMSE from regional and national random forest models") +
  theme(legend.position = "bottom") +
  ggsave(file.path("figures_manuscript", "RandomForest_Validation-CompareNationalRegionFit.png"),
         width = 190, height = 100, units = "mm")

## scatterplot: national models, test data only
p_nat_afnf_test <-
  rf_all %>% 
  subset(region_rf == "National" & metric == "annualnoflowdays" & Sample == "Test") %>% 
  ggplot(aes(x = predicted, y = observed)) + 
  geom_point(shape = 1, aes(color = region)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_x_continuous(name = "Predicted [days]",
                     limits = c(0,366), expand = c(0,0)) +
  scale_y_continuous(name = "Observed [days]",
                     limits = c(0,366), expand = c(0,0)) +
  labs(title = "(a) Annual No-Flow Days") +
  NULL

p_nat_p2z_test <-
  rf_all %>% 
  subset(region_rf == "National" & metric == "peak2z_length" & Sample == "Test") %>% 
  ggplot(aes(x = predicted, y = observed)) + 
  geom_point(shape = 1, aes(color = region)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_x_continuous(name = "Predicted [days]",
                     limits = c(0,225), expand = c(0,0)) +
  scale_y_continuous(name = "Observed [days]",
                     limits = c(0,225), expand = c(0,0)) +
  labs(title = "(b) Peak to No-Flow") +
  NULL

p_nat_zff_test <-
  rf_all %>% 
  subset(region_rf == "National" & metric == "zeroflowfirst" & Sample == "Test") %>% 
  ggplot(aes(x = predicted, y = observed)) + 
  geom_point(shape = 1, aes(color = region)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_x_continuous(name = "Predicted [day of year]",
                     limits = c(0,366), expand = c(0,0)) +
  scale_y_continuous(name = "Observed [day of year]",
                     limits = c(0,366), expand = c(0,0)) +
  labs(title = "(c) First No-Flow") +
  NULL

((p_nat_afnf_test + p_nat_p2z_test + p_nat_zff_test) + 
    plot_layout(guides = 'collect') & 
    theme(legend.position = "bottom",
          plot.title = element_text(face = "plain"))) +
  ggsave(file.path("figures_manuscript", "RandomForest_Validation-National-Test.png"),
         width = 190, height = 85, units = "mm")

## scatterplot: Regional models, test data only
p_reg_afnf_test <-
  rf_all %>% 
  subset(region_rf != "National" & metric == "annualnoflowdays" & Sample == "Test") %>% 
  ggplot(aes(x = predicted, y = observed)) + 
  geom_point(shape = 1, aes(color = region)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_x_continuous(name = "Predicted [days]",
                     limits = c(0,366), expand = c(0,0)) +
  scale_y_continuous(name = "Observed [days]",
                     limits = c(0,366), expand = c(0,0)) +
  labs(title = "(a) Annual No-Flow Days") +
  NULL

p_reg_p2z_test <-
  rf_all %>% 
  subset(region_rf != "National" & metric == "peak2z_length" & Sample == "Test") %>% 
  ggplot(aes(x = predicted, y = observed)) + 
  geom_point(shape = 1, aes(color = region)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_x_continuous(name = "Predicted [days]",
                     limits = c(0,225), expand = c(0,0)) +
  scale_y_continuous(name = "Observed [days]",
                     limits = c(0,225), expand = c(0,0)) +
  labs(title = "(b) Peak to No-Flow") +
  NULL

p_reg_zff_test <-
  rf_all %>% 
  subset(region_rf != "National" & metric == "zeroflowfirst" & Sample == "Test") %>% 
  ggplot(aes(x = predicted, y = observed)) + 
  geom_point(shape = 1, aes(color = region)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_x_continuous(name = "Predicted [day of year]",
                     limits = c(0,366), expand = c(0,0)) +
  scale_y_continuous(name = "Observed [day of year]",
                     limits = c(0,366), expand = c(0,0)) +
  labs(title = "(c) First No-Flow") +
  NULL

((p_reg_afnf_test + p_reg_p2z_test + p_reg_zff_test) + 
    plot_layout(guides = 'collect') & 
    theme(legend.position = "bottom",
          plot.title = element_text(face = "plain"))) +
  ggsave(file.path("figures_manuscript", "RandomForest_Validation-Regional-Test.png"),
         width = 190, height = 85, units = "mm")

## scatterplot: national models, Train data only
p_nat_afnf_Train <-
  rf_all %>% 
  subset(region_rf == "National" & metric == "annualnoflowdays" & Sample == "Train") %>% 
  ggplot(aes(x = predicted, y = observed)) + 
  geom_point(shape = 1, aes(color = region)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_x_continuous(name = "Predicted [days]",
                     limits = c(0,366), expand = c(0,0)) +
  scale_y_continuous(name = "Observed [days]",
                     limits = c(0,366), expand = c(0,0)) +
  labs(title = "(a) Annual No-Flow Days") +
  NULL

p_nat_p2z_Train <-
  rf_all %>% 
  subset(region_rf == "National" & metric == "peak2z_length" & Sample == "Train") %>% 
  ggplot(aes(x = predicted, y = observed)) + 
  geom_point(shape = 1, aes(color = region)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_x_continuous(name = "Predicted [days]",
                     limits = c(0,225), expand = c(0,0)) +
  scale_y_continuous(name = "Observed [days]",
                     limits = c(0,225), expand = c(0,0)) +
  labs(title = "(b) Peak to No-Flow") +
  NULL

p_nat_zff_Train <-
  rf_all %>% 
  subset(region_rf == "National" & metric == "zeroflowfirst" & Sample == "Train") %>% 
  ggplot(aes(x = predicted, y = observed)) + 
  geom_point(shape = 1, aes(color = region)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_x_continuous(name = "Predicted [day of year]",
                     limits = c(0,366), expand = c(0,0)) +
  scale_y_continuous(name = "Observed [day of year]",
                     limits = c(0,366), expand = c(0,0)) +
  labs(title = "(c) First No-Flow") +
  NULL

((p_nat_afnf_Train + p_nat_p2z_Train + p_nat_zff_Train) + 
    plot_layout(guides = 'collect') & 
    theme(legend.position = "bottom",
          plot.title = element_text(face = "plain"))) +
  ggsave(file.path("figures_manuscript", "RandomForest_Validation-National-Train.png"),
         width = 190, height = 85, units = "mm")

## scatterplot: Regional models, Train data only
p_reg_afnf_Train <-
  rf_all %>% 
  subset(region_rf != "National" & metric == "annualnoflowdays" & Sample == "Train") %>% 
  ggplot(aes(x = predicted, y = observed)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(shape = 1, aes(color = region)) +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_x_continuous(name = "Predicted [days]",
                     limits = c(0,366), expand = c(0,0)) +
  scale_y_continuous(name = "Observed [days]",
                     limits = c(0,366), expand = c(0,0)) +
  labs(title = "(a) Annual No-Flow Days") +
  NULL

p_reg_p2z_Train <-
  rf_all %>% 
  subset(region_rf != "National" & metric == "peak2z_length" & Sample == "Train") %>% 
  ggplot(aes(x = predicted, y = observed)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(shape = 1, aes(color = region)) +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_x_continuous(name = "Predicted [days]",
                     limits = c(0,225), expand = c(0,0)) +
  scale_y_continuous(name = "Observed [days]",
                     limits = c(0,225), expand = c(0,0)) +
  labs(title = "(b) Peak to No-Flow") +
  NULL

p_reg_zff_Train <-
  rf_all %>% 
  subset(region_rf != "National" & metric == "zeroflowfirst" & Sample == "Train") %>% 
  ggplot(aes(x = predicted, y = observed)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(shape = 1, aes(color = region)) +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_x_continuous(name = "Predicted [day of year]",
                     limits = c(0,366), expand = c(0,0)) +
  scale_y_continuous(name = "Observed [day of year]",
                     limits = c(0,366), expand = c(0,0)) +
  labs(title = "(c) First No-Flow") +
  NULL

((p_reg_afnf_Train + p_reg_p2z_Train + p_reg_zff_Train) + 
    plot_layout(guides = 'collect') & 
    theme(legend.position = "bottom",
          plot.title = element_text(face = "plain"))) +
  ggsave(file.path("figures_manuscript", "RandomForest_Validation-Regional-Train.png"),
         width = 190, height = 85, units = "mm")
