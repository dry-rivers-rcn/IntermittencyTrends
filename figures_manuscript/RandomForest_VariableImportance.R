## RandomForest_VariableImportance.R
# This script will plot the importance of different predictor variables
# for the random forest models.

source(file.path("code", "paths+packages.R"))

# variable importance
rf_imp <- 
  readr::read_csv(file.path("results", "04_RandomForest_RunModels_VariableImportance.csv")) %>% 
  dplyr::left_join(df_pred, by = "predictor") # join to data frame with predictor variable names and categories

# partial dependence
fit_pdp_out <- 
  readr::read_csv(file.path("results", "04_RandomForest_RunModels_PartialDependence.csv"))

## grab selected predictors to show
n_pred <- 9

# ranked predictors for each
imp_anf <- 
  rf_imp %>% 
  subset(region_rf == "National" & metric == "annualnoflowdays") %>% 
  dplyr::slice_max(order_by = IncMSE, n = n_pred) %>% 
  dplyr::arrange(-IncMSE) %>% 
  dplyr::mutate(Predictor = factor(long_name, levels = long_name))

imp_p2z <- 
  rf_imp %>% 
  subset(region_rf == "National" & metric == "peak2z_length") %>% 
  dplyr::slice_max(order_by = IncMSE, n = n_pred) %>% 
  dplyr::arrange(-IncMSE) %>% 
  dplyr::mutate(Predictor = factor(long_name, levels = long_name))

imp_zff <- 
  rf_imp %>% 
  subset(region_rf == "National" & metric == "zeroflowfirst") %>% 
  dplyr::slice_max(order_by = IncMSE, n = n_pred) %>% 
  dplyr::arrange(-IncMSE) %>% 
  dplyr::mutate(Predictor = factor(long_name, levels = long_name))

# extract pdp
pdp_anf <- 
  fit_pdp_out %>% 
  subset(region == "National" & metric == "annualnoflowdays") %>% 
  subset(predictor %in% imp_anf$predictor) %>% 
  dplyr::select(-region, -metric) %>% 
  dplyr::left_join(imp_anf, by = "predictor")

pdp_p2z <- 
  fit_pdp_out %>% 
  subset(region == "National" & metric == "peak2z_length") %>% 
  subset(predictor %in% imp_p2z$predictor) %>% 
  dplyr::select(-region, -metric) %>% 
  dplyr::left_join(imp_p2z, by = "predictor")

pdp_zff <- 
  fit_pdp_out %>% 
  subset(region == "National" & metric == "zeroflowfirst") %>% 
  subset(predictor %in% imp_zff$predictor) %>% 
  dplyr::select(-region, -metric) %>% 
  dplyr::left_join(imp_zff, by = "predictor")
  
## plots - variable importance
p_nat_anf <-
  ggplot(imp_anf, aes(x = Predictor, y = IncMSE/oobMSE, fill = Category)) +
  geom_col() +
  scale_fill_manual(drop = F,
                    values = c("Climate" = col.cat.red, 
                               "Physiography" = col.cat.blu,
                               "Land Use" = col.cat.grn),
                    labels = c("Climate" = "Climate", 
                               "Physiography" = "Physiography",
                               "Land Use" = "Land/Water Use")) +
  scale_y_continuous(name = "MSE Increase [%]", 
                     breaks = seq(0,1,0.5),
                     labels = scales::percent) +
  scale_x_discrete(limits = rev(levels(imp_anf$Predictor))) +
  coord_flip() +
  labs(title = "(a) No-Flow Days") +
  theme(axis.title.y = element_blank()) +
  NULL

p_nat_p2z <-
  ggplot(imp_p2z, aes(x = Predictor, y = IncMSE/oobMSE, fill = Category)) +
  geom_col() +
  scale_fill_manual(drop = F,
                    values = c("Climate" = col.cat.red, 
                               "Physiography" = col.cat.blu,
                               "Land Use" = col.cat.grn),
                    labels = c("Climate" = "Climate", 
                               "Physiography" = "Physiography",
                               "Land Use" = "Land/Water Use")) +
  scale_y_continuous(name = "MSE Increase [%]", labels = scales::percent) +
  scale_x_discrete(limits = rev(levels(imp_p2z$Predictor))) +
  coord_flip() +
  labs(title = "(b) Peak to No-Flow") +
  theme(axis.title.y = element_blank()) +
  NULL

p_nat_zff <-
  ggplot(imp_zff, aes(x = Predictor, y = IncMSE/oobMSE, fill = Category)) +
  geom_col() +
  scale_fill_manual(drop = F,
                    values = c("Climate" = col.cat.red, 
                               "Physiography" = col.cat.blu,
                               "Land Use" = col.cat.grn),
                    labels = c("Climate" = "Climate", 
                               "Physiography" = "Physiography",
                               "Land Use" = "Land/Water Use")) +
  scale_y_continuous(name = "MSE Increase [%]", labels = scales::percent) +
  scale_x_discrete(limits = rev(levels(imp_zff$Predictor))) +
  coord_flip() +
  labs(title = "(c) First No-Flow Day") +
  theme(axis.title.y = element_blank()) +
  NULL

((p_nat_anf + p_nat_p2z + p_nat_zff) + 
    plot_layout(guides = 'collect') & 
    theme(legend.position = "bottom",
          plot.title = element_text(face = "plain"))) +
  ggsave(file.path("figures_manuscript", "RandomForest_VariableImportance.png"),
         width = 190, height = 95, units = "mm")

((p_nat_anf + p_nat_p2z + p_nat_zff) + 
    plot_layout(guides = 'collect') & 
    theme(legend.position = "bottom",
          plot.title = element_text(face = "plain"))) +
  ggsave(file.path("figures_manuscript", "Figure6.pdf"),
         width = 190, height = 95, units = "mm", device = cairo_pdf)

## partial dependence plots
p_pdp_anf <-
  ggplot(pdp_anf, aes(x = value, y = yhat, color = Category)) +
  geom_line() +
  facet_wrap( ~ Predictor, scales = "free_x") +
  scale_x_continuous(name = "Scaled Value of Variable [z-score]", limits = c(-2, 2)) +
  scale_color_manual(drop = F,
                    values = c("Climate" = col.cat.red, 
                               "Physiography" = col.cat.blu,
                               "Land Use" = col.cat.grn),
                    labels = c("Climate" = "Climate", 
                               "Physiography" = "Physiography",
                               "Land Use" = "Land/Water Use")) +
  labs(title = "No-Flow Days") +
  theme(legend.position = "bottom") +
  NULL +
  ggsave(file.path("figures_manuscript", "RandomForest_VariableImportance-PartialDependencePlots_NoFlowDays.png"),
         width = 190, height = 190, units = "mm")

p_pdp_p2z <-
  ggplot(pdp_p2z, aes(x = value, y = yhat, color = Category)) +
  geom_line() +
  facet_wrap( ~ Predictor, scales = "free_x") +
  scale_x_continuous(name = "Scaled Value of Variable [z-score]", limits = c(-2, 2)) +
  scale_color_manual(drop = F,
                    values = c("Climate" = col.cat.red, 
                               "Physiography" = col.cat.blu,
                               "Land Use" = col.cat.grn),
                    labels = c("Climate" = "Climate", 
                               "Physiography" = "Physiography",
                               "Land Use" = "Land/Water Use")) +
  labs(title = "Peak to No-Flow") +
  theme(legend.position = "bottom") +
  NULL +
  ggsave(file.path("figures_manuscript", "RandomForest_VariableImportance-PartialDependencePlots_PeakToZero.png"),
         width = 190, height = 190, units = "mm")

ggplot(pdp_zff, aes(x = value, y = yhat, color = Category)) +
  geom_line() +
  facet_wrap( ~ Predictor, scales = "free_x") +
  scale_x_continuous(name = "Scaled Value of Variable [z-score]", limits = c(-2, 2)) +
  scale_color_manual(drop = F,
                    values = c("Climate" = col.cat.red, 
                               "Physiography" = col.cat.blu,
                               "Land Use" = col.cat.grn),
                    labels = c("Climate" = "Climate", 
                               "Physiography" = "Physiography",
                               "Land Use" = "Land/Water Use")) +
  labs(title = "First No-Flow Day") +
  theme(legend.position = "bottom") +
  NULL +
  ggsave(file.path("figures_manuscript", "RandomForest_VariableImportance-PartialDependencePlots_ZeroFlowFirst.png"),
         width = 190, height = 190, units = "mm")
