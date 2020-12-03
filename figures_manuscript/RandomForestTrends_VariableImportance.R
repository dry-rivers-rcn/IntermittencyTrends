## RandomForestTrends_VariableImportance.R
# This script will plot the importance of different predictor variables
# for the random forest models of trends.

source(file.path("code", "paths+packages.R"))

# variable importance
rf_imp <- 
  readr::read_csv(file.path("results", "RandomForestTrends_03_RunModels_VariableImportance.csv")) %>% 
  dplyr::left_join(df_pred, by = "predictor") # join to data frame with predictor variable names and categories

## grab selected predictors to show
n_pred <- 9

# ranked predictors for each
imp_anf <- 
  rf_imp %>% 
  subset(metric == "annualnoflowdays") %>% 
  dplyr::slice_max(order_by = IncMSE, n = n_pred) %>% 
  dplyr::arrange(-IncMSE) %>% 
  dplyr::mutate(Predictor = factor(long_name, levels = long_name))

imp_p2z <- 
  rf_imp %>% 
  subset(metric == "peak2z_length") %>% 
  dplyr::slice_max(order_by = IncMSE, n = n_pred) %>% 
  dplyr::arrange(-IncMSE) %>% 
  dplyr::mutate(Predictor = factor(long_name, levels = long_name))

imp_zff <- 
  rf_imp %>% 
  subset(metric == "zeroflowfirst") %>% 
  dplyr::slice_max(order_by = IncMSE, n = n_pred) %>% 
  dplyr::arrange(-IncMSE) %>% 
  dplyr::mutate(Predictor = factor(long_name, levels = long_name))

## plots - variable importance
p_nat_anf <-
  ggplot(imp_anf, aes(x = Predictor, y = IncMSE/oobMSE, fill = Category)) +
  geom_col() +
  scale_fill_manual(drop = F,
                    values = c("Climate" = col.cat.red, 
                               "Physiography" = col.cat.blu,
                               "Land Use" = col.cat.grn)) +
  scale_y_continuous(name = "MSE Increase [%]", 
                     breaks = c(0, 0.03, 0.06, 0.09),
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
                               "Land Use" = col.cat.grn)) +
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
                               "Land Use" = col.cat.grn)) +
  scale_y_continuous(name = "MSE Increase [%]", 
                     breaks = c(0, 0.03, 0.06, 0.09),
                     labels = scales::percent) +
  scale_x_discrete(limits = rev(levels(imp_zff$Predictor))) +
  coord_flip() +
  labs(title = "(c) First No-Flow Day") +
  theme(axis.title.y = element_blank()) +
  NULL

((p_nat_anf + p_nat_p2z + p_nat_zff) + 
    plot_layout(guides = 'collect') & 
    theme(legend.position = "bottom",
          plot.title = element_text(face = "plain"))) +
  ggsave(file.path("figures_manuscript", "RandomForestTrends_VariableImportance.png"),
         width = 190, height = 95, units = "mm")
