## RandomForest_VariableImportance.R
# This script will plot the importance of different predictor variables
# for the random forest models.

source(file.path("code", "paths+packages.R"))

# variable importance
rf_imp <- readr::read_csv(file.path("results", "02_RandomForest_RunModels_VariableImportance.csv"))

## data frame with long names for predictors and their category
df_pred <-
  tibble::tibble(
    predictor = c("clayave", "drain_sqkm", "elev_mean_m_basin", "p.pet_amj", 
                  "p.pet_cy", "p.pet_jas", "p_mm_cy", "permave", "pet_mm_amj", 
                  "pet_mm_jas", "sandave", "siltave", "slope_pct", "srad_wm2_amj", 
                  "srad_wm2_cy", "topwet", "depth_bedrock_m", "p_mm_amj", "p_mm_jas", 
                  "pet_mm_ond", "porosity", "T_min_c_cy", "T_min_c_ond", "pdsi_amj", 
                  "pdsi_cy", "pdsi_jas", "pet_mm_cy", "storage_m", "T_max_c_amj", 
                  "T_max_c_cy", "T_min_c_amj", "T_min_c_jas", "awcave", "srad_wm2_jas", 
                  "T_max_c_jas", "srad_wm2_jfm", "srad_wm2_ond", "T_max_c_jfm", 
                  "pdsi_ond", "T_min_c_jfm", "pcumdist10days", "T_max_c_ond", "pet_mm_jfm", 
                  "p.pet_ond", "swe.p_ond", "p_mm_ond",
                  "p.pet_cy.previous", "p_mm_cy.previous", 
                  "p_mm_jfm.previous", "pet_mm_cy.previous", "p.pet_jas.previous", 
                  "p_mm_jas.previous", "T_min_c_jas.previous", "pcumdist50days", 
                  "srad_wm2_jfm.previous", "p.pet_jfm.previous", "srad_wm2_ond.previous", 
                  "T_max_c_cy.previous", "srad_wm2_cy.previous", "pet_mm_jfm.previous", 
                  "pet_mm_amj.previous", "pet_mm_ond.previous", "pet_mm_jas.previous", 
                  "srad_wm2_jas.previous", "T_min_c_cy.previous", "T_min_c_ond.previous", 
                  "srad_wm2_amj.previous", "p.pet_ond.previous", "p_mm_ond.previous", 
                  "T_max_c_jfm.previous"),
    Category = c("Physiography", "Physiography", "Physiography", "Climate", 
                 "Climate", "Climate", "Climate", "Physiography", "Climate", 
                 "Climate", "Physiography", "Physiography", "Physiography", "Climate", 
                 "Climate", "Physiography", "Physiography", "Climate", "Climate", 
                 "Climate", "Physiography", "Climate", "Climate", "Climate", 
                 "Climate", "Climate", "Climate", "Physiography", "Climate", 
                 "Climate", "Climate", "Climate", "Physiography", "Climate", 
                 "Climate", "Climate", "Climate", "Climate", "Climate", 
                 "Climate", "Climate", "Climate", "Climate", "Climate", 
                 "Climate", "Climate","Climate", "Climate", 
                 "Climate", "Climate", "Climate", "Climate", 
                 "Climate", "Climate", "Climate", "Climate", 
                 "Climate", "Climate", "Climate", "Climate", 
                 "Climate", "Climate", "Climate", "Climate", 
                 "Climate", "Climate", "Climate", "Climate",
                  "Climate",  "Climate"),
    long_name = c("Soil Clay", "Drainage Area", "Elevation", "P/PET (AMJ)", 
                  "P/PET (CY)", "P/PET (JAS)", "P (CY)", "Soil Permeab", "PET (AMJ)", 
                  "PET (JAS)", "Soil Sand", "Soil Silt", "Slope", "SRad (AMJ)", 
                  "SRad (CY)", "Topo Wetness", "Bedrock Depth", "P (AMJ)", "P (JAS)", 
                  "PET (OND)", "Porosity", "Tmin (CY)", "Tmin (OND)", "PDSI (AMJ)", 
                  "PDSI (CY)", "PDSI (JAS)", "PET (CY)", "Storage", "Tmax (AMJ)", 
                  "Tmax (CY)", "Tmin (AMJ)", "Tmin (JAS)", "Soil AWC", "SRad (JAS)", 
                  "Tmax (JAS)", "SRad (JFM)", "SRad (OND)", "Tmax (JFM)", 
                  "PDSI (OND)", "Tmin (JFM)", "Days to 10% P", "Tmax (OND)", "PET (JFM)", 
                  "P/PET (OND)", "SWE/P (OND)", "P (OND)", "P/PET (CY-1)", "P (CY-1)", 
                  "P (JFM-1)", "PET (CY-1)", "P/PET (JAS-1)", 
                  "P (JAS-1)", "Tmin (JAS-1)", "Days to 50% P", 
                  "SRad (JFM-1)", "P/PET (JFM-1)", "SRad (OND-1)", 
                  "Tmax (CY-1)", "SRad (CY-1)", "PET (JFM-1)", 
                  "PET (AMJ-1)", "PET (OND-1)", "PET (JAS-1)", 
                  "SRad (JAS-1)", "Tmin (CY-1)", "Tmin (OND-1)", 
                  "SRad (AMJ-1)", "P/PET (OND-1)", "P (OND-1)", "Tmax (JFM-1)"))
                  
rf_imp <- dplyr::left_join(rf_imp, df_pred, by = "predictor")


## plots: national
p_nat_afnf <-
  rf_imp %>% 
  subset(region_rf == "National" & metric == "annualfractionnoflow") %>% 
  dplyr::arrange(VarPrcIncMSE) %>% 
  dplyr::mutate(Predictor = factor(long_name, levels = long_name)) %>% 
  ggplot(aes(x = Predictor, y = VarPrcIncMSE, fill = Category)) +
  geom_col() +
  scale_fill_manual(values = c("Climate" = col.cat.red, 
                               "Physiography" = col.cat.blu)) +
  scale_y_continuous(name = "% Increase MSE") +
  coord_flip() +
  labs(title = "(a) Annual Fraction Zero Flow") +
  theme(axis.title.y = element_blank()) +
  NULL

p_nat_p2z <-
  rf_imp %>% 
  subset(region_rf == "National" & metric == "peak2z_length") %>% 
  dplyr::arrange(VarPrcIncMSE) %>% 
  dplyr::mutate(Predictor = factor(long_name, levels = long_name)) %>% 
  ggplot(aes(x = Predictor, y = VarPrcIncMSE, fill = Category)) +
  geom_col() +
  scale_fill_manual(values = c("Climate" = col.cat.red, 
                               "Physiography" = col.cat.blu)) +
  scale_y_continuous(name = "% Increase MSE") +
  coord_flip() +
  labs(title = "(b) Peak to Zero") +
  theme(axis.title.y = element_blank()) +
  NULL

p_nat_zff <-
  rf_imp %>% 
  subset(region_rf == "National" & metric == "zeroflowfirst") %>% 
  dplyr::arrange(VarPrcIncMSE) %>% 
  dplyr::mutate(Predictor = factor(long_name, levels = long_name)) %>% 
  ggplot(aes(x = Predictor, y = VarPrcIncMSE, fill = Category)) +
  geom_col() +
  scale_fill_manual(values = c("Climate" = col.cat.red, 
                               "Physiography" = col.cat.blu)) +
  scale_y_continuous(name = "% Increase MSE") +
  coord_flip() +
  labs(title = "(c) First Zero Flow") +
  theme(axis.title.y = element_blank()) +
  NULL

((p_nat_afnf / p_nat_p2z / p_nat_zff) + 
    plot_layout(guides = 'collect') & 
    theme(legend.position = "bottom",
          plot.title = element_text(face = "plain"))) +
  ggsave(file.path("figures_manuscript", "RandomForest_VariableImportance-National.png"),
         width = 95, height = 240, units = "mm")
