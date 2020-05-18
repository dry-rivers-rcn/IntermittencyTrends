## 03_InspectRandomForestResults.R

source(file.path("code", "paths+packages.R"))

## load data
# random forest predictions
rf_all <- 
  readr::read_csv(file.path("results", "02_RandomForest_RunModels_Predictions.csv")) %>% 
  dplyr::mutate(residual = predicted - observed)

# variable importance
rf_imp <- readr::read_csv(file.path("results", "02_RandomForest_RunModels_VariableImportance.csv"))

# load gage characteristics
gage_sample <- 
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv")) %>% 
  dplyr::mutate(gage_ID = as.numeric(gage_ID)) %>% 
  # add some derived variables
  dplyr::mutate(p.pet_cy = p_mm_cy/pet_mm_cy,
                swe.p_cy = swe_mm_cy/p_mm_cy,
                p.pet_jfm = p_mm_jfm/pet_mm_jfm,
                swe.p_jfm = swe_mm_jfm/p_mm_jfm,
                p.pet_amj = p_mm_amj/pet_mm_amj,
                swe.p_amj = swe_mm_amj/p_mm_amj,
                p.pet_ond = p_mm_ond/pet_mm_ond,
                swe.p_ond = swe_mm_ond/p_mm_ond,
                p.pet_son = p_mm_son/pet_mm_son,
                swe.p_son = swe_mm_son/p_mm_son)

# annual gage data
gage_sample_annual <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv")) %>% 
  dplyr::left_join(gage_sample[,c("gage_ID", "region")], by = "gage_ID") %>% 
  # add some derived variables
  dplyr::mutate(p.pet_cy = p_mm_cy/pet_mm_cy,
                swe.p_cy = swe_mm_cy/p_mm_cy,
                p.pet_jfm = p_mm_jfm/pet_mm_jfm,
                swe.p_jfm = swe_mm_jfm/p_mm_jfm,
                p.pet_amj = p_mm_amj/pet_mm_amj,
                swe.p_amj = swe_mm_amj/p_mm_amj,
                p.pet_ond = p_mm_ond/pet_mm_ond,
                swe.p_ond = swe_mm_ond/p_mm_ond,
                p.pet_son = p_mm_son/pet_mm_son,
                swe.p_son = swe_mm_son/p_mm_son)

## subset to a single metric and regional model for testing
rf_test <- 
  rf_all %>% 
  subset(region_rf == "National" & metric == "annualfractionnoflow")

rf_imp_test <- 
  rf_imp %>% 
  subset(region_rf == "National" & metric == "annualfractionnoflow") %>% 
  dplyr::arrange(-VarPrcIncMSE)

## calculate mean values by gage and join to gage characteristics
rf_bygage <- 
  rf_test %>% 
  dplyr::group_by(gage_ID, CLASS, metric, region, region_rf) %>% 
  dplyr::summarize(obs_mean = mean(observed),
                   pred_mean = mean(predicted),
                   res_mean = mean(residual)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(gage_sample, by = c("gage_ID", "CLASS", "region"))

## plots
# boxplot - residual by region
ggplot(rf_bygage, aes(x = region, y = res_mean, fill = CLASS)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_boxplot()

# boxplot - all predictors by region and class
rf_bygage %>% 
  dplyr::select(CLASS, all_of(rf_imp_test$predictor)) %>% 
  reshape2::melt(id = c("CLASS")) %>% 
  ggplot(aes(x = CLASS, y = value, fill = CLASS)) +
  facet_wrap(~variable, scales = "free_y") +
  geom_boxplot()

# scatterplot - residual vs each important variable
rf_bygage %>% 
  dplyr::select(res_mean, region, all_of(rf_imp_test$predictor)) %>% 
  reshape2::melt(id = c("res_mean", "region")) %>% 
  ggplot(aes(x = value, y = res_mean)) +
  geom_point(aes(color = region)) +
  stat_smooth(method = "lm") +
  geom_hline(yintercept = 0, color = col.gray) +
  facet_wrap(~variable, scales = "free_x") +
  scale_color_manual(name = "Region", values = pal_regions) +
  theme(legend.position = "bottom")

# scatterplot - residual vs some land cover variables
lc_vars <- 
  c("accumulated_NID_storage", 
    "POWER_SUM_MW", "FORESTNLCD06", "PLANTNLCD06", "WATERNLCD06", 
    "SNOWICENLCD06", "IMPNLCD06",  "FRESHW_WITHDRAWAL", 
    "PCT_IRRIG_AG", "POWER_NUM_PTS", "DEVNLCD06")
rf_bygage %>% 
  dplyr::select(res_mean, region, all_of(lc_vars)) %>% 
  reshape2::melt(id = c("res_mean", "region")) %>% 
  ggplot(aes(x = value, y = res_mean)) +
  geom_point(aes(color = region)) +
  stat_smooth(method = "lm") +
  geom_hline(yintercept = 0, color = col.gray) +
  facet_wrap(~variable, scales = "free_x") +
  scale_color_manual(name = "Region", values = pal_regions) +
  theme(legend.position = "bottom")

# scatterplot - ref gage space vs nonref gage space, 2 most important predictors
hull_ppet.srad <- 
  rf_bygage %>%
  dplyr::group_by(CLASS) %>% 
  slice(chull(p.pet_cy, srad_wm2_cy))
ggplot(rf_bygage, aes(x = srad_wm2_cy, y = p.pet_cy)) +
  geom_polygon(data = hull_ppet.srad, aes(linetype = CLASS, fill = CLASS), alpha = 0.5) +
  geom_point(aes(color = region, shape = CLASS)) +
  scale_color_manual(name = "Region", values = pal_regions)

# scatterplot - ref gage space vs nonref gage space, drainage area and p.pet_cy
hull_ppet.da <- 
  rf_bygage %>%
  dplyr::group_by(CLASS) %>% 
  slice(chull(p.pet_cy, DRAIN_SQKM))
ggplot(rf_bygage, aes(x = DRAIN_SQKM, y = p.pet_cy)) +
  geom_polygon(data = hull_ppet.da, aes(linetype = CLASS, fill = CLASS), alpha = 0.5) +
  geom_point(aes(color = region, shape = CLASS)) +
  scale_color_manual(name = "Region", values = pal_regions)

## conduct analysis: limit non-ref gages to only those within the 5th-95th percentile of reference gages
get_prcMin <- function(x){quantile(x, 0)}
get_prcMax <- function(x){quantile(x, 1)}
rf_gage_prcMin <-
  rf_bygage %>% 
  dplyr::group_by(CLASS) %>% 
  dplyr::summarize_at(rf_imp_test$predictor, get_prcMin) %>% 
  reshape2::melt(id = "CLASS", value.name = "prcMin")
rf_gage_prcMax <-
  rf_bygage %>% 
  dplyr::group_by(CLASS) %>% 
  dplyr::summarize_at(rf_imp_test$predictor, get_prcMax) %>% 
  reshape2::melt(id = "CLASS", value.name = "prcMax")
rf_gage_criteria <-
  dplyr::left_join(rf_gage_prcMin, rf_gage_prcMax, by = c("variable", "CLASS")) %>% 
  subset(CLASS == "Ref")

rf_bygage$WithinRefParameterSpace <- T
for (pred in rf_gage_criteria$variable){
  not_meet <- 
    which(!((rf_bygage[,pred] <= rf_gage_criteria$prcMax[rf_gage_criteria$variable == pred]) &
              (rf_bygage[,pred] >= rf_gage_criteria$prcMin[rf_gage_criteria$variable == pred])))
  rf_bygage$WithinRefParameterSpace[not_meet] <- F
}
table(rf_bygage$WithinRefParameterSpace, rf_bygage$CLASS)

# plot residual
ggplot(subset(rf_bygage, CLASS == "Non-ref"), aes(x = region, y = res_mean, fill = WithinRefParameterSpace)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_boxplot()

rf_all %>% 
  subset(metric == "annualfractionnoflow") %>% 
  dplyr::left_join(rf_bygage[,c("gage_ID", "WithinRefParameterSpace")], by = "gage_ID") %>% 
  ggplot(aes(x = predicted, y = observed, color = region, shape = WithinRefParameterSpace)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf) +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1))

rf_all %>% 
  subset(metric == "annualfractionnoflow" & region_rf == "National") %>% 
  dplyr::left_join(rf_bygage[,c("gage_ID", "WithinRefParameterSpace")], by = "gage_ID") %>% 
  ggplot(aes(x = predicted, y = observed, color = region, shape = WithinRefParameterSpace)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region) +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1))

rf_all %>% 
  subset(metric == "annualfractionnoflow" & region_rf != "National") %>% 
  dplyr::left_join(rf_bygage[,c("gage_ID", "WithinRefParameterSpace")], by = "gage_ID") %>% 
  ggplot(aes(x = predicted, y = observed, color = region, shape = WithinRefParameterSpace)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region) +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1))
