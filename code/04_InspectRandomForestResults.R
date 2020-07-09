## 03_InspectRandomForestResults.R

source(file.path("code", "paths+packages.R"))

## load data
# random forest predictions
rf_all <- 
  readr::read_csv(file.path("results", "03_RandomForest_RunModels_Predictions.csv")) %>% 
  dplyr::mutate(residual = predicted - observed)

# a positive residual means that the predicted is higher than the observed
#  annualfractionnoflow: prediction is drier than observed (i.e., human impacts making drier)
#  zeroflowfirst: prediction is later than observed (i.e., human impacts making earlier)
#  peak2z_length: prediction is longer than observed (i.e., human impacts making shorter)
# so a positive residual implies that human impacts are:
#  making streams drier, with the first dry day earlier in the year, and a faster time from peak to dry. 

# variable importance
rf_imp <- readr::read_csv(file.path("results", "03_RandomForest_RunModels_VariableImportance.csv"))

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
                p.pet_jas = p_mm_jas/pet_mm_jas,
                swe.p_jas = swe_mm_jas/p_mm_jas,
                p.pet_ond = p_mm_ond/pet_mm_ond,
                swe.p_ond = swe_mm_ond/p_mm_ond)

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
                p.pet_jas = p_mm_jas/pet_mm_jas,
                swe.p_jas = swe_mm_jas/p_mm_jas,
                p.pet_ond = p_mm_ond/pet_mm_ond,
                swe.p_ond = swe_mm_ond/p_mm_ond)

## scatterplot of model fit
ggplot(subset(rf_all, CLASS == "Ref"), aes(x = observed, y = predicted, color = region)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  facet_wrap(metric ~ region_rf, scales = "free", ncol = 7) +
  labs(title = "Random forest model performance by metric (3x) and model (7x), ref gages only") +
  theme(legend.position = "bottom") +
  ggsave(file.path("results", "RFresults_PredVsObs_RefGages.png"),
         width = 380, height = 200, units = "mm")

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

## calculate trend in observed, predicted
sites <- unique(rf_test$gage_ID)

for (i in seq_along(sites)){
  rf_site <- subset(rf_test, gage_ID == sites[i])
  
  manken_obs <- Kendall::MannKendall(rf_site$observed)
  sen_obs <- zyp::zyp.sen(observed ~ currentclimyear, rf_site)
  manken_pred <- Kendall::MannKendall(rf_site$predicted)
  sen_pred <- zyp::zyp.sen(predicted ~ currentclimyear, rf_site)
  
  if (i == 1){
    rf_trends <- tibble::tibble(gage_ID = sites[i],
                 slope_pred = sen_pred$coefficients[2],
                 slope_obs = sen_obs$coefficients[2],
                 pval_pred = manken_pred$sl,
                 pval_obs = manken_obs$sl)
  } else {
    rf_trends <- dplyr::bind_rows(rf_trends, 
                                  tibble::tibble(gage_ID = sites[i],
                                                 slope_pred = sen_pred$coefficients[2],
                                                 slope_obs = sen_obs$coefficients[2],
                                                 pval_pred = manken_pred$sl,
                                                 pval_obs = manken_obs$sl))
  }
  print(paste0(i, " complete"))
}
rf_trends$slope_diff <- rf_trends$slope_pred - rf_trends$slope_obs
rf_trends <- dplyr::left_join(rf_trends, gage_sample, by = "gage_ID")

## plots

# scatterplot - residual by time, ref and nonref
ggplot(rf_test, aes(x = currentclimyear, y = residual, color = region)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_point(shape = 1) +
  facet_wrap(~CLASS, ncol = 1) +
  stat_smooth(method = "lm") +
  scale_color_manual(name = "Region", values = pal_regions)

# boxplot - residual by region
ggplot(rf_bygage, aes(x = region, y = res_mean, fill = CLASS)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_boxplot()

# scatterplot - predicted trend vs observed trend
ggplot(rf_trends, aes(x = slope_pred, y = slope_obs, color = region)) +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  geom_point(shape = 1) +
  facet_wrap(~CLASS, ncol = 1) +
  scale_color_manual(name = "Region", values = pal_regions)

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

# scatterplot - slope diff vs each important variable
rf_trends %>% 
  dplyr::select(slope_diff, region, all_of(rf_imp_test$predictor)) %>% 
  reshape2::melt(id = c("slope_diff", "region")) %>% 
  ggplot(aes(x = value, y = slope_diff)) +
  geom_point(aes(color = region)) +
  stat_smooth(method = "lm") +
  geom_hline(yintercept = 0, color = col.gray) +
  facet_wrap(~variable, scales = "free_x") +
  scale_color_manual(name = "Region", values = pal_regions) +
  theme(legend.position = "bottom")

# scatterplot - residual vs some land cover variables
lc_vars <- 
  c("POWER_SUM_MW", "FORESTNLCD06", "PLANTNLCD06", "WATERNLCD06", 
    "IMPNLCD06",  "FRESHW_WITHDRAWAL", 
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

# scatterplot - slope difference vs some land cover variables
rf_trends %>% 
  dplyr::select(slope_diff, region, all_of(lc_vars)) %>% 
  reshape2::melt(id = c("slope_diff", "region")) %>% 
  ggplot(aes(x = value, y = slope_diff)) +
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

# predictor variable importance
rf_imp_test$predictor <- factor(rf_imp_test$predictor, levels = rf_imp_test$predictor)
ggplot(rf_imp_test, aes(x = predictor, y = VarPrcIncMSE)) +
  geom_col() +
  coord_flip()

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
  ggplot(aes(x = observed, y = predicted, color = region, shape = WithinRefParameterSpace)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = col.gray) +
  scale_color_manual(values = pal_regions) +
  facet_wrap(~region_rf) +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1))

rf_all %>% 
  subset(metric == "annualfractionnoflow" & region_rf == "National") %>% 
  dplyr::left_join(rf_bygage[,c("gage_ID", "WithinRefParameterSpace")], by = "gage_ID") %>% 
  ggplot(aes(x = observed, y = predicted, color = region, shape = WithinRefParameterSpace)) +
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
