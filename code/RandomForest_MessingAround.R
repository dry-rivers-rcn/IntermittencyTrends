## RandomForest_MessingAround.R
# Sam workspace for just messing around with the random forest models.

source(file.path("code", "paths+packages.R"))

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

rf_var_importance <- 
  file.path("results", "01_RandomForest_PreliminaryVariableImportance.csv") %>% 
  readr::read_csv() %>% 
  dplyr::group_by(metric, region, predictor) %>% 
  dplyr::summarize(PrcIncMSE_mean = mean(PrcIncMSE)) %>% 
  dplyr::ungroup()

## set up predictions
# metrics and regions to predict
metrics <- c("annualfractionnoflow", "zeroflowfirst", "peak2z_length")
regions <- c("National", unique(gage_sample$region))

# all possible predictors
predictors_annual <- c("p_mm_cy", "p_mm_jas", "p_mm_ond", "p_mm_jfm", "p_mm_amj", "pet_mm_cy", 
                       "pet_mm_jas", "pet_mm_ond", "pet_mm_jfm", "pet_mm_amj", "T_max_c_cy", 
                       "T_max_c_jas", "T_max_c_ond", "T_max_c_jfm", "T_max_c_amj", "T_min_c_cy", 
                       "T_min_c_jas", "T_min_c_ond", "T_min_c_jfm", "T_min_c_amj", "pcumdist10days", 
                       "pcumdist50days", "pcumdist90days", "swe_mm_cy", "swe_mm_jas", 
                       "swe_mm_ond", "swe_mm_jfm", "swe_mm_amj", "srad_wm2_cy", "srad_wm2_jas", 
                       "srad_wm2_ond", "srad_wm2_jfm", "srad_wm2_amj", "pdsi_cy", "pdsi_jas", 
                       "pdsi_ond", "pdsi_jfm", "pdsi_amj", "p.pet_cy", "swe.p_cy", "p.pet_jfm", "swe.p_jfm",
                       "p.pet_amj", "swe.p_amj", "p.pet_jas", "swe.p_jas", "p.pet_ond", "swe.p_ond")

predictors_static <- c("drain_sqkm", "elev_mean_m_basin", "slope_pct", 
                       "awcave", "permave", "topwet", "depth_bedrock_m", 
                       "porosity", "storage_m", "clayave", "siltave", "sandave")

# previous year predictors will be calculated further down
predictors_annual_with_previous <- 
  c(predictors_annual, c("p_mm_cy.previous", "p_mm_jas.previous", "p_mm_ond.previous", 
                         "p_mm_jfm.previous", "p_mm_amj.previous", "pet_mm_cy.previous", 
                         "pet_mm_jas.previous", "pet_mm_ond.previous", "pet_mm_jfm.previous", 
                         "pet_mm_amj.previous", "T_max_c_cy.previous", "T_max_c_jas.previous", 
                         "T_max_c_ond.previous", "T_max_c_jfm.previous", "T_max_c_amj.previous", 
                         "T_min_c_cy.previous", "T_min_c_jas.previous", "T_min_c_ond.previous", 
                         "T_min_c_jfm.previous", "T_min_c_amj.previous", "pcumdist10days.previous", 
                         "pcumdist50days.previous", "pcumdist90days.previous", "swe_mm_cy.previous", 
                         "swe_mm_jas.previous", "swe_mm_ond.previous", "swe_mm_jfm.previous", 
                         "swe_mm_amj.previous", "srad_wm2_cy.previous", "srad_wm2_jas.previous", 
                         "srad_wm2_ond.previous", "srad_wm2_jfm.previous", "srad_wm2_amj.previous", 
                         "pdsi_cy.previous", "pdsi_jas.previous", "pdsi_ond.previous", 
                         "pdsi_jfm.previous", "pdsi_amj.previous", "p.pet_cy.previous", 
                         "swe.p_cy.previous", "p.pet_jfm.previous", "swe.p_jfm.previous", 
                         "p.pet_amj.previous", "swe.p_amj.previous", "p.pet_jas.previous", 
                         "swe.p_jas.previous", "p.pet_ond.previous", "swe.p_ond.previous"))

## divide gage sample into train (80% of ref), test (20% of ref), and non-ref (all non-ref)
# choose fraction of gages to use as validation
frac_val <- 0.8

# want to use same sample for all regions, metrics so need to take 80% from each region
set.seed(1)
gage_val_sample <-
  gage_sample %>% 
  subset(CLASS == "Ref") %>% 
  dplyr::group_by(region) %>% 
  dplyr::sample_frac(frac_val) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(gage_ID) %>% 
  dplyr::mutate(Sample = "Train")

# add to gage_sample; everything that is not selected will be Train (if ref) or non-ref
gage_sample <- dplyr::left_join(gage_sample, gage_val_sample, by = "gage_ID")
gage_sample$Sample[is.na(gage_sample$Sample) & gage_sample$CLASS == "Non-ref"] <- "Non-ref"
gage_sample$Sample[is.na(gage_sample$Sample) & gage_sample$CLASS == "Ref"] <- "Test"

# check count in each sample
gage_sample %>% 
  dplyr::select(region, Sample) %>% 
  table()

## calculate previous water year climate metrics
gage_sample_prevyear <- 
  gage_sample_annual[,c("gage_ID", "currentclimyear", predictors_annual)] %>% 
  dplyr::mutate(wyearjoin = currentclimyear + 1) %>% 
  dplyr::select(-currentclimyear)

## combine into one data frame
fit_data_in <- 
  gage_sample_annual %>% 
  # subset to fewer columns - metrics and predictors
  dplyr::select(c("gage_ID", "currentclimyear", all_of(metrics), all_of(predictors_annual))) %>% 
  # join with previous water year
  dplyr::left_join(gage_sample_prevyear, 
                   by = c("gage_ID", "currentclimyear"="wyearjoin"), 
                   suffix = c("", ".previous")) %>% 
  # join with static predictors
  dplyr::left_join(gage_sample[ , c("gage_ID", "CLASS", "region", "Sample", predictors_static)], by = "gage_ID")

## set metric and run model
metrics <- c("annualfractionnoflow", "zeroflowfirst", "peak2z_length")
m <- "annualfractionnoflow"
predictors <- c(predictors_annual, predictors_annual_with_previous, predictors_static)

# get rid of unneeded metrics
fit_data_m <- 
  fit_data_in %>% 
  dplyr::select(-all_of(metrics[metrics != m]))

# rename metric column
names(fit_data_m)[names(fit_data_m) == m] <- "observed"

# subset to complete cases only
fit_data_ref <- 
  fit_data_m %>% 
  dplyr::select(gage_ID, CLASS, Sample, currentclimyear, observed, region, 
                all_of(predictors)) %>% 
  subset(CLASS == "Ref") %>% 
  subset(complete.cases(.))

set.seed(1)

# try predicting binary flow/no flow
fit_data_ref$flow <- factor(fit_data_ref$observed > 0)
fit_flow_or_not <- as.formula(paste0("flow ~ ", paste(predictors, collapse = "+")))
fit_rf_flow <- randomForest::randomForest(fit_flow_or_not,
                                          data = subset(fit_data_ref, Sample == "Train"),
                                          ntree = 500,
                                          importance = T)
fit_data_ref$flow_predicted <- predict(fit_rf_flow, fit_data_ref)

# try predicting annual fraction no flow
fit_formula <- as.formula(paste0("observed ~ ", paste(predictors, collapse = "+")))
fit_rf <- randomForest::randomForest(fit_formula,
                                     data = subset(fit_data_ref, Sample == "Train"),
                                     ntree = 500,
                                     importance = T)
fit_data_ref$predicted <- predict(fit_rf, fit_data_ref)


# combine predictions
fit_data_ref$predicted_with_zeros <- fit_data_ref$predicted
fit_data_ref$predicted_with_zeros[fit_data_ref$flow_predicted != "TRUE"] <- 0

# plot
ggplot(fit_data_ref, aes(x = observed, y = predicted, color = region)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  facet_wrap(~Sample) +
  scale_color_manual(values = pal_regions) + 
  NULL

ggplot(fit_data_ref, aes(x = observed, y = predicted_with_zeros, color = region)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  facet_wrap(~Sample) +
  scale_color_manual(values = pal_regions) + 
  NULL


# stats
fit_rf

summary(lm(predicted~observed, data = subset(fit_data_ref, Sample == "Test")))$r.squared
summary(lm(predicted~observed, data = subset(fit_data_ref, Sample == "Train")))$r.squared
summary(lm(predicted_with_zeros~observed, data = subset(fit_data_ref, Sample == "Test")))$r.squared
summary(lm(predicted_with_zeros~observed, data = subset(fit_data_ref, Sample == "Train")))$r.squared

hydroGOF::rmse(subset(fit_data_ref, Sample == "Test")$predicted, subset(fit_data_ref, Sample == "Test")$observed)
hydroGOF::rmse(subset(fit_data_ref, Sample == "Train")$predicted, subset(fit_data_ref, Sample == "Train")$observed)
hydroGOF::rmse(subset(fit_data_ref, Sample == "Test")$predicted_with_zeros, subset(fit_data_ref, Sample == "Test")$observed)
hydroGOF::rmse(subset(fit_data_ref, Sample == "Train")$predicted_with_zeros, subset(fit_data_ref, Sample == "Train")$observed)

## test partial least squares regression
library(pls)
cum.var <- 0.9
fit_pls <- plsr(fit_formula, ncomp = 10, 
                data = subset(fit_data_ref, Sample == "Train"),
                validation = "LOO")

summary(fit_pls)
plot(RMSEP(fit_pls), legendpos = "topright")
plot(fit_pls, ncomp = 2, asp = 1, line = TRUE)
plot(fit_pls, plottype = "scores", comps = 1:3)

plot(RMSEP(fit_pls, newdata = subset(fit_data_ref, Sample == "Test")))

comp.var <- fit_pls$Xvar/fit_pls$Xtotvar
prop.var <- cumsum(comp.var)

plot(prop.var)

PC.cum.var <- min(which(prop.var >= cum.var))  # find which PC explains >cum.var% of cumulative proportion
PC.keep <- paste0("PC", seq(1,PC.cum.var))

df_PCs <- as.data.frame(predict(fit_pls, newdata = subset(fit_data_ref, Sample == "Train")))
colnames(df_PCs) <- paste0("PC", seq(1,dim(df_PCs)[2]))

ncomp <- length(PC.keep)  # number of principal components you want to use
fmla_pls <- as.formula(paste("observed ~", paste(PC.keep, collapse='+')))  # build your formula
df_PCs$observed <- subset(fit_data_ref, Sample == "Train")$observed
df_PCs$predicted_pls <- predict(fmla_pls, data=subset(fit_data_ref, Sample == "Train"))


ggplot(df_PCs, aes(x = observed, y = predicted_pls)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  NULL
