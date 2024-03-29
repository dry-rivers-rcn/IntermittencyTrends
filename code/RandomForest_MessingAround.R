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

## set up predictions
# metrics and regions to predict
metrics <- c("annualfractionnoflow", "zeroflowfirst", "peak2z_length")
regions <- c("National", unique(gage_sample$region))

# all possible predictors
predictors_climate <- c("p_mm_cy", "p_mm_jas", "p_mm_ond", "p_mm_jfm", "p_mm_amj", "pet_mm_cy", 
                        "pet_mm_jas", "pet_mm_ond", "pet_mm_jfm", "pet_mm_amj", "T_max_c_cy", 
                        "T_max_c_jas", "T_max_c_ond", "T_max_c_jfm", "T_max_c_amj",
                        "swe_mm_cy", "swe_mm_jas", 
                        "swe_mm_ond", "swe_mm_jfm", "swe_mm_amj", "p.pet_cy", "swe.p_cy", "p.pet_jfm", "swe.p_jfm",
                        "p.pet_amj", "swe.p_amj", "p.pet_jas", "swe.p_jas", "p.pet_ond", "swe.p_ond")

predictors_human <- c("dams_n", "maxstorage_af", "normstorage_af", "majordams_n", 
                      "wuse_mm", "irrig_prc", "lulc_water_prc", "lulc_dev_prc", "lulc_wetland_prc",
                      "lulc_forest_prc", "lulc_barren_prc", "lulc_grass_prc", "lulc_ag_prc")

predictors_static <- c("drain_sqkm", "elev_mean_m_basin", "slope_pct", 
                       "awcave", "permave", "topwet", "depth_bedrock_m", 
                       "porosity", "storage_m", "clayave", "siltave", "sandave")

# previous year predictors will be calculated further down
predictors_climate_with_previous <- 
  c(predictors_climate, c("p_mm_cy.previous", "p_mm_jas.previous", "p_mm_ond.previous", 
                          "p_mm_jfm.previous", "p_mm_amj.previous", "pet_mm_cy.previous", 
                          "pet_mm_jas.previous", "pet_mm_ond.previous", "pet_mm_jfm.previous", 
                          "pet_mm_amj.previous", "T_max_c_cy.previous", "T_max_c_jas.previous", 
                          "T_max_c_ond.previous", "T_max_c_jfm.previous", "T_max_c_amj.previous",
                          "swe_mm_cy.previous", "swe_mm_jas.previous", "swe_mm_ond.previous", 
                          "swe_mm_jfm.previous", "swe_mm_amj.previous", "p.pet_cy.previous", 
                          "swe.p_cy.previous", "p.pet_jfm.previous", "swe.p_jfm.previous", 
                          "p.pet_amj.previous", "swe.p_amj.previous", "p.pet_jas.previous", 
                          "swe.p_jas.previous", "p.pet_ond.previous", "swe.p_ond.previous"))

## calculate previous water year climate metrics
gage_sample_prevyear <- 
  gage_sample_annual[,c("gage_ID", "currentclimyear", predictors_annual)] %>% 
  dplyr::mutate(wyearjoin = currentclimyear + 1) %>% 
  dplyr::select(-currentclimyear)

## combine into one data frame
fit_data_in <- 
  gage_sample_annual %>% 
  # subset to fewer columns - metrics and predictors
  dplyr::select(c("gage_ID", "currentclimyear", all_of(metrics), 
                  all_of(predictors_climate), all_of(predictors_human))) %>% 
  # join with previous water year
  dplyr::left_join(gage_sample_prevyear, 
                   by = c("gage_ID", "currentclimyear"="wyearjoin"), 
                   suffix = c("", ".previous")) %>% 
  # join with static predictors
  dplyr::left_join(gage_sample[ , c("gage_ID", "CLASS", "Sample", "region", predictors_static)], by = "gage_ID")

## set metric and run model
m <- "peak2z_length"
r <- "National"
predictors <- c(predictors_annual, predictors_annual_with_previous, predictors_static)

# get rid of unneeded metrics
fit_data_m <- 
  fit_data_in %>% 
  dplyr::select(-all_of(metrics[metrics != m]))

# rename metric column
names(fit_data_m)[names(fit_data_m) == m] <- "observed"

# get predictor variables
n_pred <- 18
rf_var_m_r <-
  file.path("results", paste0("01_RandomForest_PreliminaryVariableImportance_", m, "_", gsub(" ", "", r, fixed = TRUE), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::group_by(metric, region, predictor) %>% 
  dplyr::summarize(PrcIncMSE_mean = mean(PrcIncMSE)) %>% 
  dplyr::ungroup() %>% 
  dplyr::top_n(n = n_pred, wt = PrcIncMSE_mean)

if (r == "National") {
  fit_data_r <- 
    fit_data_m %>% 
    dplyr::select(gage_ID, CLASS, Sample, currentclimyear, observed, region, all_of(rf_var_m_r$predictor)) %>% 
    subset(complete.cases(.))
} else {
  fit_data_r <- 
    fit_data_m %>% 
    subset(region == r) %>% 
    dplyr::select(gage_ID, CLASS, Sample, currentclimyear, observed, region, all_of(rf_var_m_r$predictor)) %>% 
    subset(complete.cases(.))
}

fit_formula <- as.formula(paste0("observed ~ ", paste(unique(rf_var_m_r$predictor), collapse = "+")))

## k-folds cross-validation and prediction
set.seed(1)
n_folds <- length(unique(gage_sample$Sample)) # choose number of folds for cross-val
for (k in 1:n_folds){
  ## train model
  fit_rf <- randomForest::randomForest(fit_formula,
                                       data = subset(fit_data_r, Sample != paste0("Test", k)),
                                       ntree = 500,
                                       localImp = T)
  
  # run model
  fit_data_r$predicted <- predict(fit_rf, fit_data_r)
  fit_data_r$metric <- m
  fit_data_r$region_rf <- r
  fit_data_i <- 
    fit_data_r %>% 
    dplyr::select(gage_ID, CLASS, currentclimyear, observed, predicted, metric, region_rf) %>% 
    dplyr::mutate(kfold = k)
  
  # extract variable importance
  fit_rf_imp_i <- tibble::tibble(predictor = rownames(randomForest::importance(fit_rf, type = 1)),
                                 VarPrcIncMSE = randomForest::importance(fit_rf, type = 1)[,'%IncMSE'],
                                 metric = m,
                                 region_rf = r,
                                 kfold = k)
  
  if (k == 1){
    fit_data_out <- fit_data_i
    fit_rf_imp <- fit_rf_imp_i
  } else {
    fit_data_out <- dplyr::bind_rows(fit_data_out, fit_data_i)
    fit_rf_imp <- dplyr::bind_rows(fit_rf_imp, fit_rf_imp_i)
  }
  
  # status update
  print(paste0(m, " ", r, " k", k, " complete, ", Sys.time()))
  
}

# identify test vs. train
rf_all <- 
  fit_data_out %>% 
  dplyr::mutate(residual = predicted - observed) %>% 
  dplyr::left_join(gage_sample[,c("gage_ID", "region", "Sample")], by = "gage_ID") %>% 
  dplyr::mutate(Test = Sample == paste0("Test", kfold))

# plot
rf_all %>% 
  ggplot(aes(x = predicted, y = observed)) + 
  geom_point(shape = 1, aes(color = region)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_grid(region ~ Test) +
  scale_color_manual(name = "Region", values = pal_regions) +
  scale_x_continuous(name = "Predicted [days]",
                     limits = c(0,225), expand = c(0,0)) +
  scale_y_continuous(name = "Observed [days]",
                     limits = c(0,225), expand = c(0,0)) +
  theme(legend.position = "bottom") +
  NULL

# fit stats
rf_fit <-
  rf_all %>% 
  dplyr::group_by(Test, region) %>% 
  dplyr::summarize(RMSE = round(hydroGOF::rmse(predicted, observed), 3),
                   NRMSE = hydroGOF::nrmse(predicted, observed, norm = "maxmin"),
                   KGE = round(hydroGOF::KGE(predicted, observed, method = "2012"), 3)) %>% 
  dplyr::ungroup()

# test mtry and ntree
mtry_all <- seq(3, 10)
ntree_all <- seq(100, 1000, 100)
for (m in mtry_all){
  for (n in ntree_all){
    k <- 1
    fit_data_r$Test <-  fit_data_r$Sample == paste0("Test", k)
    fit_rf <- randomForest::randomForest(fit_formula,
                                         data = subset(fit_data_r, Test),
                                         mtry = m,
                                         ntree = n,
                                         localImp = T)
    
    fit_data_r$predicted <- predict(fit_rf, fit_data_r)
    
    # calculate fit
    fit_tune_m <- 
      fit_data_r %>% 
      dplyr::group_by(Test) %>% 
      dplyr::summarize(MSE = round(hydroGOF::mse(predicted, observed), 3)) %>% 
      dplyr::mutate(mtry = m, 
                    ntree = n)
    
    if (m == mtry_all[1] & n == ntree_all[1]){
      fit_tune <- fit_tune_m
    } else {
      fit_tune <- dplyr::bind_rows(fit_tune, fit_tune_m)
    }
    print(paste0("mtry ", m, ", ntree ", n, " complete"))
    
  }
}

fit_tune %>% 
  subset(Test) %>% 
  ggplot(aes(x = mtry, y = ntree, fill = MSE)) +
  geom_raster()









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
