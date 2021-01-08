## Trends_CompareTrendsToDrivers.R

source(file.path("code", "paths+packages.R"))

## load data
# RandomForestTrends RF input data
fit_data_in <- 
  readr::read_csv(file = file.path("results", "RandomForestTrends_01_RFinputData.csv")) 

gage_trends <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleTrends.csv"))

## reorganize trends so that you have a row for each gage and a column for each metric
# metrics to keep
metrics <- c("annualnoflowdays", "peak2z_length", "zeroflowfirst", "p.pet_cy")

## plot each no-flow metric against p/pet trend
# figure out tau limits
tau_min <- min(subset(gage_trends, metric %in% metrics)$mk_tau, na.rm=T)
tau_max <- max(subset(gage_trends, metric %in% metrics)$mk_tau, na.rm=T)
tau_abs <- max(c(tau_min, tau_max))

p_anfd.aridity <- 
  ggplot(fit_data_in, aes(y = tau_annualnoflowdays, x = tau_p.pet_cy)) +
  geom_hline(yintercept = 0, color = col.gray) + 
  geom_vline(xintercept = 0, color = col.gray) +
  geom_point(aes(color = region)) +
  scale_x_continuous(name = "P/PET, Kendall \u03c4", expand = c(0, 0.01)) +
  scale_y_continuous(name = "Annual No-Flow Days,\nKendall \u03c4", 
                     limits = c(-tau_abs, tau_abs), expand = c(0, 0.01)) +
  scale_color_manual(name = "Region", values = pal_regions,
                     labels = lab_regions_skinny1line) +
  stat_smooth(method = "lm", color = "black") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5, 
                              ncol = 2))

p_p2z.aridity <-
  ggplot(fit_data_in, aes(y = tau_peak2z_length, x = tau_p.pet_cy)) +
  geom_hline(yintercept = 0, color = col.gray) + 
  geom_vline(xintercept = 0, color = col.gray) +
  geom_point(aes(color = region)) +
  scale_x_continuous(name = "P/PET, Kendall \u03c4", expand = c(0, 0.01)) +
  scale_y_continuous(name = "Peak to No-Flow,\nKendall \u03c4", 
                     limits = c(-tau_abs, tau_abs), expand = c(0, 0.01)) +
  scale_color_manual(name = "Region", values = pal_regions,
                     labels = lab_regions_skinny1line) +
  stat_smooth(method = "lm", color = "black") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5, 
                              ncol = 2))

p_zff.aridity <- 
  ggplot(fit_data_in, aes(y = tau_zeroflowfirst, x = tau_p.pet_cy)) +
  geom_hline(yintercept = 0, color = col.gray) + 
  geom_vline(xintercept = 0, color = col.gray) +
  geom_point(aes(color = region)) +
  scale_x_continuous(name = "P/PET, Kendall \u03c4", expand = c(0, 0.01)) +
  scale_y_continuous(name = "First No-Flow Day,\nKendall \u03c4", 
                     limits = c(-tau_abs, tau_abs), expand = c(0, 0.01)) +
  scale_color_manual(name = "Region", values = pal_regions,
                     labels = lab_regions_skinny1line) +
  stat_smooth(method = "lm", color = "black") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5, 
                              ncol = 2))

# combine using patchwork
(((p_anfd.aridity + theme(axis.title.x = element_blank())) + 
    (p_p2z.aridity + theme(axis.title.x = element_blank())) + 
    (p_zff.aridity)) +
    plot_layout(ncol = 1, guides = "collect") + 
    plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') &
    theme(legend.position = "bottom",
          plot.tag.position = c(0.05, 1))) +
  ggsave(file.path("figures_manuscript", "Trends_CompareTrendsToDrivers-MetricsVsAridity_NoText.pdf"),
         width = 95, height = 210, units = "mm", device = cairo_pdf)

# get correlation between trends
cor(x = fit_data_in$tau_p.pet_cy, y = fit_data_in$tau_annualnoflowdays, use = "complete.obs")
cor(x = fit_data_in$tau_p.pet_cy, y = fit_data_in$tau_peak2z_length, use = "complete.obs")
cor(x = fit_data_in$tau_p.pet_cy, y = fit_data_in$tau_zeroflowfirst, use = "complete.obs")


lm(tau_annualnoflowdays ~ tau_p.pet_cy, data = fit_data_in) %>% summary()
lm(tau_peak2z_length ~ tau_p.pet_cy, data = fit_data_in) %>% summary()
lm(tau_zeroflowfirst ~ tau_p.pet_cy, data = fit_data_in) %>% summary()

## exploratory
fit_data_in %>% 
  dplyr::select(region, tau_annualnoflowdays, tau_zeroflowfirst, tau_peak2z_length, T_max_c_cy) %>% 
  tidyr::pivot_longer(starts_with("tau_")) %>% 
  ggplot(aes(x = T_max_c_cy, y = value)) +
  geom_hline(yintercept = 0, color = col.gray) + 
  geom_point(aes(color = region)) +
  facet_wrap(~name) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_manual(name = "Region", values = pal_regions,
                     labels = lab_regions_skinny1line) +
  stat_smooth(method = "lm", color = "black") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5, 
                              ncol = 2))

cor(x = fit_data_in$T_max_c_cy, y = fit_data_in$tau_annualnoflowdays, use = "complete.obs")
cor(x = fit_data_in$tau_p.pet_cy, y = fit_data_in$tau_annualnoflowdays, use = "complete.obs")
cor(x = fit_data_in$p.pet_cy, y = fit_data_in$tau_annualnoflowdays, use = "complete.obs")
cor(x = fit_data_in$awcave, y = fit_data_in$tau_annualnoflowdays, use = "complete.obs")

# rank by strength of linear correlation
df_cor_anf <-
  fit_data_in %>% 
  dplyr::select(-gage_ID, -region, -Sample) %>% 
  cor() %>% 
  as.data.frame() %>% 
  dplyr::mutate(metric = rownames(.)) %>% 
  dplyr::select(metric, tau_annualnoflowdays) %>% 
  subset(complete.cases(.) & metric != "tau_annualnoflowdays") %>% 
  dplyr::rename(pearson_r = tau_annualnoflowdays) %>% 
  dplyr::arrange(-abs(pearson_r))

# take the residual of tau_annualnoflowdays and tau_p
fit_anf.p.pet <- lm(tau_annualnoflowdays ~ tau_p.pet_cy, data = fit_data_in)
fit_data_in$tau_anf_p.pet_resid <- fit_anf.p.pet$residual
fit_data_in$tau_anf_p.pet_resid.resid <- lm(tau_anf_p.pet_resid ~ tau_annualnoflowdays, data = fit_data_in)$residual

# rank by strength of linear correlation with residual
df_cor_anf_resid <-
  fit_data_in %>% 
  dplyr::select(tau_anf_p.pet_resid, tau_annualnoflowdays,
                all_of(c("dams_n", "majordams_n", "wuse_mm", "irrig_prc", "lulc_water_prc", 
                         "lulc_dev_prc", "lulc_wetland_prc", "lulc_forest_prc", "lulc_barren_prc", 
                         "lulc_grass_prc", "lulc_ag_prc", "drain_sqkm", "elev_mean_m_basin", 
                         "slope_pct", "awcave", "permave", "topwet", "depth_bedrock_m", 
                         "porosity", "clayave", "siltave"))) %>% 
  cor() %>% 
  as.data.frame() %>% 
  dplyr::mutate(metric = rownames(.)) %>% 
  dplyr::select(metric, tau_anf_p.pet_resid) %>% 
  subset(complete.cases(.) & metric != "tau_anf_p.pet_resid") %>% 
  dplyr::rename(pearson_r = tau_anf_p.pet_resid) %>% 
  dplyr::arrange(-abs(pearson_r))

ggplot(fit_data_in, aes(x = tau_p.pet_cy, y = tau_annualnoflowdays)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(fit_data_in, aes(x = tau_annualnoflowdays, y = tau_anf_p.pet_resid)) +
  geom_hline(yintercept = 0, color = col.gray) + 
  geom_vline(xintercept = 0, color = col.gray) +
  geom_point(aes(color = region)) +
  scale_y_continuous(name = "Residual of\n(\u03c4 Annual No-Flow Days ~ \u03c4 P/PET)", expand = c(0, 0.01)) +
  scale_x_continuous(name = "Annual No-Flow Days,\nKendall \u03c4", expand = c(0, 0.01)) +
  scale_color_manual(name = "Region", values = pal_regions,
                     labels = lab_regions_skinny1line) +
  stat_smooth(method = "lm", color = "black") +
  theme(legend.position = "bottom") +
  ggsave(file.path("figures_manuscript", "Trends_CompareTrendsToDrivers-NoFlowResidVsNoFlowTrend.png"),
         width = 95, height = 110, units = "mm")


# rank by strength of linear correlation with residual
df_cor_anf_resid.resid <-
  fit_data_in %>% 
  dplyr::select(tau_anf_p.pet_resid.resid, tau_annualnoflowdays,
                all_of(c("dams_n", "majordams_n", "wuse_mm", "irrig_prc", "lulc_water_prc", 
                         "lulc_dev_prc", "lulc_wetland_prc", "lulc_forest_prc", "lulc_barren_prc", 
                         "lulc_grass_prc", "lulc_ag_prc", "drain_sqkm", "elev_mean_m_basin", 
                         "slope_pct", "awcave", "permave", "topwet", "depth_bedrock_m", 
                         "porosity", "clayave", "siltave"))) %>% 
  cor() %>% 
  as.data.frame() %>% 
  dplyr::mutate(metric = rownames(.)) %>% 
  dplyr::select(metric, tau_anf_p.pet_resid.resid) %>% 
  subset(complete.cases(.) & metric != "tau_anf_p.pet_resid.resid") %>% 
  dplyr::rename(pearson_r = tau_anf_p.pet_resid.resid) %>% 
  dplyr::arrange(-abs(pearson_r))

# plot 
ggplot(fit_data_in, aes(x = topwet, y = tau_anf_p.pet_resid.resid)) +
  geom_hline(yintercept = 0, color = col.gray) + 
  geom_point(aes(color = region)) +
  scale_y_continuous(name = "Residual of Residual", expand = c(0, 0.01)) +
  scale_x_continuous(name = "Topographic Wetness Index", expand = c(0, 0.01)) +
  scale_color_manual(name = "Region", values = pal_regions,
                     labels = lab_regions_skinny1line) +
  stat_smooth(method = "lm", color = "black") +
  theme(legend.position = "bottom") +
  ggsave(file.path("figures_manuscript", "Trends_CompareTrendsToDrivers-NoFlowResidResidVsTopWet.png"),
         width = 95, height = 110, units = "mm")

## multiple linear regression: top climate, land use, and physiographic variable
fit_mlr <- lm(tau_annualnoflowdays ~ tau_p.pet_cy + lulc_ag_prc + topwet, data = fit_data_in)
summary(fit_mlr)

## multiple plots
fit_data_in %>% 
  dplyr::select(tau_annualnoflowdays, tau_p.pet_cy, tau_anf_p.pet_resid, region) %>% 
  tidyr::pivot_longer(cols = c("tau_p.pet_cy", "tau_anf_p.pet_resid")) %>% 
  dplyr::mutate(name = factor(name, levels = c("tau_p.pet_cy", "tau_anf_p.pet_resid"))) %>% 
  ggplot(aes(x = value, y = tau_annualnoflowdays, color = region)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_vline(xintercept = 0, color = col.gray) +
  geom_point() +
  facet_wrap(~name, scales = "free", 
             labeller = as_labeller(c("tau_p.pet_cy" = "P/PET Kendall \u03c4",
                                      "tau_anf_p.pet_resid" = "Residual of plot on left"))) +
  scale_y_continuous(name = "Annual No-Flow Days,\nKendall \u03c4", expand = c(0, 0.01),
                     limits = c(-0.7, 0.7)) +
  scale_x_continuous(name = "Value of Variable at top of plot", expand = c(0, 0.01)) +
  scale_color_manual(name = "Region", values = pal_regions,
                     labels = lab_regions_skinny1line) +
  stat_smooth(method = "lm", color = "black") +
  theme(legend.position = "bottom") +
  ggsave("figures_manuscript/Trends_CompareTrendsToDrivers-NoFlowToAridity+Resid.png",
         width = 160, height = 100, units = "mm")

### messing around with copulas
## example here: https://datascienceplus.com/modelling-dependence-with-copulas/
library(MASS)
library(copula)
library(VineCopula)
m <- 3
n <- 2000
sigma <- matrix(c(1, 0.4, 0.2,
                  0.4, 1, -0.8,
                  0.2, -0.8, 1), 
                nrow=3)
z <- mvrnorm(n,mu=rep(0, m),Sigma=sigma,empirical=T)

tau_anf <- fit_data_in$tau_annualnoflowdays
tau_p.pet <- fit_data_in$tau_p.pet_cy

tau_both <- matrix(data = c(tau_anf, tau_p.pet), ncol = 2)

plot(tau_p.pet, tau_anf)
pairs(tau_both)

pnorm_both <- pnorm(tau_both)
pairs(pnorm_both)

pobs_anf <- pobs(tau_anf)
pobs_p.pet <- pobs(tau_p.pet)

selectedCopula <- BiCopSelect(pobs_anf, pobs_p.pet)
selectedCopula$family # Frank copula

fit_cop <- fitCopula(t.cop,m,method='ml')

## test model mis-specification
library(lmtest)
resettest(tau_anf ~ tau_p.pet)

lm(tau_anf ~ tau_p.pet) %>% summary()
lm(tau_anf ~ (tau_p.pet^4)) %>% summary()
