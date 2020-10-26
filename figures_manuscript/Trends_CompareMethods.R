## Trends_CompareMethods.R
# 
source(file.path("code", "paths+packages.R"))

## metrics we care about
metrics <- c("annualnoflowdays", "zeroflowfirst", "peak2z_length")

## load data
gage_regions <-
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv"))

gage_trends <-
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageSampleTrends.csv")) %>%
  subset(metric %in% metrics) %>%
  dplyr::left_join(gage_regions, by = "gage_ID") %>%
  subset(complete.cases(.))

## summarize
p_thres <- 0.05
gage_trends %>%
  dplyr::group_by(metric) %>%
  dplyr::summarize(n_finite = sum(is.finite(sen_slope)),
                   n_sig = sum(mk_p < p_thres, na.rm = T),
                   n_sig_pos = sum(sen_slope > 0 & mk_p < p_thres, na.rm = T),
                   n_sig_neg = sum(sen_slope <= 0 & mk_p < p_thres, na.rm = T))

## focus on annualnoflowdays (most common metric)
sigtrends_noslope <-
  gage_trends$gage_ID[gage_trends$mk_p < 0.05 &
                      gage_trends$metric == "annualnoflowdays" &
                      gage_trends$sen_slope == 0]
sigtrends_yesslope <-
  gage_trends$gage_ID[gage_trends$mk_p < 0.05 &
                        gage_trends$metric == "annualnoflowdays" &
                        gage_trends$sen_slope != 0]

## now: load data and choose a few case studies for different types of trends
gage_sample_annual <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv"))

## explore gages for example
gage_sample_annual %>% 
  subset(gage_ID == sigtrends_yesslope[43]) %>% 
  ggplot(aes(x = currentclimyear, y = annualnoflowdays)) +
  geom_point()

# sample to a few test gages showing different types of slopes
gages_test <- c(2313230, 6177500, 7362100, 2266205, 2291580, 7315200)

gage_example <-
  gage_sample_annual %>%
  subset(gage_ID %in% gages_test) %>%
  mutate(year = currentclimyear,
         gage = factor(gage_ID, levels = gages_test)) %>%
  dplyr::select(gage, year, annualnoflowdays)

sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

ggplot(gage_example, aes(x = year, y = annualnoflowdays)) +
  geom_point() +
  facet_wrap(~gage, scales = "free_y", ncol = 2) +
  stat_smooth(method = "lm", color = "blue", se = F) +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), color = "green", se = F) +
  stat_smooth(method = sen, color = "red", se = F) +
  scale_y_continuous(name = "Annual No-Flow Days") +
  scale_x_continuous(name = "Year") +
  labs(title = "Comparison of Slope Methods for Some Example Gages",
       subtitle = "Blue = Linear, Green = Poisson, Red = Theil-Sen") +
  ggsave(file.path("figures_manuscript", "Trends_CompareMethods.png"),
         width = 190, height = 220, units = "mm")

## count number of gages that have no-flow in at least 50% of years
df_test <- 
  gage_sample_annual %>% 
  group_by(gage_ID) %>% 
  summarize(n_noflow = sum(annualnoflowdays > 0, na.rm = T),
            n_total = sum(is.finite(annualnoflowdays)),
            prc_noflow = n_noflow/n_total)
sum(df_test$prc_noflow > 0.5)
