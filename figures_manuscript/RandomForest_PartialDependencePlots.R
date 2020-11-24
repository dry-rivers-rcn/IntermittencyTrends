## RandomForest_PartialDependencePlots.R

source(file.path("code", "paths+packages.R"))

# load data
fit_pdp_out <- 
  readr::read_csv(file.path("results", "04_RandomForest_RunModels_PartialDependence.csv"))

# plot
fit_pdp_anf <- subset(fit_pdp_out, region == "National" & metric == "annualnoflowdays")
p_anf <- 
  ggplot(fit_pdp_anf, aes(x = value, y = yhat)) +
  geom_line() +
  facet_wrap(predictor~., scale = "free") +
  labs(title = "(a) Annual No-Flow Days")

fit_pdp_p2z <- subset(fit_pdp_out, region == "National" & metric == "peak2z_length")
p_p2z <- 
  ggplot(fit_pdp_p2z, aes(x = value, y = yhat)) +
  geom_line() +
  facet_wrap(predictor~., scale = "free") +
  labs(title = "(b) Days from Peak to No-Flow")

fit_pdp_zff <- subset(fit_pdp_out, region == "National" & metric == "zeroflowfirst")
p_zff <- 
  ggplot(fit_pdp_zff, aes(x = value, y = yhat)) +
  geom_line() +
  facet_wrap(predictor~., scale = "free") +
  labs(title = "(c) First No-Flow Day")

(p_anf / p_p2z / p_zff) +
  ggsave(file.path("figures_manuscript", "RandomForest_PartialDependencePlots.png"),
         width = 190, height = 500, units = "mm")

subset(fit_pdp_out, predictor == "p.pet_cy.previous" & region == "National" & metric == "annualnoflowdays") %>% 
  ggplot(aes(x = value, y = yhat)) +
  geom_line() +
  labs(title = "Partial dependence plot for annualnoflowdays ~ p.pet_cy.previous")

test <- subset(fit_pdp_out, predictor == "p.pet_cy.previous" & region == "National" & metric == "annualnoflowdays")
