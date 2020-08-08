## PoissonRegression_MessingAround.R

library(tidyverse)

# sample data
df <- 
  tibble::tibble(year = seq(1980, 2017),
                 annualfractionnoflow = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0.071232877, 0.082191781, 0.008219178, 
                                          0, 0.008219178, 0, 0.273972603, 0.270491803, 0.01369863, 0, 0, 
                                          0.092896175, 0.090410959, 0, 0, 0, 0, 0.024657534),
                 annualdaysnoflow = round(annualfractionnoflow*365))

fit_ps <- glm(annualdaysnoflow ~ year, family=poisson(link = "log"), data = df)

summary(fit_ps)

predicted_ps <- predict(fit_ps, newdata = tibble::tibble(year = seq(1980, 2017)))

exp(coef(fit_ps)[2]) # each year, 8.1% increase

pchisq(fit_ps$deviance, fit_ps$df.residual, lower.tail = FALSE)  # p-value of fit (p > 0.05 means good fit)

# not understanding why these pairs aren't matching exactly
predicted_ps[1] * exp(coef(fit_ps)[2])
predicted_ps[2]

predicted_ps[2] * exp(coef(fit_ps)[2])
predicted_ps[3]

predicted_ps[3] * exp(coef(fit_ps)[2])
predicted_ps[4]

# this is how you generate a prediction for any year
exp((coef(fit_ps)[1] + coef(fit_ps)[2]*1980))
exp((coef(fit_ps)[1] + coef(fit_ps)[2]*2017))

summary(MASS::glm.nb(annualdaysnoflow ~ year, data = df))

summary(fit_lm <- lm(annualdaysnoflow ~ year, data = df))



ggplot(df, aes(x = year, y = annualdaysnoflow)) +
  geom_point() +
  stat_smooth(method = "lm", color = "blue") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), color = "green")
