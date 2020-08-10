## MannWhitney_MessingAround.R
# Messing around with Mann-Whitney test

library(tidyverse)

# sample data
df <- 
  tibble::tibble(year = seq(1980, 2017),
                 annualfractionnoflow = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0.071232877, 0.082191781, 0.008219178, 
                                          0, 0.008219178, 0, 0.273972603, 0.270491803, 0.01369863, 0, 0, 
                                          0.092896175, 0.090410959, 0, 0, 0, 0, 0.024657534),
                 annualdaysnoflow = round(annualfractionnoflow*365))

first_half <- df$annualdaysnoflow[df$year < 1999]
second_half <- df$annualdaysnoflow[df$year >= 1999]

mw_test <- wilcox.test(first_half, second_half)
summary(mw_test)
mw_test$p.value


mean(first_half)
mean(second_half)

median(first_half)
median(second_half)
