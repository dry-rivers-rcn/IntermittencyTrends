## Climate_ZscoreByRegion.R

source(file.path("code", "paths+packages.R"))

## load annual data
gage_regions <- 
  readr::read_csv(file.path("results", "00_SelectGagesForAnalysis_GageRegions.csv"))

gage_sample_annual <-
  readr::read_csv(file = file.path("results", "00_SelectGagesForAnalysis_GageSampleAnnual.csv")) %>% 
  dplyr::left_join(gage_regions, by = "gage_ID") %>% 
  dplyr::select(gage_ID, region, currentclimyear, p_mm_cy, pet_mm_cy, T_min_c_cy)

## summarize to mean and SD by region
region_mean_sd <- 
  gage_sample_annual %>% 
  dplyr::group_by(region) %>% 
  dplyr::summarize(p_mm_mean = mean(p_mm_cy),
                   pet_mm_mean = mean(pet_mm_cy),
                   Tmin_C_mean = mean(T_min_c_cy),
                   p_mm_sd = sd(p_mm_cy),
                   pet_mm_sd = sd(pet_mm_cy),
                   Tmin_C_sd = sd(T_min_c_cy))

## merge and calculate z-score
gage_annual_z <-
  gage_sample_annual %>% 
  dplyr::left_join(region_mean_sd, by = c("region")) %>% 
  dplyr::mutate(p_z = (p_mm_cy - p_mm_mean)/p_mm_sd,
                pet_z = (pet_mm_cy - pet_mm_mean)/pet_mm_sd,
                Tmin_z = (T_min_c_cy - Tmin_C_mean)/Tmin_C_sd)

## summarize by region
region_annual_z <-
  gage_annual_z %>% 
  dplyr::group_by(region, currentclimyear) %>% 
  dplyr::summarize(p_z_mean = mean(p_z),
                   p_z_min = min(p_z),
                   p_z_max = max(p_z),
                   pet_z_mean = mean(pet_z),
                   pet_z_min = min(pet_z),
                   pet_z_max = max(pet_z),
                   Tmin_z_mean = mean(Tmin_z),
                   Tmin_z_min = min(Tmin_z),
                   Tmin_z_max = max(Tmin_z))

## plot
labs_zplot <- c("p_z_mean" = "Annual Sum Precip",
                "pet_z_mean" = "Annual Sum PET",
                "Tmin_z_mean" = "Annual Mean Tmin",
                lab_regions_2line)
region_annual_z %>% 
  dplyr::select(region, currentclimyear, p_z_mean, pet_z_mean, Tmin_z_mean) %>% 
  tidyr::pivot_longer(cols = -c(region, currentclimyear),
                      names_to = "variable", values_to = "z_score") %>% 
  ggplot(aes(x = currentclimyear, y = z_score, color = region)) +
  annotate("rect", xmin = -Inf, xmax = 1999, ymin = -Inf, ymax = Inf,
           fill = col.gray, alpha = 0.5) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_line() +
  facet_grid(variable ~ region,
             labeller = as_labeller(labs_zplot)) +
  scale_color_manual(name = "Region", values = pal_regions) + 
  scale_x_continuous(name = "Climate Year",
                     breaks = seq(1980, 2010, 10),
                     labels = c("1980", " ", "2000", " ")) +
  scale_y_continuous(name = "Mean Z-Score\n(all gages in region)") +
  theme(legend.position = "bottom") +
  ggsave(file.path("results", "Climate_ZscoreByRegion.png"),
         width = 190, height = 140, units = "mm")

ggplot(subset(gage_annual_z, gage_ID == 1109070),
       aes(x = currentclimyear, y = p_z)) +
  geom_line()
