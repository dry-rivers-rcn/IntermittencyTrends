## Figure_TrendMap.R
# Map trends in no-flow metrics.

## load data
# trend results
site_trends <-
  file.path("results", "01_CalculateTrends.csv") %>% 
  readr::read_csv()

# HPA boundary
sf_HPA <- 
  sf::st_read("C:/Users/samzipper/OneDrive - The University of Kansas/GIS_GeneralFiles/HPA_Boundary/hp_bound2010.shp") %>% 
  subset(AQUIFER == "High Plains aquifer")

# locations of stream gages
sf_gages <- 
  file.path("data", "USGS_GageLocations.gpkg") %>% 
  sf::st_read() %>% 
  subset(site_no %in% site_trends$site) %>% 
  sf::st_transform(proj_crs)

## join trends to spatial locations
sf_site_trends <- 
  dplyr::right_join(site_trends, sf_gages, by = c("site" = "site_no"))

## plot
ggplot() +
  geom_sf(data = sf_HPA) +
  geom_sf(data = sf_gages)

p_noflowdays <-
  ggplot() +
  geom_sf(data = sf_HPA, color = "black", fill = NA) +
  geom_sf(data = subset(sf_site_trends, metric == "noflowdays"), 
          aes(geometry = geom, shape = p_value < 0.05, color = slope)) +
  scale_shape_manual(name = "Significant (p < 0.05)", 
                     values = c("FALSE" = 1, "TRUE" = 16), guide = F) +
  scale_color_gradient2(name = "Trend [days/yr]", low = "blue", mid = "gray65", high = "red") +
  labs(title = "Trend, Annual No Flow Days",
       subtitle = "Filled Circles = Sig. Trend (p < 0.05)") +
  theme(legend.position = "bottom")

p_noflowperiods <- 
  ggplot() +
  geom_sf(data = sf_HPA, color = "black", fill = NA) +
  geom_sf(data = subset(sf_site_trends, metric == "noflowperiods"), 
          aes(geometry = geom, shape = p_value < 0.05, color = slope)) +
  scale_shape_manual(name = "Significant (p < 0.05)", 
                     values = c("FALSE" = 1, "TRUE" = 16), guide = F) +
  scale_color_gradient2(name = "Trend [periods/yr]", low = "blue", mid = "gray65", high = "red") +
  labs(title = "Trend, Annual No Flow Periods",
       subtitle = "Filled Circles = Sig. Trend (p < 0.05)") +
  theme(legend.position = "bottom")

p_noflowlength <- 
  ggplot() +
  geom_sf(data = sf_HPA, color = "black", fill = NA) +
  geom_sf(data = subset(sf_site_trends, metric == "noflowlength"), 
          aes(geometry = geom, shape = p_value < 0.05, color = slope)) +
  scale_shape_manual(name = "Significant (p < 0.05)", 
                     values = c("FALSE" = 1, "TRUE" = 16), guide = F) +
  scale_color_gradient2(name = "Trend [days/yr]", low = "blue", mid = "gray65", high = "red") +
  labs(title = "Trend, Max No Flow Length",
       subtitle = "Filled Circles = Sig. Trend (p < 0.05)") +
  theme(legend.position = "bottom")

plot_grid(p_noflowdays, p_noflowperiods, p_noflowlength, ncol = 3) %>% 
  save_plot(file.path("results", "Figure_TrendMap.png"), plot = .,
            base_width = 8.5, base_height = 8)
