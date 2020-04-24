## Figure_MapOfGages.R

source(file.path("code", "paths+packages.R"))

# load gages
df_gages <- 
  file.path("results", "00_SelectGagesForAnalysis_GageSampleMean.csv") %>% 
  readr::read_csv()

sf_gages <-
  df_gages %>%
  sf::st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326) 

# load state map
states <- map_data("state")

# plot
ggplot() + 
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = NA, color = col.gray) +
  geom_sf(data = sf_gages, aes(color = region, shape = CLASS)) +
  scale_x_continuous(name = "Longitude") +
  scale_y_continuous(name = "Latitude") +
  scale_color_manual(name = "Region",
                     values = pal_regions) +
  scale_shape_manual(name = "Class",
                     values = c("Non-ref" = 1, "Ref" = 2),
                     labels = c("Non-ref" = "Non-reference",
                                "Ref" = "Reference")) +
  coord_sf() +
  theme(panel.border = element_blank()) +
  ggsave(file.path("figures_manuscript", "Figure_MapOfGages.png"),
         width = 190, height = 85, units= "mm")
