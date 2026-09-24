
embc_immerse_plot <- 
  ggplot(trip_df %>%
           filter(embc_simple != 5) %>%
         # mutate(state = ifelse(embc_simple == 4, 2, embc_simple),
                # state = factor(state, labels = c("LL", "LH & HH", "HL")))) +
  mutate(state = factor(embc_simple, levels = c(1, 2, 4, 3),
                        labels = c("Rest", "Intensive search",
                                   "Extensive search", "Transit"))) %>%
  mutate(daynight = ifelse(sun_angle < -6, "night", "day"))) +
  geom_violin(aes(x = state, y = act, fill = state), scale = "area", bounds = c(0, 1)) +
  scale_fill_discrete(palette = c("#1b015e", "#018c81", "#68f2a2", "#018c81"),
                      guide = "none") +
  labs(y = "Proportion of time wet", x = "EMbC state", title = "B") +
  theme_classic()

sex_lat_plot <- 
  ggplot(trip_df_int %>% mutate(Sex = factor(sex, labels = c("Female", "Male")),
                              weight = 1/6)) + 
  geom_histogram(aes(y = Latitude, fill = Sex, weight = weight),
                 alpha = 0.6, colour = "black",
                 position = "stack") +
  labs(x = "Tracked hours", title = "A") +
  scale_fill_viridis_d(option = "H", begin = 0.1, end = 0.9) +
  scale_x_continuous(expand = F) +
  theme_classic()

fig_1 <- 
  plot_grid(sex_lat_plot + border, embc_immerse_plot + border, ncol = 1)

ggsave(fig_1, filename = "plots/fig_1.png",
       width = 8, height = 8, dpi = 500)


require(marmap)
bathy_rstr <- getNOAA.bathy(lon1 = 140, lat1 = -80,
                            lon2 = -90, lat2 = -25,
                            antimeridian = T,
                            resolution = 8)

# Convert to a dataframe
bathy_raster_df <-
  as.data.frame(rasterToPoints(marmap::as.raster(bathy_rstr / 1000)))

# Land values to 0
bathy_raster_df$layer[which(bathy_raster_df$layer >= 0)] <- 0

# Plot out some tracks
albie_track_plot <-
  ggplot() +
  coord_map(xlim = c(170, 240), ylim = c(-65, -30),
            projection = "gilbert") +
  scale_fill_viridis_c(option = "G", trans = "sqrt", begin = 0.4, end = 0,
                       guide = "none") +
  geom_raster(data = bathy_raster_df,
              aes(x = x, y = y, fill = abs(layer)),
              na.rm = T, alpha = 0.5) +
  geom_contour(data = bathy_raster_df,
               aes(x = x, y = y, z = abs(layer)),
               na.rm = T, alpha = 0.4, linewidth = 0.5, colour = "#292929") +
  scale_x_continuous(breaks = c(160, 180, 180.2, 200, 220, 240),
                     labels = c(160, "±180", "", -160, -140, -120)) +
  geom_polygon(data = land_df_cut, aes(x = long, y = lat, group = group),
               fill  = "#999999", colour = "black", linewidth = 0.1) +
  geom_path(data = trip_df,
            aes(x = Longitude_cont, y = Latitude, colour = sun_angle, group = trip_id),
            linewidth = 0.6, alpha = 0.9, linejoin = "round", lineend = "round") +
  theme(panel.background = element_rect(fill = "black"),
        legend.key = element_rect(fill = NA),
        legend.position = c(0.95, 0.875)) +
  scale_colour_viridis_c(option = "H", begin = 0, end = 1) +
  labs(x = "Longtiude", y = "Latitude", colour = "Solar angle")

ggsave(albie_track_plot, filename = "plots/albie_track_plotb.png",
       dpi = 500, width = 12, height = 8)

p_ars_moon <- 
  effect_plot(mod, moon_frac, data = night_df, interval = T,
              plot.points = F, partial.residuals = F) +
  labs(y = "P(ARS)", x = "Moon fraction", title = "E") +
  scale_y_continuous(limits = c(0.09,0.47))

p_ars_wind <- 
  effect_plot(mod, wind_sp, data = night_df, interval = T) +
  labs(y = "P(ARS)", x = "Wind speed (m/s)", title = "F") +
  scale_y_continuous(limits = c(0.09,0.47))

p_ars_lat <- 
  effect_plot(mod, Latitude, data = night_df, interval = T) +
  scale_x_continuous(limits = c(-60, -33)) +
  labs(y = "P(ARS)", x = "Latitude", title = "G") +
  scale_y_continuous(limits = c(0.09,0.47))

border <- 
  theme(panel.background =
          element_rect(colour = "black",
                       fill=NA,
                       linewidth=1))

fig_3 <- plot_grid(plot_grid(plot_grid(count_plotb + labs(title = "A") +
                               theme(legend.position = "bottom"),
                             gls_plot + labs(title = "B",
                                             y = "Proportion of points"),
                             embc_plot + labs(title = "C", 
                                              y = "Proportion of points",
                                              fill = "Movement mode"),
                             nrow = 3) + border,
                   plot_grid(p_ars_moon + labs(title = "D") + theme_classic(),
                             p_ars_wind + labs(title = "E") + theme_classic(),
                             p_ars_lat + labs(title = "F") + theme_classic(),
                             nrow = 3, rel_widths = c(1.05, 1, 1)) + border,
                   nrow = 1),
                   plot_grid(prop_cars + labs(title = "G") + theme_classic()) + border,
                   nrow = 1, rel_widths = c(2,1.2))

ggsave(fig_3, filename = "plots/fig_3.png",
       width = 12, height = 8, dpi = 500)
