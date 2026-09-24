
# Load in wind data
wind_u <- stack("e:/env_data/Copernicus/Antips/wind_20260101_20260228.nc",
                varname = "u10")
wind_v <- stack("e:/env_data/Copernicus/Antips/wind_20260101_20260228.nc",
                varname = "v10")

# Plot it out to make sure it looks okay
plot(wind_u[[1]])
plot(wind_v[[1]])

# Create empty lists for wind speed and direction
wind_speed <- stack(wind_u[[1]])
wind_dir <- stack(wind_u[[1]])

# Loop through all the wind rasters and populate the speed and direction lists
for(i in 1:1416){
  
  wind_speed[[i]] <- sqrt((wind_u[[i]])^2 + (wind_v[[i]])^2)
    
  
  wind_dir[[i]] <-
    ((atan2((wind_u[[i]]/wind_speed[[i]]),
            (wind_v[[i]]/wind_speed[[i]]))) * (180 / pi)) + 180
  
  values(wind_dir[[i]]) <-
    ifelse(values(wind_dir[[i]]) > 180,
           values(wind_dir[[i]]) - 360,
           values(wind_dir[[i]]))
  
  print(i)
}

# Save these off for later
save(wind_speed, file = "e:/env_data/Copernicus/Antips/wind_speed.RData")
save(wind_dir, file = "e:/env_data/Copernicus/Antips/wind_dir.RData")

# Load these in (and skip above steps)
load("e:/env_data/Copernicus/Antips/wind_speed.RData")
load("e:/env_data/Copernicus/Antips/wind_dir.RData")

# Create a time sequence that covers the range of wind data available
time_seq <- seq(ymd_hms("2026-01-01 00:00:00"), by =  3600, length.out = 1416)

# Create placeholder variables for wind data in trip dfs
trip_df[, c("wind_sp", "wind_dir", "wind_u", "wind_v")] <- NA
trip_df_int[, c("wind_sp", "wind_dir", "wind_u", "wind_v")] <- NA

# Fill these from the wind raster stacks
for(i in 962:1416){
  subset <- which(trip_df$date_time >= time_seq[i] &
                    trip_df$date_time < time_seq[i + 1])
  
  subset_int <- which(trip_df_int$date_time >= time_seq[i] &
                        trip_df_int$date_time < time_seq[i + 1])
  
  locations <- trip_df[subset, c("Longitude", "Latitude")]
  locations$Longitude <- ifelse(locations$Longitude > 179.875,
                                locations$Longitude - 360,
                                locations$Longitude)
  
  
  locations_int <- trip_df_int[subset_int, c("Longitude", "Latitude")]
  locations_int$Longitude <- ifelse(locations_int$Longitude > 179.875,
                                locations_int$Longitude - 360,
                                locations_int$Longitude)
  
  trip_df$wind_sp[subset] <- 
    extract(wind_speed[[i]], locations, method = "bilinear")
  trip_df$wind_dir[subset] <- 
    extract(wind_dir[[i]], locations, method = "bilinear")
  trip_df$wind_u[subset] <- 
    extract(wind_u[[i]], locations, method = "bilinear")
  trip_df$wind_v[subset] <- 
    extract(wind_v[[i]], locations, method = "bilinear")
  
  trip_df_int$wind_sp[subset_int] <- 
    extract(wind_speed[[i]], locations_int, method = "bilinear")
  trip_df_int$wind_dir[subset_int] <- 
    extract(wind_dir[[i]], locations_int,method = "bilinear")
  trip_df_int$wind_u[subset_int] <- 
    extract(wind_u[[i]], locations_int, method = "bilinear")
  trip_df_int$wind_v[subset_int] <- 
    extract(wind_v[[i]], locations_int, method = "bilinear")
  
  print(i)
}

# Check max wind speeds out of interest
max(trip_df$wind_sp, na.rm = T)

# Split up trip df and get wind offset and other variables
trip_df <- split(trip_df, trip_df$trip_id) %>%
  # lapply(., wind_traj) %>%
  lapply(., speeds) %>%
  bind_rows()

# Split up interpolated trip df and get wind offset and other variables
trip_df_int <- split(trip_df_int, trip_df_int$trip_id) %>%
  # lapply(., wind_traj) %>%
  lapply(., speeds) %>%
  bind_rows()

# Write some RData files
save(trip_df_int, file = "data/cleaned/2026/trip_df_int.RData")
save(trip_df, file = "data/cleaned/2026/trip_df.RData")

# Plotting wind and tracks together ---------------------------------------

# Put wind data into a stack and name the elements with timestamps
wind_stack <- stack(wind_speed)
names(wind_stack) <- as.character(time_seq + 1)

# Turn this raster stack into a dataframe for plotting
wind_df <- rasterToPoints(wind_stack) %>%
  as.data.frame() %>%
  `colnames<-`(c("lon", "lat", names(wind_stack))) %>%
  tidyr::pivot_longer(cols = starts_with("X"),
               names_to = "layer",
               values_to = "val") %>%
  mutate(layer = substr(layer, 2, 14)) %>%
  mutate(date_time = as.POSIXct(layer, format = "%Y.%m.%d.%H"))

# Cut down this dataframe to speed things up
wind_df_res <- wind_df %>%
  mutate(lon = ifelse(lon < 0, lon + 360, lon)) %>%
  filter(lon > 172,
         lon < 225)

# Put wind direction into a stack too
wind_dir_stack <- stack(wind_dir)

# Again name with timestamps
names(wind_dir_stack) <- as.character(time_seq + 1)

# And turn it into a dataframe
wind_dir_df <- rasterToPoints(wind_dir_stack) %>%
  as.data.frame() %>%
  `colnames<-`(c("lon", "lat", names(wind_dir_stack))) %>%
  tidyr::pivot_longer(cols = starts_with("X"),
                      names_to = "layer",
                      values_to = "val") %>%
  mutate(layer = substr(layer, 2, 14)) %>%
  mutate(date_time = as.POSIXct(layer, format = "%Y.%m.%d.%H"))

# Put speed back into the direction dataframe
wind_dir_df$speed <- wind_df$val

# Cut this dataframe down too for speed, and sample to every 2 degrees
wind_dir_df_res <- wind_dir_df %>%
  mutate(lon = ifelse(lon < 0, lon + 360, lon)) %>%
  filter(lon > 174,
         lon < 225) %>%
  filter(lon %in% seq(174, 226, by = 2),
         lat %in% seq(-46, -33, by = 2)) %>%
  mutate(angle = ifelse((val + 90) < 0, val + 450, val + 90),
         angle_rad = -angle * (pi/180))

# Edit the tracks so that timestamps are rounded to the nearest hour
graph_tracks <- 
  trip_df %>%
  mutate(dtm = format(date_time, "%d%m%y %H"),
         date_time = dmy_h(dtm))

# Plot the whole thing
wind_map <-
  ggplot() +
  coord_cartesian(xlim = c(174, 225),
                  ylim = c(-46, -33),
                  expand = F) +
  geom_tile(data = wind_df_res,
            aes(x = lon,
                y = lat,
                fill = val,
                group = date_time)) +
  geom_spoke(data = wind_dir_df_res,
               aes(x = lon, y = lat,
                 angle = angle_rad, radius = scales::rescale(speed, c(.2, .8))),
             linewidth = 1,
    arrow = arrow(length = unit(.05, 'inches'))) + 
  scale_fill_viridis_c(option = "B", breaks = c(0, 10, 20)) +
  geom_point(data = graph_tracks,
             aes(x = Longitude_cont, y = Latitude, colour = trip_id),
             size = 4) +
  geom_path(data = graph_tracks,
            aes(x = Longitude_cont, y = Latitude, colour = trip_id),
            linewidth = 0.8, alpha = 0.7) +
  scale_x_continuous(breaks = c(180, 180.2, 190, 200, 220),
                     labels = c("±180", "",-170, -160, -140)) +
  geom_polygon(data = land_df_cut, aes(x = long, y = lat, group = group),
               fill  = "#353535", colour = "black") +
  transition_time(date_time) +
  theme(legend.position = "none") +
  labs(x = "Longitude", y = "Latitude")

# And render into an mp4
gganimate::animate(
  wind_map,
  width = 1600,
  height = 700,
  renderer = av_renderer("movies/2026/wind_map.mp4"),
  fps = 30, duration = 30)

# This plot shows the distribution of wind offsets relative to strength
# wind_roses <- 
  ggplot(trip_df_int %>%
                       mutate(daynight = ifelse(sun_angle > -6, "day", "night"))) +
  geom_histogram(aes(x = -wind_off, fill = as.factor(daynight)), 
                 colour = "black", breaks = c(-12:12) * 15) +
  scale_fill_viridis_d(option = "G", end = 0.7, begin = 0.2) +
  facet_wrap(facets = ~ 
               ifelse(wind_sp < 10,
                      "1. Wind < 10m/s",
                      "2. Wind > 10m/s") * daynight,
             nrow = 2,
             scales = "free_y") +
  scale_x_continuous(breaks = c(-180, -90, 0, 90, 180)) +
  scale_y_continuous(transform = "identity", n.breaks = 3) +
  coord_radial(start = pi, inner.radius = 0.2, expand = F) +
  labs(x = "", y = "Quantity of track points", fill = "Trip type", title = "A") +
  theme_nice() +
  theme(legend.position = "bottom")

  
  ggplot(trip_df_int) + geom_point(aes(x = wind_sp, y = speed, colour = sun_angle),
                               size = 0.5) +
    scale_colour_viridis_c(option = "H") +
    facet_wrap(facets = ~id) +
    theme_nice()
  