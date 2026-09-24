
# Load in wind data !!NOT PRESENT IN CURRENT UPLOAD, ALREADY IN TRIP DATA
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
 
