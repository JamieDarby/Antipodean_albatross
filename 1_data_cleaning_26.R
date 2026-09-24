
# Load in required packages
require(tidyverse)
require(raster)

# Load in tracks
load(file = "data/cleaned/2026/axy_gps.RData")
load(file = "data/cleaned/2026/igotu_gps.RData")

head(axy_gps)
head(igotu_gps)

# List the common variables
vars <- c("id", "date_time", "Latitude", "Longitude")

# Make a new total gps tracking dataset
total_gps <- rbind(axy_gps[, vars], igotu_gps[, vars])

total_gps$TrackTime <- total_gps$date_time
total_gps$ID <- total_gps$id

# List by ID
id_ls <- split(total_gps,
               total_gps$id)

# Function to assign trip names
id_ls <- lapply(id_ls,
                Birdtrip,
                x = 178.797898,
                y = -49.674747,
                InBuff = 1,
                RetBuff = 5,
                Duration = 6)

# Combine list of birds into single dataframe
total_gps <- do.call(rbind, id_ls) %>%
  dplyr::select(-ColDist, - ID, -TrackTime, -coords.x1, -coords.x2)

# Append distance to colony to each location in the dataframe
total_gps$col_dist <- 
  pointDistance(p1 = total_gps[, c("Longitude", "Latitude")],
                p2 = c(178.797898, -49.674747), lonlat = T)

# Create list of trips, excluding points that are not part of trips
trip_ls <- total_gps %>%
  filter(trip_id != "-1") %>%
  split(., . $trip)

# Get the time interval between track points
trip_ls <- lapply(trip_ls, function(x){
  x$int <- as.numeric(difftime(x$date_time, lag(x$date_time), units = "secs"))
  
  x$int_std <- ifelse(min(x$int, na.rm = T) < 400, "short", "long")
  
  x
})

# Wrap up the trip list into a dataframe of all trips
trip_df <- trip_ls %>%
  bind_rows() %>%
  mutate(Longitude_cont = ifelse(Longitude < 0, Longitude + 360, Longitude))

# unique(short_trip_df$trip_id[which(short_trip_df$Returns == "Y")])
# unique(short_trip_df$trip_id)

# 59 short trips, 58 complete
# 11 medium trips, 10 complete
# 10 long trips, 9 complete

# Pipe functions to split trips into sections and linearly interpolate
trip_df_int <- trip_df %>%
  # filter(int_std == "short") %>%
  split(., .$trip_id) %>%
  
  # Cut trip sections after more than 20 minute gaps
  lapply(., trip_cleaner, t = 30) %>%
  
  # Rearrange list so that it's split by section
  do.call(rbind, .) %>%
  split(., .$section) %>%
  
  # Remove trips of 20 or fewer points
  .[sapply(
    ., function(x) dim(x)[1]) > 20] %>%
  
  # Interpolate tracks to 5 minute intervals along a linear trajectory
  lapply(., FUN = LinStepR,
         t = 600,
         extras = c("id", "trip_id", "section", "Returns")) %>%
  
  # Convert back to a dataframe
  do.call(rbind, .)

# Check the length of each section
length(unique(trip_df_int$section))
length(unique(trip_df_int$trip_id))

# SP object of transformed interpolated track points
tracks.projected <- SpatialPoints(
  trip_df_int[, c("X", "Y")],
  proj4string = CRS("+proj=laea +lon_0=178.797898 +lat_0=-49.674747"))

# Convert interpolated track points to WGS
tracks.wgs <- spTransform(
  tracks.projected,
  CRS = CRS("+init=epsg:4326"))

# Put the transformed data back into the interpolated dataset 
trip_df_int[, c("Longitude", "Latitude")] <- sp::coordinates(tracks.wgs)

# Get rid of intermediary SP objects
rm(tracks.projected, tracks.wgs)

# Dateline fuckery
trip_df_int$Longitude_cont <- ifelse(trip_df_int$Longitude < 0, 
                                     trip_df_int$Longitude + 360,
                                     trip_df_int$Longitude)

# Append distance to colony to each location in the dataframe
trip_df_int$col_dist <- 
  pointDistance(p1 = trip_df_int[, c("Longitude", "Latitude")],
                p2 = c(178.797898, -49.674747), lonlat = T)

# Sex data for the birds
males <- c("W870", "W638", "W18H", "B905")
trip_df$sex <- ifelse(trip_df$id %in% males, "m", "f")
trip_df_int$sex <- ifelse(trip_df_int$id %in% males, "m", "f")

# Write some RData files
save(trip_df_int, file = "data/cleaned/2026/trip_df_int.RData")
save(trip_df, file = "data/cleaned/2026/trip_df.RData")

# Plot out radar data ---------------------------------------------

load(file = "data/cleaned/2026/axy_radar.RData")

# Plot radar
axy_radar %>%# filter(id == "W704") %>%
  ggplot() +
  geom_path(aes(x = date_time, y = Radar,
                colour = Radar, group = id)) + labs(x = "Date") +
  scale_colour_viridis_c(option = "H", end = 1, guide = "none") +
  facet_wrap(facets = ~id) 
