
# Load in required packages
require(tidyverse)
require(raster)
require(marmap)

# Load in dfs if already made
load(file = "data/cleaned/2026/trip_df_int.RData")
load(file = "data/cleaned/2026/trip_df.RData")

# Axytrek input -----------------------------------------------------------

# List of Axy files
filename <-
  c("W638/CRAIG8_S1.csv",
    "G227/WWF2_S1.csv",
    "W95F/MIKE11_S1.csv",
    "W704/WWF1_S1.csv",
    "B09F/WWF5_S1.csv",
    "W64D/MARY7_S1.csv",
    "W97F/WWF2_S1.csv",
    "W870/RAD6_S1.csv")

# Create an empty list to fill with TDR files
gps_ls <- list()
tdr_ls <- list()
radar_ls <- list()

# Make a lil function to extract GPS and TDR data only from AxyTrek data
for(i in 1:length(filename)){# Reads in CSV
  acc <- read.csv(paste("c:/Users/Admin/Desktop/Tracking_data/Albie_2026/",
                        filename[i], sep = ""),
                  sep = "\t", header = F, skip = 1)
  # Asigns corrects columns to data
  colnames(acc) <- c("TagID",	"Date",	"Time",
                     "X", "Y", "Z", "Activity",
                     "Pressure", "Temp", "Latitude",
                     "Longitude", "Altitude", "Speed",
                     "Satellites", "hdop", "Signal", "Radar",
                     "Battery",	"Metadata")
  
  acc$id <- sub("/.*", "", filename[i])
  # Extract depth and location data
  tdr_ls[[i]] <- acc[which(!is.na(acc$Pressure)), ]
  gps_ls[[i]] <- acc[which(!is.na(acc$Latitude)), ]
  radar_ls[[i]] <- acc[which(!is.na(acc$Radar)), ]
}

# Bind to dataframe
axy_tdr <- bind_rows(tdr_ls) %>% mutate(date_time = ymd_hms(paste(Date, Time)))
axy_gps <- bind_rows(gps_ls) %>%
  mutate(date_time = ymd_hms(paste(Date, Time)),
         Longitude_cont = ifelse(Longitude < 0, Longitude + 360, Longitude))
axy_radar <- bind_rows(radar_ls) %>% mutate(date_time = ymd_hms(paste(Date, Time)))

# Plot depth
ggplot(axy_tdr) +
  geom_line(aes(x = date_time, y = -(Pressure), colour = Pressure)) +
  facet_wrap(facets = ~id)

# Write out GPS and TDR data
save(axy_tdr, file = "data/cleaned/2026/axy_tdr.RData")
save(axy_gps, file = "data/cleaned/2026/axy_gps.RData")
save(axy_radar, file = "data/cleaned/2026/axy_radar.RData")

# IGot-U data input -------------------------------------------------------

# List of filenames and where to find data
filename <- c("W18H/GS119.csv",
              "B905/GS103.csv",
              "B25G/GS131.csv")

# Loop through filenames and extract data
gps_ls <- list()

for(i in 1:length(filename)){
  gps_ls[[i]] <- 
    read.csv(paste("c:/Users/Admin/Desktop/Tracking_data/Albie_2026/",
                   filename[i], sep = ""))
  
  gps_ls[[i]]$id <- sub("/.*", "", filename[i])
}

# Bind together igotu dataframes
igotu_gps <- gps_ls %>%
  lapply(., function(x){
    x$date_time = ymd_hms(x$Time)
    x <- x[order(x$date_time), ]
    x <- x[!duplicated(x$date_time), ]
    x}) %>%
  bind_rows() %>%
  mutate(Longitude_cont = ifelse(Longitude < 0, Longitude + 360, Longitude))

# Write out a RData file
save(igotu_gps, file = "data/cleaned/2026/igotu_gps.RData")

# Tidying and visualisation -----------------------------------------------

# Build a bathy object
bathy_rstr <- getNOAA.bathy(lon1 = -180, lat1 = -75,
                            lon2 = 180, lat2 = -30,
                            resolution = 6)

# Convert to a dataframe
bathy_raster_df <-
  as.data.frame(rasterToPoints(as.raster(bathy_rstr / 1000))) %>%
  filter(x < 180)

# Land values to NA
bathy_raster_df$layer[which(bathy_raster_df$layer >= 0)] <- NA

# Center longitude around 180
bathy_raster_df <- bathy_raster_df %>%
  mutate(x_centred = ifelse(x < 0, x + 360, x))

# Create cropped dataframe for faster plotting
bathy_raster_df_res <- bathy_raster_df %>%
  filter(x_centred > 160, x_centred < 270, y > -75, y < -30)

# Plot out some tracks
track_plot <-
  ggplot() +
  theme(panel.background = element_rect(fill = "#454545")) +
  coord_cartesian(xlim = c(165, 250), ylim = c(-65, -33)) +
  scale_colour_viridis_d(option = "H", begin = 0.5, end = 0.8, 
                         guide = "none") +
  scale_fill_viridis_c(option = "D", trans = "sqrt", begin = 0.7, end = 0,
                       guide = "none") +
  geom_tile(data = bathy_raster_df_res,
               aes(x = x_centred, y = y, fill = abs(layer)),
               na.rm = T, alpha = 0.9) + 
  geom_contour(data = bathy_raster_df_res,
               aes(x = x_centred, y = y, z = abs(layer)),
               na.rm = T, alpha = 0.4, linewidth = 0.5, colour = "#282828") +
  scale_x_continuous(breaks = c(180, 180.2, 200, 220, 240),
                     labels = c("±180", "", -160, -140, -120)) +
  scale_y_continuous(breaks = c(-65 ,-55, -45, -35)) +
  geom_polygon(data = land_df_cut, aes(x = long, y = lat, group = group),
               fill  = "#353535", colour = "black") +
  geom_path(data = axy_gps,
            aes(x = Longitude_cont, y = Latitude, colour = id),
            linewidth = 1, alpha = 0.7) +
  geom_path(data = igotu_gps,
            aes(x = Longitude_cont, y = Latitude, colour = id),
            linewidth = 1, alpha = 0.7) +
  labs(x = "Longtiude", y = "Latitude")

# track_plot

# Save off the plot
ggsave(track_plot, filename = "plots/track_plotb.png",
       width = 16, height = 8, dpi = 500)

# Combine all the tracking data -------------------------------------------

# Load in tracks again
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

# Write some RData files
save(trip_df_int, file = "data/cleaned/2026/trip_df_int.RData")
save(trip_df, file = "data/cleaned/2026/trip_df.RData")

# Plot out TDR and radar data ---------------------------------------------

load(file = "data/cleaned/2026/axy_radar.RData")
load(file = "data/cleaned/2026/axy_tdr.RData")

# Plot radar
axy_radar %>%# filter(id == "W704") %>%
  ggplot() +
  geom_path(aes(x = date_time, y = Radar,
                colour = Radar, group = id)) + labs(x = "Date") +
  scale_colour_viridis_c(option = "H", end = 1, guide = "none") +
  facet_wrap(facets = ~id) 

# Plot depth
axy_tdr %>%
  ggplot() +
  geom_path(aes(x = date_time, y = Pressure,
                colour = Pressure, group = id)) +
  scale_colour_viridis_c(option = "G", end = 0.5, guide = "none") +
  facet_wrap(facets = ~id) 
