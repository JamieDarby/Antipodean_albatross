
# Load in activity data
load("data/cleaned/act_ls.RData")

# Turn it into a dataframe
act_df <- bind_rows(act_ls)

# Define a start time per immersion bout
act_df$start_time <- act_df$date_time - act_df$duration

# Load in trip data if not already loaded
load("data/cleaned/2026/trip_df.RData")
load("data/cleaned/2026/trip_df_int.RData")

# These next two operations append number of landings, takeoffs, a combination (activity),
# whether the device was fully wet to each trip dataframe

# Split df into list for appending act data
trip_df <- split(trip_df, trip_df$id) %>%
  # Loop around and append number of associated landings
  lapply(., function(x){
    # Subset act data to only the relevant id
    act <- act_df[which(act_df$id == x$id[1]), ]
    
    # Create variables to populate
    x[, c("landings", "takeoffs", "fully_wet", "activity")] <- NA
    
    # Check that act actually has data
    if(nrow(act) > 0){
      # Loop around x to append act data
      for(i in 1:(nrow(x) - 1)){
        # Create an interval 2.5 minutes before and after a location
        time_seq <- c(x$date_time[i:(i+1)])
        
        if(max(act$date_time) > time_seq[2]){
          # Create a subset of act data
          act_sub <- act[which(act$date_time >= time_seq[1] &
                                 act$date_time <= time_seq[2]), ]
          
          x$activity[i] <- nrow(act_sub)
          
          # Count the number of landings
          x$landings[i] <- nrow(act_sub[which(act_sub$act == "dry"), ])
          
          # x$landings_long[i] <-
          #   nrow(act_sub[which(act_sub$state == "dry" &
          #                        act_sub$seconds >= 15), ])
          
          x$takeoffs[i] <- nrow(act_sub[which(act_sub$act == "wet"), ])
          
          # Check if any landings/takeoffs occured in this time
          if(nrow(act_sub) > 0){
            x$fully_wet[i] <- F
          }else{
            # Find whether the last activity switch was landing or takeoff
            act_prior <- act[which(act$date_time < x$date_time[i]), ]
            
            # Make sure there are some act data in this
            if(nrow(act_prior > 0)){
              # Subset to nearest act_prior
              act_prior <- act_prior[which.max(act_prior$date_time), ]
              # If last activity switch was landing, fix is fully wet
              x$fully_wet[i] <- ifelse(act_prior$act == "dry", T, F)
            }else{x$fully_wet[i] <- F}
          }
        }
      }
    }
    x
  }) %>% bind_rows()

# Appending act data to interpolated data
trip_df_int <- split(trip_df_int, trip_df_int$id) %>%
  # Loop around and append number of associated landings
  lapply(., function(x){
    # Subset act data to only the relevant id
    act <- act_df[which(act_df$id == x$id[1]), ]
    
    # Create variables to populate
    x[, c("landings", "takeoffs", "fully_wet", "activity")] <- NA
    
    # Check that act actually has data
    if(nrow(act) > 0){
      # Loop around x to append act data
      for(i in 1:(nrow(x) - 1)){
        # Create an interval 2.5 minutes before and after a location
        time_seq <- c(x$date_time[i:(i+1)])
        
        if(max(act$date_time) > time_seq[2]){
          # Create a subset of act data
          act_sub <- act[which(act$date_time >= time_seq[1] &
                                 act$date_time <= time_seq[2]), ]
          
          x$activity[i] <- nrow(act_sub)
          
          # Count the number of landings
          x$landings[i] <- nrow(act_sub[which(act_sub$act == "dry"), ])
          
          # x$landings_long[i] <-
          #   nrow(act_sub[which(act_sub$state == "dry" &
          #                        act_sub$seconds >= 15), ])
          
          x$takeoffs[i] <- nrow(act_sub[which(act_sub$act == "wet"), ])
          
          # Check if any landings/takeoffs occured in this time
          if(nrow(act_sub) > 0){
            x$fully_wet[i] <- F
          }else{
            # Find whether the last activity switch was landing or takeoff
            act_prior <- act[which(act$date_time < x$date_time[i]), ]
            
            # Make sure there are some act data in this
            if(nrow(act_prior > 0)){
              # Subset to nearest act_prior
              act_prior <- act_prior[which.max(act_prior$date_time), ]
              # If last activity switch was landing, fix is fully wet
              x$fully_wet[i] <- ifelse(act_prior$act == "dry", T, F)
            }else{x$fully_wet[i] <- F}
          }
        }
      }
    }
    x
  }) %>% bind_rows()

# Create continuous activity data to calculate proportion
# of each inter-location period spent wet/dry
act_df_dense <- split(act_df, act_df$id) %>%
  lapply(., function(x){
    out <- data.frame(date_time = seq(from = (x$date_time[1] - x$duration[1]),
                                      to = x$date_time[nrow(x)], by = 1))
    
    out$act <- NA
    
    out$act[which(out$date_time %in% x$date_time)] <- x$act
    
    out <- fill(out, act, .direction = "up")
    
    out$id <- x$id[1]
    
    out
  }) %>% bind_rows()

# Placeholder
trip_df$act <- NA

# Calculate time spent wet per location interval
for(i in unique(act_df_dense$id)){
  
  df <- trip_df %>%
    filter(id == i) %>%
    mutate(dt_next = lead(date_time) - 1)
  
  act_sub <- act_df_dense %>%
    filter(id == i)
  
  if(nrow(df) > 1){
    for(j in 1:(nrow(df) - 1)){
      act <- act_sub %>%
        filter(date_time >= df$date_time[j],
               date_time <= df$dt_next[j])
      
      if(nrow(act) > 60){
        df$act[j] <- sum(act$act == "wet") / nrow(act)}
    }
  }    
  
  trip_df$act[which(trip_df$id == i)] <- df$act
  
  print(i)
}

# Define activity class based on proportion wet/dry
trip_df$act_class <- ifelse(trip_df$act <= 0.05, "dry",
                            ifelse(trip_df$act >= 0.95, "wet", "mixed"))


# Placeholder for interpolated data
trip_df_int$act <- NA

# Append immersion to interpolated locations
for(i in unique(act_df_dense$id)){
  
  df <- trip_df_int %>%
    filter(id == i) %>%
    mutate(dt_next = lead(date_time) - 1)
  
  act_sub <- act_df_dense %>%
    filter(id == i)
  
  if(nrow(df) > 1){
    for(j in 1:(nrow(df) - 1)){
      act <- act_sub %>%
        filter(date_time >= df$date_time[j],
               date_time <= df$dt_next[j])
      
      if(nrow(act) > 60){
        df$act[j] <- sum(act$act == "wet") / nrow(act)}
    }
  }    
  
  trip_df_int$act[which(trip_df_int$id == i)] <- df$act
  
  print(i)
}

# Activity class for interpolated data
trip_df_int$act_class <- ifelse(trip_df_int$act <= 0.05, "dry",
                                ifelse(trip_df_int$act >= 0.95, "wet", "mixed"))

# Save these off once data are interpolated
save(trip_df, file = "data/cleaned/2026/trip_df.RData")
save(trip_df_int, file = "data/cleaned/2026/trip_df_int.RData")

# Load in light data
load("data/cleaned/2026/lig_ls")

# Loop through and pull out a location for each light data
for(i in 1:length(lig_ls)){
  locations <- trip_df_int %>% filter(id == lig_ls[[i]]$id[1])
  lig_ls[[i]]$lat <- NA
  lig_ls[[i]]$lon <- NA
  for(j in 1:nrow(lig_ls[[i]])){
    diff <- (abs(as.numeric(difftime(locations$date_time,
                                     lig_ls[[i]]$date_time[j],
                                     units = "mins"))))
    if(min(diff) < 30){
      lig_ls[[i]]$lat[j] <- locations$Latitude[which.min(diff)]
      lig_ls[[i]]$lon[j] <- locations$Longitude[which.min(diff)]
    }
  }
}

# Format light data into a dataframe
lig_df <- bind_rows(lig_ls) %>% filter(!is.na(lat)) %>%
  mutate(solar = sunAngle(t = date_time, longitude = lon, latitude = lat)$altitude)

# Look for any light spikes at night to pick up any vessel interactions not
# captured in the radar dataset
ggplot(lig_df %>% mutate(light = ifelse(light > 20, 20, light)) %>% 
         filter(solar < -6)) +
  geom_path(aes(x = light, y = solar))
