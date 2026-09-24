

require(TwGeos)

filename <- c("B09F/CX078_06Feb26_054012driftadj",
              "B25G/P068_11Feb26_032227driftadj",
              "G227/CX069_20Jan26_055054driftadj",
              "W638/P067_09Feb26_043900driftadj",
              "W64D/P078_03Feb26_062949driftadj",
              "W704/CX073_06Jun26_155003",
              "W870/CX073_17Jan26_084732driftadj",
              "W97F/CX069_09Feb26_044023driftadj")

mt_lig_ls <- list()
mt_act_ls <- list()

for(i in 1:length(filename)){
  
  mt_lig_ls[[i]] <-
    readMTlux(file = paste("c:/Users/Admin/Desktop/Tracking_data/Albie_2026/",
                           filename[i], ".lux",
                           sep = "")) %>%
    mutate(id = sub("/.*", "", filename[i]),
           logger = "mt") %>%
    rename(date_time = Date, light = Light)
  
  mt_act_ls[[i]] <-
    read.delim(file = paste("c:/Users/Admin/Desktop/Tracking_data/Albie_2026/",
                            filename[i], ".deg",
                            sep = ""),
               skip = 19, sep = "\t") %>%
    mutate(date_time = dmy_hms(DD.MM.YYYY.HH.MM.SS),
           id = sub("/.*", "", filename[i]),
           logger = "mt") %>%
    rename(act = wet.dry) %>%
    dplyr::select(-DD.MM.YYYY.HH.MM.SS)
}

filename <- c("B905/A2080_000",
              "W18H/A2055_000",
              "W95F/A2806_000")

lt_lig_ls <- list()
lt_act_ls <- list()

for(i in 1:length(filename)){
  
  lt_lig_ls[[i]] <-
    readLig(file = paste("c:/Users/Admin/Desktop/Tracking_data/Albie_2026/",
                         filename[i], ".lig",
                         sep = "")) %>%
    mutate(id = sub("/.*", "", filename[i]),
           logger = "lt") %>%
    rename(date_time = Date, light = Light) %>%
    filter(Valid == "ok") %>%
    dplyr::select(-Valid)
  
  x <-
    readAct2(file = paste("c:/Users/Admin/Desktop/Tracking_data/Albie_2026/",
                          filename[i], ".act",
                          sep = "")) %>%
    mutate(date_time = lead(Date),
           id = sub("/.*", "", filename[i]),
           logger = "lt") %>%
    rename(act = Wet, duration = Activity) %>%
    filter(Valid == "ok") %>%
    dplyr::select(duration, act, date_time, id, logger)
  
  x$date_time[nrow(x)] <- x$date_time[(nrow(x) - 1)] + x$duration[nrow(x)]
  
  lt_act_ls[[i]] <- x
}

ggplot(lt_lig_ls[[3]]) + geom_line(aes(x = date_time, y = log(ifelse(light > 10, 10, light))))
ggplot(lt_act_ls[[2]]) + geom_step(aes(x = date_time, y = as.numeric(act == "dry")))

act_ls <- c(mt_act_ls, lt_act_ls)
lig_ls <- c(mt_lig_ls, lt_lig_ls)

save(lig_ls, file = "data/cleaned/2026/lig_ls")
save(act_ls, file = "data/cleaned/2026/act_ls")


load("data/cleaned/act_ls.RData")


ggplot(bind_rows(act_ls[1:3])) + 
  geom_step(aes(x = date_time, y = ifelse(act == "dry", 0, 1))) +
  facet_wrap(facets = ~id, nrow = 4)


act_df <- bind_rows(act_ls)


act_df$start_time <- act_df$date_time - act_df$duration


load("data/cleaned/2026/trip_df.RData")
load("data/cleaned/2026/trip_df_int.RData")


# Split df into list for appending act data
trip_df <- split(trip_df, trip_df$id) %>%
  # Loop around and append number of associated landings and ingestions
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

trip_df$fully_dry <- (!trip_df$fully_wet &
                        trip_df$landings == 0 &
                        trip_df$takeoffs == 0)


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

trip_df_int$fully_dry <- (!trip_df_int$fully_wet &
                            trip_df_int$landings == 0 &
                            trip_df_int$takeoffs == 0)



ggplot(trip_df %>% filter(!is.na(landings))) +
  geom_bar(aes(x = (round((sun_angle*2), -1)/2),
               weight = landings,
               fill = id), colour = "black", stat = "count") +
  scale_fill_viridis_d(option = "H") +
  jtools::theme_nice() +
  labs(x = "Solar angle", y = "Landings", fill = "Bird ID")


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

trip_df$act <- NA

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

trip_df$act_class <- ifelse(trip_df$act <= 0.05, "dry",
                            ifelse(trip_df$act >= 0.95, "wet", "mixed"))



trip_df_int$act <- NA

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

mean(trip_df_int$act, na.rm = T)
plot(trip_df_int$act)

trip_df_int$act_class <- ifelse(trip_df_int$act <= 0.05, "dry",
                                ifelse(trip_df_int$act >= 0.95, "wet", "mixed"))


ggplot(trip_df, aes(x = date_time, y = speed, colour = act)) +
  geom_path() + scale_colour_viridis_c(option = "H", trans = "reverse") +
  facet_wrap(facets = ~id)

table(trip_df$act_class)

save(trip_df, file = "data/cleaned/2026/trip_df.RData")
save(trip_df_int, file = "data/cleaned/2026/trip_df_int.RData")




load("data/cleaned/2026/lig_ls")

ggplot(lig_ls %>% bind_rows() %>% mutate(light = ifelse(light > 20, 20, light))) +
  geom_line(aes(x = date_time, y = log(light))) + facet_wrap(facets = ~id)


ggplot(trip_df %>% filter(id == "W18H")) +
  geom_path(aes(x = Longitude_cont, y = Latitude))

trip_df %>% group_by(embc_simple) %>%
  filter(embc_simple != 5) %>%
  summarise(landings = mean((landings / (time / 3600)), na.rm = T)) %>%
  ggplot() + geom_bar(aes(x = embc_simple, weight = landings))



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

lig_df <- bind_rows(lig_ls) %>% filter(!is.na(lat)) %>%
  mutate(solar = sunAngle(t = date_time, longitude = lon, latitude = lat)$altitude)


ggplot(lig_df %>% mutate(light = ifelse(light > 20, 20, light)) %>% 
         filter(solar < -6)) +
  geom_path(aes(x = light, y = solar))
