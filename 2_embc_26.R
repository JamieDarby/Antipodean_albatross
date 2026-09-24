
require(EMbC)

embc_obj <- trip_df %>% select(date_time, Longitude, Latitude, sun_angle) |>
  stbc(scv = "height")

EMbC::sctr(embc_obj)

EMbC::varp(embc_obj)
EMbC::lblp(embc_obj)
smth_embc_obj <- EMbC::smth(embc_obj)

EMbC::sctr(smth_embc_obj)

trip_df$embc <- smth_embc_obj@A
trip_df$embc_simple <- ifelse(trip_df$embc > 4, trip_df$embc - 4, trip_df$embc)


ggplot(trip_df %>%
         filter(embc_simple != 5) %>%
         mutate(state = ifelse(embc_simple == 4, 2, embc_simple),
           state = factor(state, labels = c("LL", "LH & HH", "HL")))) +
  geom_violin(aes(x = state, y = act, fill = state)) +
  scale_fill_discrete(palette = c("#1b015e", "#018c81", "#68f2a2"), guide = "none") +
  labs(y = "Proportion of time wet", x = "EMbC state") +
  theme_classic()

ggplot(trip_df) + geom_point(aes(x = wind_sp, y = speed, colour = sun_angle),
                             size = 0.5) +
  scale_colour_viridis_c(option = "H") +
  facet_wrap(facets = ~as.factor(embc)) +
  theme_nice()


EMbC::pmap(embc_obj)

ggplot(trip_df %>%
         mutate(daynight = ifelse(sun_angle > -6, "day", "night"))) +
  geom_histogram(aes(x = -wind_off, fill = as.factor(embc)), 
                 colour = "black", breaks = c(-12:12) * 15) +
  scale_fill_viridis_d(option = "G", end = 0.7, begin = 0.2) +
  facet_wrap(facets = ~ 
               ifelse(wind_sp < 10,
                      "1. Wind < 10m/s",
                      "2. Wind > 10m/s") * embc,
             nrow = 2,
             scales = "free_y") +
  scale_x_continuous(breaks = c(-180, -90, 0, 90, 180)) +
  scale_y_continuous(transform = "identity", n.breaks = 3) +
  coord_radial(start = pi, inner.radius = 0.2, expand = F) +
  labs(x = "", y = "Quantity of track points", fill = "Trip type", title = "A") +
  theme_nice() +
  theme(legend.position = "bottom")




embc_obj_simple <- trip_df %>% select(date_time, Longitude, Latitude, sun_angle) |>
  stbc()

EMbC::sctr(embc_obj_simple)

EMbC::varp(embc_obj)
EMbC::lblp(embc_obj)
smth_embc_obj <- EMbC::smth(embc_obj)

EMbC::sctr(smth_embc_obj)

test_a <- embc_obj_simple@A
test_b <- embc_obj@A

test_b <- ifelse(test_b > 4, test_b - 4, test_b)

test_a <- test_a[which(test_b != 5)]
test_speed <- trip_df$speed[which(test_b != 5)]
test_sun <- trip_df$sun_angle[which(test_b != 5)]
test_b <- test_b[which(test_b != 5)]

ggplot() + 
  geom_point(aes(x = test_a, y = test_b,
                 colour = test_sun),
             position = "jitter", size = 1, alpha = 0.25) +
  scale_colour_viridis_c(option = "A", end = 0.8) +
  theme_nice() +
  labs(x = "EMbC classification without time of day",
       y = "EMbC classification with time of day",
       colour = "Solar angle") +
  scale_x_continuous(breaks = c(1:4),
                     labels = c("LL", "LH", "HL", "HH")) +
  scale_y_continuous(breaks = c(1:4),
                     labels = c("LL", "LH", "HL", "HH"))


caret::confusionMatrix(factor(test_a, labels = c("LL", "LH", "HL", "HH")),
                       factor(test_b, labels = c("LL", "LH", "HL", "HH")),
                       dnn = c("Bivariate EMbC", "Trivariate EMbC"),
                       mode = "everything")


trip_df |>
  filter(embc_simple != 5) |>
  mutate(state = ifelse(embc_simple == 4, 2, embc_simple),
         state = factor(state, labels = c("LL", "LH & HH", "HL"))) |>
  ggplot() +
  geom_bar(aes(x = state),
           fill = "white",
           colour = "black") +
  # geom_bar(aes(x = as.factor(state), weight = landings),
  #          fill = "#018c81",
  #          colour = "black") +
  geom_bar(aes(x = state, weight = act),
           fill = "#018c81",
           colour = "black") +
  labs(y = "Track points", x = "EMbC state") +
  theme_classic()

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

embc_immerse_plot
