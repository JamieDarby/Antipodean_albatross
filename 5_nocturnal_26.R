
# Load in packages required for modelling
require(mgcv)
require(MuMIn)
require(itsadug)
require(jtools)
require(interactions)
require(cowplot)
require(ROCR)

# Load in dfs if already made
load(file = "data/cleaned/2026/trip_df_int.RData")
load(file = "data/cleaned/2026/trip_df.RData")

# Subset trip dataframe to night time only
night_df <- trip_df %>% filter(sun_angle < -6) %>%
  mutate(ars = ifelse(embc %in% c(2,4,6,8), 1, 0),
         id = as.factor(id),
         night_id = paste(as.Date(date_time), id, sep = "_"),
         ar_start = (night_id != lag(night_id)),
         ar_start = ifelse(is.na(ar_start), F, ar_start),
         sex = as.factor(sex))

# Global model, ARS (presence/absence) ~ environment
mod <- bam(data = night_df,
           formula = ars ~ 
             s(moon_frac, bs = "ts") +
             s(moon_angle, bs = "ts") +
             s(wind_sp, bs = "ts") +
             s(sun_angle, bs = "ts") +
             s(Latitude, bs = "ts") +
             s(id, bs = "re") + sex,
           select = T,
           family = binomial(),
           AR.start = night_df$ar_start,
           rho = 0.5, discrete = T, method = "fREML",
           na.action = "na.fail")

# Test and select model
summary(mod)
acf(residuals(mod))
MuMIn::dredge(mod)

# Model with updated AR(1) specs and covariates
mod <- bam(data = night_df,
            formula = ars ~ 
              s(moon_frac, bs = "ts") +
              s(wind_sp, bs = "ts") +
              s(sun_angle, bs = "ts") +
              s(Latitude, bs = "ts") +
              s(id, bs = "re") + sex,
            select = T,
            family = binomial(),
            AR.start = night_df$ar_start,
            rho = 0.4, discrete = T, method = "fREML",
            na.action = "na.fail")

# Summarise, test, and diagnose residuals
summary(mod)
acf_resid(mod)
concurvity(mod)
testResiduals(simulateResiduals(mod))

# Calculate and visualise AUC
pr <- as.numeric(predict.gam(mod, night_df, type="response"))            
pred <- prediction(pr, night_df$ars)
perf <- performance(pred, measure="tpr", x.measure="fpr")  
plot(perf, colorize = TRUE, print.cutoffs.at = c(0.1,0.2,0.3,0.4,0.5))
perf <- performance(pred, measure="auc")  

# Plot out marginal effects plots
p_ars_moon <- 
  effect_plot(mod, moon_frac, data = night_df, interval = T,
              plot.points = F, partial.residuals = F) +
  labs(y = "P(ARS)", x = "Moon fraction") +
  scale_y_continuous(limits = c(0.09,0.48))

p_ars_wind <- 
  effect_plot(mod, wind_sp, data = night_df, interval = T) +
  labs(y = "P(ARS)", x = "Wind speed (m/s)") +
  scale_y_continuous(limits = c(0.09,0.48))

p_ars_lat <- 
  effect_plot(mod, Latitude, data = night_df, interval = T) +
  scale_x_continuous(limits = c(-60, -33)) +
  labs(y = "P(ARS)", x = "Latitude") +
  scale_y_continuous(limits = c(0.09,0.48))

plot_grid(p_ars_moon, p_ars_wind, p_ars_lat, nrow = 1)

# Create meta dataframe on a per night basis
night_meta <- trip_df %>%
  filter(sun_angle < -6) %>%
  mutate(night_id = paste(id, as.Date(date_time), sep = "_")) %>%
  group_by(night_id) %>%
  summarise(n = n(),
            n_std = n / ifelse(int_std[1] == "short", 2, 1),
            prop_ars = sum(embc %in% c(2,4,6,8)) / n,
            prop_coarse_ars = sum(embc %in% c(4,8)) / sum(embc %in% c(2,4,6,8)),
            moon_frac = max(moon_frac),
            wind_sp = max(wind_sp),
            id = as.factor(id[1]),
            sex = sex[1],
            speed = mean(speed, na.rm = T),
            landings = sum(landings),
            latitude = mean(Latitude),
            longitude = mean(Longitude),
            mixed = sum(act_class == "mixed") / n)

# Model proportion of ARS per night ~ environment
mod_meta <- gam(data = night_meta,
                formula = prop_ars ~
                  s(moon_frac, bs = "ts") +
                  s(latitude, bs = "ts") +
                  s(wind_sp, bs = "ts") +
                  s(id, bs = "re") + sex,
                select = T,
                weights = n_std,
                family = gaussian(),
                na.action = "na.fail")

# Select, summarise, diagnose
MuMIn::dredge(mod_meta)
summary(mod_meta)
acf(residuals(mod_meta))
testResiduals(simulateResiduals(mod_meta))

# Marginal effects plots for this model
prop_ars_moon <- 
  effect_plot(mod_meta, moon_frac, data = night_meta, interval = T,
            plot.points = F, partial.residuals = F) +
  labs(y = "Proportion ARS per night", x = "Moon fraction") +
  scale_y_continuous(limits = c(0.06,0.45))

prop_ars_wind <- 
  effect_plot(mod_meta, wind_sp, data = night_meta, interval = T) +
  labs(y = "Proportion ARS per night", x = "Wind speed (m/s)") +
  scale_y_continuous(limits = c(0.06,0.45))

prop_ars_lat <- 
  effect_plot(mod_meta, latitude, data = night_meta, interval = T) +
  scale_x_continuous(limits = c(-60, -33)) +
  labs(y = "Proportion ARS per night", x = "Latitude") +
  scale_y_continuous(limits = c(0.06,0.45))

ars_effects <- 
  plot_grid(prop_ars_moon,
          prop_ars_wind + labs(y = ""),
          prop_ars_lat + labs(y = ""), 
          nrow = 1, rel_widths = c(1.1, 1, 1))

# Model proportion of coarse ARS per ARS ~ environment
mod_prop_meta <- gam(data = night_meta_b,
                formula = prop_coarse_ars ~
                  s(moon_frac, bs = "ts", k = 3) +
                  s(latitude, bs = "ts", k = 3) +
                  s(wind_sp, bs = "ts", k = 3) +
                  s(id, bs = "re") + sex,
                select = T,
                weights = prop_ars,
                family = gaussian(),
                na.action = "na.fail")

# Summarise, select, diagnose
summary(mod_prop_meta)
MuMIn::dredge(mod_prop_meta)
testResiduals(simulateResiduals(mod_prop_meta))

# Plot out heavy lifting relationship as a non-interacting interaction
prop_cars <-
  interact_plot(mod_prop_meta, moon_frac, modx = wind_sp, data = night_meta_b, interval = T,
              plot.points = T, partial.residuals = F, modx.values = c(4, 8, 12),
              colors = bpy.colors(n = 4), point.size = 0.5,
              point.alpha = 0.5, legend.main = "Wind speed\n(m/s)") +
  labs(y = "Proportion extensive ARS per night", x = "Moon fraction") +
  scale_y_continuous(limits = c(0,1))

# Shape data into set split by solar angle in 2 degree bins for plotting
solar_meta <- trip_df %>%
  mutate(solar_split = (round(sun_angle/2))*2,
         embc_simple = ifelse(embc_simple == 5, NA, embc_simple)) %>%
  filter(!is.na(embc_simple)) %>%
  group_by(solar_split) %>%
  summarise(n = n(),
            n_gls = sum(!is.na(act_class)),
            n_embc = sum(!is.na(embc_simple)),
            prop_ars = sum(embc_simple %in% c(2,4)) / n_embc,
            prop_lh = sum(embc_simple %in% c(2)) / sum(embc_simple %in% c(2,4)),
            prop_hh = sum(embc_simple %in% c(4)) / sum(embc_simple %in% c(2,4)),
            ars_step = mean(speed[which(embc_simple %in% c(2,4))], na.rm = T),
            prop_trans = sum(embc_simple %in% c(3)) / n_embc,
            prop_rest = sum(embc_simple %in% c(1)) / n_embc,
            landings = sum(landings, na.rm = T) / n_gls,
            land_per_hour = mean(landings/(time /3600), nr.rm = T),
            prop_mixed = sum(act_class == "mixed", na.rm = T) / n_gls,
            prop_dry = sum(act_class == "dry", na.rm = T) / n_gls,
            prop_wet = sum(act_class == "wet", na.rm = T) / n_gls)

head(solar_meta)

# Plot EMBC state per solar angle
embc_plot <- solar_meta %>%
  pivot_longer(cols = c(prop_ars, prop_trans, prop_rest), values_to = "value",
               names_to = "prop") %>%
  mutate(prop = factor(prop, levels = c("prop_trans", "prop_ars", "prop_rest"),
                       labels = c("Transit", "ARS", "Rest"))) %>%
  ggplot() + geom_bar(aes(x = solar_split, fill = prop, weight = value),
                      colour = "black", just = 0, width = 2) +
  scale_fill_viridis_d(option = "G", begin = 0.8, end = 0.2) + theme_classic() +
  scale_x_continuous(expand = F, limits = c(-42, 78)) +
  scale_y_continuous(expand = F) +
  theme(legend.position = "bottom") +
  labs(x = "Solar angle", y = "Proportion of EMbC states",
       fill = "EMbC state")

# Plot immserion state per solar angle
gls_plot <- solar_meta %>%
  filter(!is.na(prop_mixed)) %>%
  pivot_longer(cols = c(prop_mixed, prop_wet, prop_dry), values_to = "value",
               names_to = "prop") %>%
  mutate(prop = factor(prop, levels = c("prop_dry", "prop_mixed", "prop_wet"),
                       labels = c("Dry", "Mixed", "Wet"))) %>%
  ggplot() + geom_bar(aes(x = solar_split, fill = prop, weight = value),
                      colour = "black", just = 0, width = 2) +
  scale_fill_viridis_d(option = "G", begin = 0.8, end = 0.2) +theme_classic() +
  scale_x_continuous(expand = F, limits = c(-42, 78)) +
  scale_y_continuous(expand = F) +
  theme(legend.position = "bottom") +
  labs(x = "Solar angle", y = "Proportion of immersion states",
       fill = "Immersion state")

#Plot count of points per sex by solar angle
count_plot <- trip_df %>%
  mutate(solar_split = (round(sun_angle/2))*2,
         sex = ifelse(sex == "m", "Male", "Female")) %>%
  ggplot() + geom_bar(aes(x = solar_split, fill = sex),
                      colour = "black", just = 0, width = 2) +
  scale_fill_viridis_d(option = "H", begin = 0.95, end = 0.4) +theme_classic() +
  scale_x_continuous(expand = F, limits = c(-42, 78)) +
  scale_y_continuous(expand = F) +
  theme(legend.position = "bottom") +
  labs(x = "Solar angle", y = "Count of track points",
       fill = "")

solar_dist_plots <- 
  plot_grid(count_plot + labs(title = "A"),
          gls_plot + labs(title = "B"),
          embc_plot + labs(title = "C"),
          nrow = 3, rel_heights = c(1.5, 1, 1))

ggsave(solar_dist_plots, filename = "plots/solar_dist_plots.png",
       width = 6, height = 10, dpi = 500)

