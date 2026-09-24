
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

embc_obj_simple <- trip_df %>% select(date_time, Longitude, Latitude, sun_angle) |>
  stbc()

EMbC::sctr(embc_obj_simple)

EMbC::varp(embc_obj_simple)
EMbC::lblp(embc_obj_simple)
smth_embc_obj_simple <- EMbC::smth(embc_obj_simple)

EMbC::sctr(smth_embc_obj_simple)

test_a <- embc_obj_simple@A
test_b <- embc_obj@A

test_b <- ifelse(test_b > 4, test_b - 4, test_b)

test_a <- test_a[which(test_b != 5)]
test_b <- test_b[which(test_b != 5)]

caret::confusionMatrix(factor(test_a, labels = c("LL", "LH", "HL", "HH")),
                       factor(test_b, labels = c("LL", "LH", "HL", "HH")),
                       dnn = c("Bivariate EMbC", "Trivariate EMbC"),
                       mode = "everything")

