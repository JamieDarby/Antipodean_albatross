
# Load in package
require(EMbC)

# Create object using embc of locations, times, and solar angle
embc_obj <- trip_df %>% select(date_time, Longitude, Latitude, sun_angle) |>
  stbc(scv = "height")

# Some diagnostics
EMbC::sctr(embc_obj)
EMbC::varp(embc_obj)
EMbC::lblp(embc_obj)

# Smooth the embc object
smth_embc_obj <- EMbC::smth(embc_obj)

# Look at the scatter of the smoothed object
EMbC::sctr(smth_embc_obj)

# Write embc state into trip_df, and simplify to 4 states
trip_df$embc <- smth_embc_obj@A
trip_df$embc_simple <- ifelse(trip_df$embc > 4, trip_df$embc - 4, trip_df$embc)

# Test out the same without including solar angle
embc_obj_simple <- trip_df %>% select(date_time, Longitude, Latitude, sun_angle) |>
  stbc()

# Similar diagnostics and smoothing
EMbC::sctr(embc_obj_simple)
EMbC::varp(embc_obj_simple)
EMbC::lblp(embc_obj_simple)
smth_embc_obj_simple <- EMbC::smth(embc_obj_simple)
EMbC::sctr(smth_embc_obj_simple)

# Run some comparisons, first make data comparable
test_a <- embc_obj_simple@A
test_b <- embc_obj@A

test_b <- ifelse(test_b > 4, test_b - 4, test_b)

test_a <- test_a[which(test_b != 5)]
test_b <- test_b[which(test_b != 5)]

# Create a confusion matrix
caret::confusionMatrix(factor(test_a, labels = c("LL", "LH", "HL", "HH")),
                       factor(test_b, labels = c("LL", "LH", "HL", "HH")),
                       dnn = c("Bivariate EMbC", "Trivariate EMbC"),
                       mode = "everything")

# EMBC states are practically identical, whether solar angle is included or not


