# Introduction. -----------------------------------------------------------

# 1) Validate the ATEI with a dataset
#   published by Lu et al. (2020).

# Updated: 6/25/2023.


# 0) Setup. ---------------------------------------------------------------

# Load Packages.
library(tidyverse)
library(sf)
library(gridExtra)

# Disable printing results in scientific notation.
options(scipen = 999)

# Set the theme and font size of plots.
theme_set(theme_bw())

# Define the working directories.
wd_Local <- "C:/Research_Projects/Treeline_LOCAL"

wd_ATEI <- 
  file.path(wd_Local, 
            "ATEI_Estimation_LOCAL")

wd_Validation <- 
  file.path(wd_ATEI, 
            "ATEI_Validation")

wd_Figs <- 
  file.path(wd_Validation, 
            "Figures")


# 1) Load the validation samples. -----------------------------------------

samples_FN <- "ATEelevationTrend_TransectJoined_5kmBuffered_2020Lu"

samples_SHP <- st_read(
  file.path(wd_Validation,
            samples_FN),
  layer = samples_FN,
  stringsAsFactors = TRUE)

nrow(samples_SHP) # 71

samples_DF <- samples_SHP %>% 
  st_drop_geometry()

samples_DF %>% 
  select(Latitude, Longitude) %>% 
  head(20)


# 2) Preprocess the validation samples. -----------------------------------

# Combine duplicated rows
nonDuplicated_DF <- samples_DF %>% 
  group_by(Latitude, Longitude) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  as.data.frame()

nonDuplicated_DF %>% 
  select(Latitude, Longitude) %>% 
  head(20)

nrow(nonDuplicated_DF) # 51

# Select samples with at least 10 joined transects.
filtered_DF <- 
  nonDuplicated_DF %>% 
  filter(tran_Num >= 10)

nrow(filtered_DF) # 35

# Add the geometry of each selected sample.
samples_SHP_nonNumeric <- samples_SHP %>% 
  group_by(Latitude, Longitude) %>% 
  slice(1)

nrow(samples_SHP_nonNumeric) # 51

samples_SHP_nonNumeric$Lat_x_Long <- as.character(
  samples_SHP_nonNumeric$Latitude * 
  samples_SHP_nonNumeric$Longitude)

samples_SHP_nonNumeric <- 
  samples_SHP_nonNumeric %>% 
  select_if(negate(is.numeric))

str(samples_SHP_nonNumeric$Lat_x_Long)

filtered_DF$Lat_x_Long <- as.character(
  filtered_DF$Latitude * 
  filtered_DF$Longitude)

str(filtered_DF$Lat_x_Long)

filtered_SHP <- merge(
  x = samples_SHP_nonNumeric,
  y = filtered_DF,
  by = c("Lat_x_Long", 
         "Latitude", 
         "Longitude"),
  all.y = TRUE
)

nrow(filtered_SHP) # 35

filteredCentroids_SHP <- filtered_SHP %>% 
  st_centroid()

head(filteredCentroids_SHP)

# Output the selected samples.
filtered_FN <- "ATEelevationTrend_FilteredCentroids_2020Lu"

st_write(obj = filteredCentroids_SHP,
         dsn = file.path(wd_Validation,
                         filtered_FN),
         layer = filtered_FN,
         driver = "ESRI Shapefile")


# 3) Correlation analysis. ------------------------------------------------

filtered_FN <- "ATEelevationTrend_FilteredCentroids_2020Lu"

filteredCentroids_SHP <- st_read(
  file.path(wd_Validation,
            filtered_FN),
  layer = filtered_FN,
  stringsAsFactors = TRUE)

nrow(filteredCentroids_SHP) # 35

filtered_DF <- filteredCentroids_SHP %>% 
  st_drop_geometry()

summary(filtered_DF)


#### Elevation. ####

# cor(filtered_DF$Elevation, 
#     filtered_DF$avg_RawElv) # 0.9707378
# cor(filtered_DF$Elevation, 
#     filtered_DF$ATEelv_Avg) # 0.9838075
# cor(filtered_DF$Elevation, 
#     filtered_DF$tran_AeAvg) # 0.9867323

summary(
  lm(Elevation ~ avg_RawElv, 
     data = filtered_DF))$r.squared # 0.9423318

summary(
  lm(Elevation ~ ATEelv_Avg, 
     data = filtered_DF))$r.squared # 0.9678772

summary(
  lm(Elevation ~ tran_AeAvg, 
     data = filtered_DF))$r.squared # 0.9736407


ATE_elevation_Plot <- ggplot(filtered_DF) +
  geom_point(aes(x = avg_RawElv, y = Elevation), 
             color = "darkgreen", 
             size = 1.5,
             alpha = 0.5) +
  geom_point(aes(x = ATEelv_Avg, y = Elevation), 
             color = "blue", 
             size = 1.5,
             alpha = 0.5) +
  geom_point(aes(x = tran_AeAvg, y = Elevation), 
             color = "red", 
             size = 1.5,
             alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1, 
              color = "black",
              alpha = 0.5) +
  xlab("Estimated ATE elevation (m)") + 
  ylab("Observed treeline elevation (m)") + 
  xlim(1500, 4500) +
  ylim(1500, 4500) +
  annotate(geom = "text",
           x = 1500,
           y = 4500,
           label = "a",
           color = "black",
           fontface = "bold")

ATE_elevation_Plot

ATE_elevation_SinglePlot <- 
  ggplot(filtered_DF) +
  geom_point(aes(x = avg_RawElv, y = Elevation), 
             color = "darkgreen", 
             size = 1.5,
             alpha = 0.5) +
  geom_point(aes(x = ATEelv_Avg, y = Elevation), 
             color = "blue", 
             size = 1.5,
             alpha = 0.5) +
  geom_point(aes(x = tran_AeAvg, y = Elevation), 
             color = "red", 
             size = 1.5,
             alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1, 
              color = "black",
              alpha = 0.5) +
  xlab("Estimated ATE elevation (m)") + 
  ylab("Observed treeline elevation (m)") + 
  xlim(1500, 4500) +
  ylim(1500, 4500)

ATE_elevation_SinglePlot

# Save the single plot.
png(
  file.path(wd_Figs, "ATE_elevation_SinglePlot.png"),
  width = 3000, height = 3000,
  units = "px", res = 800)

ATE_elevation_SinglePlot

dev.off()


#### Trend. ####

summary(
  lm(Mean_eleva ~ ATEelv_Trd, 
     data = filtered_DF))$r.squared # 0.1225106

summary(
  lm(Mean_eleva ~ tran_AeTrd, 
     data = filtered_DF))$r.squared # 0.08850063


ATE_dynamics_Plot <- ggplot(filtered_DF) +
  geom_point(aes(x = ATEelv_Trd, y = Mean_eleva), 
             color = "blue", 
             size = 1.5,
             alpha = 0.5) +
  geom_point(aes(x = tran_AeTrd, y = Mean_eleva), 
             color = "red", 
             size = 1.5,
             alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1, 
              color = "black",
              alpha = 0.5) +
  xlab("Estimated ATE dynamics (m/year)") + 
  ylab("Observed treeline dynamics (m/year)") +
  annotate(geom = "text",
           x = -1,
           y = 1.2,
           label = "b",
           color = "black",
           fontface = "bold")

ATE_dynamics_Plot

ATE_dynamics_SinglePlot <- 
  ggplot(filtered_DF) +
  geom_point(aes(x = ATEelv_Trd, y = Mean_eleva), 
             color = "blue", 
             size = 1.5,
             alpha = 0.5) +
  geom_point(aes(x = tran_AeTrd, y = Mean_eleva), 
             color = "red", 
             size = 1.5,
             alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1, 
              color = "black",
              alpha = 0.5) +
  xlab("Estimated ATE dynamics (m/year)") + 
  ylab("Observed treeline dynamics (m/year)")

ATE_dynamics_SinglePlot

# Save the single plot.
png(
  file.path(wd_Figs, "ATE_dynamics_SinglePlot.png"),
  width = 3000, height = 3000,
  units = "px", res = 800)

ATE_dynamics_SinglePlot

dev.off()

# Save a combined plot.
png(
  file.path(wd_Figs, "ATEI_validation.png"),
  width = 6000, height = 3000,
  units = "px", res = 800)

grid.arrange(ATE_elevation_Plot,
             ATE_dynamics_Plot,
             ncol = 2)

dev.off()


# 4) Normalized RMSE. -----------------------------------------------------

# Elevation.
# Calculate the standard deviation of the observed values.
obsElv_SD <- sd(filtered_DF$Elevation)

# Compute the NRMSE.
sqrt(mean(
  (filtered_DF$avg_RawElv - 
             filtered_DF$Elevation) ^ 2)) / 
  obsElv_SD # 0.2652254

sqrt(mean(
  (filtered_DF$ATEelv_Avg - 
             filtered_DF$Elevation) ^ 2)) / 
  obsElv_SD # 0.2410915

sqrt(mean(
  (filtered_DF$tran_AeAvg - 
             filtered_DF$Elevation) ^ 2)) / 
  obsElv_SD # 0.2104343

# Trend
# Calculate the standard deviation of the observed values.
obsTrd_SD <- sd(filtered_DF$Mean_eleva)

# Compute the NRMSE.
sqrt(mean(
  (filtered_DF$ATEelv_Trd - 
             filtered_DF$Mean_eleva) ^ 2)) / 
  obsTrd_SD # 8.213377

sqrt(mean(
  (filtered_DF$tran_AeTrd - 
             filtered_DF$Mean_eleva) ^ 2)) / 
  obsTrd_SD # 4.381078

