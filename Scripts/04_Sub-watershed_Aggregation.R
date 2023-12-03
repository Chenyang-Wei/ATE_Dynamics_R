# Introduction. -----------------------------------------------------------

# 1) Aggregate the estimated ATE elevation trend 
#   to sub-watersheds.

# Updated: 6/8/2023.


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

wd_NDVI_SHP <- 
  file.path(wd_Local, 
            "NDVI_Gradient_Analysis_LOCAL", 
            "SHP_files")

wd_ATEI <- 
  file.path(wd_Local, 
            "ATEI_Estimation_LOCAL")

wd_Dynamics <- 
  file.path(wd_ATEI, 
            "ATE_Dynamics")


# 1) Load the filtered transects ------------------------------------------

filtered_FN <- "filteredTransects_ATEelevationTrend_Gte20obs"

filtered_Transects <- st_read(
  file.path(wd_Dynamics,
            filtered_FN),
  layer = filtered_FN,
  stringsAsFactors = TRUE)

nrow(filtered_Transects) # 492932.


# 2) Aggregate variables by sub-watershed. --------------------------------

filtered_Transects %>% 
  select(starts_with(c(
    "ATE",
    "Shift_Type"
  ))) %>% 
  head()

# Derive the average sub-watershed variables.
Hybas_Means_DF <- filtered_Transects %>% 
  st_drop_geometry() %>%
  group_by(Hybas_ID) %>%
  summarise(
    tranNum = n(),
    
    ATEelv_Trd = mean(ATEelv_Trd),
    ATEelv_Avg = mean(ATEelv_Avg),
    ATE_relPos = mean(ATE_relPos),
    ATEelv_Num = mean(ATEelv_Num),
    
    NDVItrend = mean(avgNDVI_Tr),
    NDVImean = mean(avgNDVI_Me),
    
    EGtrend_km = mean(EGtrend_km),
    EGmean_km = mean(EGmean_km)
  ) %>% 
  as.data.frame()

nrow(Hybas_Means_DF) # 29058.

# Classify the sub-watersheds based on 
#   their ATE positions.
Hybas_Means_DF$ATEposType <- "Upper half"

Hybas_Means_DF$ATEposType[
  Hybas_Means_DF$ATE_relPos <= 0.5
] <- "Lower half"

Hybas_Means_DF$ATEposType <- 
  as.factor(Hybas_Means_DF$ATEposType)

# Classify the sub-watersheds based on 
#   the ATE elevation trend.
Hybas_Means_DF$Shift_Type <- "Stable"

Hybas_Means_DF$Shift_Type[
  Hybas_Means_DF$ATEelv_Trd <= -0.1
] <- "Downward"

Hybas_Means_DF$Shift_Type[
  Hybas_Means_DF$ATEelv_Trd >= 0.1
] <- "Upward"

Hybas_Means_DF$Shift_Type <- 
  as.factor(Hybas_Means_DF$Shift_Type)

summary(Hybas_Means_DF)


# 3) Add the geometries of sub-watersheds. --------------------------------

# Load the sub-watersheds with geometries.
raw_Hybas_FN <- "HybasMeans_Gte30obs_NegEGmean_withGeom"

raw_Hybas <- st_read(
  file.path(wd_NDVI_SHP,
            raw_Hybas_FN),
  layer = raw_Hybas_FN,
  stringsAsFactors = FALSE)

nrow(raw_Hybas) # 36042.

raw_Hybas_withID <- raw_Hybas %>% 
  select(Hybas_ID)

Hybas_Means_withGeom <- merge(
  x = raw_Hybas_withID,
  y = Hybas_Means_DF,
  by = "Hybas_ID",
  all.y = TRUE
)

nrow(Hybas_Means_withGeom) # 29058.

summary(Hybas_Means_withGeom)

Hybas_Means_withGeom$Hybas_ID <- 
  as.character(Hybas_Means_withGeom$Hybas_ID)

str(Hybas_Means_withGeom)

# Output the aggregated sub-watersheds.
Hybas_Output_FN <- "HybasMeans_ATEelevationTrend_Gte20obs"

st_write(obj = Hybas_Means_withGeom,
         dsn = file.path(wd_Dynamics,
                         Hybas_Output_FN),
         layer = Hybas_Output_FN,
         driver = "ESRI Shapefile")

