# Introduction. -----------------------------------------------------------

# 1) Check the distribution of ATE elevation trend.
# 2) Select transects with at least 20 observations.
# 3) Classify the filtered transects.

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
wd_ATEI <- "C:/Research_Projects/Treeline_LOCAL/ATEI_Estimation_LOCAL"

wd_Dynamics <- 
  file.path(wd_ATEI, 
            "ATE_Dynamics")


# 1) Load global transects with the estimated ATE dynamics. ---------------

# Load the global transects (SHP).
transects_FN <- "Global_PreprocessedTransects_ATEelevationTrend"

transects <- st_read(
  file.path(wd_Dynamics,
            transects_FN),
  layer = transects_FN,
  stringsAsFactors = TRUE)

# Check the global transects.
nrow(transects) # 798066.


# 2) Check the distribution of ATE elevation trend. -----------------------

# head(transects)
# 
# transects %>% 
#   select(starts_with(c(
#     "ATEelv_",
#     "Regn_GMBA",
#     "avgNDVI_Tr",
#     "EGtrend_km",
#     "CF_elv"
#   ))) %>% 
#   summary()

# ## Scaled distribution.
# # By observation number.
# for (obsNum in 1:36) {
#   p <- 
#     ggplot() + 
#       geom_density(
#         data = transects, 
#         aes(x = ATEelv_Trd, 
#             y = after_stat(scaled), 
#             color = ATEelv_Num, 
#             group = ATEelv_Num)) + 
#       scale_color_viridis_c() +
#       geom_density(data = transects[
#         transects$ATEelv_Num == obsNum, ],
#         aes(x = ATEelv_Trd,
#             y = after_stat(scaled)),
#         color = "red",
#         lwd = 1) +
#       geom_vline(xintercept = 0,
#                  color = "black",
#                  lty = "dashed", lwd = 0.5,
#                  alpha = 0.5) +
#       ggtitle(paste0("obsNum = ", obsNum)) +
#       ylab("Scaled density") +
#       xlim(-5, 5)
#   
#   png(filename = 
#         file.path(wd_Figs, 
#                   "Observation_Number", 
#                   "Scaled_Distribution", 
#                   paste0("obsNum_", 
#                          obsNum, 
#                          ".png")), 
#       width = 4000, height = 2000, 
#       units = "px", res = 600)
#   print(p)
#   dev.off()
# }
# 
# # Observation number over a threshold.
# for (obsNum_Thres in seq(5, 30, by = 5)) {
#   p <- 
#     ggplot() + 
#       geom_density(data = transects[
#         transects$ATEelv_Num > obsNum_Thres, ], 
#         aes(x = ATEelv_Trd, 
#             y = after_stat(scaled), 
#             color = ATEelv_Num, 
#             group = ATEelv_Num)) + 
#       scale_color_viridis_c() +
#       geom_density(data = transects[
#         transects$ATEelv_Num == obsNum_Thres, ],
#         aes(x = ATEelv_Trd,
#             y = after_stat(scaled)),
#         color = "red",
#         lwd = 1) +
#       geom_vline(xintercept = 0,
#                  color = "black",
#                  lty = "dashed", lwd = 0.5,
#                  alpha = 0.5) +
#       ggtitle(paste0(
#         "obsNum >= ", 
#         obsNum_Thres, 
#         ", nrow: ", 
#         nrow(
#           transects[
#             transects$ATEelv_Num > obsNum_Thres, ]))) +
#       ylab("Scaled density") +
#       xlim(-5, 5)
#   
#   png(filename = 
#         file.path(wd_Figs, 
#                   "Observation_Number", 
#                   "Scaled_Distribution", 
#                   "Threshold", 
#                   paste0("obsNum_gte", 
#                          obsNum_Thres, 
#                          ".png")), 
#       width = 4000, height = 2000, 
#       units = "px", res = 600)
#   print(p)
#   dev.off()
# }
# 
# ## Un-scaled distribution.
# # By observation number.
# for (obsNum in 1:36) {
#   p <- 
#     ggplot() + 
#     geom_density(
#       data = transects, 
#       aes(x = ATEelv_Trd, 
#           y = after_stat(count), 
#           color = ATEelv_Num, 
#           group = ATEelv_Num)) + 
#     scale_color_viridis_c() +
#     geom_density(data = transects[
#       transects$ATEelv_Num == obsNum, ],
#       aes(x = ATEelv_Trd,
#           y = after_stat(count)),
#       color = "red",
#       lwd = 1) +
#     geom_vline(xintercept = 0,
#                color = "black",
#                lty = "dashed", lwd = 0.5,
#                alpha = 0.5) +
#     ggtitle(paste0("obsNum = ", obsNum)) +
#     ylab("Un-scaled density") +
#     xlim(-5, 5)
#   
#   png(filename = 
#         file.path(wd_Figs, 
#                   "Observation_Number", 
#                   "Unscaled_Distribution", 
#                   paste0("obsNum_", 
#                          obsNum, 
#                          ".png")), 
#       width = 4000, height = 2000, 
#       units = "px", res = 600)
#   print(p)
#   dev.off()
# }
# 
# # Observation number over a threshold.
# for (obsNum_Thres in seq(5, 30, by = 5)) {
#   p <- 
#     ggplot() + 
#     geom_density(data = transects[
#       transects$ATEelv_Num > obsNum_Thres, ], 
#       aes(x = ATEelv_Trd, 
#           y = after_stat(count), 
#           color = ATEelv_Num, 
#           group = ATEelv_Num)) + 
#     scale_color_viridis_c() +
#     geom_density(data = transects[
#       transects$ATEelv_Num == obsNum_Thres, ],
#       aes(x = ATEelv_Trd,
#           y = after_stat(count)),
#       color = "red",
#       lwd = 1) +
#     geom_vline(xintercept = 0,
#                color = "black",
#                lty = "dashed", lwd = 0.5,
#                alpha = 0.5) +
#     ggtitle(paste0(
#       "obsNum >= ", 
#       obsNum_Thres, 
#       ", nrow: ", 
#       nrow(
#         transects[
#           transects$ATEelv_Num > obsNum_Thres, ]))) +
#     ylab("Un-scaled density") +
#     xlim(-5, 5)
#   
#   png(filename = 
#         file.path(wd_Figs, 
#                   "Observation_Number", 
#                   "Unscaled_Distribution", 
#                   "Threshold", 
#                   paste0("obsNum_gte", 
#                          obsNum_Thres, 
#                          ".png")), 
#       width = 4000, height = 2000, 
#       units = "px", res = 600)
#   print(p)
#   dev.off()
# }
# 
# # Check the transect numbers with different threshold.
# for (obsNum_Thres in seq(5, 35, by = 5)) {
#   print(obsNum_Thres)
#   
#   transects %>% 
#     select(
#       ATEelv_Num,
#       Regn_GMBA,
#       ATEelv_Trd) %>% 
#     filter(
#       ATEelv_Num >= obsNum_Thres) %>% 
#     summary() %>% 
#     print()
# }


# 3) Filter and classify global transects. --------------------------------

# With At least 20 observations and 
#   the ATE located between the lower and upper 
#   endpoints.
filtered_Transects <- transects %>% 
  filter(ATEelv_Num >= 20 & 
           ATEelv_Avg > CF_elv & 
           ATEelv_Avg < nonF_elv)

nrow(filtered_Transects) # 492932.

# Add an attribute of continents.
filtered_Transects$Continent <- 
  filtered_Transects$Regn_GMBA

# Change "Pacific Ocean" to "Asia".
filtered_Transects$Continent[
  filtered_Transects$Continent == "Pacific Ocean"
] <- "Asia"

# Calculate the relative position of 
#   the temporal average ATE.
filtered_Transects$ATE_relPos <- 
  (filtered_Transects$ATEelv_Avg - 
     filtered_Transects$CF_elv) / 
  filtered_Transects$elvRange

# Classify the filtered transects based on 
#   their ATE positions.
filtered_Transects$ATEposType <- "Upper half"

filtered_Transects$ATEposType[
  filtered_Transects$ATE_relPos <= 0.5
] <- "Lower half"

filtered_Transects$ATEposType <- 
  as.factor(filtered_Transects$ATEposType)

# Classify the filtered transects based on 
#   the ATE elevation trend.
filtered_Transects$Shift_Type <- "Stable"

filtered_Transects$Shift_Type[
  filtered_Transects$ATEelv_Trd <= -0.1
] <- "Downward"

filtered_Transects$Shift_Type[
  filtered_Transects$ATEelv_Trd >= 0.1
] <- "Upward"

filtered_Transects$Shift_Type <- 
  as.factor(filtered_Transects$Shift_Type)

# Output the filtered transects.
filtered_FN <- "filteredTransects_ATEelevationTrend_Gte20obs"

st_write(obj = filtered_Transects,
         dsn = file.path(wd_Dynamics,
                         filtered_FN),
         layer = filtered_FN,
         driver = "ESRI Shapefile")

