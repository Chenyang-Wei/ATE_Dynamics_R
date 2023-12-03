# Introduction. -----------------------------------------------------------

# Updated: 6/15/2023.


# 0) Setup. ---------------------------------------------------------------

# Load Packages.
library(tidyverse)
library(sf)
library(gridExtra)
library(rstatix) # T-test
library(ggpubr) # Box plots

# Disable printing results in scientific notation.
options(scipen = 999)

# Set the theme and font size of plots.
theme_set(theme_bw())

# Define the working directories.
wd_Local <- "C:/Research_Projects/Treeline_LOCAL"

wd_ATEI <- 
  file.path(wd_Local, 
            "ATEI_Estimation_LOCAL")

wd_Dynamics <- 
  file.path(wd_ATEI, 
            "ATE_Dynamics")

wd_Figs <- 
  file.path(wd_Dynamics, 
            "Figures")


# 1) Load the filtered transects ------------------------------------------

filtered_FN <- "filteredTransects_ATEelevationTrend_Gte20obs"

filtered_Transects <- st_read(
  file.path(wd_Dynamics,
            filtered_FN),
  layer = filtered_FN,
  stringsAsFactors = TRUE)

nrow(filtered_Transects) # 492932


# 2) ATE dynamics analysis. -----------------------------------------------

filtered_Transects %>% 
  st_drop_geometry() %>% 
  select(ATEelv_Trd) %>% 
  summarise(median = median(ATEelv_Trd)) # 0.9863846

# Divide the transects based on the ATE dynamics.
Upward_Shift <- 
  filtered_Transects %>% 
  filter(Shift_Type == "Upward")

Downward_Shift <- 
  filtered_Transects %>% 
  filter(Shift_Type == "Downward")

Stable_Shift <- 
  filtered_Transects %>% 
  filter(Shift_Type == "Stable")

# Proportion.
nrow(Upward_Shift) / nrow(filtered_Transects) # 0.8323481
nrow(Downward_Shift) / nrow(filtered_Transects) # 0.1128797
nrow(Stable_Shift) / nrow(filtered_Transects) # 0.05477226

# Median trend.
median(Upward_Shift$ATEelv_Trd) # 1.275764
median(Downward_Shift$ATEelv_Trd) # -0.6863173

# Density plot.
ATEelv_Trend_Density <- 
  ggplot(data = filtered_Transects,
         aes(x = ATEelv_Trd, 
             y = after_stat(scaled))) +
  geom_density(fill = "gray50", 
               color = "black", 
               alpha = 0.5) +
  xlab("Estimated ATE dynamics (m/year)") +
  ylab("Scaled transect density") +
  xlim(-5, 5) +
  annotate("rect",
           xmin = -0.1, xmax = 0.1,
           ymin = 0, ymax = 1, 
           alpha = 0.5,
           fill = "darkgreen") +
  annotate("rect", 
           xmin = -5, xmax = -0.1, 
           ymin = 0, ymax = 1, 
           alpha = 0.5, 
           fill = "darkblue") + 
  annotate("rect", 
           xmin = 0.1, xmax = 5, 
           ymin = 0, ymax = 1, 
           alpha = 0.5, 
           fill = "darkred") + 
  geom_vline(xintercept = 0,
             color= "black",
             lwd = 0.5, 
             alpha = 0.5) + 
  geom_vline(xintercept = median(Downward_Shift$ATEelv_Trd),
             color= "blue",
             lty = "dashed",
             lwd = 1) + 
  geom_vline(xintercept = median(Upward_Shift$ATEelv_Trd),
             color= "red",
             lty = "dashed",
             lwd = 1) + 
  theme(legend.position = "none", 
        axis.title = element_text(face = "bold"))

ATEelv_Trend_Density

# Density plot 2.
ATEelv_Trend_Density_2 <- 
  ggplot(
    data = filtered_Transects) +
  geom_density(
    aes(x = ATEelv_Trd, 
        y = after_stat(scaled),
        color = ATEposType,
        group = ATEposType)) +
  scale_color_manual(
    values = c("red", "blue"), 
    name = "Transect type",
    labels = c("Lower ATE", "Higher ATE")) +
  xlab("Estimated ATE dynamics (m/year)") +
  ylab("Scaled transect density") +
  xlim(-5, 5) +
  geom_vline(xintercept = 0,
             color= "black",
             lwd = 0.5, 
             alpha = 1) +
  theme(legend.position = "bottom", 
        axis.title = element_text(face = "bold"))

ATEelv_Trend_Density_2

# Output.
png(
  file.path(wd_Figs, "ATEelv_Trend_Density_2.png"),
  width = 6000, height = 3000,
  units = "px", res = 800)

ATEelv_Trend_Density_2

dev.off()


# 3) Vs. NDVI trend. ------------------------------------------------------

# Divide the transects based on the ATE position.
lower_Half <- 
  filtered_Transects %>% 
  filter(ATEposType == "Lower half")

upper_Half <- 
  filtered_Transects %>% 
  filter(ATEposType == "Upper half")

# Proportion.
nrow(lower_Half) / nrow(filtered_Transects) # 0.5586551
nrow(upper_Half) / nrow(filtered_Transects) # 0.4413449

# Median trend.
median(lower_Half$ATEelv_Trd) # 1.079337
median(upper_Half$ATEelv_Trd) # 0.8765018

# Upward shift in the lower Half.
lower_Half %>% 
  filter(Shift_Type == "Upward") %>% 
  nrow() / nrow(lower_Half) # 0.8585985

lower_Half %>% 
  filter(Shift_Type == "Upward") %>% 
  select(ATEelv_Trd) %>% 
  st_drop_geometry() %>% 
  summarise(median = median(ATEelv_Trd)) # 1.339503

# Upward shift in the upper Half.
upper_Half %>% 
  filter(Shift_Type == "Upward") %>% 
  nrow() / nrow(upper_Half) # 0.7991202

upper_Half %>% 
  filter(Shift_Type == "Upward") %>% 
  select(ATEelv_Trd) %>% 
  st_drop_geometry() %>% 
  summarise(median = median(ATEelv_Trd)) # 1.199728

# Correlation coefficients.
cor(filtered_Transects$avgNDVI_Tr, 
    filtered_Transects$ATEelv_Trd) # 0.4751576
cor(lower_Half$avgNDVI_Tr, 
    lower_Half$ATEelv_Trd) # 0.4732997
cor(upper_Half$avgNDVI_Tr, 
    upper_Half$ATEelv_Trd) # 0.479185

# Hex plots.
ATEelv_NDVI_trends <-
  ggplot(filtered_Transects,
         aes(ATEelv_Trd, 
             avgNDVI_Tr)) +
  geom_hex(bins = 1e3) +
  scale_fill_viridis_c(trans = "log",
                       name = "Number of ATETs",
                       breaks = c(1, 5, 20),
                       guide = guide_colourbar(
                         title.position = "left"
                       )) +
  geom_hline(yintercept = 0,
             color = "black",
             alpha = 1,
             lwd = 0.5) +
  stat_smooth(geom = "line",
              method = "lm",
              formula = y ~ x,
              se = FALSE,
              color = "red",
              alpha = 1,
              lwd = 0.5) +
  annotate(geom = "text",
           x = -5,
           y = 0.01,
           label = "a",
           color = "black",
           fontface = "bold") +
  xlab("ATE dynamics (m/year)") +
  ylab("NDVI trend (per year)") +
  lims(x = c(-5, 5),
       y = c(-0.01, 0.01)) +
  theme(legend.position = "bottom"
        # axis.title.x = element_blank(),
        # axis.text.x = element_blank(),
        # axis.ticks.x = element_blank()
  )

ATEelv_NDVI_trends

lowerHalf_NDVItrend <-
  ggplot(lower_Half,
         aes(ATEelv_Trd, 
             avgNDVI_Tr)) +
  geom_hex(bins = 1e3) +
  scale_fill_viridis_c(trans = "log",
                       name = "Number of ATETs",
                       breaks = c(1, 5, 15),
                       guide = guide_colourbar(
                         title.position = "left"
                       )) +
  geom_hline(yintercept = 0,
             color = "black",
             alpha = 1,
             lwd = 0.5) +
  stat_smooth(geom = "line",
              method = "lm",
              formula = y ~ x,
              se = FALSE,
              color = "red",
              alpha = 1,
              lwd = 0.5) +
  annotate(geom = "text",
           x = -5,
           y = 0.01,
           label = "b",
           color = "black",
           fontface = "bold") +
  xlab("ATE dynamics (m/year)") +
  ylab("NDVI trend (per year)") +
  lims(x = c(-5, 5),
       y = c(-0.01, 0.01)) +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )

lowerHalf_NDVItrend

upperHalf_NDVItrend <-
  ggplot(upper_Half,
         aes(ATEelv_Trd, 
             avgNDVI_Tr)) +
  geom_hex(bins = 1e3) +
  scale_fill_viridis_c(trans = "log",
                       name = "Number of ATETs",
                       breaks = c(1, 5, 15),
                       guide = guide_colourbar(
                         title.position = "left"
                       )) +
  geom_hline(yintercept = 0,
             color = "black",
             alpha = 1,
             lwd = 0.5) +
  stat_smooth(geom = "line",
              method = "lm",
              formula = y ~ x,
              se = FALSE,
              color = "red",
              alpha = 1,
              lwd = 0.5) +
  annotate(geom = "text",
           x = -5,
           y = 0.01,
           label = "c",
           color = "black",
           fontface = "bold") +
  xlab("ATE dynamics (m/year)") +
  ylab("NDVI trend (per year)") +
  lims(x = c(-5, 5),
       y = c(-0.01, 0.01)) +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )

upperHalf_NDVItrend

# Output.
png(
  file.path(wd_Figs, "ATEelv_NDVI_trends.png"),
  width = 9000, height = 3000,
  units = "px", res = 800)

grid.arrange(
  ATEelv_NDVI_trends, 
  lowerHalf_NDVItrend, 
  upperHalf_NDVItrend, 
  ncol = 3, 
  widths = c(6, 5, 5))

dev.off()


# 4) Vs. EG trend. --------------------------------------------------------

# Correlation coefficients.
cor(filtered_Transects$EGtrend_km, 
    filtered_Transects$ATEelv_Trd) # 0.1146261
cor(lower_Half$EGtrend_km, 
    lower_Half$ATEelv_Trd) # 0.06267626
cor(upper_Half$EGtrend_km, 
    upper_Half$ATEelv_Trd) # 0.2137478.

# Hex plots.
ATEelv_EG_trends <-
  ggplot(filtered_Transects,
         aes(ATEelv_Trd, 
             EGtrend_km)) +
  geom_hex(bins = 1e3) +
  scale_fill_viridis_c(trans = "log",
                       name = "Number of ATETs",
                       breaks = c(1, 5, 20),
                       guide = guide_colourbar(
                         title.position = "left"
                       )) +
  geom_hline(yintercept = 0,
             color = "black",
             alpha = 1,
             lwd = 0.5) +
  stat_smooth(geom = "line",
              method = "lm",
              formula = y ~ x,
              se = FALSE,
              color = "red",
              alpha = 1,
              lwd = 0.5) +
  annotate(geom = "text",
           x = -5,
           y = 0.05,
           label = "a",
           color = "black",
           fontface = "bold") +
  xlab("ATE dynamics (m/year)") +
  ylab("EG trend (per km*year)") +
  lims(x = c(-5, 5),
       y = c(-0.05, 0.05)) +
  theme(legend.position = "bottom"
        # axis.title.x = element_blank(),
        # axis.text.x = element_blank(),
        # axis.ticks.x = element_blank()
  )

ATEelv_EG_trends

lowerHalf_EGtrend <-
  ggplot(lower_Half,
         aes(ATEelv_Trd, 
             EGtrend_km)) +
  geom_hex(bins = 1e3) +
  scale_fill_viridis_c(trans = "log",
                       name = "Number of ATETs",
                       breaks = c(1, 5, 15),
                       guide = guide_colourbar(
                         title.position = "left"
                       )) +
  geom_hline(yintercept = 0,
             color = "black",
             alpha = 1,
             lwd = 0.5) +
  stat_smooth(geom = "line",
              method = "lm",
              formula = y ~ x,
              se = FALSE,
              color = "red",
              alpha = 1,
              lwd = 0.5) +
  annotate(geom = "text",
           x = -5,
           y = 0.05,
           label = "b",
           color = "black",
           fontface = "bold") +
  xlab("ATE dynamics (m/year)") +
  ylab("EG trend (per km*year)") +
  lims(x = c(-5, 5),
       y = c(-0.05, 0.05)) +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )

lowerHalf_EGtrend

upperHalf_EGtrend <-
  ggplot(upper_Half,
         aes(ATEelv_Trd, 
             EGtrend_km)) +
  geom_hex(bins = 1e3) +
  scale_fill_viridis_c(trans = "log",
                       name = "Number of ATETs",
                       breaks = c(1, 5, 15),
                       guide = guide_colourbar(
                         title.position = "left"
                       )) +
  geom_hline(yintercept = 0,
             color = "black",
             alpha = 1,
             lwd = 0.5) +
  stat_smooth(geom = "line",
              method = "lm",
              formula = y ~ x,
              se = FALSE,
              color = "red",
              alpha = 1,
              lwd = 0.5) +
  annotate(geom = "text",
           x = -5,
           y = 0.05,
           label = "c",
           color = "black",
           fontface = "bold") +
  xlab("ATE dynamics (m/year)") +
  ylab("EG trend (per km*year)") +
  lims(x = c(-5, 5),
       y = c(-0.05, 0.05)) +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )

upperHalf_EGtrend

# Output.
png(
  file.path(wd_Figs, "ATEelv_EG_trends.png"),
  width = 9000, height = 3000,
  units = "px", res = 800)

grid.arrange(
  ATEelv_EG_trends, 
  lowerHalf_EGtrend, 
  upperHalf_EGtrend, 
  ncol = 3, 
  widths = c(6, 5, 5))

dev.off()



# # 5) T-test between lower-ATE and higher-ATE. -----------------------------
# 
# filtered_DF <- filtered_Transects %>% 
#   st_drop_geometry()
# 
# # Summary statistics.
# filtered_DF %>%
#   group_by(ATEposType) %>%
#   get_summary_stats(ATEelv_Trd, 
#                     type = "mean_sd")
# 
# # # Visualization.
# # bxp <- ggboxplot(
# #   filtered_DF, x = "ATEposType", y = "ATEelv_Trd", 
# #   ylab = "ATEelv_Trd", xlab = "ATEposType", add = "jitter"
# # )
# # bxp
# 
# # Identify outliers by groups.
# filtered_DF %>%
#   group_by(ATEposType) %>%
#   identify_outliers(ATEelv_Trd)
# 
# # Check normality by groups.
# # Compute Shapiro wilk test by groups.
# filtered_DF %>%
#   group_by(ATEposType) %>%
#   shapiro_test(ATEelv_Trd)
