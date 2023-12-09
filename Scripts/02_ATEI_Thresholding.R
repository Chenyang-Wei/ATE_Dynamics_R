# Introduction. -----------------------------------------------------------

# 1) Determine an optimal threshold of ATEI.

# Updated: 12/8/2023.


# 0) Setup. ---------------------------------------------------------------

# Load Packages.
library(tidyverse)
library(sf)
library(gridExtra)
library(ROCR) # AUC analysis.

# Disable printing results in scientific notation.
options(scipen = 999)

# Set the theme and font size of plots.
theme_set(theme_bw())

# Define the working directories.
wd_ATEI <- "C:/Research_Projects/Treeline_LOCAL/ATEI_Estimation_LOCAL"

wd_Sampling <- 
  file.path(wd_ATEI, 
            "Manual_Sampling")

wd_Figs <- 
  file.path(wd_Sampling, 
            "Figures")


# 1) Load global variables with ATEI. -------------------------------------

# Load the global sample points (SHP).
samples_withATEI_FN <- "GlobalSamplePoints_withEstimatedATEI"

samples_withATEI <- st_read(
  file.path(wd_Sampling,
            samples_withATEI_FN),
  layer = samples_withATEI_FN,
  stringsAsFactors = TRUE)

# Check the global samples.
nrow(samples_withATEI) # 1970.
summary(samples_withATEI)

# Add the ATEI-predicted pixel location (threshold = 0.5).
samples_withATEI$ATEI_gtPt5 <- 
  ifelse(samples_withATEI$ATEI > 0.5, 1, 0)

# Compare the result with the observed location.
table(samples_withATEI$ATEI_gtPt5, 
      samples_withATEI$Binomial)

#           Reference
# Prediction   0   1
#          0 889 240
#          1 141 700

#  Omission error:
240 / (240 + 700) # 0.2553191.

# Commission error:
141 / (141 + 889) # 0.1368932.

# Accuracy.
mean(samples_withATEI$ATEI_gtPt5 == 
       samples_withATEI$Binomial) # 0.806599.

# Corrlation analysis.
cor(samples_withATEI$normal_Mag, samples_withATEI$mag_Comp) # 0.5769187.
cor(samples_withATEI$green_Comp, samples_withATEI$mag_Comp) # 0.4558763.
cor(samples_withATEI$dir_Comp, samples_withATEI$mag_Comp) # 0.2217633.


## Plot the pixel-based NDVI density.

samples_withATEI$Class <- factor(
  samples_withATEI$Class, 
  levels = c("Below", "Within", "Above"))

# Original Gaussian function of the western US.
GaussianFun_WesternUS <- function(x_Value) {
  base <- 
    (x_Value - 0.44) / 0.06
  
  power <- 
    (base ^ 2) * (-1 / 2)
  
  y_Value <- 
    exp(power)
  
  return(y_Value)
}

pixelNDVI_Plot <- 
  ggplot(
    samples_withATEI, 
    aes(x = medianNDVI)) +
  xlim(-0.2, 1) +
  xlab("Local NDVI") +
  geom_density(
    aes(y = after_stat(scaled), 
        fill = Class,
        color = Class
    ), 
    alpha = 0.5) +
  scale_fill_manual(
    values = c("lightgreen", "orange", "lightblue"), 
    name = "Pixel location") +
  scale_color_manual(
    values = c("darkgreen", "red", "darkblue")) +
  stat_function(
    fun = GaussianFun_WesternUS, color = "grey30", 
    lty = "dashed", lwd = 1) +
  scale_y_continuous(
    "Scaled pixel density", 
    sec.axis = dup_axis(name = "Greenness component")) +
  guides(
    fill = guide_legend(
      override.aes = list(
        color = c("darkgreen", "red", "darkblue"))),
    color = "none"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"))

pixelNDVI_Plot

# Output the result.
png(
  file.path(wd_Figs, "PixelNDVI_density.png"),
  width = 4500, height = 3000,
  units = "px", res = 800)

pixelNDVI_Plot

dev.off()


#### Plot the orientation component. ####

dir_Fun <- function(theta_Degree) {
  theta <- (theta_Degree / 180) * pi
  return ((1 - cos(theta)) / 2)
}

dir_Curve <- 
  ggplot(data.frame(x = c(0, 360)), 
         aes(x = x)) +
  stat_function(
    fun = dir_Fun,
    xlim = c(90, 270),
    color = "red",
    lwd = 1) +
  stat_function(
    fun = dir_Fun,
    xlim = c(0, 90),
    color = "darkgreen",
    lwd = 1) +
  stat_function(
    fun = dir_Fun,
    xlim = c(270, 360),
    color = "darkgreen",
    lwd = 1) +
  geom_hline(
    yintercept = 0.5,
    color = "black",
    lty = "dashed", lwd = 1) +
  scale_x_continuous(breaks = seq(0, 360, 90)) +
  xlab("Gradient angle (°)") +
  ylab("Orientation component") +
  guides(fill = "legend", color = "none") +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold"))

dir_Curve

# Output the result.
png(
  file.path(wd_Figs, "dir_Curve.png"),
  width = 4500, height = 3000,
  units = "px", res = 800)

dir_Curve

dev.off()


#### Plot the Gaussian curve at each pixel example. ####

# Sorted by greenness component.
samples_sorted <- 
  samples_withATEI %>% 
  arrange(green_Comp)

samples_sorted$PixelID <- 
  row.names(samples_sorted)

# Pixel example 1 ("above").
factor <- 0.3
pixel_ID <- nrow(samples_sorted) * factor

b_Pixel_1 <- 
  samples_sorted$OpenF_NDVI[pixel_ID]

c_Pixel_1 <- 
  samples_sorted$NDVI_diff[pixel_ID]

GaussianFun_Pixel_1 <- function(x_Value) {
  base <- 
    (x_Value - b_Pixel_1) / 
    c_Pixel_1
  
  power <- 
    (base ^ 2) * (-1 / 2)
  
  y_Value <- 
    exp(power)
  
  return(y_Value)
}

GaussianExample_Pixel_1 <- 
  ggplot(
    samples_sorted, 
    aes(x = medianNDVI)) + 
  annotate("rect",
           xmin = b_Pixel_1 - c_Pixel_1, 
           xmax = b_Pixel_1 + c_Pixel_1,
           ymin = 0, 
           ymax = 1, 
           alpha = 0.5,
           fill = "gray50") +
  stat_function(
    fun = GaussianFun_Pixel_1, 
    color = "black", 
    lwd = 1) +
  # geom_vline(
  #   xintercept = c(
  #     b_Pixel_1,
  #     b_Pixel_1 - c_Pixel_1,
  #     b_Pixel_1 + c_Pixel_1, 
  #     samples_sorted$medianNDVI[pixel_ID]),
  #   color = c("red", "blue", "blue", "darkgreen"), 
  #   lty = "dashed", lwd = 1) +
  geom_vline(
    xintercept = c(
      b_Pixel_1,
      samples_sorted$medianNDVI[pixel_ID]),
    color = c("black", "blue"),
    lty = "dashed", lwd = 1) +
  xlim(0, 1) +
  xlab("Local NDVI") +
  ylab("Greenness component") +
  annotate(geom = "text",
           x = 0.9,
           y = 0.9,
           label = 'Pixel 1\n("above")',
           color = "black",
           fontface = "bold") +
  guides(fill = "legend", color = "none") +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold"), 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

GaussianExample_Pixel_1

# Pixel example 2 ("below").
pixel_ID <- 353

b_Pixel_2 <- 
  samples_sorted$OpenF_NDVI[pixel_ID]

c_Pixel_2 <- 
  samples_sorted$NDVI_diff[pixel_ID]

GaussianFun_Pixel_2 <- function(x_Value) {
  base <- 
    (x_Value - b_Pixel_2) / 
    c_Pixel_2
  
  power <- 
    (base ^ 2) * (-1 / 2)
  
  y_Value <- 
    exp(power)
  
  return(y_Value)
}

GaussianExample_Pixel_2 <- 
  ggplot(
    samples_sorted, 
    aes(x = medianNDVI)) + 
  annotate("rect",
           xmin = b_Pixel_2 - c_Pixel_2, 
           xmax = b_Pixel_2 + c_Pixel_2,
           ymin = 0, 
           ymax = 1, 
           alpha = 0.5,
           fill = "gray50") +
  stat_function(
    fun = GaussianFun_Pixel_2, 
    color = "black", 
    lwd = 1) +
  # geom_vline(
  #   xintercept = c(
  #     b_Pixel_2,
  #     b_Pixel_2 - c_Pixel_2,
  #     b_Pixel_2 + c_Pixel_2, 
  #     samples_sorted$medianNDVI[pixel_ID]),
  #   color = c("red", "blue", "blue", "darkgreen"), 
  #   lty = "dashed", lwd = 1) +
  geom_vline(
    xintercept = c(
      b_Pixel_2,
      samples_sorted$medianNDVI[pixel_ID]),
    color = c("black", "darkgreen"), 
    lty = "dashed", lwd = 1) +
  xlim(0, 1) +
  xlab("Local NDVI") +
  ylab("Greenness component") +
  annotate(geom = "text",
           x = 0.1,
           y = 0.9,
           label = 'Pixel 2\n("below")',
           color = "black",
           fontface = "bold") +
  guides(fill = "legend", color = "none") +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold"), 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

GaussianExample_Pixel_2

# Pixel example 3 ("within").
factor <- 0.9
pixel_ID <- nrow(samples_sorted) * factor

b_Pixel_3 <- 
  samples_sorted$OpenF_NDVI[pixel_ID]

c_Pixel_3 <- 
  samples_sorted$NDVI_diff[pixel_ID]

GaussianFun_Pixel_3 <- function(x_Value) {
  base <- 
    (x_Value - b_Pixel_3) / 
    c_Pixel_3
  
  power <- 
    (base ^ 2) * (-1 / 2)
  
  y_Value <- 
    exp(power)
  
  return(y_Value)
}

GaussianExample_Pixel_3 <- 
  ggplot(
    samples_sorted, 
    aes(x = medianNDVI)) + 
  annotate("rect",
           xmin = b_Pixel_3 - c_Pixel_3, 
           xmax = b_Pixel_3 + c_Pixel_3,
           ymin = 0, 
           ymax = 1, 
           alpha = 0.5,
           fill = "gray50") +
  stat_function(
    fun = GaussianFun_Pixel_3, 
    color = "black", 
    lwd = 1) +
  # geom_vline(
  #   xintercept = c(
  #     b_Pixel_3,
  #     b_Pixel_3 - c_Pixel_3,
  #     b_Pixel_3 + c_Pixel_3, 
  #     samples_sorted$medianNDVI[pixel_ID]),
  #   color = c("red", "blue", "blue", "darkgreen"), 
  #   lty = "dashed", lwd = 1) +
  geom_vline(
    xintercept = c(
      b_Pixel_3,
      samples_sorted$medianNDVI[pixel_ID]),
    color = c("black", "red"), 
    lty = "dashed", lwd = 1) +
  xlim(0, 1) +
  xlab("Local NDVI") +
  ylab("Greenness component") +
  annotate(geom = "text",
           x = 0.1,
           y = 0.9,
           label = 'Pixel 3\n("within")',
           color = "black",
           fontface = "bold") +
  guides(fill = "legend", color = "none") +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold"))

GaussianExample_Pixel_3

# Save a combined plot.
png(
  file.path(wd_Figs, "Greenness_Pixel_Examples_2.png"),
  width = 4500, height = 6000,
  units = "px", res = 800)

grid.arrange(GaussianExample_Pixel_1,
             GaussianExample_Pixel_2,
             GaussianExample_Pixel_3,
             nrow = 3,
             heights = c(6, 6, 7))

dev.off()


###### Plot the ATEI and the generated components. ######

text_X <- 1
text_Y <- 1.15

colnames(samples_withATEI)


#### Plot the gradient magnitude component. ####

samples_withATEI$Class <- factor(
  samples_withATEI$Class, 
  levels = c("Within", "Below", "Above"))

# Plot the normalized gradient magnitude.
normalMag_SinglePlot <- 
  ggplot(
    samples_withATEI, 
    aes(x = normal_Mag)) +
  xlim(0, 1) +
  xlab("Gradient magnitude (normalized)") +
  ylab("Scaled pixel density") +
  geom_density(
    aes(y = after_stat(scaled), 
        fill = Class,
        color = Class
    ), 
    alpha = 0.5) +
  scale_fill_manual(
    values = c("orange", "lightgreen", "lightblue"), 
    name = "Pixel location") +
  scale_color_manual(
    values = c("red", "darkgreen", "darkblue")) +
  guides(
    fill = guide_legend(
      override.aes = list(
        color = c("red", "darkgreen", "darkblue"))),
    color = "none"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"))

normalMag_SinglePlot

# Output the created plot.
png(
  file.path(wd_Figs, "normalMag_SinglePlot.png"),
  width = 4500, height = 3000,
  units = "px", res = 800)

normalMag_SinglePlot

dev.off()

# Create a single plot.
mag_Comp_SinglePlot <- 
  ggplot(
    samples_withATEI, 
    aes(x = mag_Comp)) +
  xlim(0, 1) +
  xlab("Gradient component") +
  ylab("Scaled pixel density") +
  geom_density(
    aes(y = after_stat(scaled), 
        fill = Class,
        color = Class
    ), 
    alpha = 0.5) +
  scale_fill_manual(
    values = c("orange", "lightgreen", "lightblue"), 
    name = "Pixel location") +
  scale_color_manual(
    values = c("red", "darkgreen", "darkblue")) +
  guides(
    fill = guide_legend(
      override.aes = list(
        color = c("red", "darkgreen", "darkblue"))),
    color = "none"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"))

mag_Comp_SinglePlot

# Output the single plot.
png(
  file.path(wd_Figs, "mag_Comp_SinglePlot.png"),
  width = 4500, height = 3000,
  units = "px", res = 800)

mag_Comp_SinglePlot

dev.off()

# mag_Comp_Plot <- 
#   ggplot(
#     samples_withATEI, 
#     aes(x = mag_Comp)) +
#   xlim(0, 1) +
#   xlab("Gradient component") +
#   geom_density(
#     aes(y = after_stat(scaled), 
#         fill = Class,
#         color = Class
#     ), 
#     alpha = 0.5) +
#   scale_fill_manual(
#     values = c("orange", "lightgreen", "lightblue"), 
#     name = "Pixel location") +
#   scale_color_manual(
#     values = c("red", "darkgreen", "darkblue")) +
#   scale_y_continuous(
#     name = "Scaled density",
#     breaks = c(0, 0.5, 1),
#     limits = c(0, 1.2)) +
#   annotate(geom = "text",
#            x = text_X,
#            y = text_Y,
#            label = "a",
#            color = "black",
#            fontface = "bold") +
#   theme(
#     legend.position = "none",
#     axis.title = element_text(face = "bold"))
# 
# mag_Comp_Plot


#### Plot the greenness component. ####

samples_withATEI$Class <- factor(
  samples_withATEI$Class, 
  levels = c("Below", "Within", "Above"))

# Create a single plot.
green_Comp_SinglePlot <- 
  ggplot(
    samples_withATEI, 
    aes(x = green_Comp)) +
  xlim(0, 1) +
  xlab("Greenness component") +
  ylab("Scaled pixel density") +
  geom_density(
    aes(y = after_stat(scaled), 
        fill = Class,
        color = Class
    ), 
    alpha = 0.5) +
  scale_fill_manual(
    values = c("lightgreen", "orange", "lightblue"), 
    name = "Pixel location") +
  scale_color_manual(
    values = c("darkgreen", "red", "darkblue")) +
  guides(
    fill = guide_legend(
      override.aes = list(
        color = c("darkgreen", "red", "darkblue"))),
    color = "none"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"))

green_Comp_SinglePlot

# Output the single plot.
png(
  file.path(wd_Figs, "green_Comp_SinglePlot.png"),
  width = 4500, height = 3000,
  units = "px", res = 800)

green_Comp_SinglePlot

dev.off()

# green_Comp_Plot <-
#   ggplot(
#     samples_withATEI,
#     aes(x = green_Comp)) +
#   xlim(0, 1) +
#   xlab("Greenness component") +
#   geom_density(
#     aes(y = after_stat(scaled),
#         fill = Class,
#         color = Class
#     ),
#     alpha = 0.5) +
#   scale_fill_manual(
#     values = c("lightgreen", "orange", "lightblue"),
#     name = "Pixel location") +
#   scale_color_manual(
#     values = c("darkgreen", "red", "darkblue")) +
#   scale_y_continuous(
#     name = "Scaled density",
#     breaks = c(0, 0.5, 1),
#     limits = c(0, 1.2)) +
#   annotate(geom = "text",
#            x = text_X,
#            y = text_Y,
#            label = "b",
#            color = "black",
#            fontface = "bold") +
#   theme(
#     legend.position = "none",
#     axis.title = element_text(face = "bold"))
# 
# green_Comp_Plot


#### Plot the gradient direction component. ####

samples_withATEI$Class <- factor(
  samples_withATEI$Class, 
  levels = c("Below", "Above", "Within"))

# Create a single plot.
dir_Comp_SinglePlot <- 
  ggplot(
    samples_withATEI, 
    aes(x = dir_Comp)) +
  xlim(0, 1) +
  xlab("Orientation component") +
  ylab("Scaled pixel density") +
  geom_density(
    aes(y = after_stat(scaled), 
        fill = Class,
        color = Class
    ), 
    alpha = 0.5) +
  scale_fill_manual(
    values = c("lightgreen", "lightblue", "orange"), 
    name = "Pixel location", 
    labels = c("Below\nthe ATE",
               "Above\nthe ATE",
               "Within\nthe ATE")) +
  scale_color_manual(
    values = c("darkgreen", "darkblue", "red")) +
  guides(
    fill = guide_legend(
      override.aes = list(
        color = c("darkgreen", "darkblue", "red"))),
    color = "none"
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"))

dir_Comp_SinglePlot

# Output the single plot.
png(
  file.path(wd_Figs, "dir_Comp_SinglePlot.png"),
  width = 4500, height = 3000,
  units = "px", res = 800)

dir_Comp_SinglePlot

dev.off()

# dir_Comp_Plot <- 
#   ggplot(
#     samples_withATEI, 
#     aes(x = dir_Comp)) +
#   xlim(0, 1) +
#   xlab("Orientation component") +
#   geom_density(
#     aes(y = after_stat(scaled), 
#         fill = Class,
#         color = Class
#     ), 
#     alpha = 0.5) +
#   scale_fill_manual(
#     values = c("lightgreen", "lightblue", "orange"), 
#     name = "Pixel location", 
#     labels = c("Below\nthe ATE",
#                "Above\nthe ATE",
#                "Within\nthe ATE")) +
#   scale_color_manual(
#     values = c("darkgreen", "darkblue", "red")) +
#   scale_y_continuous(
#     name = "Scaled density",
#     breaks = c(0, 0.5, 1),
#     limits = c(0, 1.2)) +
#   annotate(geom = "text",
#            x = text_X,
#            y = text_Y,
#            label = "c",
#            color = "black",
#            fontface = "bold") +
#   guides(
#     fill = guide_legend(
#       override.aes = list(
#         color = c("darkgreen", "darkblue", "red"))),
#     color = "none"
#   ) +
#   theme(
#     legend.position = "bottom", 
#     legend.title = element_text(face = "bold"),
#     axis.title = element_text(face = "bold"))
# 
# dir_Comp_Plot


#### Save a combined plot. ####

png(
  file.path(wd_Figs, "ATEI_components.png"),
  width = 3500, height = 4500,
  units = "px", res = 800)

grid.arrange(mag_Comp_Plot,
             green_Comp_Plot,
             dir_Comp_Plot,
             nrow = 3,
             heights = c(6, 6, 8))

dev.off()


# 2) Generate the ROC curve. ----------------------------------------------

# Combined the estimated ATEI with the observed location type.
prediction <- 
  prediction(samples_withATEI$ATEI, 
             samples_withATEI$Binomial)

# Generate the ROC curve.
performance <- 
  performance(prediction, 
              measure = "tpr", 
              x.measure = "fpr")

str(performance)
plot(performance)

# Calculate the area under the ROC curve, or AUROC. 
auc <- 
  performance(prediction, 
              measure = "auc")

auc <- auc@y.values[[1]]

auc_Value <- 
  auc %>% 
  round(4)

auc_Value # 0.8758.


# 3) Optimize the ATEI threshold. -----------------------------------------

# Create a data frame for the ROC curve.
roc_DF <- data.frame(
  cutoff = performance@alpha.values[[1]], 
  fpr = performance@x.values[[1]], 
  tpr = performance@y.values[[1]])

# Sort the data frame based on the true positive rate.
roc_DF <- 
  roc_DF[order(roc_DF$tpr, 
               decreasing = TRUE), ]

# Select thresholds with the false positive rate < 0.05.
low_FPR <- 
  roc_DF %>% 
  filter(fpr < 0.05)

head(low_FPR)
#      cutoff        fpr       tpr
# 1 0.5460715 0.04854369 0.5702128

# Extract the threshold with the highest true positive rate.
optimal_Thres <- 
  low_FPR[1, "cutoff"]

# Check the optimized ATEI threshold.
optimal_Thres # 0.5460715.

# Self-testing of the optimal threshold.
samples_withATEI$ATEI_gtOpt <- 
  ifelse(samples_withATEI$ATEI > optimal_Thres, 1, 0)

table(samples_withATEI$ATEI_gtOpt, 
      samples_withATEI$Binomial)

#           Reference
# Prediction   0   1
#          0 980 405
#          1  50 535

#  Omission error:
405 / (405 + 535) # 0.4308511.

# Commission error:
50 / (50 + 980) # 0.04854369.

# Accuracy.
mean(samples_withATEI$ATEI_gtOpt == 
       samples_withATEI$Binomial) # 0.7690355.


# 4) Plot the ROC and ATEI. -----------------------------------------------

text_Y <- 1.05

# Generate a ROC plot.
roc_Plot <- ggplot(roc_DF) + 
  geom_segment(aes(x = 0, y = 0, 
                   xend = 1, yend = 1),
               color = "gray50",
               lty = "dashed",
               lwd = 1) + 
  geom_line(aes(x = fpr, y = tpr),
            color = "black",
            lwd = 1) + 
  geom_vline(xintercept = low_FPR[1, "fpr"], 
             color = "darkgreen", 
             lty = "dashed",
             lwd = 0.5) + 
  geom_hline(yintercept = low_FPR[1, "tpr"], 
             color = "darkgreen", 
             lty = "dashed",
             lwd = 0.5) + 
  geom_label(aes(x = 1, y = 0, 
                 hjust = 1, vjust = 0, 
                 label = paste0("AUC = ", auc_Value)), 
             size = 4, color = "red", fontface = "italic") + 
  xlab("False positive rate") + 
  ylab("True positive rate") + 
  annotate(geom = "text",
           x = 1,
           y = text_Y,
           label = "a",
           color = "black",
           fontface = "bold") +
  theme(
    axis.title = element_text(face = "bold"), 
    legend.position = "none"
  )

roc_Plot

# Create a boxplot for the ATEI classification result.
ATEI_boxPlot <- 
  ggplot(
    data = samples_withATEI, 
    mapping = aes(
      x = Binomial, 
      y = ATEI)) +
  geom_boxplot(
    mapping = aes(
      x = Binomial,
      y = ATEI),
    color = c("darkblue", "red"),
    outlier.alpha = 0.5,
    outlier.size = 1,
    outlier.color = "gray50") +
  xlab("Pixel location") + 
  ylab("Estimated ATEI") + 
  scale_x_discrete(labels = c("Out of the ATE", 
                              "Within the ATE")) + 
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  geom_hline(yintercept = optimal_Thres,
             lty = "dashed",
             lwd = 0.5, 
             color = "darkgreen") +
  annotate(geom = "text",
           x = 2.5,
           y = text_Y,
           label = "b",
           color = "black",
           fontface = "bold") +
  theme(legend.position = "none", 
        axis.title = element_text(face = "bold"))

ATEI_boxPlot

# Save a combined plot.
png(
  file.path(wd_Figs, "ATEI_ROC.png"),
  width = 6000, height = 3000,
  units = "px", res = 800)

grid.arrange(roc_Plot,
             ATEI_boxPlot,
             ncol = 2)

dev.off()

# Plot the three classes.
samples_withATEI$Class <- factor(
  samples_withATEI$Class, 
  levels = c("Within", "Above", "Below"))

estimatedATEI_plot <- 
  ggplot(
    data = samples_withATEI) +
  geom_boxplot(
    mapping = aes(
      x = Class, 
      y = ATEI), 
    color = c("red", "darkblue", "darkgreen"), 
    outlier.shape = NA) +
  scale_x_discrete(
    labels = c("Within\nthe ATE",
               "Above\nthe ATE",
               "Below\nthe ATE")) +
  geom_hline(yintercept = optimal_Thres,
             lty = "dashed",
             lwd = 0.5, 
             color = "black") +
  xlab("Pixel location") +
  ylab("Estimated ATEI") +
  theme(
    legend.position = "none", 
    axis.title = element_text(face = "bold"))

estimatedATEI_plot

# Save the ATEI box-plot.
png(
  file.path(wd_Figs, "Estimated_ATEI.png"),
  width = 2000, height = 3000,
  units = "px", res = 800)

estimatedATEI_plot

dev.off()

