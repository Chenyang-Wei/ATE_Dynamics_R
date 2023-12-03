# Introduction. -----------------------------------------------------------

# 1) Generate the "greenness" component based on
#   the Gaussian-transformed NDVI.
# 2) Generate the gradient direction component.
# 3) Generate the weighted gradient magnitude component.
# 4) Construct the ATEI.
# 5) Plot the ATEI and the generated components.

# Updated: 12/3/2023.


# 0) Setup. ---------------------------------------------------------------

# Load Packages.
library(tidyverse)
library(sf)
library(gridExtra)
library(caret) # Cross-validation.
library(car) # VIF.

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


# 1) Load the sampled ATEI variables. -------------------------------------

# Load the global sample points (SHP).
samples_FN <- "GlobalSamplePoints_withATEIvars"

samples_DF <- st_read(
  file.path(wd_Sampling,
            samples_FN),
  layer = samples_FN,
  stringsAsFactors = TRUE)

# Remove the unnecessary columns.
samples_DF <- samples_DF %>% 
  select(!c(starts_with("Image"), 
            Type, withinATE, 
            InNewCATE, Location))

# Change the level order of "Class" for better ggplots.
samples_DF$Class <- factor(
  samples_DF$Class, 
  levels = c("Within", "Above", "Below"))

# Check the global samples.
nrow(samples_DF) # 1970.
summary(samples_DF)


# 2) Generate the "greenness" component. ----------------------------------

# Mean.
b <- samples_DF$OpenF_NDVI

# Standard deviation.
c <- samples_DF$NDVI_diff

# Transform the NDVI using a Gaussian function based on
#   the open-forest NDVI and the NDVI difference.
samples_DF$green_Comp <- 
  exp(-(1 / 2) * (((samples_DF$medianNDVI - b) / c) ^ 2))


# 3) Generate the gradient direction component. ---------------------------

samples_DF$dir_Comp <- 
  (1 - cos(samples_DF$dir_Angle)) / 2


# 4) Generate the weighted magnitude component. ---------------------------

# Normalize the gradient magnitude.
min(samples_DF$NDVI_mag) # 0.
max(samples_DF$NDVI_mag) # 0.00855141.

samples_DF$normal_Mag <-
  samples_DF$NDVI_mag / max(samples_DF$NDVI_mag)

# Generate the magnitude component.
samples_DF$mag_Comp <- 
  samples_DF$normal_Mag * samples_DF$green_Comp * samples_DF$dir_Comp


# 5) Construct the ATEI. --------------------------------------------------

# Create a binomial variable of the location type.
samples_DF$Binomial <- 0

samples_DF$Binomial[samples_DF$Class == "Within"] <- 1

samples_DF$Binomial <- 
  samples_DF$Binomial %>% 
  as.factor()

summary(samples_DF)

# Define the training control using 
#   10-fold cross-validation with 100 repeats.
set.seed(9999)

train_control <- 
  trainControl(method = "repeatedcv", 
               number = 10, 
               repeats = 100)

# Define a formula for fitting the model.
formula <- 
  Binomial ~ mag_Comp + green_Comp + dir_Comp

# Train the model between the binomial location type and 
#   the constructed components.
mod_fit <- 
  train(formula, 
        data = samples_DF, 
        method = "glm", 
        family = "binomial", 
        trControl = train_control)

# Check the fitted model.
summary(mod_fit)

print(mod_fit)

# Accuracy: 0.8014061.
# Kappa: 0.5997187.

mod_fit$results
#   parameter  Accuracy     Kappa AccuracySD    KappaSD
# 1      none 0.8014061 0.5997187 0.02704982 0.05508876

# Fit a full model between the location type and the ATEI components.
fullModel <- glm(
  formula, 
  data = samples_DF, 
  family = "binomial")

# Check the full model (the same as the cross-validation result).
summary(fullModel)

#   Coefficients:
#               Estimate Std. Error z value            Pr(>|z|)    
#   (Intercept)  -2.3637     0.2080 -11.365 <0.0000000000000002 ***
#   mag_Comp     83.0523     6.9588  11.935 <0.0000000000000002 ***
#   green_Comp   -2.2642     0.2757  -8.212 <0.0000000000000002 ***
#   dir_Comp      2.4022     0.2374  10.117 <0.0000000000000002 ***
#   ---
#   AIC: 1891.5

# Calculate the VIF for each predictor variable in the model.
vif(fullModel)
# mag_Comp green_Comp   dir_Comp 
# 2.256136   2.178000   1.053681 

# Extract the model coefficients.
coef_DF <- 
  fullModel$coefficients %>% 
  as.data.frame()

coef_DF

# Intercept   -2.363688
# mag_Coef    83.052317
# green_Coef  -2.264222
# dir_Coef     2.402248

# Estimate the ATEIs of global samples with the full model.
estimated_ATEI <- 
  predict(fullModel, type = "response")

# Merge the estimated ATEI into the original dataset.
samples_withATEI <- 
  cbind(samples_DF, 
        ATEI = estimated_ATEI)

summary(samples_withATEI)

# Output a SHP file with the estimated ATEI.
output_withATEI_FN <- "GlobalSamplePoints_withEstimatedATEI"

st_write(obj = samples_withATEI,
         dsn = file.path(wd_Sampling,
                         output_withATEI_FN),
         layer = output_withATEI_FN,
         driver = "ESRI Shapefile")

