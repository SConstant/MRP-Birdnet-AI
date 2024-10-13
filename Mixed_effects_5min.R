## HEADER ####
## Who: Sophie Constant
## What: MRP-Birdsong-AI- Mixed effects on cumulative 
## Last edited: 2024-08-16 format)
####



## Packages ####

# Fixing this warning: 
#1: package ‘lme4’ was built under R version 4.3.3 
#2: In check_dep_version() : ABI version mismatch: 
#  lme4 was built with Matrix ABI version 1
#Current Matrix ABI version is 0
#please re-install lme4 from source or restore original ‘Matrix’ package


# Reinstall Matrix from source
install.packages("Matrix", type = "source")

# Reinstall the Matrix package
install.packages("Matrix", dependencies = TRUE)

# Reinstall lme4 from source
install.packages("lme4", type = "source")

# Reinstall the lme4 package
install.packages("lme4", dependencies = TRUE)

# Updating all packages to ensure compatibility
update.packages(ask = FALSE)

# None of this worked

# Load and verify packages
library(Matrix)
library(lme4)

# Check package versions
packageVersion("Matrix")
packageVersion("lme4")


install.packages("lattice")

library(ggplot2)
library(dplyr)
library(lme4) # for linear mixed-effects models
library(lattice) # for diagnostic plots

# Check Matrix version
packageVersion("Matrix")

# Check lme4 version
packageVersion("lme4")

## Index of Sppecum objects ####

XC668910_curve_5_or
XC727263_curve_5_or
XC660688_curve_5_or
XC661713_curve_5_or
XC663119_curve_5_or

## Data manipulation

# XC668910

# Converting to a dataframe

sites <- XC668910_curve_5_or$sites
richness <- XC668910_curve_5_or$richness

# Combine them into a data frame
XC668910_curve_5_df_or <- data.frame(Sites = sites, Richness = richness)

# Print the data frame
print(XC668910_curve_5_df_or)


# XC727263

sites <- XC727263_curve_5_or$sites
richness <- XC727263_curve_5_or$richness

# Combine them into a data frame
XC727263_curve_5_df_or <- data.frame(Sites = sites, Richness = richness)

# Print the data frame
print(XC727263_curve_5_df_or)


# XC660688

sites <- XC660688_curve_5_or$sites
richness <- XC660688_curve_5_or$richness

# Combine them into a data frame
XC660688_curve_5_df_or <- data.frame(Sites = sites, Richness = richness)

# Print the data frame
print(XC660688_curve_5_df_or)


# XC661713

sites <- XC661713_curve_5_or$sites
richness <- XC661713_curve_5_or$richness

# Combine them into a data frame
XC661713_curve_5_df_or <- data.frame(Sites = sites, Richness = richness)

# Print the data frame
print(XC661713_curve_5_df_or)


# XC663119

sites <- XC663119_curve_5_or$sites
richness <- XC663119_curve_5_or$richness

# Combine them into a data frame
XC663119_curve_5_df_or <- data.frame(Sites = sites, Richness = richness)

# Print the data frame
print(XC663119_curve_5_df_or)

## Combining the data frames #### 

# Combining data frames

combined_speaccum_5_or <- rbind(XC668910_curve_5_df_or, XC727263_curve_5_df_or, XC660688_curve_5_df_or, XC663119_curve_5_df_or, XC661713_curve_5_df_or)

# checking the data frame

print(combined_speaccum_5_or)

## Adding site information and cumulative values ####

# Create the categorical values vector with the specified names
categories <- rep(c("XC668910", "XC727263", "XC660688", "XC663119", "XC661713"), each = 30)

# Add the categorical column to the data frame
combined_speaccum_5_or$Category <- categories

# Calculating the cumulative species richness for each site

combined_speaccum_5_or <- combined_speaccum_5_or %>%
  group_by(Category) %>%
  arrange(Sites) %>%  # Ensure data is sorted by Sites within each Category
  mutate(Cumulative_Richness = cumsum(Richness)) %>%
  ungroup()

# Print the updated data frame
print(combined_speaccum_5_or)

# Adding a column for sample quantity

combined_speaccum_5_or <- combined_speaccum_5_or %>%
  mutate(sample_quantity = as.factor(LETTERS[ceiling(Sites / 5)]))

# Renaming columns
names(combined_speaccum_5_or)[names(combined_speaccum_5_or) == "Sites"] <- "Sample"
names(combined_speaccum_5_or)[names(combined_speaccum_5_or) == "Category"] <- "Site"

# Print the updated dataframe
print(combined_speaccum_5_or)

# Print the updated data frame
print(combined_speaccum_5_or)


## EDA #### 

# Summary

summary(combined_speaccum_5_or)


# Histogram of Richness
hist(combined_speaccum_5_or$Richness, breaks = 20, main = "Histogram of Richness", xlab = "Richness")

# Density plot of Richness
plot(density(combined_speaccum_5_or$Richness), main = "Density Plot of Richness", xlab = "Richness")

# Boxplot of Richness by Category
boxplot(Richness ~ Site, data = combined_speaccum_5_or, main = "Boxplot of Richness by Site", xlab = "Site", ylab = "Richness")

# Q-Q plot for Richness
qqnorm(combined_speaccum_5_or$Richness, main = "Q-Q Plot of Richness")
qqline(combined_speaccum_5_or$Richness, col = "orchid")

# checking residuals
model <- lm(Richness ~ Sample + Cumulative_Richness + Site + sample_quantity, data = combined_speaccum_5_or)

# Residuals of the model
residuals <- resid(model)

# Q-Q plot for residuals to check normality
qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "orchid")

# Histogram of residuals
hist(residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

# Shapiro - Wilks test for residual normalirt

# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals)

# Looks gaussian enough

# Troubleshooting

# Print the structure of the data frame
str(combined_speaccum_5_or)

# Print the first few rows of the data frame to verify
head(combined_speaccum_5_or)

combined_speaccum_5_or_df <- as.data.frame(combined_speaccum_5_or)


## Mixed effects linear regression ####

model <- lmer(sample_quantity ~ Richness + (1 | Site), data = combined_speaccum_5_or_df)

# Didn't work :(

# Model summary

summary(model)

# Check residuals

# Extract residuals
residuals <- residuals(model)

# Plot residuals
plot(residuals, main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "orchid")
