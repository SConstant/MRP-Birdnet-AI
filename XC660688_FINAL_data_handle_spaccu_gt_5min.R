## HEADER ####
## Who: Sophie Constant
## What: MRP-Birdsong-AI-XC660688
## Last edited: 2024-07-23 format)
####

## CONTENTS ####
## 1 Packages 
## 2 Index of files 
## 3 Data Handling  Preamble 
## 4 Data Handling XC660688
## 5 Data Handling XC660688
##  Species Accumulation curve

## 1 Packages  ####

install.packages("readxl")

# For data manipulation

install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")


# For species accumulation curves

install.packages("knitr")
install.packages("picante")
install.packages("vegan") # Package with the species accumulation curve sunctions

# For pretty graphs

install.packages("ggplot2") # Pretty graphs
install.packages("patchwork") # Combining graphs on a sheet
install.packages("RColorBrewer")
install.packages("paletteer")


library("readxl")
library(dplyr) 
library(tidyr)
library(picante)
library(knitr)
library(vegan)

# For Pretty graphs

library(ggplot2)
library(patchwork)
library(tidyverse)
library(RColorBrewer)
library(paletteer)



## 2 XC660688 ####

## 2.1 Data tidy, converting the BirdNet outputs for the samples with 5 minute durations to a species accumulation curve. 

setwd("D:/Bird Project/birdsong_output/XC660688_birdnet_output/XC660688_5_birdnet_output")

# Create a list object to store data frames

data_list <- list()

# Reading in data for XC660688 5 minute clips

file_name <- paste0("XC660688 - Soundscape_clip_1_30_clip_", i, "_5_analysis.xlsx")

for (i in 1:30) {
  file_name <- paste0("XC660688 - Soundscape_clip_1_30_clip_", i, "_5_analysis.xlsx")
  data_list[[i]] <- read_excel(file_name)
}

# Printing the head of the first data frame to check
head(data_list[[1]])

# Sorting into counts

count_list <- list()



S3 <- list()

for (i in 1:30) {
  count_list[[i]] <- data_list[[i]] %>% count(common_name, sort = TRUE)
  S3[[i]] <- data_list[[i]] %>% count(common_name, sort = TRUE)
}


# Print count_list

print(count_list)


# Pivoting to wide data and adding a column to depict the individual 5 minute samples

wide_list <- list()


for (i in 1:30) {
  wide_list[[i]] <- pivot_wider(count_list[[i]], names_from = common_name, values_from = n)
}

print(wide_list[[2]])


# Bind the wide data frames into one
XC660688_5_wide_bird_matrix <- bind_rows(wide_list)

# Replacing NAs with 0

XC660688_5_wide_bird_matrix [is.na(XC660688_5_wide_bird_matrix)] <- 0

# Print to check

print(XC660688_5_wide_bird_matrix)


## 2.3 Species Accumulation Curve Cummulative

XC660688_curve_5 <- specaccum(XC660688_5_wide_bird_matrix, method = "random", permutations = 1000)

XC660688_curve_5


# For observed richness

XC660688_curve_5_or <- specaccum(XC660688_5_wide_bird_matrix, method = "collector", permutations = 1000)

XC660688_curve_5_or

plot(XC660688_curve_5_or, ci.type = "poly", col = "orchid", ci.col = "lavender", 
     lwd = 2, ci.lty = 0, xlab = "5 minute intervals",
     ylab = "Cumulative number of Bird Species")


# Plot

plot(XC660688_curve_5, ci.type = "poly", col = "orchid", ci.col = "lavender", 
     lwd = 2, ci.lty = 0, xlab = "5 minute intervals",
     ylab = "Cumulative number of Bird Species")

# ggplot

# Checking the object class
class(XC660688_curve_5)

# It is a specaccum object, so converting to a datadframe

specaccum_df_XC660688_5 <- data.frame(
  Sites = XC660688_curve_5$sites,
  Richness = XC660688_curve_5$richness,
  SD = XC660688_curve_5$sd
)

# Printing the dataframe: 

print(specaccum_df_XC660688_5)

# Calculating confidence intervals

specaccum_df_XC660688_5$lower_ci <- specaccum_df_XC660688_5$Richness - 1.96 * specaccum_df_XC660688_5$SD
specaccum_df_XC660688_5$upper_ci <- specaccum_df_XC660688_5$Richness + 1.96 * specaccum_df_XC660688_5$SD

# Making a prettier plot with ggplot

ggplot(specaccum_df_XC660688_5, aes(x = Sites, y = Richness)) +
  geom_line(aes(color = "Richness"), linewidth = 1.5) + # Line plot with legend
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = "Confidence Interval"), alpha = 0.5) + # Confidence interval ribbon with legend
  labs(
    x = "Number of Sites", 
    y = "Cumulative number of Species", 
    color = "Legend", 
    fill = "Legend"
  ) + # Axis labels and legend titles
  ggtitle("Species Accumulation Curve Based on Number of 5 Minute Samples for Site XC660688") + # Plot title
  scale_color_manual(values = "orchid") + # Manually setting the color for the line
  scale_fill_manual(values = "lavender") + # Manually setting the fill color for the ribbon
  theme_minimal() + # Minimal theme
  theme(plot.title = element_text(hjust = 0.5) # Center the title
  )

## 3 Heat map visualisation for presence and absence of species across the samples, and the species densities ####

print(XC660688_5_wide_bird_matrix)

# Convert the tibble to a data frame
XC660688_5_wide_bird_matrix_df <- as.data.frame(XC660688_5_wide_bird_matrix)

# Add observation identifiers
XC660688_5_wide_bird_matrix_df <- XC660688_5_wide_bird_matrix_df %>%
  mutate(Observation = paste("Sample", 1:nrow(XC660688_5_wide_bird_matrix_df), sep = " "))

# Convert the dataframe to long format
XC660688_5_long_bird_matrix_df <- XC660688_5_wide_bird_matrix_df %>%
  pivot_longer(cols = -Observation, names_to = "Species", values_to = "Density")

# Replace periods with spaces in species names
XC660688_5_long_bird_matrix_df$Species <- gsub("\\.", " ", XC660688_5_long_bird_matrix_df$Species)

# Reorder the Observation factor to ensure correct order
XC660688_5_long_bird_matrix_df$Observation <- factor(XC660688_5_long_bird_matrix_df$Observation, levels = unique(XC660688_5_long_bird_matrix_df$Observation))

# Choose a color palette from RColorBrewer
palette <- brewer.pal(n = 9, name = "PuBuGn")

# Create the heatmap using ggplot2
ggplot(XC660688_5_long_bird_matrix_df, aes(x = Species, y = Observation, fill = Density)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = palette) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "Species Density Heatmap for XC660688 ", x = "Species", y = "Sample", fill = "Density")


## 4 Presence - absence matrices for ground truthing ####

# Groundtruthing samples

# change directory 

setwd("D:/Bird Project")

# Read in excel for species list from xeno canto

gt_df <- read_excel("MRP_groundtruth_v0.2.xlsx")

print(gt_df)

# Filter rows for XC660688

# Filter rows where site is "XC660688" and create a new data frame
XC660688_gt_df <- gt_df %>%
  filter(Reference == "XC660688")

print(XC660688_gt_df)

print(XC660688_5_long_bird_matrix_df)


# Converting the data frames to presence - absence matrices

# species density matrix

# Create species density presence-absence matrix for species_density_df
presence_absence_density <- XC660688_5_long_bird_matrix_df %>%
  mutate(Presence = ifelse(Density > 0, 1, 0)) %>%
  select(Species, Presence)

# Create species reference presence-absence matrix for XC660688_gt_df
presence_absence_reference <- XC660688_gt_df %>%
  mutate(Presence = 1) %>%
  select(Species = `Common name`, Presence)

# Merge the presence-absence matrices
presence_absence_combined <- full_join(presence_absence_density, presence_absence_reference, by = "Species", suffix = c("_Density", "_Reference"))

# Replace NAs with 0
presence_absence_combined[is.na(presence_absence_combined)] <- 0

# Print the combined presence-absence matrix
print(presence_absence_combined)

# Making the function for the Jaccard Index
jaccard_index <- function(x, y) {
  intersection <- sum(x & y)
  union <- sum(x | y)
  jaccard <- intersection / union
  return(jaccard)
}

# Calculate Jaccard Index for presence-absence matrices
jaccard_similarity <- jaccard_index(
  presence_absence_combined$Presence_Density, 
  presence_absence_combined$Presence_Reference
)

# Print the Jaccard similarity
print(paste("Jaccard Similarity Index:", jaccard_similarity))


## 5 Heat Map visualisation Presence - Absence matrix ####

# Convert the data frame to long format
presence_absence_combined_long <- presence_absence_combined %>%
  pivot_longer(cols = starts_with("Presence"), names_to = "Source", values_to = "Presence")

print(presence_absence_combined_long)

# Replace underscores with spaces in Source names
presence_absence_combined_long$Source <- gsub("_", " ", presence_absence_combined_long$Source)

# Create the heatmap
ggplot(presence_absence_combined_long, aes(x = Source, y = Species, fill = factor(Presence))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "lavender", "1" = "orchid"), 
                    name = "Presence",
                    labels = c("Absent", "Present")) +
  scale_x_discrete(labels = c("Presence Density" = "BirdNet Derived", 
                              "Presence Reference" = "Human Derived")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "Presence-Absence Heatmap for XC660688", x = "Source", y = "Species")

## Organised density heatmap ####

# Convert the tibble to a data frame
XC660688_5_wide_bird_matrix_df <- as.data.frame(XC660688_5_wide_bird_matrix)

# Add observation identifiers
XC660688_5_wide_bird_matrix_df <- XC660688_5_wide_bird_matrix_df %>%
  mutate(Observation = paste("Sample", 1:nrow(XC660688_5_wide_bird_matrix_df), sep = " "))

# Convert the dataframe to long format
XC660688_5_long_bird_matrix_df <- XC660688_5_wide_bird_matrix_df %>%
  pivot_longer(cols = -Observation, names_to = "Species", values_to = "Density")

# Replace periods with spaces in species names
XC660688_5_long_bird_matrix_df$Species <- gsub("\\.", " ", XC660688_5_long_bird_matrix_df$Species)

# Aggregate density values to calculate the total density per species
species_density <- XC660688_5_long_bird_matrix_df %>%
  group_by(Species) %>%
  summarise(Total_Density = sum(Density, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Total_Density))

# Reorder the Species factor based on Total_Density
XC660688_5_long_bird_matrix_df$Species <- factor(XC660688_5_long_bird_matrix_df$Species,
                                                 levels = species_density$Species)

# Reorder the Observation factor to ensure it remains in the original order
XC660688_5_long_bird_matrix_df$Observation <- factor(XC660688_5_long_bird_matrix_df$Observation,
                                                     levels = unique(XC660688_5_long_bird_matrix_df$Observation))

# Choose a color palette from RColorBrewer
palette <- brewer.pal(n = 9, name = "PuBuGn")

# Create the heatmap using ggplot2
ggplot(XC660688_5_long_bird_matrix_df, aes(x = Species, y = Observation, fill = Density)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = palette) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "Species Density Heatmap for XC660688", x = "Species", y = "Sample", fill = "Density")