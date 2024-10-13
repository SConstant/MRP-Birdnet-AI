## HEADER ####
## Who: Sophie Constant
## What: MRP-Birdsong-AI- Boosted regression trees for optimal sampling 
## Last edited: 2024-08-01 format)
####

## CONTENTS ####
## 1  
## 2 Packages 
## 3 Index of species accumulation files
## 4 Boosted Regression trees
## 5 Data Split 
## 6 Training
## 7 Evaluating the model
## 8 Graph of rate of change vs sampling effort

## 2 Packages  ####

install.packages("gbm") # For boosted regression trees
install.packages("dplyr") # For data manipulation
install.packages("caret") # For training a model
install.packages("ggplot2") # For graphs


library(gbm)
library(dplyr)
library(ggplot2)
library(caret)


## 3 Index of species accumulation files #### 

specaccum_df_XC660688_5
specaccum_df_XC668910_5
specaccum_df_XC661713_5
specaccum_df_XC663119_5
specaccum_df_XC727263_5

## 4 Data Preparation #### 

# Combining data frames

combined_speaccum_5 <- rbind(specaccum_df_XC660688_5, specaccum_df_XC661713_5, specaccum_df_XC663119_5, specaccum_df_XC668910_5, specaccum_df_XC727263_5)

# checking the data frame

print(combined_speaccum_5)


## 5 Data Split ####

# Splitting into training and testing datasets for validation (check with Ed, had to fudge this slightly)
# The meaning of life

set.seed(42)

# Picking out a samples that's 80% of the data

train_indices <- sample(seq_len(nrow(combined_speaccum_5)), size = 0.8 * nrow(combined_speaccum_5))

# Creating training and test data sets

train_data <- combined_speaccum_5[train_indices, ]
test_data <- combined_speaccum_5 [- train_indices,]

print(train_data)

# Checking the dimesions to ensure the splits are as expected (training set is 80%, and testing set is 20%)

# Original number of rows for the combined dataset is 150

print(dim(train_data))  # Should show 80% of the original data
print(dim(test_data))   # Should show 20% of the original data



## 6 Training #### 

brt_model <- gbm(
  formula = Richness ~ Sites + SD, # Response variable is Richness, as a function of number of samples (Sites) and SD. HAve exccluded uppe and lower Ci as these were for graphs
  data = train_data, # Popping in training data
  distribution = "gaussian", # Leaving as gaussian, measuring quadratic loss/mse (squared difference between predicted and actual)
  n.trees = 1500, # Originally 5000 but was way too much, tried 2000, was also quite a lot, saw overfitting (increases)
  interaction.depth = 3, 
  shrinkage = 0.01,
  cv.folds = 5,
  n.minobsinnode = 10,
  verbose = FALSE
)

# Check the function outputs

print(brt_model)


# Here's the best number of trees based on cross-validation

best_trees <- gbm.perf(brt_model, method = "cv")
title(main = "Performance of Boosted Regression Trees (Cross Validation Method)")
abline(v = best_trees, col = "blue", lwd = 2, lty = 2)
text(x = best_trees, y = min(par("usr")[3:4]), labels = paste("Best Trees:", best_trees), pos = 3, offset = 1.75, col = "blue")
legend("topright", legend = "Cross-validation error", col = "green", lty = 1) # ask how I can add training error


print(best_trees)

# Green line - squared error loss on cross validation set
# Black line is squared error loss on training set
# Blue line - optimal number of trees based on method (cross validation)


# Preditions on test data

predictions <- predict(brt_model, newdata = test_data, n.trees = best_trees)

print(predictions)

## 7 Evaluating the model ####

# Using root mean squared error to evaluate the accuracy of the model

rmse <- sqrt(mean((test_data$Richness - predictions)^2))
print(paste("RMSE on test data: ", rmse))


## 8 Graph of rate of change vs Sampling effort ####

# Create a sequence of sampling efforts to test - fixed with seperating out the making a new data frame function below
sampling_effort <- seq(min(combined_speaccum_5$Sites), max(combined_speaccum_5$Sites), length.out = 100)

# Making sampling effort a data frame

sampling_effort_new <- data.frame(Sites = sampling_effort,
                       SD = mean(combined_speaccum_5$SD), 
                       lower_ci = mean(combined_speaccum_5$lower_ci), 
                       upper_ci = mean(combined_speaccum_5$upper_ci))

# Checking the new data frame

print(sampling_effort_new)


# Predict the response variable for these sampling efforts
predictions <- predict(brt_model, newdata = sampling_effort_new, n.trees = best_trees)


# Create a data frame with the results
results <- data.frame(
  SamplingEffort = sampling_effort,
  PredictedResponse = predictions
)

# Calculate the rate of change (e.g., difference between predictions)
# Calculate the rate of change (slope) by differentiating predictions with respect to sampling effort
results$RateOfChange <- c(NA, diff(results$PredictedResponse) / diff(results$SamplingEffort))

# Plot the results
plot(results$SamplingEffort, results$PredictedResponse, type = "l", xlab = "Sampling Effort", ylab = "Predicted Response", main = "Rate of Change vs Sampling Effort")


# ggplot for a prettier graph

ggplot(data = results, aes(x = SamplingEffort, y = PredictedResponse, color = "Predicted Response")) +
  geom_line(size = 1) + # Added the line
  labs(title = "Rate of Change vs Sampling Effort", x = "Sampling Effort", y = "Predicted Response", color = "Legend") + # Added labels and legend title
  theme_minimal() + # Picked a clean theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Format title
    axis.title.x = element_text(size = 11), # Increase x-axis label size
    axis.title.y = element_text(size = 11) # Increase y-axis label size
  ) +
  scale_color_manual(values = c("Predicted Response" = "orchid")) # Specified the color for the legend 

