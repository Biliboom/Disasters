rm(list = ls())

# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(broom)

# Load the dataset
data <- read.csv("//Users//stewiemurithi//Documents//ACADEMIC//1UNIL//SEM 2//MacEcono//M.E. Project//Treated Data 2//MortTemp_vv3.csv")

# View the first few rows of the dataset to understand its structure
head(data)

# Convert age range columns to long format for analysis
data_long <- data %>%
  pivot_longer(cols = starts_with("Age_"),
               names_to = "age_range",
               values_to = "age_indicator") %>%
  filter(age_indicator == 1) %>%
  select(-age_indicator)

# Ensure temperature and mortality columns are numeric
data_long <- data_long %>%
  mutate(across(c(max_temp, min_temp, median_temp, diff_temp, total_mortality), as.numeric))

# Define the regression formula
# You can modify the formula to include other covariates if necessary
# formula <- total_mortality ~ max_temp + min_temp + median_temp + diff_temp + Age_0_24 + Age_25_64 + Age_65_plus
formula_2 <- total_mortality ~ median_temp + diff_temp + Age_0_24 + Year
formula_3 <- total_mortality ~ median_temp + diff_temp + Age_25_64 + Year
formula_4 <- total_mortality ~ median_temp + diff_temp + Age_65_plus + Year

# Fit the OLS regression model
# model <- lm(formula, data = data_long)
model_2 <- lm(formula_2, data = data)
model_3 <- lm(formula_3, data = data)
model_4 <- lm(formula_4, data = data)

# Summarize the model
summary(model_2)
summary(model_3)
summary(model_4)

# Output the coefficients and significance levels
# tidy(model_2)
# tidy(model_3)
# tidy(model_4)

