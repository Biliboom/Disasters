rm(list = ls())

# Load libraries
library(forecast)
library(TSA)
library(tseries)

# Load your dataset
data <- read.csv("//Users//stewiemurithi//Documents//ACADEMIC//1UNIL//SEM 2//MacEcono//M.E. Project//Treated Data 2//MortTemp_vv3.csv")

# Convert to a time series object for mortality
data_ts <- ts(data$total_mortality, start=c(1876), frequency=1)

# Create interaction terms for temperature variables with age ranges
data$max_temp_0_24 <- data$max_temp * data$Age_0_24
data$max_temp_25_64 <- data$max_temp * data$Age_25_64
data$max_temp_65_plus <- data$max_temp * data$Age_65_plus

data$min_temp_0_24 <- data$min_temp * data$Age_0_24
data$min_temp_25_64 <- data$min_temp * data$Age_25_64
data$min_temp_65_plus <- data$min_temp * data$Age_65_plus

data$median_temp_0_24 <- data$median_temp * data$Age_0_24
data$median_temp_25_64 <- data$median_temp * data$Age_25_64
data$median_temp_65_plus <- data$median_temp * data$Age_65_plus

data$diff_temp_0_24 <- data$diff_temp * data$Age_0_24
data$diff_temp_25_64 <- data$diff_temp * data$Age_25_64
data$diff_temp_65_plus <- data$diff_temp * data$Age_65_plus

# Check for stationarity
adf_test <- adf.test(data_ts)

# Difference the series if necessary
if(adf_test$p.value > 0.05) {
  data_ts <- diff(data_ts)
}

# Define the exogenous variables with interaction terms
exog_data <- data.frame(
  max_temp_0_24 = data$max_temp_0_24, 
  max_temp_25_64 = data$max_temp_25_64, 
  max_temp_65_plus = data$max_temp_65_plus,
  min_temp_0_24 = data$min_temp_0_24,
  min_temp_25_64 = data$min_temp_25_64,
  min_temp_65_plus = data$min_temp_65_plus,
  median_temp_0_24 = data$median_temp_0_24,
  median_temp_25_64 = data$median_temp_25_64,
  median_temp_65_plus = data$median_temp_65_plus,
  diff_temp_0_24 = data$diff_temp_0_24,
  diff_temp_25_64 = data$diff_temp_25_64,
  diff_temp_65_plus = data$diff_temp_65_plus
)

# Convert exogenous data frame to matrix
exog_matrix <- as.matrix(exog_data)

# Compute the correlation matrix for the exogenous variables
cor_matrix <- cor(exog_matrix)
print(cor_matrix)

# Suppose we choose to remove `max_temp_0_24` and `median_temp_0_24` for demonstration purposes
# exog_data_reduced <- exog_data[, !names(exog_data) %in% c("max_temp_0_24", "diff_temp_0_24")]

# Fit ARMAX model
fit_armax <- auto.arima(data_ts, xreg=exog_matrix, seasonal=FALSE)

# Diagnose the model
tsdisplay(residuals(fit_armax), main="ARMAX Model Residuals")
Box.test(residuals(fit_armax), type="Ljung-Box")
summary(fit_armax)
