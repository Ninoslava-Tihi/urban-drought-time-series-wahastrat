# ---- Packages ----
# Required packages for holdout validation, forecasting, and table export.
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(stringr)
library(forecast)
library(tidyr)
library(knitr)
library(writexl)
library(tibble)
library(readxl)

# ---- Load data ----
# Load the aggregated monthly climatic dataset (2014–2020).
data_climate=read_excel("monthly-data.xlsx")


# ---- Accuracy metrics ----
# Define RMSE, MAE, and MAPE to evaluate out-of-sample forecast accuracy.
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
mae  <- function(actual, pred) mean(abs(actual - pred), na.rm = TRUE)

mape <- function(actual, pred) {
  idx <- which(!is.na(actual) & actual != 0)
  if (length(idx) == 0) return(NA_real_)
  mean(abs((actual[idx] - pred[idx]) / actual[idx]), na.rm = TRUE) * 100
}

# ---- Holdout evaluation ----
# Fit models on the first 80% of observations and evaluate on the remaining 20%
# while preserving temporal ordering.

holdout_eval <- function(x, freq = 12, train_frac = 0.8, method = c("sarima","hw")) {
  method <- match.arg(method)
  n <- length(x)
  n_train <- floor(train_frac * n)
  train <- x[1:n_train]
  test  <- x[(n_train + 1):n]
  h <- length(test)
  
  
  ts_train <- ts(train, frequency = freq)
  
  if (method == "sarima") {
    fit <- auto.arima(ts_train, seasonal = TRUE)
    fc  <- forecast(fit, h = h)$mean
  } else { # Holt–Winters (ETS)
    fit <- ets(ts_train)  
    fc  <- forecast(fit, h = h)$mean
  }
  
  list(
    rmse = rmse(test, as.numeric(fc)),
    mae  = mae(test,  as.numeric(fc)),
    mape = mape(test, as.numeric(fc)),
    fit  = fit,
    fc   = as.numeric(fc),
    test = test
  )
}

# ---- Compute metrics for all variables ----
# Apply both (S)ARIMA and Holt–Winters (ETS) models to each climatic driver and
# export results to reproduce Tables 2–3 in the manuscript.
vars <- c("Temperature","Precipitation","SoilMoisture","WindSpeed")

results <- lapply(vars, function(v) {
  
  x <- data_climate[[v]]
  
  sar <- holdout_eval(x, method = "sarima")
  hw  <- holdout_eval(x, method = "hw")
  
  tibble(
    ClimaticVariable = v,
    SARIMA_RMSE = sar$rmse,
    SARIMA_MAE  = sar$mae,
    SARIMA_MAPE = sar$mape,
    HW_RMSE     = hw$rmse,
    HW_MAE      = hw$mae,
    HW_MAPE     = hw$mape
  )
}) %>% bind_rows()

table2 <- results %>%
  transmute(
    ClimaticVariable,
    RMSE = round(SARIMA_RMSE, 4),
    MAE  = round(SARIMA_MAE,  4),
    MAPE = round(SARIMA_MAPE, 2)
  )

table3 <- results %>%
  transmute(
    ClimaticVariable,
    RMSE = round(HW_RMSE, 4),
    MAE  = round(HW_MAE,  4),
    MAPE = round(HW_MAPE, 2)
  )


kable(table2, caption = "Table 2. Forecasting accuracy of (S)ARIMA under holdout validation (80/20).")
kable(table3, caption = "Table 3. Forecasting accuracy of Holt–Winters (ETS) under holdout validation (80/20).")



# ---- Export results ----
# Save Tables 2–3 as CSV/Excel for reporting and manuscript integration.
write_xlsx(
  list(
    "SARIMA_Results" = table2,
    "HoltWinters_Results" = table3
  ),
  path = "Results_Tables.xlsx"
)

write.csv(table2, "Table2_SARIMA_Holdout.csv", row.names = FALSE)
write.csv(table3, "Table3_HoltWinters_Holdout.csv", row.names = FALSE)





