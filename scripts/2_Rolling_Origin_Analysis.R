library(readxl)
library(dplyr)
library(stringr)
library(forecast)
library(ggplot2)
library(tibble)


# =========================================================
# Rolling-Origin Cross-Validation (Expanding Window)
# Purpose: Evaluate (S)ARIMA and Holt–Winters (ETS) forecasting performance
#          using walk-forward validation on monthly aggregated climate series.
# Output:  Fold-level metrics and aggregated mean ± SD (Table 4).
# =========================================================

# -----------------------------
# 1) Load and prepare monthly aggregated data
#    - Convert Year to integer
#    - Map Month (text) to numeric month index
#    - Create a continuous monthly Date index (first day of each month)
# -----------------------------
# -----------------------------
data_climate <- read_excel("monthly-data.xlsx") %>%
  mutate(
    Year = as.integer(Year),
    MonthNum = match(str_to_title(substr(as.character(Month), 1, 3)), month.abb),
    time_index = as.Date(sprintf("%04d-%02d-01", Year, MonthNum))
  ) %>%
  arrange(time_index)

# -----------------------------
# 2) Define forecast accuracy metrics
#    Notes:
#    - MAPE is computed only for non-zero actual values to avoid division by zero
# -----------------------------
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
mae  <- function(actual, pred) mean(abs(actual - pred), na.rm = TRUE)

mape <- function(actual, pred) {
  idx <- which(!is.na(actual) & actual != 0)
  if (length(idx) == 0) return(NA_real_)
  mean(abs((actual[idx] - pred[idx]) / actual[idx]), na.rm = TRUE) * 100
}

# -----------------------------
# 3) Rolling-origin cross-validation (walk-forward / expanding window)
#    Parameters:
#    - initial: initial training window length (in months)
#    - h: forecast horizon (h = 1 corresponds to one-month-ahead forecasting)
#    - step: origin shift (step = 1 evaluates every month)
rolling_cv <- function(x, freq = 12, initial = 36, h = 1, step = 1,
                       method = c("sarima", "hw")) {
  
  method <- match.arg(method)
  n <- length(x)
  origins <- seq(initial, n - h, by = step)
  
  out <- lapply(origins, function(train_end) {
    
    train <- x[1:train_end]
    test  <- x[(train_end + 1):(train_end + h)]
    ts_train <- ts(train, frequency = freq)
    
    if (method == "sarima") {
      fit <- auto.arima(ts_train, seasonal = TRUE)
    } else {
      fit <- ets(ts_train)   # Holt–Winters via ETS formulation
    }
    # One-step-ahead forecast (or h-step if h > 1)
    fc <- forecast(fit, h = h)$mean
    pred <- as.numeric(fc)
    
    tibble(
      train_end = train_end,
      rmse = rmse(test, pred),
      mae  = mae(test, pred),
      mape = mape(test, pred)
    )
  })
  
  bind_rows(out)
}


# -----------------------------
# 4) Run rolling-origin CV for all climatic variables and both models
#    Recommended settings:
#    - initial = 36 months (3 years)
#    - h = 1 (one-month-ahead)
#    - step = 1 (evaluate each month)
# -----------------------------
vars <- c("Temperature", "Precipitation", "SoilMoisture", "WindSpeed")

cv_fold_results <- lapply(vars, function(v) {
  x <- data_climate[[v]]
  
  cv_sar <- rolling_cv(x, method = "sarima", initial = 36, h = 1, step = 1) %>%
    mutate(ClimaticVariable = v, Model = "(S)ARIMA")
  
  cv_hw  <- rolling_cv(x, method = "hw", initial = 36, h = 1, step = 1) %>%
    mutate(ClimaticVariable = v, Model = "Holt–Winters")
  
  bind_rows(cv_sar, cv_hw)
}) %>% bind_rows()

# -----------------------------
# 5) Aggregate fold-level results (mean ± SD) for reporting (Table 4)
# -----------------------------
cv_summary <- cv_fold_results %>%
  group_by(ClimaticVariable, Model) %>%
  summarise(
    Mean_RMSE = mean(rmse, na.rm = TRUE),
    SD_RMSE   = sd(rmse, na.rm = TRUE),
    Mean_MAE  = mean(mae, na.rm = TRUE),
    SD_MAE    = sd(mae, na.rm = TRUE),
    Mean_MAPE = mean(mape, na.rm = TRUE),
    SD_MAPE   = sd(mape, na.rm = TRUE),
    Folds     = n(),
    .groups = "drop"
  ) %>%
  arrange(ClimaticVariable, Model)

print(cv_summary)
# -----------------------------
# 6) Export results (CSV + Excel)
#    - FoldResults: fold-level error metrics
#    - Summary_Table4: mean ± SD aggregated across folds
# -----------------------------
write.csv(cv_fold_results, "RollingCV_FoldResults.csv", row.names = FALSE)
write.csv(cv_summary, "RollingCV_Summary_Table4.csv", row.names = FALSE)

write_xlsx(
  list(
    "FoldResults" = cv_fold_results,
    "Summary_Table4" = cv_summary
  ),
  path = "RollingCV_Results.xlsx"
)


