

# =========================================================
# Figures.R
# Generates Figure 1 (monthly time series), Figure 2 (climatology),
# and Figure 3 (holdout forecast vs observed with prediction intervals).
# Input: monthly-data.xlsx (columns: Year, Month, Temperature, Precipitation,
#        SoilMoisture, WindSpeed)
# Output: PNG files saved
# =========================================================

# ---- Packages (install once manually if missing) ----
library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(tibble)
library(patchwork)
library(forecast)


# ---- Load data ----
data_climate<-read_excel("monthly-data.xlsx")
view(data_climate)

# ---- Build a continuous time index (Year–Month) ----
# Month may be numeric (1–12) or text (e.g., "January").
data<- data_climate %>%
  mutate(
    Year = as.integer(Year),
    Month = case_when(
      suppressWarnings(!is.na(as.integer(Month))) ~ as.integer(Month),
      TRUE ~ match(str_to_title(substr(as.character(Month), 1, 3)), month.abb)
    ),
    Date = as.Date(sprintf("%04d-%02d-01", Year, Month))
  ) %>%
  arrange(Date)



# =========================================================
# Figure 1: Monthly aggregated time series (2014–2020)
# =========================================================

# (a) Temperature
p1_ts <- ggplot(data, aes(x = Date, y = Temperature)) +
  geom_line(color = "darkred", linewidth = 1) +
  labs(title = "(a) Air Temperature",
       x = "Time",
       y = "Monthly Mean (°C)") +
  theme_minimal()

# (b) Precipitation
p2_ts <- ggplot(data, aes(x = Date, y = Precipitation)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "(b) Precipitation",
       x = "Time",
       y = "Monthly Total (mm)") +
  theme_minimal()

# (c) Soil Moisture
p3_ts <- ggplot(data, aes(x = Date, y = SoilMoisture)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  labs(title = "(c) Soil Moisture",
       x = "Time",
       y = "Monthly Mean (%)") +
  theme_minimal()

# (d) Wind Speed
p4_ts <- ggplot(data, aes(x = Date, y = WindSpeed)) +
  geom_line(color = "purple", linewidth = 1) +
  labs(title = "(d) Wind Speed",
       x = "Time",
       y = "Monthly Mean (m/s)") +
  theme_minimal()

# Create 4 panels
figure_time <- (p1_ts | p2_ts) / (p3_ts | p4_ts)

figure_time


ggsave("Figure1_TimeSeries_2014_2020.png",
       figure_time, width = 11, height = 5.8, dpi = 300)

# =========================================================
# Figure 2: Climatological monthly averages (2014–2020)
# Note: precipitation is already monthly totals in your dataset;
# here we compute the mean of monthly totals across years.
# =========================================================

# Climatological average by month (2014–2020)

climatology <- data %>%
  group_by(Month) %>%
  summarise(
    temp_mean  = mean(Temperature, na.rm = TRUE),
    precip_mean_total = mean(Precipitation, na.rm = TRUE),  # prosjek mjesečnih suma
    soil_mean  = mean(SoilMoisture, na.rm = TRUE),
    wind_mean  = mean(WindSpeed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    MonthLab = factor(month.abb[Month],
                           levels = month.abb,
                           ordered = TRUE))

# (a) Temperature
p1_clim <- ggplot(climatology, aes(x = MonthLab, y = temp_mean, group = 1)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred") +
  labs(title = "(a) Air Temperature",
       x = "Month",
       y = "Mean Temperature (°C)") +
  theme_minimal()

# (b) Precipitation
p2_clim <- ggplot(climatology, aes(x = MonthLab, y = precip_mean_total, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  labs(title = "(b) Precipitation",
       x = "Month",
       y = "Mean Monthly Total (mm)") +
  theme_minimal()

# (c) Soil Moisture
p3_clim <- ggplot(climatology, aes(x = MonthLab, y = soil_mean, group = 1)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen") +
  labs(title = "(c) Soil Moisture",
       x = "Month",
       y = "Mean Soil Moisture (%)") +
  theme_minimal()

# (d) Wind Speed
p4_clim <- ggplot(climatology, aes(x = MonthLab, y = wind_mean, group = 1)) +
  geom_line(color = "purple", linewidth = 1) +
  geom_point(color = "purple") +
  labs(title = "(d) Wind Speed",
       x = "Month",
       y = "Mean Wind Speed (m/s)") +
  theme_minimal()

# Create 4 panels
figure2 <- (p1_clim | p2_clim) / (p3_clim | p4_clim)

figure2


ggsave("Figure2_ClimatologicalMonthly_Averages.png",
       figure2, width = 11, height = 5.8, dpi = 300)



# =========================================================
# Figure 3: Observed vs forecasted air temperature (holdout 80/20)
# Holt–Winters implemented via ETS; show 80% & 95% prediction intervals
# =========================================================

# Assumption that data_climate already exist as a tibble with columns:
# Year, Month, Temperature, ...

x <- data$Temperature
dates <- data$Date

n <- length(x)
n_train <- floor(0.8 * n)
h <- n - n_train

ts_train <- ts(x[1:n_train], frequency = 12)
fit_hw <- ets(ts_train)
fc_hw  <- forecast(fit_hw, h = h, level = c(80, 95))

df_full <- tibble(
  time_index = dates,
  Observed   = x
)

df_fc <- tibble(
  time_index = dates[(n_train + 1):n],
  Forecast   = as.numeric(fc_hw$mean),
  Lo80 = as.numeric(fc_hw$lower[, "80%"]),
  Hi80 = as.numeric(fc_hw$upper[, "80%"]),
  Lo95 = as.numeric(fc_hw$lower[, "95%"]),
  Hi95 = as.numeric(fc_hw$upper[, "95%"])
)

split_date <- dates[n_train]

fig3 <- ggplot() +
  # observed (whole time series)
  geom_line(data = df_full, aes(x = time_index, y = Observed), color = "black", linewidth = 0.8) +
  # prediction intervals (only test period)
  geom_ribbon(data = df_fc, aes(x = time_index, ymin = Lo95, ymax = Hi95),
              fill = "steelblue", alpha = 0.15) +
  geom_ribbon(data = df_fc, aes(x = time_index, ymin = Lo80, ymax = Hi80),
              fill = "steelblue", alpha = 0.25) +
  # forecast mean (test period)
  geom_line(data = df_fc, aes(x = time_index, y = Forecast),
            color = "steelblue", linewidth = 0.9, linetype = "dashed") +
  # vertical line for split
  geom_vline(xintercept = split_date, linetype = "dotdash", color = "gray40") +
  annotate("text", x = split_date, y = max(df_full$Observed, na.rm = TRUE),
           label = "Train/Test split", vjust = -0.6, hjust = -0.05, size = 3.2, color = "gray30") +
  labs(
    title = "Observed and Forecasted Temperature (Holdout Validation)",
    x = "Time",
    y = "Temperature (monthly mean)",
    caption = "Dashed line: Holt–Winters forecast; shaded areas: 80% and 95% prediction intervals."
  ) +
  theme_minimal(base_size = 12)

fig3

ggsave("Figure3_Temperature_Holdout.png",
       fig3, width = 11, height = 5.8, dpi = 300)





