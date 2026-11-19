# ==============================================================================
# COMPLETED R SCRIPT: DUAL-TECHNOLOGY PANELS WITH ENLARGED FONT SIZES FOR TITLES
# ==============================================================================

# Required packages
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(patchwork)
library(arm) 
library(openxlsx)

# 1. Create output directory for final figures
output_dir <- "PV_Bayes_DualTech_Analysis"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 2. Load datasets (Columns A to M)
file_polySi <- "Kyocera, 5.4kW, poly-Si.xlsx"
file_CdTe   <- "Calyxo, 5.4kW, CdTe.xlsx"

df_polySi_raw <- read_excel(file_polySi, range = cell_limits(c(1, 1), c(NA, 13)))
df_CdTe_raw   <- read_excel(file_CdTe, range = cell_limits(c(1, 1), c(NA, 13)))

# 3. Preprocessing function in English with Wind Direction normalization (0 to 360)
preprocess_pv_data <- function(df) {
  colnames(df)[1:13] <- c("timestamp", "Active_Energy_Delivered_Receive", "Current_Phase_Averag", 
                          "Active_Power", "Performance_Ratio", "Wind_Speed", 
                          "Weather_Temperature_Celsius", "Weather_Relative_Humidity", 
                          "Global_Horizontal_Radiation", "Diffuse_Horizontal_Radiation", 
                          "Wind_Direction", "Weather_Daily_Rainfall", "Radiation_Global_Tilted")
  
  df$timestamp <- ymd_hms(df$timestamp)
  
  start_date <- ymd_hms("2014-01-01 00:00:00")
  end_date   <- ymd_hms("2017-12-25 14:05:00")
  
  df_filtered <- df %>%
    filter(timestamp >= start_date & timestamp <= end_date) %>%
    dplyr::select(-Performance_Ratio)
  
  df_filtered <- df_filtered %>% 
    filter(!is.na(Active_Power), !is.na(Global_Horizontal_Radiation), 
           !is.na(Wind_Speed), !is.na(Weather_Temperature_Celsius))
  
  # Ensure Wind Direction is normalized strictly between 0 and 360 degrees
  df_filtered$Wind_Direction <- as.numeric(df_filtered$Wind_Direction) %% 360
  
  return(df_filtered)
}

df_polySi <- preprocess_pv_data(df_polySi_raw)
df_CdTe   <- preprocess_pv_data(df_CdTe_raw)

# 4. Define Roman months labeling function with Arabic years under June / VI
roman_months_labels <- function(x) {
  d <- as.POSIXct(x, origin = "1970-01-01")
  m_num <- as.numeric(format(d, "%m"))
  yr <- format(d, "%Y")
  roms <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII")
  r_str <- roms[m_num]
  
  ifelse(m_num == 6, paste0(r_str, "\n", yr), r_str)
}

# Common x-axis definition for time series
common_x_scale <- scale_x_datetime(
  labels = roman_months_labels, 
  date_breaks = "1 month",
  limits = range(c(df_polySi$timestamp, df_CdTe$timestamp))
)

# 5. Generate Full Time-Series Two-Column Comparison Plot (Enlarged Titles)
p_polySi_ts <- ggplot(df_polySi, aes(x = timestamp, y = Active_Power)) +
  geom_line(color = "#1E90FF", linewidth = 0.25) +
  common_x_scale +
  labs(title = "Poly-Si (Kyocera)", x = "Timeline", y = "Active Power [kW]") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(size = 11, face = "bold"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )

p_CdTe_ts <- ggplot(df_CdTe, aes(x = timestamp, y = Active_Power)) +
  geom_line(color = "#1E90FF", linewidth = 0.25) +
  common_x_scale +
  labs(title = "CdTe (Calyxo)", x = "Timeline", y = "Active Power [kW]") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(size = 11, face = "bold"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )

combined_two_column_plot <- p_polySi_ts | p_CdTe_ts
ggsave(filename = file.path(output_dir, "Active_Power_FullSeries_TwoColumns.png"), 
       plot = combined_two_column_plot, width = 16, height = 5.5, dpi = 300)


# ==============================================================================
# 6. BAYESIAN REGRESSION MODELING & DUAL-TECHNOLOGY 5-ROW PANELS
# ==============================================================================

prepare_model_data <- function(df) {
  df <- df %>%
    mutate(
      GHR_sc  = as.vector(scale(Global_Horizontal_Radiation)),
      DHR_sc  = as.vector(scale(Diffuse_Horizontal_Radiation)),
      WS_sc   = as.vector(scale(Wind_Speed)),
      WD_sc   = as.vector(scale(Wind_Direction)),
      Temp_sc = as.vector(scale(Weather_Temperature_Celsius)),
      Hum_sc  = as.vector(scale(Weather_Relative_Humidity))
    )
  return(df)
}

df_polySi <- prepare_model_data(df_polySi)
df_CdTe   <- prepare_model_data(df_CdTe)

df_poly_sample <- df_polySi %>% filter(Global_Horizontal_Radiation > 5) %>% sample_n(min(5000, nrow(.)))
df_cdte_sample <- df_CdTe %>% filter(Global_Horizontal_Radiation > 5) %>% sample_n(min(5000, nrow(.)))

# --- Models for Poly-Si ---
m_rad_poly   <- bayesglm(Active_Power ~ GHR_sc + DHR_sc + I(GHR_sc * DHR_sc), data = df_poly_sample, family = gaussian())
m_wind_poly  <- bayesglm(Active_Power ~ WS_sc + WD_sc + I(WS_sc * WD_sc), data = df_poly_sample, family = gaussian())
m_therm_poly <- bayesglm(Active_Power ~ Temp_sc + Hum_sc + I(Temp_sc * Hum_sc), data = df_poly_sample, family = gaussian())

pred_rad_poly   <- predict(m_rad_poly, newdata = df_polySi)
pred_wind_poly  <- predict(m_wind_poly, newdata = df_polySi)
pred_therm_poly <- predict(m_therm_poly, newdata = df_polySi)

df_polySi$Impact_GHR        <- pred_rad_poly * df_polySi$GHR_sc
df_polySi$Impact_DHR        <- pred_rad_poly * df_polySi$DHR_sc
df_polySi$Impact_Comb_Rad   <- pred_rad_poly * (df_polySi$GHR_sc * df_polySi$DHR_sc)

# Preprocess wind impacts to be strictly positive and capped at max 10
df_polySi$Impact_WindSpeed  <- pmin(pmax(abs(pred_wind_poly * df_polySi$WS_sc), 0), 10)
df_polySi$Impact_WindDir    <- pmin(pmax(abs(pred_wind_poly * df_polySi$WD_sc), 0), 10)
df_polySi$Impact_Comb_Wind  <- pmin(pmax(abs(pred_wind_poly * (df_polySi$WS_sc * df_polySi$WD_sc)), 0), 10)

df_polySi$Impact_Temp       <- pred_therm_poly * df_polySi$Temp_sc
df_polySi$Impact_Hum        <- pred_therm_poly * df_polySi$Hum_sc
df_polySi$Impact_Comb_TH    <- pred_therm_poly * (df_polySi$Temp_sc * df_polySi$Hum_sc)

# --- Models for CdTe ---
m_rad_cdte   <- bayesglm(Active_Power ~ GHR_sc + DHR_sc + I(GHR_sc * DHR_sc), data = df_cdte_sample, family = gaussian())
m_wind_cdte  <- bayesglm(Active_Power ~ WS_sc + WD_sc + I(WS_sc * WD_sc), data = df_cdte_sample, family = gaussian())
m_therm_cdte <- bayesglm(Active_Power ~ Temp_sc + Hum_sc + I(Temp_sc * Hum_sc), data = df_cdte_sample, family = gaussian())

pred_rad_cdte   <- predict(m_rad_cdte, newdata = df_CdTe)
pred_wind_cdte  <- predict(m_wind_cdte, newdata = df_CdTe)
pred_therm_cdte <- predict(m_therm_cdte, newdata = df_CdTe)

df_CdTe$Impact_GHR        <- pred_rad_cdte * df_CdTe$GHR_sc
df_CdTe$Impact_DHR        <- pred_rad_cdte * df_CdTe$DHR_sc
df_CdTe$Impact_Comb_Rad   <- pred_rad_cdte * (df_CdTe$GHR_sc * df_CdTe$DHR_sc)

# Preprocess wind impacts to be strictly positive and capped at max 10
df_CdTe$Impact_WindSpeed  <- pmin(pmax(abs(pred_wind_cdte * df_CdTe$WS_sc), 0), 10)
df_CdTe$Impact_WindDir    <- pmin(pmax(abs(pred_wind_cdte * df_CdTe$WD_sc), 0), 10)
df_CdTe$Impact_Comb_Wind  <- pmin(pmax(abs(pred_wind_cdte * (df_CdTe$WS_sc * df_CdTe$WD_sc)), 0), 10)

df_CdTe$Impact_Temp       <- pred_therm_cdte * df_CdTe$Temp_sc
df_CdTe$Impact_Hum        <- pred_therm_cdte * df_CdTe$Hum_sc
df_CdTe$Impact_Comb_TH    <- pred_therm_cdte * (df_CdTe$Temp_sc * df_CdTe$Hum_sc)


# Helper function with significantly enlarged title and subtitle font sizes
plot_pair <- function(y_var, y_short_label, plot_title_text, color_hex, y_limits = NULL, show_subtitle = FALSE) {
  sub_poly <- if (show_subtitle) "Poly-Si (Kyocera)" else NULL
  sub_cdte <- if (show_subtitle) "CdTe (Calyxo)" else NULL
  
  p_poly <- ggplot(df_polySi, aes(x = timestamp, y = .data[[y_var]])) + 
    geom_line(color = color_hex, linewidth = 0.2) + 
    labs(title = plot_title_text, y = y_short_label, subtitle = sub_poly) + 
    theme_minimal() + common_x_scale + 
    { if (!is.null(y_limits)) coord_cartesian(ylim = y_limits) } +
    theme(
      axis.title.x = element_blank(), 
      axis.text.x = element_blank(), 
      axis.title.y = element_text(size = 9, face = "bold"),
      axis.text.y = element_text(size = 7),
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),      # Enlarged row title
      plot.subtitle = element_text(face = "bold", size = 16, hjust = 0.5)    # Enlarged technology column header
    )
  
  p_cdte <- ggplot(df_CdTe, aes(x = timestamp, y = .data[[y_var]])) + 
    geom_line(color = color_hex, linewidth = 0.2) + 
    labs(title = plot_title_text, y = y_short_label, subtitle = sub_cdte) + 
    theme_minimal() + common_x_scale + 
    { if (!is.null(y_limits)) coord_cartesian(ylim = y_limits) } +
    theme(
      axis.title.x = element_blank(), 
      axis.text.x = element_blank(), 
      axis.title.y = element_text(size = 9, face = "bold"),
      axis.text.y = element_text(size = 7),
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),      # Enlarged row title
      plot.subtitle = element_text(face = "bold", size = 16, hjust = 0.5)    # Enlarged technology column header
    )
  
  list(p_poly, p_cdte)
}

# --- 1. RADIATION PANEL ---
r1 <- plot_pair("Global_Horizontal_Radiation", "GHR", "Global Horizontal Radiation", "#1E90FF", y_limits = c(0, 1200), show_subtitle = TRUE)
r2 <- plot_pair("Diffuse_Horizontal_Radiation", "DHR", "Diffuse Horizontal Radiation", "#4169E1", show_subtitle = FALSE)
r3 <- plot_pair("Impact_GHR", "Impact GHR", "Impact of Global Horizontal Radiation on Generated Power", "#00BFFF", y_limits = c(0, 18), show_subtitle = FALSE)
r4 <- plot_pair("Impact_DHR", "Impact DHR", "Impact of Diffuse Horizontal Radiation on Generated Power", "#4682B4", y_limits = c(0, 25), show_subtitle = FALSE)
r5 <- plot_pair("Impact_Comb_Rad", "Combined GHR&DHR", "Combined Radiation Impact on Generated Power", "#104E8B", y_limits = c(0, 80), show_subtitle = FALSE)

r5[[1]] <- r5[[1]] + theme(axis.text.x = element_text(size = 7), axis.title.x = element_text(face = "bold", size = 10))
r5[[2]] <- r5[[2]] + theme(axis.text.x = element_text(size = 7), axis.title.x = element_text(face = "bold", size = 10))

radiation_panel <- (r1[[1]] | r1[[2]]) / (r2[[1]] | r2[[2]]) / (r3[[1]] | r3[[2]]) / (r4[[1]] | r4[[2]]) / (r5[[1]] | r5[[2]])
ggsave(file.path(output_dir, "Radiation_Impacts_DualTech.png"), plot = radiation_panel, width = 16, height = 18, dpi = 300)


# --- 2. WIND PANEL (Wind impacts capped at max 10) ---
w1 <- plot_pair("Wind_Speed", "Wind Speed", "Wind Speed", "#1E90FF", y_limits = c(0, 25), show_subtitle = TRUE)
w2 <- plot_pair("Wind_Direction", "Wind Dir", "Wind Direction", "#4169E1", y_limits = c(0, 360), show_subtitle = FALSE)
w3 <- plot_pair("Impact_WindSpeed", "Impact Speed", "Impact of Wind Speed on Generated Power", "#00BFFF", y_limits = c(0, 15), show_subtitle = FALSE)
w4 <- plot_pair("Impact_WindDir", "Impact Dir", "Impact of Wind Direction on Generated Power", "#4682B4", y_limits = c(0, 15), show_subtitle = FALSE)
w5 <- plot_pair("Impact_Comb_Wind", "Combined WS&WD", "Combined Wind Impact on Generated Power", "#104E8B", y_limits = c(0, 15), show_subtitle = FALSE)

w5[[1]] <- w5[[1]] + theme(axis.text.x = element_text(size = 7), axis.title.x = element_text(face = "bold", size = 10))
w5[[2]] <- w5[[2]] + theme(axis.text.x = element_text(size = 7), axis.title.x = element_text(face = "bold", size = 10))

wind_panel <- (w1[[1]] | w1[[2]]) / (w2[[1]] | w2[[2]]) / (w3[[1]] | w3[[2]]) / (w4[[1]] | w4[[2]]) / (w5[[1]] | w5[[2]])
ggsave(file.path(output_dir, "Wind_Impacts_DualTech.png"), plot = wind_panel, width = 16, height = 18, dpi = 300)


# --- 3. THERMAL PANEL ---
t1 <- plot_pair("Weather_Temperature_Celsius", "Temperature", "Weather Temperature Celsius", "#1E90FF", y_limits = c(-5, 45), show_subtitle = TRUE)
t2 <- plot_pair("Weather_Relative_Humidity", "Humidity", "Weather Relative Humidity", "#4169E1", show_subtitle = FALSE)
t3 <- plot_pair("Impact_Temp", "Impact Temp", "Impact of Temperature on Generated Power", "#00BFFF", y_limits = c(-5, 7.5), show_subtitle = FALSE)
t4 <- plot_pair("Impact_Hum", "Impact Hum", "Impact of Relative Humidity on Generated Power", "#4682B4", y_limits = c(-4, 2), show_subtitle = FALSE)
t5 <- plot_pair("Impact_Comb_TH", "Combined Temp&Hum", "Combined Thermal and Humidity Impact on Generated Power", "#104E8B", y_limits = c(-12, 4), show_subtitle = FALSE)

t5[[1]] <- t5[[1]] + theme(axis.text.x = element_text(size = 7), axis.title.x = element_text(face = "bold", size = 10))
t5[[2]] <- t5[[2]] + theme(axis.text.x = element_text(size = 7), axis.title.x = element_text(face = "bold", size = 10))

thermal_panel <- (t1[[1]] | t1[[2]]) / (t2[[1]] | t2[[2]]) / (t3[[1]] | t3[[2]]) / (t4[[1]] | t4[[2]]) / (t5[[1]] | t5[[2]])
ggsave(file.path(output_dir, "Thermal_Impacts_DualTech.png"), plot = thermal_panel, width = 16, height = 18, dpi = 300)