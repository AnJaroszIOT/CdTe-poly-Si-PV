# =====================================================================
# Skrypt: Modelowanie i porównanie wpływów pogodowych (Wszystko w odcieniach niebieskiego)
# =====================================================================

library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(patchwork)

# 1. Wczytywanie i przygotowanie danych
output_dir <- file.path(getwd(), "PV_Models_Output")
if (!dir.exists(output_dir)) dir.create(output_dir)

ds <- list(file = "Calyxo, 5.4kW, CdTe.xlsx", title = "CdTe (Calyxo)", prefix = "cdte")
raw_df <- read_excel(ds$file, col_names = FALSE)
headers <- as.character(raw_df[1, ])
df <- raw_df[-1, ]
colnames(df) <- gsub(" ", "_", headers)

# Funkcja pomocnicza do bezpiecznego wyciągania kolumn
find_col <- function(data, patterns) {
  cols <- colnames(data)
  for (p in patterns) {
    match_idx <- grep(p, cols, ignore.case = TRUE)
    if (length(match_idx) > 0) return(cols[match_idx[1]])
  }
  return(NULL)
}

time_col <- find_col(df, c("timestamp", "time", "data"))
power_col <- find_col(df, c("Active_Power", "Power", "AC"))
ghr_col <- find_col(df, c("Global_Horizontal_Radiation", "GHR", "Radiation"))
dhr_col <- find_col(df, c("Diffuse_Horizontal_Radiation", "DHR", "Diffuse"))
ws_col <- find_col(df, c("Wind_Speed", "Wind", "Speed"))
wd_col <- find_col(df, c("Wind_Direction", "Direction"))
temp_col <- find_col(df, c("Temperature", "Temp", "Ambient"))
hum_col <- find_col(df, c("Humidity", "Rel_Humidity", "RH"))

# Czyszczenie i konwersja danych
df_clean <- df %>%
  mutate(
    Time = ymd_hms(.data[[time_col]]),
    Active_Power = suppressWarnings(as.numeric(.data[[power_col]])),
    GHR = if (!is.null(ghr_col)) suppressWarnings(as.numeric(.data[[ghr_col]])) else 0,
    DHR = if (!is.null(dhr_col)) suppressWarnings(as.numeric(.data[[dhr_col]])) else 0,
    Wind_Speed = if (!is.null(ws_col)) suppressWarnings(as.numeric(.data[[ws_col]])) else 0,
    Wind_Direction = if (!is.null(wd_col)) suppressWarnings(as.numeric(.data[[wd_col]])) else 0,
    Temperature = if (!is.null(temp_col)) suppressWarnings(as.numeric(.data[[temp_col]])) else 0,
    Humidity = if (!is.null(hum_col)) suppressWarnings(as.numeric(.data[[hum_col]])) else 0
  ) %>%
  filter(!is.na(Time), !is.na(Active_Power)) %>%
  arrange(Time) %>%
  slice_head(n = 1000)

cat("\n[1/4] Dopasowywanie 9 modeli regresji...\n")

# 2. Definiowanie i dopasowanie 9 modeli
m1 <- lm(Active_Power ~ GHR, data = df_clean)
m2 <- lm(Active_Power ~ DHR, data = df_clean)
m3 <- lm(Active_Power ~ GHR + DHR, data = df_clean)

m4 <- lm(Active_Power ~ Wind_Speed, data = df_clean)
m5 <- lm(Active_Power ~ Wind_Direction, data = df_clean)
m6 <- lm(Active_Power ~ Wind_Speed + Wind_Direction, data = df_clean)

m7 <- lm(Active_Power ~ Temperature, data = df_clean)
m8 <- lm(Active_Power ~ Humidity, data = df_clean)
m9 <- lm(Active_Power ~ Temperature + Humidity, data = df_clean)

models_list <- list(
  "1_GHR" = m1, "2_DHR" = m2, "3_GHR_DHR" = m3,
  "4_Wind_Speed" = m4, "5_Wind_Direction" = m5, "6_Wind_Speed_Dir" = m6,
  "7_Temperature" = m7, "8_Humidity" = m8, "9_Temp_Hum" = m9
)

# 3. Zapisywanie statystyk do pliku CSV (zamiast Excela blokowanego przez DLL)
cat("[2/4] Zapisywanie miar porównawczych do pliku CSV...\n")
metrics_df <- data.frame(
  Model_ID = 1:9,
  Model_Name = c("GHR", "DHR", "GHR & DHR", "Wind Speed", "Wind Direction", 
                 "Wind Speed & Direction", "Temperature", "Humidity", "Temperature & Humidity"),
  R_Squared = sapply(models_list, function(m) summary(m)$r.squared),
  Adj_R_Squared = sapply(models_list, function(m) summary(m)$adj.r.squared),
  AIC = sapply(models_list, AIC),
  BIC = sapply(models_list, BIC),
  RMSE = sapply(models_list, function(m) sqrt(mean(residuals(m)^2)))
)

csv_path <- file.path(output_dir, "PV_Models_Comparison_Metrics.csv")
write.csv2(metrics_df, csv_path, row.names = FALSE)
cat(paste("Zapisano plik:", csv_path, "(można go otworzyć bezpośrednio w Excelu)\n"))

# Dodanie wartości dopasowanych (wpływów) do ramki danych
df_clean$Imp_1 <- fitted(m1)
df_clean$Imp_2 <- fitted(m2)
df_clean$Imp_3 <- fitted(m3)
df_clean$Imp_4 <- fitted(m4)
df_clean$Imp_5 <- fitted(m5)
df_clean$Imp_6 <- fitted(m6)
df_clean$Imp_7 <- fitted(m7)
df_clean$Imp_8 <- fitted(m8)
df_clean$Imp_9 <- fitted(m9)

# 4. Generowanie wielopanelowych wykresów w odcieniach niebieskiego
cat("[3/4] Tworzenie wykresów w odcieniach niebieskiego...\n")

common_theme <- theme_minimal() + 
  theme(axis.title.x = element_blank(), 
        plot.margin = margin(2, 5, 2, 5),
        axis.text.x = element_text(size = 8))

# FIGURA I: Promieniowanie (wszystkie w odcieniach niebieskiego)
p_I1 <- ggplot(df_clean, aes(x = Time, y = Active_Power)) + geom_line(color = "midnightblue") + labs(title = "I. Active Power") + common_theme
p_I2 <- ggplot(df_clean, aes(x = Time, y = GHR)) + geom_line(color = "royalblue") + labs(title = "GHR") + common_theme
p_I3 <- ggplot(df_clean, aes(x = Time, y = DHR)) + geom_line(color = "dodgerblue") + labs(title = "DHR") + common_theme
p_I4 <- ggplot(df_clean, aes(x = Time, y = Imp_1)) + geom_line(color = "steelblue") + labs(title = "Impact of GHR") + common_theme
p_I5 <- ggplot(df_clean, aes(x = Time, y = Imp_2)) + geom_line(color = "skyblue3") + labs(title = "Impact of DHR") + common_theme
p_I6 <- ggplot(df_clean, aes(x = Time, y = Imp_3)) + geom_line(color = "navy") + labs(title = "Impact of GHR & DHR") + common_theme

fig_I <- (p_I1 / p_I2 / p_I3 / p_I4 / p_I5 / p_I6)

# FIGURA II: Wiatr (wszystkie w odcieniach niebieskiego)
p_II1 <- ggplot(df_clean, aes(x = Time, y = Active_Power)) + geom_line(color = "midnightblue") + labs(title = "II. Active Power") + common_theme
p_II2 <- ggplot(df_clean, aes(x = Time, y = Wind_Speed)) + geom_line(color = "royalblue") + labs(title = "Wind Speed") + common_theme
p_II3 <- ggplot(df_clean, aes(x = Time, y = Wind_Direction)) + geom_line(color = "dodgerblue") + labs(title = "Wind Direction") + common_theme
p_II4 <- ggplot(df_clean, aes(x = Time, y = Imp_4)) + geom_line(color = "steelblue") + labs(title = "Impact of Wind Speed") + common_theme
p_II5 <- ggplot(df_clean, aes(x = Time, y = Imp_5)) + geom_line(color = "skyblue3") + labs(title = "Impact of Wind Direction") + common_theme
p_II6 <- ggplot(df_clean, aes(x = Time, y = Imp_6)) + geom_line(color = "navy") + labs(title = "Impact of Wind Speed & Direction") + common_theme

fig_II <- (p_II1 / p_II2 / p_II3 / p_II4 / p_II5 / p_II6)

# FIGURA III: Temperatura i Wilgotność (wszystkie w odcieniach niebieskiego)
p_III1 <- ggplot(df_clean, aes(x = Time, y = Active_Power)) + geom_line(color = "midnightblue") + labs(title = "III. Active Power") + common_theme
p_III2 <- ggplot(df_clean, aes(x = Time, y = Temperature)) + geom_line(color = "royalblue") + labs(title = "Temperature") + common_theme
p_III3 <- ggplot(df_clean, aes(x = Time, y = Humidity)) + geom_line(color = "dodgerblue") + labs(title = "Humidity") + common_theme
p_III4 <- ggplot(df_clean, aes(x = Time, y = Imp_7)) + geom_line(color = "steelblue") + labs(title = "Impact of Temperature") + common_theme
p_III5 <- ggplot(df_clean, aes(x = Time, y = Imp_8)) + geom_line(color = "skyblue3") + labs(title = "Impact of Humidity") + common_theme
p_III6 <- ggplot(df_clean, aes(x = Time, y = Imp_9)) + geom_line(color = "navy") + labs(title = "Impact of Temperature & Humidity") + common_theme

fig_III <- (p_III1 / p_III2 / p_III3 / p_III4 / p_III5 / p_III6)

# 5. Zapis wykresów do plików graficznych PNG
cat("[4/4] Zapisywanie wykresów do folderu wyników...\n")
ggsave(file.path(output_dir, "Figure_I_Radiation_Blue.png"), fig_I, width = 10, height = 14, dpi = 300)
ggsave(file.path(output_dir, "Figure_II_Wind_Blue.png"), fig_II, width = 10, height = 14, dpi = 300)
ggsave(file.path(output_dir, "Figure_III_Temp_Hum_Blue.png"), fig_III, width = 10, height = 14, dpi = 300)

cat("\nWszystkie zadania zakończone pomyślnie! Wyniki w folderze: ", output_dir, "\n")