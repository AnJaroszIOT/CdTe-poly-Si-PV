# ==============================================================================
# COMPLETE R SCRIPT: 9 SEPARATE IMPACT MODELS WITH POSTERIOR PREDICTIVE ERROR (PPE)
# ==============================================================================

library(readxl)
library(dplyr)
library(lubridate)
library(arm) 
library(openxlsx)
library(loo)

# 1. Output directory & Safe file path handling
output_dir <- "PV_Bayes_DualTech_Analysis"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

excel_file_path <- file.path(output_dir, "PV_Mechanical_Impact_Comparison.xlsx")

con <- try(file(excel_file_path, open = "w"), silent = TRUE)
if (inherits(con, "try-error")) {
  timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  excel_file_path <- file.path(output_dir, paste0("PV_Mechanical_Impact_Comparison_", timestamp_str, ".xlsx"))
  message("Main file is open in Excel. Saving output to new file: ", excel_file_path)
} else {
  close(con)
}

# 2. Load and preprocess datasets
file_polySi <- "Kyocera, 5.4kW, poly-Si.xlsx"
file_CdTe   <- "Calyxo, 5.4kW, CdTe.xlsx"

df_polySi_raw <- read_excel(file_polySi, range = cell_limits(c(1, 1), c(NA, 13)))
df_CdTe_raw   <- read_excel(file_CdTe, range = cell_limits(c(1, 1), c(NA, 13)))

preprocess_pv_data <- function(df) {
  colnames(df)[1:13] <- c("timestamp", "Active_Energy_Delivered_Receive", "Current_Phase_Averag", 
                          "Active_Power", "Performance_Ratio", "Wind_Speed", 
                          "Weather_Temperature_Celsius", "Weather_Relative_Humidity", 
                          "Global_Horizontal_Radiation", "Diffuse_Horizontal_Radiation", 
                          "Wind_Direction", "Weather_Daily_Rainfall", "Radiation_Global_Tilted")
  df$timestamp <- ymd_hms(df$timestamp)
  
  start_date <- ymd_hms("2014-01-01 00:00:00")
  end_date   <- ymd_hms("2017-12-25 14:05:00")
  
  df %>%
    filter(timestamp >= start_date & timestamp <= end_date) %>%
    dplyr::select(-Performance_Ratio) %>%
    filter(!is.na(Active_Power), !is.na(Global_Horizontal_Radiation), 
           !is.na(Wind_Speed), !is.na(Weather_Temperature_Celsius)) %>%
    mutate(
      Active_Power = as.numeric(Active_Power),
      Wind_Direction = as.numeric(Wind_Direction) %% 360
    )
}

df_polySi <- preprocess_pv_data(df_polySi_raw)
df_CdTe   <- preprocess_pv_data(df_CdTe_raw)

prepare_model_data <- function(df) {
  df %>%
    mutate(
      GHR_sc  = as.vector(scale(Global_Horizontal_Radiation)),
      DHR_sc  = as.vector(scale(Diffuse_Horizontal_Radiation)),
      WS_sc   = as.vector(scale(Wind_Speed)),
      WD_sc   = as.vector(scale(Wind_Direction)),
      Temp_sc = as.vector(scale(Weather_Temperature_Celsius)),
      Hum_sc  = as.vector(scale(Weather_Relative_Humidity))
    )
}

df_polySi <- prepare_model_data(df_polySi)
df_CdTe   <- prepare_model_data(df_CdTe)

df_poly_sample <- df_polySi %>% filter(Global_Horizontal_Radiation > 5) %>% sample_n(min(5000, nrow(.)))
df_cdte_sample <- df_CdTe %>% filter(Global_Horizontal_Radiation > 5) %>% sample_n(min(5000, nrow(.)))

# ==============================================================================
# 3. DEFINE 9 SEPARATE BAYESIAN MODELS FOR EACH IMPACT CATEGORY
# ==============================================================================

fit_models_for_tech <- function(df_sample) {
  list(
    Temp        = bayesglm(Active_Power ~ Temp_sc, data = df_sample, family = gaussian()),
    Hum         = bayesglm(Active_Power ~ Hum_sc, data = df_sample, family = gaussian()),
    Temp_Hum    = bayesglm(Active_Power ~ Temp_sc + Hum_sc + I(Temp_sc * Hum_sc), data = df_sample, family = gaussian()),
    GHR         = bayesglm(Active_Power ~ GHR_sc, data = df_sample, family = gaussian()),
    DHR         = bayesglm(Active_Power ~ DHR_sc, data = df_sample, family = gaussian()),
    GHR_DHR     = bayesglm(Active_Power ~ GHR_sc + DHR_sc + I(GHR_sc * DHR_sc), data = df_sample, family = gaussian()),
    WindSpeed   = bayesglm(Active_Power ~ WS_sc, data = df_sample, family = gaussian()),
    WindDir     = bayesglm(Active_Power ~ WD_sc, data = df_sample, family = gaussian()),
    Wind_Comb   = bayesglm(Active_Power ~ WS_sc + WD_sc + I(WS_sc * WD_sc), data = df_sample, family = gaussian())
  )
}

poly_models <- fit_models_for_tech(df_poly_sample)
cdte_models <- fit_models_for_tech(df_cdte_sample)

# ==============================================================================
# 4. METRICS EVALUATION FUNCTION WITH POSTERIOR PREDICTIVE ERROR (PPE)
# ==============================================================================

get_comprehensive_stats <- function(model, df_sample, tech_label, model_name, n.sims = 1000) {
  y <- as.numeric(model.response(model.frame(model)))
  n <- length(y)
  p <- length(coef(model))
  fitted_vals <- as.numeric(fitted(model))
  
  residuals_val <- y - fitted_vals
  sigma_resid <- tryCatch(sqrt(sum(residuals_val^2) / (n - p)), error = function(e) as.numeric(summary(model)$sigma))
  
  sum_sq_resid <- sum(residuals_val^2)
  log_lik_val <- - (n / 2) * log(2 * pi) - n * log(sigma_resid) - (sum_sq_resid / (2 * sigma_resid^2))
  
  aic_val <- tryCatch(as.numeric(AIC(model)), error = function(e) NA)
  bic_val <- tryCatch(as.numeric(BIC(model)), error = function(e) NA)
  
  rmse_val <- sqrt(mean(residuals_val^2, na.rm = TRUE))
  mae_val <- mean(abs(residuals_val), na.rm = TRUE)
  r_sq_val <- 1 - (sum(residuals_val^2, na.rm = TRUE) / sum((y - mean(y))^2, na.rm = TRUE))
  
  # Posterior Simulations for Bayesian criteria & Posterior Predictive Error (PPE)
  sims <- sim(model, n.sims = n.sims)
  beta_mat <- as.matrix(coef(sims))
  sigma_vec <- as.numeric(sims@sigma)
  X <- model.matrix(formula(model), data = df_sample)
  
  log_lik_mat <- matrix(NA, nrow = n.sims, ncol = n)
  y_rep_mat <- matrix(NA, nrow = n.sims, ncol = n)
  
  for (s in 1:n.sims) {
    mu_s <- as.vector(X %*% beta_mat[s, ])
    log_lik_mat[s, ] <- dnorm(y, mean = mu_s, sd = sigma_vec[s], log = TRUE)
    y_rep_mat[s, ] <- rnorm(n, mean = mu_s, sd = sigma_vec[s])
  }
  
  # Calculate Posterior Predictive Error (RMSPPE)
  y_rep_mean <- colMeans(y_rep_mat)
  ppe_val <- sqrt(mean((y - y_rep_mean)^2, na.rm = TRUE))
  
  lppd <- sum(log(colMeans(exp(log_lik_mat), na.rm = TRUE)), na.rm = TRUE)
  p_waic <- sum(apply(log_lik_mat, 2, var), na.rm = TRUE)
  waic_val <- tryCatch(-2 * (lppd - p_waic), error = function(e) NA)
  
  beta_bar <- colMeans(beta_mat)
  sigma_bar <- mean(sigma_vec)
  mu_bar <- as.vector(X %*% beta_bar)
  dev_at_mean <- -2 * sum(dnorm(y, mean = mu_bar, sd = sigma_bar, log = TRUE), na.rm = TRUE)
  mean_dev <- mean(-2 * rowSums(log_lik_mat), na.rm = TRUE)
  p_dic <- mean_dev - dev_at_mean
  dic_val <- tryCatch(dev_at_mean + 2 * p_dic, error = function(e) NA)
  
  looic_val <- tryCatch({
    r_eff <- relative_eff(exp(log_lik_mat), chain_id = rep(1, n.sims))
    loo_res <- loo(log_lik_mat, r_eff = r_eff)
    as.numeric(loo_res$estimates["looic", "Estimate"])
  }, error = function(e) {
    NA
  })
  
  safe_val <- function(v) if(length(v) == 1 && !is.null(v)) v else NA
  
  data.frame(
    Technology = tech_label,
    Model_Category = model_name,
    AIC = round(safe_val(aic_val), 2),
    BIC = round(safe_val(bic_val), 2),
    Post_Pred_Error = round(safe_val(ppe_val), 4), # Replaces raw likelihood
    Log_Likelihood = round(safe_val(log_lik_val), 2),
    Residual_SD = round(safe_val(sigma_resid), 4),
    RMSE = round(safe_val(rmse_val), 4),
    MAE = round(safe_val(mae_val), 4),
    R_Squared = round(safe_val(r_sq_val), 4),
    WAIC = round(safe_val(waic_val), 2),
    DIC = round(safe_val(dic_val), 2),
    LOOIC_PSIS = round(safe_val(looic_val), 2)
  )
}

# Evaluate all 9 models for Poly-Si and CdTe
poly_stats <- bind_rows(
  get_comprehensive_stats(poly_models$Temp, df_poly_sample, "Poly-Si (Kyocera)", "1_Temperature"),
  get_comprehensive_stats(poly_models$Hum, df_poly_sample, "Poly-Si (Kyocera)", "2_Humidity"),
  get_comprehensive_stats(poly_models$Temp_Hum, df_poly_sample, "Poly-Si (Kyocera)", "3_Temp_Humidity"),
  get_comprehensive_stats(poly_models$GHR, df_poly_sample, "Poly-Si (Kyocera)", "4_GHR"),
  get_comprehensive_stats(poly_models$DHR, df_poly_sample, "Poly-Si (Kyocera)", "5_DHR"),
  get_comprehensive_stats(poly_models$GHR_DHR, df_poly_sample, "Poly-Si (Kyocera)", "6_GHR_DHR"),
  get_comprehensive_stats(poly_models$WindSpeed, df_poly_sample, "Poly-Si (Kyocera)", "7_WindSpeed"),
  get_comprehensive_stats(poly_models$WindDir, df_poly_sample, "Poly-Si (Kyocera)", "8_WindDir"),
  get_comprehensive_stats(poly_models$Wind_Comb, df_poly_sample, "Poly-Si (Kyocera)", "9_Wind_Speed_Dir")
)

cdte_stats <- bind_rows(
  get_comprehensive_stats(cdte_models$Temp, df_cdte_sample, "CdTe (Calyxo)", "1_Temperature"),
  get_comprehensive_stats(cdte_models$Hum, df_cdte_sample, "CdTe (Calyxo)", "2_Humidity"),
  get_comprehensive_stats(cdte_models$Temp_Hum, df_cdte_sample, "CdTe (Calyxo)", "3_Temp_Humidity"),
  get_comprehensive_stats(cdte_models$GHR, df_cdte_sample, "CdTe (Calyxo)", "4_GHR"),
  get_comprehensive_stats(cdte_models$DHR, df_cdte_sample, "CdTe (Calyxo)", "5_DHR"),
  get_comprehensive_stats(cdte_models$GHR_DHR, df_cdte_sample, "CdTe (Calyxo)", "6_GHR_DHR"),
  get_comprehensive_stats(cdte_models$WindSpeed, df_cdte_sample, "CdTe (Calyxo)", "7_WindSpeed"),
  get_comprehensive_stats(cdte_models$WindDir, df_cdte_sample, "CdTe (Calyxo)", "8_WindDir"),
  get_comprehensive_stats(cdte_models$Wind_Comb, df_cdte_sample, "CdTe (Calyxo)", "9_Wind_Speed_Dir")
)

all_models_comparison <- bind_rows(poly_stats, cdte_stats)

# ==============================================================================
# 5. EXTRACT PEAK ACTIVE POWER TIMESTAMPS FOR SEASONAL START DAYS (2014-2017)
# ==============================================================================
years <- 2014:2017
target_dates <- c()
for (yr in years) {
  target_dates <- c(target_dates, 
                    paste0(yr, "-03-01"), 
                    paste0(yr, "-06-01"), 
                    paste0(yr, "-09-01"), 
                    paste0(yr, "-12-01"))
}
target_dates_as_date <- as.Date(target_dates)

df_polySi$Impact_GHR      <- predict(poly_models$GHR_DHR, newdata = df_polySi) * df_polySi$GHR_sc
df_polySi$Impact_WindSpeed <- pmin(pmax(abs(predict(poly_models$Wind_Comb, newdata = df_polySi) * df_polySi$WS_sc), 0), 10)
df_polySi$Impact_Temp     <- predict(poly_models$Temp_Hum, newdata = df_polySi) * df_polySi$Temp_sc

df_CdTe$Impact_GHR      <- predict(cdte_models$GHR_DHR, newdata = df_CdTe) * df_CdTe$GHR_sc
df_CdTe$Impact_WindSpeed <- pmin(pmax(abs(predict(cdte_models$Wind_Comb, newdata = df_CdTe) * df_CdTe$WS_sc), 0), 10)
df_CdTe$Impact_Temp     <- predict(cdte_models$Temp_Hum, newdata = df_CdTe) * df_CdTe$Temp_sc

extract_peak_seasonal_snapshots <- function(df, tech_name) {
  df %>%
    mutate(Date_Only = as.Date(timestamp)) %>%
    filter(Date_Only %in% target_dates_as_date) %>%
    group_by(Date_Only) %>%
    slice_max(Active_Power, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(Technology = tech_name) %>%
    dplyr::select(Technology, timestamp, Active_Power, Global_Horizontal_Radiation, 
                  Wind_Speed, Weather_Temperature_Celsius, Impact_GHR, Impact_WindSpeed, Impact_Temp)
}

combined_peak_snapshots <- bind_rows(
  extract_peak_seasonal_snapshots(df_polySi, "Poly-Si (Kyocera)"),
  extract_peak_seasonal_snapshots(df_CdTe, "CdTe (Calyxo)")
) %>% arrange(timestamp)

# ==============================================================================
# 6. EXPORT TO EXCEL
# ==============================================================================
wb <- createWorkbook()

addWorksheet(wb, "Model_Selection_Criteria")
writeData(wb, sheet = "Model_Selection_Criteria", x = all_models_comparison)

addWorksheet(wb, "Peak_Seasonal_Snapshots")
writeData(wb, sheet = "Peak_Seasonal_Snapshots", x = combined_peak_snapshots)

saveWorkbook(wb, excel_file_path, overwrite = TRUE)


cat("Success! Results with scientific likelihood format saved to:", excel_file_path, "\n")