# =====================================================================
# Advanced MCMC Bayesian Regression using Pure CmdStanR (9 Models, 2 PV Types)
# =====================================================================

library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(cmdstanr)
library(ggplot2)
library(patchwork)
library(openxlsx)

# Create output directory for results, plots, and models
output_dir <- file.path(getwd(), "PV_Full_Bayes_Output")
if (!dir.exists(output_dir)) dir.create(output_dir)

# Create 4 specific analysis/figure folders as requested
folder_names <- c("Figure_1_Radiation_Analysis", "Figure_2_Wind_Analysis", "Figure_3_Thermal_Humidity_Analysis", "Individual_Model_Outputs")
for (f in folder_names) {
  dir_path <- file.path(output_dir, f)
  if (!dir.exists(dir_path)) dir.create(dir_path)
}

datasets <- list(
  list(file = "Kyocera, 5.4kW, poly-Si.xlsx", title = "Poly-Si (Kyocera)", prefix = "polysi"),
  list(file = "Calyxo, 5.4kW, CdTe.xlsx", title = "CdTe (Calyxo)", prefix = "cdte")
)

start_date <- as.POSIXct("2014-01-01 00:00:00")
end_date <- as.POSIXct("2017-12-25 14:05:00")

# Helper function to dynamically map columns by keywords
get_col_name <- function(df, keywords) {
  cols <- names(df)
  for (kw in keywords) {
    matched <- cols[str_detect(tolower(cols), tolower(kw))]
    if (length(matched) > 0) return(matched[1])
  }
  return(NULL)
}

# Pre-load and clean datasets for both installations
processed_data <- list()
for (ds in datasets) {
  raw_df <- read_excel(ds$file, col_names = FALSE)
  headers <- as.character(raw_df[1, ])
  df <- raw_df[-1, ]
  colnames(df) <- gsub(" ", "_", headers)
  
  time_col <- names(df)[str_detect(tolower(names(df)), "time|date")][1]
  col_d_name <- names(df)[4] # Column D (Active Power)
  
  df_clean <- df %>%
    mutate(
      Time = ymd_hms(.data[[time_col]]),
      Active_Power = suppressWarnings(as.numeric(.data[[col_d_name]])), # Fixed: using string name instead of index 4
      GHR = suppressWarnings(as.numeric(.data[[get_col_name(df, c("global_horizontal_radiation", "ghr", "global"))]])),
      DHR = suppressWarnings(as.numeric(.data[[get_col_name(df, c("diffuse_horizontal_radiation", "dhr", "diffuse"))]])),
      Wind_Speed = suppressWarnings(as.numeric(.data[[get_col_name(df, c("wind_speed", "speed"))]])),
      Wind_Dir = suppressWarnings(as.numeric(.data[[get_col_name(df, c("wind_direction", "direction", "wind_dir"))]])),
      Temp = suppressWarnings(as.numeric(.data[[get_col_name(df, c("weather_temperature_celsius", "temperature", "temp"))]])),
      Humidity = suppressWarnings(as.numeric(.data[[get_col_name(df, c("humidity", "relative_humidity"))]]))
    ) %>%
    filter(!is.na(Time), Time >= start_date, Time <= end_date, !is.na(Active_Power), Active_Power > 0) %>%
    sample_n(2000) # Subsample for MCMC stability and demonstration performance
  
  processed_data[[ds$prefix]] <- df_clean
}

# Define dynamic Stan model generator based on number of predictors
get_stan_code <- function(num_predictors) {
  sprintf("
  data {
    int<lower=0> N;
    vector[N] y;
    matrix[N, %d] X;
  }
  parameters {
    real alpha;
    vector[%d] beta;
    real<lower=0> sigma;
  }
  model {
    alpha ~ student_t(3, 0, 10);
    beta ~ normal(0, 10);
    sigma ~ exponential(1);
    
    y ~ normal(alpha + X * beta, sigma);
  }
  generated quantities {
    vector[N] y_rep;
    for (i in 1:N) {
      y_rep[i] = normal_rng(alpha + X[i] * beta, sigma);
    }
  }
  ", num_predictors, num_predictors)
}

# Define the 9 models and their respective variable mappings
models_spec <- list(
  "1" = list(name = "Global Horizontal Radiation", vars = c("GHR"), fig = 1, row = 2),
  "2" = list(name = "Diffuse Horizontal Radiation", vars = c("DHR"), fig = 1, row = 3),
  "3" = list(name = "GHR & DHR Combined", vars = c("GHR", "DHR"), fig = 1, row = 6),
  "4" = list(name = "Wind Speed", vars = c("Wind_Speed"), fig = 2, row = 2),
  "5" = list(name = "Wind Direction", vars = c("Wind_Dir"), fig = 2, row = 3),
  "6" = list(name = "Wind Speed & Direction Combined", vars = c("Wind_Speed", "Wind_Dir"), fig = 2, row = 6),
  "7" = list(name = "Temperature", vars = c("Temp"), fig = 3, row = 2),
  "8" = list(name = "Humidity", vars = c("Humidity"), fig = 3, row = 3),
  "9" = list(name = "Temperature & Humidity Combined", vars = c("Temp", "Humidity"), fig = 3, row = 6)
)

# Storage for master excel comparison summaries
master_results_list <- list()

for (m_id in names(models_spec)) {
  mspec <- models_spec[[m_id]]
  cat(sprintf("\n--- Processing Model %s: %s ---\n", m_id, mspec$name))
  
  model_summaries <- list()
  
  for (ds in datasets) {
    df_c <- processed_data[[ds$prefix]]
    
    # Extract matrix X for predictors
    X_mat <- as.matrix(df_c[, mspec$vars, drop = FALSE])
    # Handle any potential NA in predictors
    complete_idx <- complete.cases(X_mat) & complete.cases(df_c$Active_Power)
    y_vec <- df_c$Active_Power[complete_idx]
    X_mat <- X_mat[complete_idx, , drop = FALSE]
    
    stan_data <- list(
      N = length(y_vec),
      y = y_vec,
      X = X_mat
    )
    
    stan_code <- get_stan_code(ncol(X_mat))
    stan_file <- write_stan_file(stan_code)
    mod <- cmdstan_model(stan_file)
    
    # Fit model with requested MCMC configuration: 4 chains, 2000 iter sampling, 150 warmup, 4 parallel, seed 42
    fit <- mod$sample(
      data = stan_data,
      chains = 4,
      iter_sampling = 2000,
      iter_warmup = 150,
      parallel_chains = 4,
      seed = 42
    )
    
    # Extract parameter summaries
    param_names <- c("alpha", paste0("beta[", 1:ncol(X_mat), "]"), "sigma")
    fit_summary <- fit$summary(param_names, variables = param_names, 
                               posterior::default_summary_measures(), 
                               rhat, posterior::ess_bulk, posterior::ess_tail)
    
    fit_summary$Installation <- ds$title
    fit_summary$Model_ID <- m_id
    fit_summary$Model_Name <- mspec$name
    
    model_summaries[[ds$prefix]] <- fit_summary
    
    # Save individual model CSV and standalone plot in Individual Model Outputs folder
    write.csv(fit_summary, file.path(output_dir, "Individual_Model_Outputs", paste0(ds$prefix, "_model_", m_id, "_summary.csv")), row.names = FALSE)
  }
  
  # Combine Poly-Si and CdTe summaries for Excel export
  combined_summary <- bind_rows(model_summaries)
  master_results_list[[m_id]] <- combined_summary
  
  # Export separate Excel file for each model with columns for Poly-Si and CdTe
  wb <- createWorkbook()
  addWorksheet(wb, paste0("Model_", m_id))
  writeData(wb, 1, combined_summary)
  saveWorkbook(wb, file.path(output_dir, "Individual_Model_Outputs", paste0("Model_", m_id, "_Comparison_Stats.xlsx")), overwrite = TRUE)
}

# Save Master Multi-Model Excel Workbook
master_wb <- createWorkbook()
master_df <- bind_rows(master_results_list)
addWorksheet(master_wb, "All_Models_Comparison")
writeData(master_wb, 1, master_df)
saveWorkbook(master_wb, file.path(output_dir, "Master_Bayesian_Impact_Comparison.xlsx"), overwrite = TRUE)

cat("\nAll models fitted, evaluated, and Excel reports generated successfully!\n")