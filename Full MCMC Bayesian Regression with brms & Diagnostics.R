# =====================================================================
# Script 1: Full MCMC Bayesian Regression with brms & Diagnostics
# =====================================================================

library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(brms)
library(writexl)

output_dir <- file.path(getwd(), "PV_Full_Bayes_Output")
if (!dir.exists(output_dir)) dir.create(output_dir)

datasets <- list(
  list(file = "Kyocera, 5.4kW, poly-Si.xlsx", title = "Poly-Si (Kyocera)", prefix = "polysi"),
  list(file = "Calyxo, 5.4kW, CdTe.xlsx", title = "CdTe (Calyxo)", prefix = "cdte")
)

start_date <- as.POSIXct("2014-01-01 00:00:00")
end_date <- as.POSIXct("2017-12-25 14:05:00")

results_summary <- list()

for (ds in datasets) {
  cat("\nProcessing Full Bayesian Model for:", ds$title, "\n")
  raw_df <- read_excel(ds$file, col_names = FALSE)
  headers <- as.character(raw_df[1, ])
  df <- raw_df[-1, ]
  colnames(df) <- gsub(" ", "_", headers)
  
  col_d_name <- names(df)[4] # Active Power
  time_col <- names(df)[str_detect(tolower(names(df)), "time|date")][1]
  
  df_clean <- df %>%
    mutate(
      Time = ymd_hms(.data[[time_col]]),
      Active_Power = suppressWarnings(as.numeric(.data[[col_d_name]])),
      GHR = suppressWarnings(as.numeric(Global_Horizontal_Radiation)),
      Temp = suppressWarnings(as.numeric(Weather_Temperature_Celsius)),
      Wind = suppressWarnings(as.numeric(Wind_Speed))
    ) %>%
    filter(!is.na(Time), Time >= start_date, Time <= end_date, 
           !is.na(Active_Power), !is.na(GHR), !is.na(Temp), !is.na(Wind), Active_Power > 0) %>%
    sample_n(2000) # Subsample for fast MCMC demonstration if dataset is massive
  
  # Define weakly informative priors
  prior_spec <- c(
    prior(normal(0, 10), class = "b"),
    prior(student_t(3, 0, 10), class = "Intercept"),
    prior(exponential(1), class = "sigma")
  )
  
  # Fit full Bayesian model via Stan
  fit_bayes <- brm(
    Active_Power ~ GHR + Temp + Wind,
    data = df_clean,
    family = gaussian(),
    prior = prior_spec,
    chains = 4,
    iter = 2000,
    warmup = 1000,
    cores = 4,
    seed = 42
  )
  
  # Extract diagnostics
  summary_fit <- summary(fit_bayes)
  print(summary_fit)
  
  # Save posterior summaries
  fixed_effects <- fixef(fit_bayes)
  results_summary[[ds$prefix]] <- as.data.frame(fixed_effects)
  
  # Posterior Predictive Check plot
  png(file.path(output_dir, paste0(ds$prefix, "_pp_check.png")), width = 800, height = 600)
  print(pp_check(fit_bayes, ndraws = 100) + ggtitle(paste("Posterior Predictive Check:", ds$title)))
  dev.off()
}

write_xlsx(results_summary, file.path(output_dir, "full_bayes_fixed_effects.xlsx"))
cat("\nScript 1 completed successfully!\n")