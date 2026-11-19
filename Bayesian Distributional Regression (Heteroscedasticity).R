# =====================================================================
# Script 2: Bayesian Distributional Regression (Heteroscedasticity)
# =====================================================================

library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(brms)

output_dir <- file.path(getwd(), "PV_Full_Bayes_Output")
if (!dir.exists(output_dir)) dir.create(output_dir)

# Load Poly-Si dataset
ds <- list(file = "Kyocera, 5.4kW, poly-Si.xlsx", title = "Poly-Si (Kyocera)", prefix = "polysi")
raw_df <- read_excel(ds$file, col_names = FALSE)
headers <- as.character(raw_df[1, ])
df <- raw_df[-1, ]
colnames(df) <- gsub(" ", "_", headers)

df_clean <- df %>%
  mutate(
    Time = ymd_hms(timestamp),
    Active_Power = suppressWarnings(as.numeric(Active_Power)),
    GHR = suppressWarnings(as.numeric(Global_Horizontal_Radiation)),
    Temp = suppressWarnings(as.numeric(Weather_Temperature_Celsius))
  ) %>%
  filter(!is.na(Time), !is.na(Active_Power), !is.na(GHR), !is.na(Temp), Active_Power > 0) %>%
  sample_n(1500)

# Formula specifying predictors for both mean (Active_Power) and variance (sigma)
bf_formula <- bf(
  Active_Power ~ GHR + Temp,  # Model for mean
  sigma ~ GHR                  # Model for residual dispersion / variance
)

cat("\nFitting Bayesian Distributional Regression (Mean + Variance)...\n")
dist_fit <- brm(
  formula = bf_formula,
  data = df_clean,
  family = gaussian(),
  chains = 2,
  iter = 1500,
  cores = 2,
  seed = 123
)

print(summary(dist_fit))
cat("\nScript 2 completed successfully!\n")
