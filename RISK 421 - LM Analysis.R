library(tidyverse)
library(CASdatasets)
library(lme4)
library(glmmTMB)

# Data Cleaning -----------------------------------------------------------

# Severity and frequency are assumed independent
# Gathering raw data
data("brvehins1a")
raw_data <- as_tibble(brvehins1a) |> 
  mutate(ExposColl = ExposTotal - ExposFireRob) |> 
  mutate(
    VehBrand = str_extract(VehGroup, "^\\w+"), # Brand is first word
    VehBrand = as.factor(VehBrand)
  ) |> 
  mutate(VehBrand = fct_lump_min(VehBrand, 
                                 min = 1000, 
                                 other_level = "Other"))

raw_freq <- raw_data |> 
  select(Gender, 
         DrivAge,
         VehYear,
         VehModel,
         VehGroup,
         VehBrand,
         Area,
         StateAb,
         ExposTotal,
         ExposFireRob,
         ExposColl,
         ClaimNbPartColl
  ) 

raw_sev <- raw_data |> 
  select(Gender,
         DrivAge,
         VehYear,
         VehModel,
         VehGroup,
         VehBrand,
         Area,
         StateAb,
         ClaimAmountPartColl
  ) 


# Results: No one has exposure to fire and robberies in this dataset
#raw_freq |> 
#  filter(ExposTotal >= ExposFireRob) |> 
#  nrow()
#raw_freq |> 
#  filter(ExposFireRob > 0) |> 
#  nrow()

# Cleaning data
clean_freq <- raw_freq |> 
  drop_na() |> 
  filter(Gender %in% c("Male", "Female"),
         DrivAge %in% c("18-25", "26-35", "36-45", "46-55", ">55"),
         VehYear >= 1885,      # 1st car invented 1885
         ExposTotal > 0,       # No exposure means no policy
         ExposFireRob >= 0,    # They could have no fire/robbery coverage
         ExposColl > 0,        # No exposure means no claims
         ClaimNbPartColl >= 0, # Claims are whole numbers
  ) 
clean_sev <- raw_sev |> 
  drop_na() |>  
  filter(Gender %in% c("Male", "Female"),
         DrivAge %in% c("18-25", "26-35", "36-45", "46-55", ">55"),
         VehYear >= 1885,         # 1st car invented 1885
         ClaimAmountPartColl > 0  # Claims are positive
  )

# NOTE: VehBrand was introduced to simplify VehGroup into less levels. 
#       VehGroup has 397 levels before, but VehBrand has been reduced to 
#       only 23. Only groups with 1000 observations were saved. 


# Test Data (Cleaning) ----------------------------------------------------

# Severity and frequency are assumed independent
# Gathering raw data
data("brvehins1b")
raw_test_data <- as_tibble(brvehins1b) |> 
  mutate(ExposColl = ExposTotal - ExposFireRob) |> 
  mutate(
    VehBrand = str_extract(VehGroup, "^\\w+"), # Brand is first word
    VehBrand = as.factor(VehBrand)
  )

raw_test_freq <- raw_test_data |> 
  select(Gender, 
         DrivAge,
         VehYear,
         VehModel,
         VehGroup,
         VehBrand,
         Area,
         StateAb,
         ExposTotal,
         ExposFireRob,
         ExposColl,
         ClaimNbPartColl
  ) 

raw_test_sev <- raw_test_data |> 
  select(Gender,
         DrivAge,
         VehYear,
         VehModel,
         VehGroup,
         VehBrand,
         Area,
         StateAb,
         ClaimAmountPartColl
  ) 

# Cleaning data
clean_test_freq <- raw_test_freq |> 
  drop_na() |> 
  filter(Gender %in% c("Male", "Female"),
         DrivAge %in% c("18-25", "26-35", "36-45", "46-55", ">55"),
         VehYear >= 1885,      # 1st car invented 1885
         ExposTotal > 0,       # No exposure means no policy
         ExposFireRob >= 0,    # They could have no fire/robbery coverage
         ExposColl > 0,        # No exposure means no claims
         ClaimNbPartColl >= 0, # Claims are whole numbers
  ) 
clean_test_sev <- raw_test_sev |> 
  drop_na() |>  
  filter(Gender %in% c("Male", "Female"),
         DrivAge %in% c("18-25", "26-35", "36-45", "46-55", ">55"),
         VehYear >= 1885,         # 1st car invented 1885
         ClaimAmountPartColl > 0  # Claims are positive
  )

# Match categorical variables level with training set.
align_to_na <- function(test_df, train_df, col_name) {
  train_levels <- levels(as.factor(train_df[[col_name]]))
  test_df[[col_name]] <- factor(test_df[[col_name]], 
                                levels = train_levels)
  return(test_df)
}

# First, turn new VehBrands into NA, then turns NA's into Other.
clean_test_sev <- align_to_na(clean_test_sev, clean_sev, "VehBrand")
clean_test_sev <- clean_test_sev |> 
  mutate(VehBrand = fct_na_value_to_level(VehBrand, level = "Other"))

clean_test_freq <- align_to_na(clean_test_freq, clean_freq, "VehBrand")
clean_test_freq <- clean_test_freq |> 
  mutate(VehBrand = fct_na_value_to_level(VehBrand, level = "Other"))



# Severity LM Modeling Code


# 1. Check severity distribution
hist(clean_sev$ClaimAmountPartColl, breaks = 50,
     main = "Histogram of Partial Collision Claim Amount",
     xlab = "Claim Amount")

# 2. Create log severity variable
clean_sev$log_claim <- log(clean_sev$ClaimAmountPartColl)

# 3. Convert categorical variables to factors
clean_sev$Gender   <- as.factor(clean_sev$Gender)
clean_sev$DrivAge  <- as.factor(clean_sev$DrivAge)
clean_sev$VehBrand <- as.factor(clean_sev$VehBrand)
clean_sev$Area     <- as.factor(clean_sev$Area)
clean_sev$StateAb  <- as.factor(clean_sev$StateAb)

# 4. Fit first simple linear model
lm1 <- lm(log_claim ~ Gender + DrivAge + VehYear, data = clean_sev)
summary(lm1)

# 5. Fit fuller linear model
lm2 <- lm(log_claim ~ Gender + DrivAge + VehYear +
            VehBrand + Area + StateAb,
          data = clean_sev)
summary(lm2)

# 6. Compare models
AIC(lm1, lm2)
BIC(lm1, lm2)

# 7. Diagnostic plots for chosen model
par(mfrow = c(2, 2))
plot(lm2)

# 8. Convert coefficients into approximate percentage effects
100 * (exp(coef(lm2)) - 1)



# 9. RMSE Evaluation on Test Data


clean_test_sev$Gender   <- as.factor(clean_test_sev$Gender)
clean_test_sev$DrivAge  <- as.factor(clean_test_sev$DrivAge)
clean_test_sev$VehBrand <- as.factor(clean_test_sev$VehBrand)
clean_test_sev$Area     <- as.factor(clean_test_sev$Area)
clean_test_sev$StateAb  <- as.factor(clean_test_sev$StateAb)

# Predictions 
pred_log_lm1 <- predict(lm1, newdata = clean_test_sev)
pred_log_lm2 <- predict(lm2, newdata = clean_test_sev)

pred_sev_lm1 <- exp(pred_log_lm1)
pred_sev_lm2 <- exp(pred_log_lm2)
head(pred_sev_lm1)
head(pred_sev_lm2)
# Actual claim amounts
actual_claim <- clean_test_sev$ClaimAmountPartColl
head(actual_claim)

# Compute RMSE 
rmse_lm1 <- sqrt(mean((actual_claim - pred_sev_lm1)^2))
rmse_lm2 <- sqrt(mean((actual_claim - pred_sev_lm2)^2))


cat("RMSE for LM1 (Simple Model):", rmse_lm1, "\n")
cat("RMSE for LM2 (Full Model):", rmse_lm2, "\n")

