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
# NOTE: Corporate "gender" was removed from all models because of quasi-
#       seperation. All corporate policies have 0 claims and 0 severity.
# Testing Data (Cleaned)


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


# dataset C
data("brvehins1c")
raw_test_data2 <- as_tibble(brvehins1c) |> 
  mutate(ExposColl = ExposTotal - ExposFireRob) |> 
  mutate(
    VehBrand = str_extract(VehGroup, "^\\w+"), # Brand is first word
    VehBrand = as.factor(VehBrand)
  )

raw_test_freq2 <- raw_test_data2 |> 
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

# raw_test_sev <- raw_test_data |> 
#   select(Gender,
#          DrivAge,
#          VehYear,
#          VehModel,
#          VehGroup,
#          VehBrand,
#          Area,
#          StateAb,
#          ClaimAmountPartColl
#   ) 

# Cleaning data
clean_test_freq2 <- raw_test_freq2 |> 
  drop_na() |> 
  filter(Gender %in% c("Male", "Female"),
         DrivAge %in% c("18-25", "26-35", "36-45", "46-55", ">55"),
         VehYear >= 1885,      # 1st car invented 1885
         ExposTotal > 0,       # No exposure means no policy
         ExposFireRob >= 0,    # They could have no fire/robbery coverage
         ExposColl > 0,        # No exposure means no claims
         ClaimNbPartColl >= 0, # Claims are whole numbers
  ) 
# clean_test_sev <- raw_test_sev |> 
#   drop_na() |>  
#   filter(Gender %in% c("Male", "Female"),
#          DrivAge %in% c("18-25", "26-35", "36-45", "46-55", ">55"),
#          VehYear >= 1885,         # 1st car invented 1885
#          ClaimAmountPartColl > 0  # Claims are positive
#   )

# Match categorical variables level with training set.
# align_to_na <- function(test_df, train_df, col_name) {
#   train_levels <- levels(as.factor(train_df[[col_name]]))
#   test_df[[col_name]] <- factor(test_df[[col_name]], 
#                                 levels = train_levels)
#   return(test_df)
# }

# First, turn new VehBrands into NA, then turns NA's into Other.
# clean_test_sev <- align_to_na(clean_test_sev, clean_sev, "VehBrand")
# clean_test_sev <- clean_test_sev |> 
#   mutate(VehBrand = fct_na_value_to_level(VehBrand, level = "Other"))

clean_test_freq2 <- align_to_na(clean_test_freq2, clean_freq, "VehBrand")
clean_test_freq2 <- clean_test_freq2 |> 
  mutate(VehBrand = fct_na_value_to_level(VehBrand, level = "Other"))

# Frequency Modelling -----------------------------------------------------

# Poisson GLMMs
# Regular Poisson GLMMs
poisson_area <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + VehYear +
                           + VehBrand + (1|Area) + 
                           offset(log(ExposColl)),
                         data = clean_freq,
                         family = poisson()
                        )
poisson_state <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + VehYear +
                           + VehBrand + (1|StateAb) + 
                           offset(log(ExposColl)),
                         data = clean_freq,
                         family = poisson()
                         )
 
# 3 parameters 1 effect
# Area
poisson_area_no_brand <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                   VehYear + (1|Area) + 
                                   offset(log(ExposColl)),
                                 data = clean_freq,
                                 family = poisson()
                                 ) 
poisson_area_no_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                   VehBrand + (1|Area) + 
                                   offset(log(ExposColl)),
                                 data = clean_freq,
                                 family = poisson()
                                 ) 
poisson_area_no_age <- glmmTMB(ClaimNbPartColl ~ Gender + VehYear + 
                                   VehBrand + (1|Area) + 
                                   offset(log(ExposColl)),
                                 data = clean_freq,
                                 family = poisson()
                                 ) 
poisson_area_no_gender <- glmmTMB(ClaimNbPartColl ~ DrivAge + VehYear + 
                                    VehBrand + (1|Area) + 
                                   offset(log(ExposColl)),
                                 data = clean_freq,
                                 family = poisson()
                                 )
# State
poisson_state_no_brand <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                   VehYear + (1|StateAb) + 
                                   offset(log(ExposColl)),
                                 data = clean_freq,
                                 family = poisson()
                                 ) 
poisson_state_no_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                     VehBrand + (1|StateAb) + 
                                     offset(log(ExposColl)),
                                   data = clean_freq,
                                   family = poisson()
                                   ) 
poisson_state_no_age <- glmmTMB(ClaimNbPartColl ~ Gender + VehYear + 
                                 VehBrand + (1|StateAb) + 
                                 offset(log(ExposColl)),
                               data = clean_freq,
                               family = poisson()
                               ) 
poisson_state_no_gender <- glmmTMB(ClaimNbPartColl ~ DrivAge + VehYear + 
                                    VehBrand + (1|StateAb) + 
                                    offset(log(ExposColl)),
                                  data = clean_freq,
                                  family = poisson()
                                  )

# 2 parameters 1 effect
# Area
poisson_area_gender_age <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge +
                                     (1|Area) + offset(log(ExposColl)),
                                   data = clean_freq,
                                   family = poisson()
                                   )
poisson_area_gender_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + 
                                         VehYear + (1|Area) + 
                                         offset(log(ExposColl)),
                                       data = clean_freq,
                                       family = poisson()
                                       )
poisson_area_gender_brand <- glmmTMB(ClaimNbPartColl ~ Gender + 
                                       VehBrand + (1|Area) + 
                                       offset(log(ExposColl)),
                                     data = clean_freq,
                                     family = poisson()
                                     )
poisson_area_age_vehyear <- glmmTMB(ClaimNbPartColl ~ DrivAge +
                                     VehYear + (1|Area) + 
                                      offset(log(ExposColl)),
                                    data = clean_freq,
                                    family = poisson()
                                    )
poisson_area_age_brand <- glmmTMB(ClaimNbPartColl ~ DrivAge +
                                      VehBrand + (1|Area) + 
                                      offset(log(ExposColl)),
                                    data = clean_freq,
                                    family = poisson()
                                  )
poisson_area_vehyear_brand <- glmmTMB(ClaimNbPartColl ~ VehYear +
                                    VehBrand + (1|Area) + 
                                    offset(log(ExposColl)),
                                  data = clean_freq,
                                  family = poisson()
                                  )
# Zero-inflated Poisson
# Constant probability for P(N = 0)
poisson_zi1_area <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                               VehYear + VehBrand + (1|Area) + 
                               offset(log(ExposColl)),
                             ziformula = ~1,
                             data = clean_freq,
                             family = poisson()
)
poisson_zi1_state <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                   VehYear + VehBrand + (1|StateAb) + 
                                    offset(log(ExposColl)),
                                 ziformula = ~1,
                                 data = clean_freq,
                                 family = poisson()
                                 )
# 3 parameters 1 effect
# Area
poisson_zi1_area_no_brand <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                       VehYear + (1|Area) + 
                                       offset(log(ExposColl)),
                                     ziformula = ~1,
                                     data = clean_freq,
                                     family = poisson()
) 
poisson_zi1_area_no_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                         VehBrand + (1|Area) + 
                                         offset(log(ExposColl)),
                                       ziformula = ~1,
                                       data = clean_freq,
                                       family = poisson()
) 
poisson_zi1_area_no_age <- glmmTMB(ClaimNbPartColl ~ Gender + VehYear + 
                                     VehBrand + (1|Area) + 
                                     offset(log(ExposColl)),
                                   ziformula = ~1,
                                   data = clean_freq,
                                   family = poisson()
) 
poisson_zi1_area_no_gender <- glmmTMB(ClaimNbPartColl ~ DrivAge + VehYear + 
                                        VehBrand + (1|Area) + 
                                        offset(log(ExposColl)),
                                      ziformula = ~1,
                                      data = clean_freq,
                                      family = poisson()
)
# State
poisson_zi1_state_no_brand <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                        VehYear + (1|StateAb) + 
                                        offset(log(ExposColl)),
                                      ziformula = ~1,
                                      data = clean_freq,
                                      family = poisson()
) 
poisson_zi1_state_no_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                          VehBrand + (1|StateAb) + 
                                          offset(log(ExposColl)),
                                        ziformula = ~1,
                                        data = clean_freq,
                                        family = poisson()
) 
poisson_zi1_state_no_age <- glmmTMB(ClaimNbPartColl ~ Gender + VehYear + 
                                      VehBrand + (1|StateAb) + 
                                      offset(log(ExposColl)),
                                    ziformula = ~1,
                                    data = clean_freq,
                                    family = poisson()
) 
poisson_zi1_state_no_gender <- glmmTMB(ClaimNbPartColl ~ DrivAge + VehYear + 
                                         VehBrand + (1|StateAb) + 
                                         offset(log(ExposColl)),
                                       ziformula = ~1,
                                       data = clean_freq,
                                       family = poisson()
)

# 2 parameters 1 effect
# Area
poisson_zi1_area_gender_age <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge +
                                         (1|Area) + offset(log(ExposColl)),
                                       ziformula = ~1,
                                       data = clean_freq,
                                       family = poisson()
)
poisson_zi1_area_gender_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + 
                                             VehYear + (1|Area) + 
                                             offset(log(ExposColl)),
                                           ziformula = ~1,
                                           data = clean_freq,
                                           family = poisson()
)
poisson_zi1_area_gender_brand <- glmmTMB(ClaimNbPartColl ~ Gender + 
                                           VehBrand + (1|Area) + 
                                           offset(log(ExposColl)),
                                         ziformula = ~1,
                                         data = clean_freq,
                                         family = poisson()
)
poisson_zi1_area_age_vehyear <- glmmTMB(ClaimNbPartColl ~ DrivAge +
                                          VehYear + (1|Area) + 
                                          offset(log(ExposColl)),
                                        ziformula = ~1,
                                        data = clean_freq,
                                        family = poisson()
)
poisson_zi1_area_age_brand <- glmmTMB(ClaimNbPartColl ~ DrivAge +
                                        VehBrand + (1|Area) + 
                                        offset(log(ExposColl)),
                                      ziformula = ~1,
                                      data = clean_freq,
                                      family = poisson()
)
poisson_zi1_area_vehyear_brand <- glmmTMB(ClaimNbPartColl ~ VehYear +
                                            VehBrand + (1|Area) + 
                                            offset(log(ExposColl)),
                                          ziformula = ~1,
                                          data = clean_freq,
                                          family = poisson()
)


# Seperate Model for P(N = 0)
poisson_zi2_area <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                    VehYear + VehBrand + (1|Area) + 
                                    offset(log(ExposColl)),
                                  ziformula = ~.,
                                  data = clean_freq,
                                  family = poisson()
                                  )
# singular, no convergence
poisson_zi2_state <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                               VehYear + VehBrand + (1|StateAb) + 
                               offset(log(ExposColl)),
                             ziformula = ~.,
                             data = clean_freq,
                             family = poisson()
)
# 3 parameters 1 effect
# Area
poisson_zi2_area_no_brand <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                       VehYear + (1|Area) + 
                                       offset(log(ExposColl)),
                                     ziformula = ~.,
                                     data = clean_freq,
                                     family = poisson()
) 
poisson_zi2_area_no_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                         VehBrand + (1|Area) + 
                                         offset(log(ExposColl)),
                                       ziformula = ~.,
                                       data = clean_freq,
                                       family = poisson()
) 

# singular, no convergence
poisson_zi2_area_no_age <- glmmTMB(ClaimNbPartColl ~ Gender + VehYear + 
                                     VehBrand + (1|Area) + 
                                     offset(log(ExposColl)),
                                   ziformula = ~.,
                                   data = clean_freq,
                                   family = poisson()
) 

# singular, no convergence
poisson_zi2_area_no_gender <- glmmTMB(ClaimNbPartColl ~ DrivAge + VehYear + 
                                        VehBrand + (1|Area) + 
                                        offset(log(ExposColl)),
                                      ziformula = ~.,
                                      data = clean_freq,
                                      family = poisson()
)
# State
poisson_zi2_state_no_brand <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                        VehYear + (1|StateAb) + 
                                        offset(log(ExposColl)),
                                      ziformula = ~.,
                                      data = clean_freq,
                                      family = poisson()
) 

# singular, no convergence
poisson_zi2_state_no_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                          VehBrand + (1|StateAb) + 
                                          offset(log(ExposColl)),
                                        ziformula = ~.,
                                        data = clean_freq,
                                        family = poisson()
) 

# singular, no convergence
poisson_zi2_state_no_age <- glmmTMB(ClaimNbPartColl ~ Gender + VehYear + 
                                      VehBrand + (1|StateAb) + 
                                      offset(log(ExposColl)),
                                    ziformula = ~.,
                                    data = clean_freq,
                                    family = poisson()
) 

# singular, no convergence
poisson_zi2_state_no_gender <- glmmTMB(ClaimNbPartColl ~ DrivAge + VehYear + 
                                         VehBrand + (1|StateAb) + 
                                         offset(log(ExposColl)),
                                       ziformula = ~.,
                                       data = clean_freq,
                                       family = poisson()
)

# 2 parameters 1 effect
# Area
poisson_zi2_area_gender_age <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge +
                                         (1|Area) + offset(log(ExposColl)),
                                       ziformula = ~.,
                                       data = clean_freq,
                                       family = poisson()
)

# Failed to invert Hessian
poisson_zi2_area_gender_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + 
                                             VehYear + (1|Area) + 
                                             offset(log(ExposColl)),
                                           ziformula = ~.,
                                           data = clean_freq,
                                           family = poisson()
)
poisson_zi2_area_gender_brand <- glmmTMB(ClaimNbPartColl ~ Gender + 
                                           VehBrand + (1|Area) + 
                                           offset(log(ExposColl)),
                                         ziformula = ~.,
                                         data = clean_freq,
                                         family = poisson()
)
poisson_zi2_area_age_vehyear <- glmmTMB(ClaimNbPartColl ~ DrivAge +
                                          VehYear + (1|Area) + 
                                          offset(log(ExposColl)),
                                        ziformula = ~.,
                                        data = clean_freq,
                                        family = poisson()
)
poisson_zi2_area_age_brand <- glmmTMB(ClaimNbPartColl ~ DrivAge +
                                        VehBrand + (1|Area) + 
                                        offset(log(ExposColl)),
                                      ziformula = ~.,
                                      data = clean_freq,
                                      family = poisson()
)
poisson_zi2_area_vehyear_brand <- glmmTMB(ClaimNbPartColl ~ VehYear +
                                            VehBrand + (1|Area) + 
                                            offset(log(ExposColl)),
                                          ziformula = ~.,
                                          data = clean_freq,
                                          family = poisson()
)


# Negative Binomial GLMMS

# Regular NB GLMMs
nb_area <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + VehYear +
                     + VehBrand + (1|Area) + 
                     offset(log(ExposColl)),
                   data = clean_freq,
                   family = nbinom2
)
nb_state <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + VehYear +
                      + VehBrand + (1|StateAb) + offset(log(ExposColl)),
                    data = clean_freq,
                    family = nbinom2
)

# 3 parameters 1 effect
# Area
nb_area_no_brand <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                              VehYear + (1|Area) + 
                              offset(log(ExposColl)),
                            data = clean_freq,
                            family = nbinom2
) 
nb_area_no_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                VehBrand + (1|Area) + 
                                offset(log(ExposColl)),
                              data = clean_freq,
                              family = nbinom2
) 
nb_area_no_age <- glmmTMB(ClaimNbPartColl ~ Gender + VehYear + 
                            VehBrand + (1|Area) + 
                            offset(log(ExposColl)),
                          data = clean_freq,
                          family = nbinom2
) 
nb_area_no_gender <- glmmTMB(ClaimNbPartColl ~ DrivAge + VehYear + 
                               VehBrand + (1|Area) + 
                               offset(log(ExposColl)),
                             data = clean_freq,
                             family = nbinom2
)
# State
nb_state_no_brand <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                               VehYear + (1|StateAb) + 
                               offset(log(ExposColl)),
                             data = clean_freq,
                             family = nbinom2
) 
nb_state_no_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                 VehBrand + (1|StateAb) + 
                                 offset(log(ExposColl)),
                               data = clean_freq,
                               family = nbinom2
) 
nb_state_no_age <- glmmTMB(ClaimNbPartColl ~ Gender + VehYear + 
                             VehBrand + (1|StateAb) + 
                             offset(log(ExposColl)),
                           data = clean_freq,
                           family = nbinom2
) 
nb_state_no_gender <- glmmTMB(ClaimNbPartColl ~ DrivAge + VehYear + 
                                VehBrand + (1|StateAb) + 
                                offset(log(ExposColl)),
                              data = clean_freq,
                              family = nbinom2
)

# 2 parameters 1 effect
# Area
nb_area_gender_age <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge +
                                (1|Area) + offset(log(ExposColl)),
                              data = clean_freq,
                              family = nbinom2
)
nb_area_gender_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + 
                                    VehYear + (1|Area) + 
                                    offset(log(ExposColl)),
                                  data = clean_freq,
                                  family = nbinom2
)
nb_area_gender_brand <- glmmTMB(ClaimNbPartColl ~ Gender + 
                                  VehBrand + (1|Area) + 
                                  offset(log(ExposColl)),
                                data = clean_freq,
                                family = nbinom2
)
nb_area_age_vehyear <- glmmTMB(ClaimNbPartColl ~ DrivAge +
                                 VehYear + (1|Area) + 
                                 offset(log(ExposColl)),
                               data = clean_freq,
                               family = nbinom2
)
nb_area_age_brand <- glmmTMB(ClaimNbPartColl ~ DrivAge +
                               VehBrand + (1|Area) + 
                               offset(log(ExposColl)),
                             data = clean_freq,
                             family = nbinom2
)
nb_area_vehyear_brand <- glmmTMB(ClaimNbPartColl ~ VehYear +
                                   VehBrand + (1|Area) + 
                                   offset(log(ExposColl)),
                                 data = clean_freq,
                                 family = nbinom2
)
# Zero-inflated NB
# Constant probability for P(N = 0)
nb_zi1_area <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                         VehYear + VehBrand + (1|Area) + 
                         offset(log(ExposColl)),
                       ziformula = ~1,
                       data = clean_freq,
                       family = nbinom2
)
nb_zi1_state <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                          VehYear + VehBrand + (1|StateAb) + 
                          offset(log(ExposColl)),
                        ziformula = ~1,
                        data = clean_freq,
                        family = nbinom2
)
# 3 parameters 1 effect
# Area
nb_zi1_area_no_brand <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                  VehYear + (1|Area) + 
                                  offset(log(ExposColl)),
                                ziformula = ~1,
                                data = clean_freq,
                                family = nbinom2
) 
nb_zi1_area_no_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                    VehBrand + (1|Area) + 
                                    offset(log(ExposColl)),
                                  ziformula = ~1,
                                  data = clean_freq,
                                  family = nbinom2
) 
nb_zi1_area_no_age <- glmmTMB(ClaimNbPartColl ~ Gender + VehYear + 
                                VehBrand + (1|Area) + 
                                offset(log(ExposColl)),
                              ziformula = ~1,
                              data = clean_freq,
                              family = nbinom2
) 
nb_zi1_area_no_gender <- glmmTMB(ClaimNbPartColl ~ DrivAge + VehYear + 
                                   VehBrand + (1|Area) + 
                                   offset(log(ExposColl)),
                                 ziformula = ~1,
                                 data = clean_freq,
                                 family = nbinom2
)
# State
nb_zi1_state_no_brand <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                   VehYear + (1|StateAb) + 
                                   offset(log(ExposColl)),
                                 ziformula = ~1,
                                 data = clean_freq,
                                 family = nbinom2
) 
nb_zi1_state_no_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                     VehBrand + (1|StateAb) + 
                                     offset(log(ExposColl)),
                                   ziformula = ~1,
                                   data = clean_freq,
                                   family = nbinom2
) 

# Failed to converge
nb_zi1_state_no_age <- glmmTMB(ClaimNbPartColl ~ Gender + VehYear + 
                                 VehBrand + (1|StateAb) + 
                                 offset(log(ExposColl)),
                               ziformula = ~1,
                               data = clean_freq,
                               family = nbinom2
) 
nb_zi1_state_no_gender <- glmmTMB(ClaimNbPartColl ~ DrivAge + VehYear + 
                                    VehBrand + (1|StateAb) + 
                                    offset(log(ExposColl)),
                                  ziformula = ~1,
                                  data = clean_freq,
                                  family = nbinom2
)

# 2 parameters 1 effect
# Area
nb_zi1_area_gender_age <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge +
                                    (1|Area) + offset(log(ExposColl)),
                                  ziformula = ~1,
                                  data = clean_freq,
                                  family = nbinom2
)
nb_zi1_area_gender_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + 
                                        VehYear + (1|Area) + 
                                        offset(log(ExposColl)),
                                      ziformula = ~1,
                                      data = clean_freq,
                                      family = nbinom2
)
nb_zi1_area_gender_brand <- glmmTMB(ClaimNbPartColl ~ Gender + 
                                      VehBrand + (1|Area) + 
                                      offset(log(ExposColl)),
                                    ziformula = ~1,
                                    data = clean_freq,
                                    family = nbinom2
)
nb_zi1_area_age_vehyear <- glmmTMB(ClaimNbPartColl ~ DrivAge +
                                     VehYear + (1|Area) + 
                                     offset(log(ExposColl)),
                                   ziformula = ~1,
                                   data = clean_freq,
                                   family = nbinom2
)
nb_zi1_area_age_brand <- glmmTMB(ClaimNbPartColl ~ DrivAge +
                                   VehBrand + (1|Area) + 
                                   offset(log(ExposColl)),
                                 ziformula = ~1,
                                 data = clean_freq,
                                 family = nbinom2
)
nb_zi1_area_vehyear_brand <- glmmTMB(ClaimNbPartColl ~ VehYear +
                                       VehBrand + (1|Area) + 
                                       offset(log(ExposColl)),
                                     ziformula = ~1,
                                     data = clean_freq,
                                     family = nbinom2
)


# Seperate Model for P(N = 0)

# Failed to converge
nb_zi2_area <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                         VehYear + VehBrand + (1|Area) + 
                         offset(log(ExposColl)),
                       ziformula = ~.,
                       data = clean_freq,
                       family = nbinom2
)
# NB Part 1 cutoff

# Failed to converge
nb_zi2_state <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                          VehYear + VehBrand + (1|StateAb) + 
                          offset(log(ExposColl)),
                        ziformula = ~.,
                        data = clean_freq,
                        family = nbinom2
)
# 3 parameters 1 effect
# Area

# Failed to converge
nb_zi2_area_no_brand <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                  VehYear + (1|Area) + 
                                  offset(log(ExposColl)),
                                ziformula = ~.,
                                data = clean_freq,
                                family = nbinom2
) 
nb_zi2_area_no_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                    VehBrand + (1|Area) + 
                                    offset(log(ExposColl)),
                                  ziformula = ~.,
                                  data = clean_freq,
                                  family = nbinom2
) 

# Failed to converge
nb_zi2_area_no_age <- glmmTMB(ClaimNbPartColl ~ Gender + VehYear + 
                                VehBrand + (1|Area) + 
                                offset(log(ExposColl)),
                              ziformula = ~.,
                              data = clean_freq,
                              family = nbinom2
) 

# Failed to converge
nb_zi2_area_no_gender <- glmmTMB(ClaimNbPartColl ~ DrivAge + VehYear + 
                                   VehBrand + (1|Area) + 
                                   offset(log(ExposColl)),
                                 ziformula = ~.,
                                 data = clean_freq,
                                 family = nbinom2
)
# State
# The state models were not run because area models all failed
nb_zi2_state_no_brand <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                   VehYear + (1|StateAb) + 
                                   offset(log(ExposColl)),
                                 ziformula = ~.,
                                 data = clean_freq,
                                 family = nbinom2
) 
nb_zi2_state_no_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge + 
                                     VehBrand + (1|StateAb) + 
                                     offset(log(ExposColl)),
                                   ziformula = ~.,
                                   data = clean_freq,
                                   family = nbinom2
) 
nb_zi2_state_no_age <- glmmTMB(ClaimNbPartColl ~ Gender + VehYear + 
                                 VehBrand + (1|StateAb) + 
                                 offset(log(ExposColl)),
                               ziformula = ~.,
                               data = clean_freq,
                               family = nbinom2
) 
nb_zi2_state_no_gender <- glmmTMB(ClaimNbPartColl ~ DrivAge + VehYear + 
                                    VehBrand + (1|StateAb) + 
                                    offset(log(ExposColl)),
                                  ziformula = ~.,
                                  data = clean_freq,
                                  family = nbinom2
)

# 2 parameters 1 effect
# Area
nb_zi2_area_gender_age <- glmmTMB(ClaimNbPartColl ~ Gender + DrivAge +
                                    (1|Area) + offset(log(ExposColl)),
                                  ziformula = ~.,
                                  data = clean_freq,
                                  family = nbinom2
)

# Failed to converge
nb_zi2_area_gender_vehyear <- glmmTMB(ClaimNbPartColl ~ Gender + 
                                        VehYear + (1|Area) + 
                                        offset(log(ExposColl)),
                                      ziformula = ~.,
                                      data = clean_freq,
                                      family = nbinom2
)
nb_zi2_area_gender_brand <- glmmTMB(ClaimNbPartColl ~ Gender + 
                                      VehBrand + (1|Area) + 
                                      offset(log(ExposColl)),
                                    ziformula = ~.,
                                    data = clean_freq,
                                    family = nbinom2
)

# Failed to converge
nb_zi2_area_age_vehyear <- glmmTMB(ClaimNbPartColl ~ DrivAge +
                                     VehYear + (1|Area) + 
                                     offset(log(ExposColl)),
                                   ziformula = ~.,
                                   data = clean_freq,
                                   family = nbinom2
)
nb_zi2_area_age_brand <- glmmTMB(ClaimNbPartColl ~ DrivAge +
                                   VehBrand + (1|Area) + 
                                   offset(log(ExposColl)),
                                 ziformula = ~.,
                                 data = clean_freq,
                                 family = nbinom2
)
# Failed to converge
nb_zi2_area_vehyear_brand <- glmmTMB(ClaimNbPartColl ~ VehYear +
                                       VehBrand + (1|Area) + 
                                       offset(log(ExposColl)),
                                     ziformula = ~.,
                                     data = clean_freq,
                                     family = nbinom2
)


# AIC and BIC comparison
poisson_freq_models <- list(pois1 = poisson_zi2_area_vehyear_brand,
                            pois2 = poisson_zi2_area_age_brand,
                            pois3 = poisson_zi2_area_age_vehyear,
                            pois4 = poisson_zi2_area_gender_brand,
                            pois5 = poisson_zi2_area_gender_vehyear,
                            pois6 = poisson_zi2_area_gender_age,
                            pois7 = poisson_zi2_state_no_gender,
                            pois8 = poisson_zi2_state_no_age,
                            pois9 = poisson_zi2_state_no_vehyear,
                            pois10 = poisson_zi2_state_no_brand,
                            pois11 = poisson_zi2_area_no_gender,
                            pois12 = poisson_zi2_area_no_age,
                            pois13 = poisson_zi2_area_no_vehyear,
                            pois14 = poisson_zi2_area_no_brand,
                            pois15 = poisson_zi2_state,
                            pois16 = poisson_zi2_area,
                            pois17 = poisson_zi1_area_vehyear_brand,
                            pois18 = poisson_zi1_area_age_brand,
                            pois19 = poisson_zi1_area_age_vehyear,
                            pois20 = poisson_zi1_area_gender_brand,
                            pois21 = poisson_zi1_area_gender_vehyear,
                            pois22 = poisson_zi1_area_gender_age,
                            pois23 = poisson_zi1_state_no_gender,
                            pois24 = poisson_zi1_state_no_age,
                            pois25 = poisson_zi1_state_no_vehyear,
                            pois26 = poisson_zi1_state_no_brand,
                            pois27 = poisson_zi1_area_no_gender,
                            pois28 = poisson_zi1_area_no_age,
                            pois29 = poisson_zi1_area_no_vehyear,
                            pois30 = poisson_zi1_area_no_brand,
                            pois31 = poisson_zi1_state,
                            pois32 = poisson_zi1_area,
                            pois33 = poisson_area_vehyear_brand,
                            pois34 = poisson_area_age_brand,
                            pois35 = poisson_area_age_vehyear,
                            pois36 = poisson_area_gender_brand,
                            pois37 = poisson_area_gender_vehyear,
                            pois38 = poisson_area_gender_age,
                            pois39 = poisson_state_no_gender,
                            pois40 = poisson_state_no_age,
                            pois41 = poisson_state_no_vehyear,
                            pois42 = poisson_state_no_brand,
                            pois43 = poisson_state,
                            pois44 = poisson_area)
poisson_model_stats <- data.frame(
  Model = names(poisson_freq_models),
  AIC = sapply(poisson_freq_models, AIC),
  BIC = sapply(poisson_freq_models, BIC)
)
# Rank by AIC (Lowest is best)
poisson_rank_aic <- poisson_model_stats[order(poisson_model_stats$AIC), ]
# Top 5
# 16, 32, 27, 44, 31 

# Rank by BIC (Lowest is best)
poisson_rank_bic <- poisson_model_stats[order(poisson_model_stats$BIC), ]
# Top 5
# 16, 32, 27, 44, 14

# AIC and BIC comparison
nb_freq_models <- list(nb1 = nb_zi2_area_vehyear_brand,
                       nb2 = nb_zi2_area_age_brand,
                       nb3 = nb_zi2_area_age_vehyear,
                       nb4 = nb_zi2_area_gender_brand,
                       nb5 = nb_zi2_area_gender_vehyear,
                       nb6 = nb_zi2_area_gender_age,
                       # nb7 = nb_zi2_state_no_gender,
                       # nb8 = nb_zi2_state_no_age,
                       # nb9 = nb_zi2_state_no_vehyear,
                       # nb10 = nb_zi2_state_no_brand,
                       nb11 = nb_zi2_area_no_gender,
                       nb12 = nb_zi2_area_no_age,
                       nb13 = nb_zi2_area_no_vehyear,
                       nb14 = nb_zi2_area_no_brand,
                       nb15 = nb_zi2_state,
                       nb16 = nb_zi2_area,
                       nb17 = nb_zi1_area_vehyear_brand,
                       nb18 = nb_zi1_area_age_brand,
                       nb19 = nb_zi1_area_age_vehyear,
                       nb20 = nb_zi1_area_gender_brand,
                       nb21 = nb_zi1_area_gender_vehyear,
                       nb22 = nb_zi1_area_gender_age,
                       nb23 = nb_zi1_state_no_gender,
                       nb24 = nb_zi1_state_no_age,
                       nb25 = nb_zi1_state_no_vehyear,
                       nb26 = nb_zi1_state_no_brand,
                       nb27 = nb_zi1_area_no_gender,
                       nb28 = nb_zi1_area_no_age,
                       nb29 = nb_zi1_area_no_vehyear,
                       nb30 = nb_zi1_area_no_brand,
                       nb31 = nb_zi1_state,
                       nb32 = nb_zi1_area,
                       nb33 = nb_area_vehyear_brand,
                       nb34 = nb_area_age_brand,
                       nb35 = nb_area_age_vehyear,
                       nb36 = nb_area_gender_brand,
                       nb37 = nb_area_gender_vehyear,
                       nb38 = nb_area_gender_age,
                       nb39 = nb_state_no_gender,
                       nb40 = nb_state_no_age,
                       nb41 = nb_state_no_vehyear,
                       nb42 = nb_state_no_brand,
                       nb43 = nb_state,
                       nb44 = nb_area)
nb_model_stats <- data.frame(
  Model = names(nb_freq_models),
  AIC = sapply(nb_freq_models, AIC),
  BIC = sapply(nb_freq_models, BIC)
)
# Rank by AIC (Lowest is best)
nb_rank_aic <- nb_model_stats[order(nb_model_stats$AIC), ]
# Top 5
# 32, 44, 27, 31, 43

# Rank by BIC (Lowest is best)
nb_rank_bic <- nb_model_stats[order(nb_model_stats$BIC), ]
# Top
# 32, 44, 27, 31, 30


# Predictions
actual_freq <- clean_test_freq$ClaimNbPartColl

"rmse_freq" <- function(model, test_data) {
  actual_freq <- test_data$ClaimNbPartColl
  pred <- predict(model,
                  newdata = test_data,
                  type = "response",
                  allow.new.levels = TRUE)
  return(sqrt(mean((actual_freq - pred)^2)))
}

poisson_results <- tibble(
  model_id = names(poisson_freq_models),
  type = "Poisson",
  rmse = map_dbl(poisson_freq_models, ~rmse_freq(.x, clean_test_freq))
)
# Top 5
# 16, 32, 11, 44, 27
# Caution: 11 did not converge


nb_results <- tibble(
  model_id = names(nb_freq_models),
  type = "Negative Binomial",
  rmse = map_dbl(nb_freq_models, ~rmse_freq(.x, clean_test_freq))
)
# Top 5
# 16, 11, 32, 44, 27
# Caution: 11 and 16 have no AIC and BIC because failure to converge

# 3. Combine and Rank
ranked_freq_models <- bind_rows(poisson_results, nb_results) %>%
  arrange(rmse)
# Top 5
# pois16, nb16, pois32, pois11, pois44


# Final analysis and model selection: 

# The complex zero-inflated models are not significantly better. 
# They are not worth the added model complexity/ lack of parsimony.

# pois27, simpler versoin of pois11.
# Model    AIC    BIC
# pois27 196062 196399.1

# nb44 
# Model   AIC     BIC
# nb44 195273.8 195621.4 

# RMSE = 0.573 for both. Best models has RMSE 0.568

# nb44 has better AIC and BIC, thus choose nb44.
# ClaimNbPartColl ~ Gender + DrivAge + VehYear + VehBrand  + (1 | Area) 

# Severity Modelling ------------------------------------------------------

# State as random effect
gamma_state <- glmmTMB(ClaimAmountPartColl ~ Gender + DrivAge + VehYear +
                         VehBrand + (1|StateAb),
                       data = clean_sev,
                       family = Gamma(link = "log"))
lognormal_state <- glmmTMB(log(ClaimAmountPartColl) ~ Gender + DrivAge + 
                                    VehYear + VehBrand + (1|StateAb),
                                  data = clean_sev,
                                  family = gaussian())
# This model fails as VtV is not positive definite
inverse_gaussian_state <- glmer(ClaimAmountPartColl ~ Gender + 
                                  DrivAge + VehYear + VehBrand + 
                                  (1|StateAb),
                                data = clean_sev,
                                family = inverse.gaussian(link = "log"))

# Area as random effect
gamma_area <- glmmTMB(ClaimAmountPartColl ~ Gender + DrivAge + VehYear +
                         VehBrand + (1|Area),
                       data = clean_sev,
                       family = Gamma(link = "log"))
lognormal_area <- glmmTMB(log(ClaimAmountPartColl) ~ Gender + DrivAge + 
                             VehYear + VehBrand + (1|Area),
                           data = clean_sev,
                           family = gaussian())
# This model fails as VtV is not positive definite
inverse_gaussian_area <- glmer(ClaimAmountPartColl ~ Gender + 
                                  DrivAge + VehYear + VehBrand + 
                                  (1|Area),
                                data = clean_sev,
                                family = inverse.gaussian(link = "log"))

AIC(lognormal_area,lognormal_state, gamma_area, gamma_state)
BIC(lognormal_area,lognormal_state, gamma_area, gamma_state)
# Gamma GLMMs have AIC, BIC = 660k+
# Lognormal GLMMs have AIC, BIC = 110k+
# The likelihoods are much larger for lognormal.

# Gamma GLMMs
gamma_area_age_slope <- glmmTMB(ClaimAmountPartColl ~ Gender + DrivAge + 
                                  VehYear + VehBrand + 
                                  (1 + DrivAge|Area),
                      data = clean_sev,
                      family = Gamma(link = "log"))
gamma_area_vehyear_slope <- glmmTMB(ClaimAmountPartColl ~ Gender + 
                                      DrivAge + VehYear + VehBrand + 
                                  (1 + VehYear|Area),
                                data = clean_sev,
                                family = Gamma(link = "log"))
gamma_state_age_slope <- glmmTMB(ClaimAmountPartColl ~ Gender + DrivAge + 
                                  VehYear + VehBrand + 
                                  (1 + DrivAge|StateAb),
                                data = clean_sev,
                                family = Gamma(link = "log"))
gamma_state_vehyear_slope <- glmmTMB(ClaimAmountPartColl ~ Gender + 
                                      DrivAge + VehYear + VehBrand + 
                                      (1 + VehYear|StateAb),
                                    data = clean_sev,
                                    family = Gamma(link = "log"))
# Random Slopes do not seem to be worth it. The models AIC and BIC are
# barely different.

# Less parameters
# 3 parameters + 1 random effect
gamma_area_no_brand <- glmmTMB(ClaimAmountPartColl ~ Gender + DrivAge + 
                                 VehYear + (1 | Area),
                               data = clean_sev,
                               family = Gamma(link = "log"))
gamma_area_no_vehyear <- glmmTMB(ClaimAmountPartColl ~ Gender + DrivAge + 
                                 VehBrand + (1 | Area),
                               data = clean_sev,
                               family = Gamma(link = "log"))
gamma_area_no_drivage <- glmmTMB(ClaimAmountPartColl ~ Gender + VehYear + 
                                 VehBrand + (1 | Area),
                               data = clean_sev,
                               family = Gamma(link = "log"))
gamma_area_no_gender <- glmmTMB(ClaimAmountPartColl ~ DrivAge + VehYear + 
                                  VehBrand + (1 | Area),
                               data = clean_sev,
                               family = Gamma(link = "log"))

gamma_state_no_brand <- glmmTMB(ClaimAmountPartColl ~ Gender + DrivAge + 
                                 VehYear + (1 | StateAb),
                               data = clean_sev,
                               family = Gamma(link = "log"))
gamma_state_no_vehyear <- glmmTMB(ClaimAmountPartColl ~ Gender + DrivAge + 
                                   VehBrand + (1 | StateAb),
                                 data = clean_sev,
                                 family = Gamma(link = "log"))
gamma_state_no_drivage <- glmmTMB(ClaimAmountPartColl ~ Gender + VehYear + 
                                   VehBrand + (1 | StateAb),
                                 data = clean_sev,
                                 family = Gamma(link = "log"))
gamma_state_no_gender <- glmmTMB(ClaimAmountPartColl ~ DrivAge + VehYear + 
                                  VehBrand + (1 | StateAb),
                                data = clean_sev,
                                family = Gamma(link = "log"))
# 2 parameters + 1 random effect
# Area
gamma_area_gender_age <- glmmTMB(ClaimAmountPartColl ~ Gender + DrivAge + 
                                 (1 | Area),
                               data = clean_sev,
                               family = Gamma(link = "log"))
gamma_area_gender_vehyear <- glmmTMB(ClaimAmountPartColl ~ Gender + 
                                       VehYear + (1 | Area),
                                 data = clean_sev,
                                 family = Gamma(link = "log"))
gamma_area_gender_brand <- glmmTMB(ClaimAmountPartColl ~ Gender + 
                                     VehBrand + (1 | Area),
                                 data = clean_sev,
                                 family = Gamma(link = "log"))
gamma_area_age_vehyear <- glmmTMB(ClaimAmountPartColl ~ DrivAge + 
                                    VehYear + (1 | Area),
                                 data = clean_sev,
                                 family = Gamma(link = "log"))
gamma_area_age_brand <- glmmTMB(ClaimAmountPartColl ~ DrivAge + 
                                   VehBrand + (1 | Area),
                                 data = clean_sev,
                                 family = Gamma(link = "log"))
gamma_area_vehyear_brand <- glmmTMB(ClaimAmountPartColl ~ VehYear + 
                                   VehBrand + (1 | Area),
                                 data = clean_sev,
                                 family = Gamma(link = "log"))
# State
gamma_state_gender_age <- glmmTMB(ClaimAmountPartColl ~ Gender + DrivAge 
                                  + (1 | StateAb),
                                 data = clean_sev,
                                 family = Gamma(link = "log"))
gamma_state_gender_vehyear <- glmmTMB(ClaimAmountPartColl ~ Gender + 
                                       VehYear + (1 | StateAb),
                                     data = clean_sev,
                                     family = Gamma(link = "log"))
gamma_state_gender_brand <- glmmTMB(ClaimAmountPartColl ~ Gender + 
                                     VehBrand + (1 | StateAb),
                                   data = clean_sev,
                                   family = Gamma(link = "log"))
gamma_state_age_vehyear <- glmmTMB(ClaimAmountPartColl ~ DrivAge + 
                                    VehYear + (1 | StateAb),
                                  data = clean_sev,
                                  family = Gamma(link = "log"))
gamma_state_age_brand <- glmmTMB(ClaimAmountPartColl ~ DrivAge + 
                                  VehBrand + (1 | StateAb),
                                data = clean_sev,
                                family = Gamma(link = "log"))
gamma_state_vehyear_brand <- glmmTMB(ClaimAmountPartColl ~ VehYear + 
                                      VehBrand + (1 | StateAb),
                                    data = clean_sev,
                                    family = Gamma(link = "log"))
# 1 parameters + 1 random effect
# Area
gamma_area_gender <- glmmTMB(ClaimAmountPartColl ~ Gender + (1 | Area),
                                    data = clean_sev,
                                    family = Gamma(link = "log"))
gamma_area_age <- glmmTMB(ClaimAmountPartColl ~ DrivAge + (1 | Area),
                             data = clean_sev,
                             family = Gamma(link = "log"))
gamma_area_vehyear <- glmmTMB(ClaimAmountPartColl ~ VehYear + (1 | Area),
                          data = clean_sev,
                          family = Gamma(link = "log"))
gamma_area_vehbrand <- glmmTMB(ClaimAmountPartColl ~ VehBrand + (1 | Area),
                          data = clean_sev,
                          family = Gamma(link = "log"))
# State
gamma_state_gender <- glmmTMB(ClaimAmountPartColl ~ Gender + (1 | StateAb),
                             data = clean_sev,
                             family = Gamma(link = "log"))
gamma_state_age <- glmmTMB(ClaimAmountPartColl ~ DrivAge + (1 | StateAb),
                          data = clean_sev,
                          family = Gamma(link = "log"))
gamma_state_vehyear <- glmmTMB(ClaimAmountPartColl ~ VehYear + (1 | StateAb),
                              data = clean_sev,
                              family = Gamma(link = "log"))
gamma_state_vehbrand <- glmmTMB(ClaimAmountPartColl ~ VehBrand + (1 | StateAb),
                               data = clean_sev,
                               family = Gamma(link = "log"))


# Lognormal GLMMs

lognormal_area_state <- glmmTMB(log(ClaimAmountPartColl) ~ Gender + 
                                  DrivAge + VehYear + VehBrand + 
                                  (1|Area) + (1|StateAb),
                                data = clean_sev,
                                family = gaussian())
lognormal_area_age_slope <- glmmTMB(log(ClaimAmountPartColl) ~ Gender + 
                                  DrivAge + VehYear + VehBrand +
                                  (1 + DrivAge|Area),
                                data = clean_sev,
                                family = gaussian())
lognormal_area_vehyear_slope <- glmmTMB(log(ClaimAmountPartColl) ~ 
                                          Gender + DrivAge + VehYear + 
                                          VehBrand +
                                      (1 + VehYear|Area),
                                    data = clean_sev,
                                    family = gaussian())
lognormal_state_age_slope <- glmmTMB(log(ClaimAmountPartColl) ~ Gender + 
                                      DrivAge + VehYear + VehBrand +
                                      (1 + DrivAge|StateAb),
                                    data = clean_sev,
                                    family = gaussian())
lognormal_state_vehyear_slope <- glmmTMB(log(ClaimAmountPartColl) ~ 
                                          Gender + DrivAge + VehYear + 
                                          VehBrand +
                                          (1 + VehYear|StateAb),
                                        data = clean_sev,
                                        family = gaussian())
# The below 2 models fail to converge
lognormal_both_age_slope <- glmmTMB(log(ClaimAmountPartColl) ~ Gender + 
                                      DrivAge + VehYear + VehBrand +
                                      (1 + DrivAge|Area) + 
                                      (1 + DrivAge|StateAb),
                                    data = clean_sev,
                                    family = gaussian())
lognormal_both_vehyear_slope <- glmmTMB(log(ClaimAmountPartColl) ~ 
                                          Gender + DrivAge + VehYear + 
                                          VehBrand +
                                          (1 + VehYear|Area) + 
                                          (1 + VehYear|StateAb),
                                        data = clean_sev,
                                        family = gaussian())
AIC(lognormal_area, lognormal_area_age_slope, 
    lognormal_area_vehyear_slope, lognormal_area_state, 
    lognormal_both_age_slope, lognormal_both_vehyear_slope,
    lognormal_state, lognormal_state_age_slope, 
    lognormal_state_vehyear_slope)
BIC(lognormal_area, lognormal_area_age_slope, 
    lognormal_area_vehyear_slope, lognormal_area_state, 
    lognormal_both_age_slope, lognormal_both_vehyear_slope,
    lognormal_state, lognormal_state_age_slope, 
    lognormal_state_vehyear_slope)
# Models with both age and state fail to converge using just intercepts 
# or slopes and intercepts. 

# Less Parameters
# 3 parameters + 1 random effect
lognormal_area_no_brand <- glmmTMB(log(ClaimAmountPartColl) ~ Gender + 
                                     DrivAge + VehYear + (1 | Area),
                                   data = clean_sev,
                                   family = gaussian())
lognormal_area_no_vehyear <- glmmTMB(log(ClaimAmountPartColl) ~ Gender + 
                                       DrivAge + VehBrand + (1 | Area),
                                     data = clean_sev,
                                     family = gaussian())
lognormal_area_no_drivage <- glmmTMB(log(ClaimAmountPartColl) ~ Gender + 
                                       VehYear + VehBrand + (1 | Area),
                                     data = clean_sev,
                                     family = gaussian())
lognormal_area_no_gender <- glmmTMB(log(ClaimAmountPartColl) ~ DrivAge + 
                                      VehYear + VehBrand + (1 | Area),
                                    data = clean_sev,
                                    family = gaussian())

lognormal_state_no_brand <- glmmTMB(log(ClaimAmountPartColl) ~ Gender + 
                                      DrivAge + VehYear + (1 | StateAb),
                                    data = clean_sev,
                                    family = gaussian())
lognormal_state_no_vehyear <- glmmTMB(log(ClaimAmountPartColl) ~ Gender + 
                                        DrivAge + VehBrand + (1 | StateAb),
                                      data = clean_sev,
                                      family = gaussian())
lognormal_state_no_drivage <- glmmTMB(log(ClaimAmountPartColl) ~ Gender + 
                                        VehYear + VehBrand + (1 | StateAb),
                                      data = clean_sev,
                                      family = gaussian())
lognormal_state_no_gender <- glmmTMB(log(ClaimAmountPartColl) ~ DrivAge + 
                                       VehYear + VehBrand + (1 | StateAb),
                                     data = clean_sev,
                                     family = gaussian())
# 2 parameters + 1 random effect
# Area
lognormal_area_gender_age <- glmmTMB(log(ClaimAmountPartColl) ~ Gender + 
                                       DrivAge + (1 | Area),
                                     data = clean_sev,
                                     family = gaussian())
lognormal_area_gender_vehyear <- glmmTMB(log(ClaimAmountPartColl) ~ 
                                           Gender + VehYear + (1 | Area),
                                         data = clean_sev,
                                         family = gaussian())
lognormal_area_gender_brand <- glmmTMB(log(ClaimAmountPartColl) ~ 
                                         Gender + VehBrand + (1 | Area),
                                       data = clean_sev,
                                       family = gaussian())
lognormal_area_age_vehyear <- glmmTMB(log(ClaimAmountPartColl) ~ 
                                        DrivAge + VehYear + (1 | Area),
                                      data = clean_sev,
                                      family = gaussian())
lognormal_area_age_brand <- glmmTMB(log(ClaimAmountPartColl) ~ DrivAge + 
                                      VehBrand + (1 | Area),
                                    data = clean_sev,
                                    family = gaussian())
lognormal_area_vehyear_brand <- glmmTMB(log(ClaimAmountPartColl) ~ 
                                          VehYear + VehBrand + (1 | Area),
                                        data = clean_sev,
                                        family = gaussian())
# State
lognormal_state_gender_age <- glmmTMB(log(ClaimAmountPartColl) ~ Gender + 
                                        DrivAge + (1 | StateAb),
                                      data = clean_sev,
                                      family = gaussian())
lognormal_state_gender_vehyear <- glmmTMB(log(ClaimAmountPartColl) ~ 
                                            Gender + VehYear + 
                                            (1 | StateAb),
                                          data = clean_sev,
                                          family = gaussian())
lognormal_state_gender_brand <- glmmTMB(log(ClaimAmountPartColl) ~ 
                                          Gender + VehBrand + 
                                          (1 | StateAb),
                                        data = clean_sev,
                                        family = gaussian())
lognormal_state_age_vehyear <- glmmTMB(log(ClaimAmountPartColl) ~ 
                                         DrivAge + VehYear + (1 | StateAb),
                                       data = clean_sev,
                                       family = gaussian())
lognormal_state_age_brand <- glmmTMB(log(ClaimAmountPartColl) ~ DrivAge + 
                                       VehBrand + (1 | StateAb),
                                     data = clean_sev,
                                     family = gaussian())
lognormal_state_vehyear_brand <- glmmTMB(log(ClaimAmountPartColl) ~ 
                                           VehYear + VehBrand + 
                                           (1 | StateAb),
                                         data = clean_sev,
                                         family = gaussian())
# 1 parameters + 1 random effect
# Area
lognormal_area_gender <- glmmTMB(log(ClaimAmountPartColl) ~ Gender 
                                 + (1 | Area),
                                 data = clean_sev,
                                 family = gaussian())
lognormal_area_age <- glmmTMB(log(ClaimAmountPartColl) ~ DrivAge 
                              + (1 | Area),
                              data = clean_sev,
                              family = gaussian())
lognormal_area_vehyear <- glmmTMB(log(ClaimAmountPartColl) ~ VehYear 
                                  + (1 | Area),
                                  data = clean_sev,
                                  family = gaussian())
lognormal_area_vehbrand <- glmmTMB(log(ClaimAmountPartColl) ~ VehBrand 
                                   + (1 | Area),
                                   data = clean_sev,
                                   family = gaussian())
# State
lognormal_state_gender <- glmmTMB(log(ClaimAmountPartColl) ~ Gender 
                                  + (1 | StateAb),
                                  data = clean_sev,
                                  family = gaussian())
lognormal_state_age <- glmmTMB(log(ClaimAmountPartColl) ~ DrivAge 
                               + (1 | StateAb),
                               data = clean_sev,
                               family = gaussian())
lognormal_state_vehyear <- glmmTMB(log(ClaimAmountPartColl) ~ VehYear 
                                   + (1 | StateAb),
                                   data = clean_sev,
                                   family = gaussian())
lognormal_state_vehbrand <- glmmTMB(log(ClaimAmountPartColl) ~ VehBrand 
                                    + (1 | StateAb),
                                    data = clean_sev,
                                    family = gaussian())


# AIC and BIC for all models
gamma_severity_models <- list(gam1 = gamma_area,
                              gam2 = gamma_area_age, 
                              gam3 = gamma_area_age_brand,
                              gam4 = gamma_area_age_slope,
                              gam5 = gamma_area_age_vehyear,
                              gam6 = gamma_area_gender,
                              gam7 = gamma_area_gender_age,
                              gam8 = gamma_area_gender_brand,
                              gam9 = gamma_area_gender_vehyear,
                              gam10 = gamma_area_no_brand,
                              gam11 = gamma_area_no_drivage,
                              gam12 = gamma_area_no_gender,
                              gam13 = gamma_area_no_vehyear,
                              gam14 = gamma_area_vehbrand,
                              gam15 = gamma_area_vehyear,
                              gam16 = gamma_area_vehyear_brand,
                              gam17 = gamma_area_vehyear_slope,
                              gam18 = gamma_state, 
                              gam19 = gamma_state_age, 
                              gam20 = gamma_state_age_brand,
                              gam21 = gamma_state_age_slope,
                              gam22 = gamma_state_age_vehyear,
                              gam23 = gamma_state_gender,
                              gam24 = gamma_state_gender_age,
                              gam25 = gamma_state_gender_brand,
                              gam26 = gamma_state_gender_vehyear,
                              gam27 = gamma_state_no_brand,
                              gam28 = gamma_state_no_drivage,
                              gam29 = gamma_state_no_gender,
                              gam30 = gamma_state_no_vehyear,
                              gam31 = gamma_state_vehbrand,
                              gam32 = gamma_state_vehyear,
                              gam33 = gamma_state_vehyear_brand,
                              gam34 = gamma_state_vehyear_slope
                        )
gamma_model_stats <- data.frame(
  Model = names(gamma_severity_models),
  AIC = sapply(gamma_severity_models, AIC),
  BIC = sapply(gamma_severity_models, BIC),
  AICadj = sapply(gamma_severity_models, AIC),
  BICadj = sapply(gamma_severity_models, BIC)
)
# Rank by AIC (Lowest is best)
gamma_rank_aic <- gamma_model_stats[order(gamma_model_stats$AIC), ]
# Top 5
# 4, 17, 1, 12, 11 

# Rank by BIC (Lowest is best)
gamma_rank_bic <- gamma_model_stats[order(gamma_model_stats$BIC), ]
# Top 5
# 17, 4, 1, 12, 11

# 
log_sev_response <- sum(log(clean_sev$ClaimAmountPartColl))
"AICadj" <- function(model) {
  logliklihood <- as.numeric(logLik(model))
  adj_likelihood <- logliklihood - log_sev_response
  num_parameters <- length(model$fit$par) - 2
  return(-2*adj_likelihood + 2*num_parameters)
}
n_obs_sev <- nrow(clean_sev)
"BICadj" <- function(model) {
  logliklihood <- as.numeric(logLik(model))
  adj_likelihood <- logliklihood - log_sev_response
  num_parameters <- length(model$fit$par) - 2
  return(-2*adj_likelihood + log(n_obs_sev)*num_parameters)
}

lognormal_severity_models <- list(log1 = lognormal_area,
                                  log2 = lognormal_area_age, 
                                  log3 = lognormal_area_age_brand,
                                  log4 = lognormal_area_age_slope,
                                  log5 = lognormal_area_age_vehyear,
                                  log6 = lognormal_area_gender,
                                  log7 = lognormal_area_gender_age,
                                  log8 = lognormal_area_gender_brand,
                                  log9 = lognormal_area_gender_vehyear,
                                  log10 = lognormal_area_no_brand,
                                  log11 = lognormal_area_no_drivage,
                                  log12 = lognormal_area_no_gender,
                                  log13 = lognormal_area_no_vehyear,
                                  log14 = lognormal_area_vehbrand,
                                  log15 = lognormal_area_vehyear,
                                  log16 = lognormal_area_vehyear_brand,
                                  log17 = lognormal_area_vehyear_slope,
                                  log18 = lognormal_state, 
                                  log19 = lognormal_state_age, 
                                  log20 = lognormal_state_age_brand,
                                  log21 = lognormal_state_age_slope,
                                  log22 = lognormal_state_age_vehyear,
                                  log23 = lognormal_state_gender,
                                  log24 = lognormal_state_gender_age,
                                  log25 = lognormal_state_gender_brand,
                                  log26 = lognormal_state_gender_vehyear,
                                  log27 = lognormal_state_no_brand,
                                  log28 = lognormal_state_no_drivage,
                                  log29 = lognormal_state_no_gender,
                                  log30 = lognormal_state_no_vehyear,
                                  log31 = lognormal_state_vehbrand,
                                  log32 = lognormal_state_vehyear,
                                  log33 = lognormal_state_vehyear_brand,
                                  log34 = lognormal_state_vehyear_slope
)
lognormal_model_stats <- data.frame(
  Model = names(lognormal_severity_models),
  AIC = sapply(lognormal_severity_models, AIC),
  BIC = sapply(lognormal_severity_models, BIC),
  AICadj = sapply(lognormal_severity_models, AICadj),
  BICadj = sapply(lognormal_severity_models, BICadj)
)
# Rank by AIC (Lowest is best)
lognormal_rank_aic <- lognormal_model_stats[order(lognormal_model_stats$AIC), ]
# Top 5
# 4, 1, 17, 12, 11 

# Rank by BIC (Lowest is best)
lognormal_rank_bic <- lognormal_model_stats[order(lognormal_model_stats$BIC), ]
# Top 5
# 1, 17, 11, 12, 16

# Rank by Adj AIC (Lowest is best)
lognormal_rank_adj_aic <- lognormal_model_stats[order(lognormal_model_stats$AICadj), ]
# Top 5
# 4, 1, 17, 12, 11 

# Rank by Adj BIC (Lowest is best)
lognormal_rank_adj_bic <- lognormal_model_stats[order(lognormal_model_stats$BICadj), ]
# Top 5
# 1, 17, 11, 12, 16


# Predictions
actual_sev <- clean_test_sev$ClaimAmountPartColl
"lognormal_rmse_sev" <- function(model, test_data) { 
  actual_sev <- test_data$ClaimAmountPartColl
  pred <- predict(model,
                   newdata = test_data,
                   type = "response",
                   allow.new.levels = TRUE)
  sig2 <- sigma(model)^2
  pred_corrected <- pred * exp(sig2 / 2)
  return(sqrt(mean((actual_sev - pred_corrected)^2)))
}
"rmse_sev" <- function(model, test_data) {
  actual_sev <- test_data$ClaimAmountPartColl
  pred <- predict(model,
                  newdata = test_data,
                  type = "response",
                  allow.new.levels = TRUE)
  return(sqrt(mean((actual_sev - pred)^2)))
}

gamma_results <- tibble(
  model_id = names(gamma_severity_models),
  type = "Gamma",
  rmse = map_dbl(gamma_severity_models, ~rmse_sev(.x, clean_test_sev))
)

# 2. Calculate RMSE for Lognormal models
# Note: Using your specific lognormal function here
lognormal_results <- tibble(
  model_id = names(lognormal_severity_models),
  type = "Lognormal",
  rmse = map_dbl(lognormal_severity_models, ~lognormal_rmse_sev(.x, clean_test_sev))
)

# 3. Combine and Rank
ranked_sev_models <- bind_rows(gamma_results, lognormal_results) %>%
  arrange(rmse)

# Top 5
# gam17, gam1, gam4, gam12, gam11


# Gamma GLMMs far outperformed Lognormal GLMMs. All Gamma GLMMs 
# outpredicted all lognormal GLMMs

# Final model selection
# Model gam12 has comparable AIC and BIC and RMSE to gam1, gam4, gam17,
# while being much simpler. Random slopes are not justified, and not 
# all predictors are required for a good fit.

# gam12: ClaimAmountPartColl ~ DrivAge + VehYear + VehBrand + (1 | Area)


# Linear Models -----------------------------------------------------------

lm1 <- lm(log(ClaimAmountPartColl) ~ Gender + DrivAge + VehYear, data = clean_sev)
summary(lm1)

lm2 <- lm(log(ClaimAmountPartColl) ~ Gender + DrivAge + VehYear +
            VehBrand + Area + StateAb,
          data = clean_sev)
summary(lm2)

# Adjusted AIC and BIC
log_sev_response <- sum(log(clean_sev$ClaimAmountPartColl))
n_obs_sev <- nrow(clean_sev)

"AICadj_LM" <- function(model) {
  logliklihood <- as.numeric(logLik(model))
  adj_likelihood <- logliklihood - log_sev_response
  num_parameters <- length(model$coefficients)
  return(-2*adj_likelihood + 2*num_parameters)
}

"BICadj_LM" <- function(model) {
  logliklihood <- as.numeric(logLik(model))
  adj_likelihood <- logliklihood - log_sev_response
  num_parameters <- length(model$coefficients)
  return(-2*adj_likelihood + log(n_obs_sev)*num_parameters)
}

AICadj_LM(lm1)
BICadj_LM(lm1)

AICadj_LM(lm2)
BICadj_LM(lm2)
