# 7. Modelling
# Julia Biesiada

# Fit the model on the whole data 
# positive and negatice, checking the p-value
# do the diagnostic check
# covairte, std, beta postive or negative
library(tidyverse)

model_data_end <- readRDS("model_data_end502.rds")

# Helper group position 
group_position <- function(position) {
  case_when(
    position == "Goalkeeper" ~ "Goalkeeper",
    
    position %in% c(
      "Center Back",
      "Left Center Back",
      "Right Center Back"
    ) ~ "Center Back",
    
    position %in% c(
      "Left Back",
      "Right Back",
      "Left Wing Back",
      "Right Wing Back"
    ) ~ "Wide Back",
    
    position %in% c(
      "Defensive Midfield",
      "Left Defensive Midfield",
      "Right Defensive Midfield",
      "Center Midfield",
      "Attacking Midfield"
    ) ~ "Midfield",
    
    position %in% c(
      "Left Midfield",
      "Right Midfield",
      "Left Winger",
      "Right Winger"
    ) ~ "Winger",
    
    position %in% c(
      "Center Forward",
      "Left Forward",
      "Right Forward"
    ) ~ "Forward",
    
    TRUE ~ "Other"
  )
}

# Remove observations where any relevant player is labeled Substitute
model_data_end <- model_data_end |>
  filter(
    carrier_position != "Substitute",
    best_option_position != "Substitute",
    targeted_position != "Substitute"
  ) |>
  droplevels()

# Position Variables model_data_end
model_data_end <- model_data_end |>
  mutate(
    carrier_position_group =
      factor(group_position(carrier_position)),
    
    targeted_position_group =
      factor(group_position(targeted_position)),
    
    best_option_position_group =
      factor(group_position(best_option_position))
  )

model_data_end |>
  count(carrier_position_group, sort = TRUE)

model_data_end |>
  count(best_option_position_group, sort = TRUE)

model_data_end|>
  count(targeted_position_group)

# Split the Data
set.seed(42)
games <- sample(unique(model_data_end$match_id))
n     <- length(games)

# Spliting Games for training, validation and Test
split_ids <- tibble(
  match_id = games,
  split = c(rep("train", round(0.8 * n)),
            rep("valid", round(0.1 * n)),
            rep("test",  n - round(0.8 * n) - round(0.1 * n))))

# Saving 
saveRDS(split_ids, "match_split502.rds")

split_ids <- readRDS("match_split502.rds")

# Training Dataset Split + Prepearing Data
train <- model_data_end |>
  left_join(split_ids, by = "match_id") |>
  filter(split == "train") |>
  mutate(
    match_id = factor(match_id),
    player_id = factor(player_id), # Converting IDs into factors -> grouping categories
    across(where(is.character), factor),
    duration_z = as.numeric(scale(duration)), # Standardizing numeric variables
    n_options_realistic_z =                 # Standardization can help model convergence and makes coefficients more comparable
      as.numeric(scale(n_options_realistic)),
    max_xthreat_realistic_z =
      as.numeric(scale(max_xthreat_realistic))
  ) |>
  select(-split) |>
  droplevels() # Removing unused factor levels)


# Creating a Model
library(glmmTMB) # fits the zero-inflated beta mixed model, multiple crossed random effects
# ?family glmmTMB -> for a current list including details of parameterizations
# Distributions defined in ?glmmTMB::family glmmTMB 
# a zero-inflation model (via the ziformula argument) with fixed and/or random effects

library(mgcv)  # provides the smooth function s()

# 2. Tier 1 formula:  Passer & situation
# Spatial smooth -> carrier location as one flexible two-dimensional surface
# the model can learn that PTR behaves differently in different areas of the pitch
# k = 15 controls the maximum complexity of the smooth

# When PTR is positive, how large is it?
# What is the overall probability that PTR equals zero?

fit_t1 <- glmmTMB(
  PTR ~ 
    s(carrier_x_end, carrier_y_end, k = 15) + # For Future: Fit a sensitivity model with k = 20 or 25
    duration_z + # It controls for the fact that longer possessions give both defenders and attackers more time to move and react
    game_state + # This controls for different tactical behavior depending on the score
    organised_defense + 
    inside_defensive_shape_start +
    n_options_realistic_z +
    max_xthreat_realistic_z +
    carrier_position_group + # Do forwards tend to have different positive PTR values than midfielders, after controlling for location and game context?
    (1 | match_id) + # Match random effect -> prevents the model from treating every possession as fully independent
    (1 | player_id), # It accounts for repeated observations from the same player and individual differences in passing behavior
  
  # Begin with a simple zero component
  ziformula = ~ 1, # Zero-inflation model with ~ 1, the model estimates only one overall zero probability for the entire datas
  family = beta_family(link = "logit"), # The beta distribution is used for positive PTR because PTR is continuous and falls below 1, 
  data = train, # The logit link keeps predicted positive PTR values within the valid interval.
  REML = TRUE)

summary(fit_t1)

# Model Information:
# AIC       = -762071.6
# BIC       = -761725.7
# logLik    = 381069.8
# n         = 193680

# Dispersion parameter for beta family: 333

# Fixed Effects:
# Positive coefficient -> larger positive PTR
# Negative coefficient -> smaller positive PTR

saveRDS(fit_t1, "fit_t1.rds")

fit_t1$sdr$pdHess
glmmTMB::diagnose(fit_t1)
