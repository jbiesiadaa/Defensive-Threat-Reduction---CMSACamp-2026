## Modelling
library(tidyverse)
model_data_end <- readRDS("model_data_end502.rds")

# helper
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
# defending team id

model_data_end_binary  <- model_data_end |>
  group_by(match_id) |>
  mutate(
    defending_team_id = if_else(
      team_id == unique(team_id)[1],
      unique(team_id)[2],
      unique(team_id)[1]
    )
  ) |>
  ungroup()

# Remove observations where any relevant player is labeled Substitute or Goalkeeper
# Binary Outcome
# 0 = PTR = 0 → selected the highest-xThreat realistic option
# 1 = PTR > 0 → selected a lower-xThreat option

model_data_end_binary <- model_data_end |>
  filter(
    !carrier_position %in% c("Substitute", "Goalkeeper"),
    !best_option_position %in% c("Substitute", "Goalkeeper"),
    !targeted_position %in% c("Substitute", "Goalkeeper")) |>
  mutate(
    PTR_binary = as.integer(PTR > 0),
    
    carrier_position_group =
      group_position(carrier_position),
    
    best_option_position_group =
      group_position(best_option_position),
    
    targeted_position_group =
      group_position(targeted_position)
  ) |>
  droplevels()


# Working with XGboost
split_ids <- readRDS("match_split502.rds")

model_data_split <- model_data_end_binary |>
  left_join(split_ids, by = "match_id")

train_data <- model_data_split |>
  filter(split == "train")

valid_data <- model_data_split |>
  filter(split == "valid")

test_data <- model_data_split |>
  filter(split == "test")

library(tidyverse)
library(xgboost)

library(tidyverse)
library(xgboost)

# ==============================================================================
# 1. PREPARE THE SAME TRAINING DATA USED DURING CROSS-VALIDATION
# ==============================================================================

x_train <- train_data |>
  select(
    carrier_x_end, carrier_y_end, duration,
    carrier_position_group, game_state, organised_defense,
    inside_defensive_shape_start,
    
    nearest_def_dist,
    any_pressure, any_pressing,
    any_counter_press, any_recovery_press,
    
    n_passing_options_dangerous_not_difficult,
    n_passing_options_line_break,
    
    best_option_pass_distance_end,
    nearest_def_dist_best_option_end,
    n_within_5m_best_option_end,
    
    n_defenders_in_best_option_lane,
    min_dist_to_best_option_lane,
    
    dc_defmid_spread_end,
    PTR_binary,
    match_id
  )

# Create the XGBoost training matrix
train_matrix <- model.matrix(
  ~ . - PTR_binary - match_id - 1,
  data = x_train
)

dtrain <- xgb.DMatrix(
  data = train_matrix,
  label = x_train$PTR_binary
)

# ==============================================================================
# 2. LOAD THE SAVED CROSS-VALIDATION RESULTS
# ==============================================================================

cv_results <- readRDS("xgb_cv_results.rds")

# Select the combination with the highest CV AUC
best_result <- cv_results |>
  slice_max(
    order_by = cv_auc,
    n = 1,
    with_ties = FALSE
  )

best_result

# ==============================================================================
# 3. CREATE THE BEST PARAMETER LIST
# ==============================================================================

best_params <- list(
  objective        = "binary:logistic",
  eval_metric      = "auc",
  max_depth        = best_result$max_depth,
  eta              = best_result$eta,
  gamma            = best_result$gamma,
  colsample_bytree = best_result$colsample_bytree,
  min_child_weight = best_result$min_child_weight,
  subsample        = best_result$subsample
)

# ==============================================================================
# 4. FIT THE FINAL MODEL ON ALL TRAINING MATCHES
# ==============================================================================

set.seed(42)

xg_fit <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = best_result$nrounds,
  verbose = FALSE
)

# Save the fitted model so you do not need to fit it again
xgb.save(xg_fit, "PTR_xgboost_model.ubj")




# XG BOOST FIITING with FIRST
dtrain <- xgb.DMatrix(
  data  = model.matrix(~ . - PTR_binary - match_id - 1, data = x_train),
  label = as.numeric(x_train$PTR_binary == 1))

# Cross validation 1

# Define a grid of hyperparameters
xg_grid <- expand.grid(
  nrounds = seq(20, 150, 10),
  max_depth = c(2, 3, 4),
  eta = c(0.01, 0.05, 0.1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# Cross Validation 2
# Create 5 folds
# THE ONE CHANGE: folds built from match_id, whole games stay together
# ==============================================================================
set.seed(42)

# Assign each training match to one CV fold
match_fold_table <- tibble(
  match_id = sample(unique(x_train$match_id)),
  fold = rep(
    1:5,
    length.out = n_distinct(x_train$match_id)
  )
)

# Row indices held out in each fold
match_folds <- map(
  1:5,
  function(k) {
      fold_matches <- match_fold_table |>
      filter(fold == k) |>
      pull(match_id) which(x_train$match_id %in% fold_matches)})

# Cross Validation
library(purrr)
library(dplyr)

cv_results <- map_dfr(1:nrow(xg_grid), function(i) {   # map_dfr: rows -> ONE table
  
  # hyperparameters for this grid row
  params <- list(
    objective        = "binary:logistic",
    eval_metric      = "auc",
    max_depth        = xg_grid$max_depth[i],
    eta              = xg_grid$eta[i],
    gamma            = xg_grid$gamma[i],
    colsample_bytree = xg_grid$colsample_bytree[i],
    min_child_weight = xg_grid$min_child_weight[i],
    subsample        = xg_grid$subsample[i]
  )
  
  # 5-fold CV, folds = whole matches (no leakage between games)
  cv <- xgb.cv(
    params   = params,
    data     = dtrain,
    folds    = match_folds,
    nrounds  = xg_grid$nrounds[i],
    maximize = TRUE,
    verbose  = FALSE
  )
  
  # one row of results for this combo
  tibble(
    xg_grid[i, ],                                        # all hyperparams at once
    # AUC at the FINAL round (honest: evaluates exactly this nrounds,
    # unlike max() which peeks at every intermediate round)
    cv_auc    = tail(cv$evaluation_log$test_auc_mean, 1),
    # +/- across the 5 folds: tells you if top combos are truly different
    cv_auc_sd = tail(cv$evaluation_log$test_auc_std, 1)
  )
})

# best combos first
cv_results |> arrange(desc(cv_auc)) |> head(10)

# save: the record of the tuning run (pairs with cv_fold_table.rds)
saveRDS(cv_results, "xgb_cv_results.rds")

xgb_cv_results <- readRDS("xgb_cv_results.rds")

# Best Overall Configuration
# Find the hyperparameter combination with the highest CV AUC
best_result <- cv_results |>
  slice_max(
    order_by = cv_auc,
    n = 1,
    with_ties = FALSE
  )

best_result

# Choose the best parameters
best_params <- list(
  objective        = "binary:logistic",
  eval_metric      = "auc",
  max_depth        = best_result$max_depth,
  eta              = best_result$eta,
  gamma            = best_result$gamma,
  colsample_bytree = best_result$colsample_bytree,
  min_child_weight = best_result$min_child_weight,
  subsample        = best_result$subsample
)

# FIT XGBOOST
set.seed(42)

xg_fit <- xgb.train(
  params   = best_params,
  data     = dtrain,
  nrounds  = best_result$nrounds,
  verbose  = FALSE
)


# Variable Importance
library(vip)
xg_fit |> 
  vip()

# Brier score
x_train |>
  mutate(ptr_prob = predict(xg_fit, newdata = dtrain)) |>
  summarize(brier_score = mean((ptr_prob - PTR_binary)^2))

# AUC 
library(pROC)

ptr_xgroc <- x_train |>
  mutate(
    PTR_prob = predict(xg_fit, dtrain)
  ) |>
  roc(PTR_binary, PTR_prob)

ptr_xgroc$auc

############
library(dplyr)
library(xgboost)
library(pROC)

# Use the same variables as the training data
x_valid <- valid_data |>
  select(all_of(names(x_train)))

# Create training and validation predictor matrices
train_matrix <- model.matrix(
  ~ . - PTR_binary - match_id - 1,
  data = x_train
)

valid_matrix <- model.matrix(
  ~ . - PTR_binary - match_id - 1,
  data = x_valid
)

# Make validation columns match the training columns
missing_cols <- setdiff(colnames(train_matrix), colnames(valid_matrix))

for (col in missing_cols) {
  valid_matrix <- cbind(
    valid_matrix,
    setNames(rep(0, nrow(valid_matrix)), col)
  )
}

valid_matrix <- valid_matrix[
  , colnames(train_matrix),
  drop = FALSE
]

# Create validation XGBoost data
dvalid <- xgb.DMatrix(
  data = valid_matrix,
  label = x_valid$PTR_binary
)

# Predict probability that PTR_binary = 1
valid_predictions <- x_valid |>
  mutate(
    pred_non_optimal = predict(xg_fit, dvalid),
    
    # Convert probability into a 0/1 prediction
    predicted_class = if_else(
      pred_non_optimal >= 0.50,
      1L,
      0L
    )
  )

# Validation AUC
valid_roc <- roc(
  valid_predictions$PTR_binary,
  valid_predictions$pred_non_optimal
)

valid_roc$auc

# Validation Brier score
mean(
  (
    valid_predictions$PTR_binary -
      valid_predictions$pred_non_optimal
  )^2
)

# Confusion matrix
table(
  Actual = valid_predictions$PTR_binary,
  Predicted = valid_predictions$predicted_class
)

# Accuracy:    71.5%
# Recall:      80.9%
# Specificity: 59.6%
# Precision:   71.8%
# F1 score:    76.0%

# SHAP VALUES

shap_values <- predict(
  xg_fit,
  dtrain,
  predcontrib = TRUE
)

library(dplyr)
library(ggplot2)
# install.packages("shapviz")   # once
library(shapviz)

#
# PART 1 -- SHAP: what shape does the headline feature have?

library(shapviz)
library(dplyr)
library(ggplot2)

# Positive SHAP value -> pushes the prediction toward PTR_binary = 1
#        the player chooses a lower-threat / suboptimal option.

# Negative SHAP value -> pushes the prediction toward PTR_binary = 0
#         the player chooses the highest-threat realistic option.

# SHAP value near 0 -> that variable has little influence on that particular prediction.

# SHAP ANALYSIS

# Use a sample for faster SHAP calculations
set.seed(42)

shap_rows <- sample(
  seq_len(nrow(train_matrix)),
  size = min(10000, nrow(train_matrix))
)

shap_matrix <- train_matrix[shap_rows, , drop = FALSE]

# Calculate SHAP values from the fitted XGBoost model
sv <- shapviz(
  xg_fit,
  X_pred = shap_matrix
)

# 1. Overall importance, direction and spread
p_beeswarm <- sv_importance( 
  sv,
  kind = "beeswarm",
  max_display = 12
)

ggsave(
  "shap_beeswarm.png",
  plot = p_beeswarm,
  width = 9,
  height = 6,
  dpi = 300
)

# 2. Shape of best-option marking effect
p_dependence <- sv_dependence(
  sv,
  "nearest_def_dist_best_option_end",
  color_var = NULL
) +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  labs(
    x = "Nearest defender to best option (m)",
    y = "SHAP contribution to predicted log-odds",
    title = "Effect of marking the best passing option",
    subtitle = "Positive values push the model toward PTR > 0"
  )

ggsave(
  "shap_dependence_best_option.png",
  plot = p_dependence,
  width = 9,
  height = 6,
  dpi = 300
)

# 3. Possible interaction with pressure on the carrier
p_interaction <- sv_dependence(
  sv,
  "nearest_def_dist_best_option_end",
  color_var = "nearest_def_dist"
)

ggsave(
  "shap_dependence_interaction.png",
  plot = p_interaction,
  width = 9,
  height = 6,
  dpi = 300
)

# 4. Numerical summary by marking-distance band
shap_df <- tibble(
  distance =
    shap_matrix[, "nearest_def_dist_best_option_end"],
  
  shap =
    get_shap_values(sv)[
      , "nearest_def_dist_best_option_end"
    ]
)

shap_summary <- shap_df |>
  mutate(
    distance_band = cut(
      distance,
      breaks = c(0, 2, 4, 6, 8, 10, 15, Inf),
      include.lowest = TRUE
    )
  ) |>
  group_by(distance_band) |>
  summarise(
    n = n(),
    mean_shap = mean(shap, na.rm = TRUE),
    .groups = "drop"
  )

shap_summary
