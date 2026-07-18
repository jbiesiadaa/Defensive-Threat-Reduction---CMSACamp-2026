## 7. XGBoost Model Configuration
## Defines the XGBoost parameters, number of boosting rounds -> Which one is the best


library(tidyverse)
library(Matrix)     # sparse.model.matrix (speed change 4)
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

# defending team id

model_data_end_binary  <- model_data_end_binary |>
  group_by(match_id) |>
  mutate(
    defending_team_id = if_else(
      team_id == unique(team_id)[1],
      unique(team_id)[2],
      unique(team_id)[1]
    )
  ) |>
  ungroup()

# K = 5 Folds
set.seed(42)

N_FOLDS <- 5

match_folds <- model_data_end_binary |>
  distinct(match_id) |>
  mutate(
    fold = sample(
      rep(1:N_FOLDS, length.out = n())
    )
  )

ptr_data <- model_data_end_binary |>
  left_join(match_folds, by = "match_id")

# Check How many Folds
table(match_folds$fold)
table(ptr_data$fold)

# Save the fold assignments
saveRDS(
  match_folds,
  "PTR_match_folds.rds"
)
# Original Fold 1         Final test set
# Original Folds 2,3,4,5  Training and model tuning


library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(xgboost)
library(pROC)
library(vip)

set.seed(42)

# 1. CREATE TRAINING AND FINAL TEST DATA ---------------------------------------

# Fold 1 is hidden until final testing
train_data <- ptr_data |>
  filter(fold != 1)

test_data <- ptr_data |>
  filter(fold == 1)

# Check sizes
train_data |>
  summarise(
    observations = n(),
    matches = n_distinct(match_id)
  )

test_data |>
  summarise(
    observations = n(),
    matches = n_distinct(match_id)
  )

# Confirm no match appears in both datasets
shared_matches <- intersect(
  unique(train_data$match_id),
  unique(test_data$match_id)
)

stopifnot(length(shared_matches) == 0)


# 2. SELECT MODEL VARIABLES ----------------------------------------------------

model_variables <- c(
  "carrier_x_end",
  "carrier_y_end",
  "duration",
  "carrier_position_group",
  "game_state",
  "organised_defense",
  "inside_defensive_shape_start",
  "nearest_def_dist",
  "any_pressure",
  "any_pressing",
  "any_counter_press",
  "any_recovery_press",
  "n_passing_options_dangerous_not_difficult",
  "n_passing_options_line_break",
  "best_option_pass_distance_end",
  "nearest_def_dist_best_option_end",
  "n_within_5m_best_option_end",
  "n_defenders_in_best_option_lane",
  "min_dist_to_best_option_lane",
  "dc_defmid_spread_end"
)

x_train <- train_data |>
  select(
    all_of(model_variables),
    PTR_binary,
    match_id,
    fold
  )

x_test <- test_data |>
  select(
    all_of(model_variables),
    PTR_binary,
    match_id,
    fold
  )

# Confirm PTR_binary contains only 0 and 1
stopifnot(
  all(x_train$PTR_binary %in% c(0L, 1L)),
  all(x_test$PTR_binary %in% c(0L, 1L))
)

# Stop if any selected variable has missing values
stopifnot(
  sum(is.na(x_train)) == 0,
  sum(is.na(x_test)) == 0
)

# Check the percentage of positive outcomes
mean(x_train$PTR_binary)
mean(x_test$PTR_binary)


# 3. CREATE MODEL MATRICES -----------------------------------------------------

# Convert categorical variables into dummy variables
# PTR_binary, match_id, and fold are not predictors
# SPEED CHANGE 4: sparse.model.matrix() instead of model.matrix().
# Dummy-encoded categoricals are mostly zeros, so a sparse matrix is
# built faster, uses far less RAM, and feeds directly into xgb.DMatrix.
train_matrix <- sparse.model.matrix(
  ~ . - PTR_binary - match_id - fold - 1,
  data = x_train
)

test_matrix <- sparse.model.matrix(
  ~ . - PTR_binary - match_id - fold - 1,
  data = x_test
)

# Put columns in the same order
test_matrix <- test_matrix[
  ,
  colnames(train_matrix),
  drop = FALSE
]

# Confirm that the matrices match
stopifnot(
  identical(
    colnames(train_matrix),
    colnames(test_matrix)
  )
)


# 4. CREATE XGBOOST OBJECTS ----------------------------------------------------

dtrain <- xgb.DMatrix(
  data = train_matrix,
  label = x_train$PTR_binary
)

dtest <- xgb.DMatrix(
  data = test_matrix,
  label = x_test$PTR_binary
)


# 5. CREATE FOUR CROSS VALIDATION FOLDS ----------------------------------------

# Fold 1 is excluded because it is the final test set
cv_fold_numbers <- c(2, 3, 4, 5)

# Each list element contains held out validation rows
xgb_folds <- map(cv_fold_numbers, \(k) which(x_train$fold == k))
names(xgb_folds) <- paste0("Fold_", cv_fold_numbers)

# Check observations in each CV fold
map_int(xgb_folds, length)

# Confirm each training observation belongs to one CV fold
stopifnot(
  sum(lengths(xgb_folds)) == nrow(x_train),
  length(unique(unlist(xgb_folds))) == nrow(x_train),
  !any(x_train$fold == 1)
)


# 6. DEFINE THE HYPERPARAMETER GRID --------------------------------------------

# learning_rate is called eta inside XGBoost
xg_grid <- expand.grid(
  max_depth = c(3, 4, 5),
  learning_rate = c(0.05, 0.10),
  gamma = c(0, 0.5),
  min_child_weight = c(1, 5),
  subsample = c(0.8, 1),
  colsample_bytree = c(0.8, 1)
)

MAX_ROUNDS <- 300
EARLY_STOPPING <- 30

# Number of parameter combinations
nrow(xg_grid)

# For faster usage
N_THREADS <- max(1,parallel::detectCores() - 1)

# 7. RUN FOUR FOLD CROSS VALIDATION --------------------------------------------
set.seed(42)

cv_results_table <- pmap_dfr(
  xg_grid,
  function(max_depth, learning_rate, gamma,
           min_child_weight, subsample, colsample_bytree) {
    
    # Parameters for this model configuration
    params <- list(
      objective = "binary:logistic",
      eval_metric = "auc",
      tree_method = "hist", # makes the tree building algorithm faster.
      nthread = N_THREADS, # gives that algorithm more CPU resources
      max_depth = max_depth,
      eta = learning_rate,
      gamma = gamma,
      min_child_weight = min_child_weight,
      subsample = subsample,
      colsample_bytree = colsample_bytree
    )
    
    # Cross validation uses the existing match grouped folds
    cv <- xgb.cv(
      params = params,
      data = dtrain,
      folds = xgb_folds,
      nrounds = MAX_ROUNDS,
      early_stopping_rounds = EARLY_STOPPING,
      maximize = TRUE,
      verbose = FALSE
    )
    
    # Find the round with the highest validation AUC
    best_round <- which.max(
      cv$evaluation_log$test_auc_mean)
    
    tibble(
      max_depth = max_depth,
      learning_rate = learning_rate,
      gamma = gamma,
      min_child_weight = min_child_weight,
      subsample = subsample,
      colsample_bytree = colsample_bytree,
      nrounds = best_round,
      cv_auc = cv$evaluation_log$test_auc_mean[best_round],
      cv_auc_sd = cv$evaluation_log$test_auc_std[best_round]
    )
  }
) |>
  arrange(desc(cv_auc))

# 8. SELECT THE BEST CONFIGURATION ---------------------------------------------

# The first row has the highest cross validation AUC
best_parameters <- cv_results_table |>
  slice(1)

best_parameters
# > best_parameters
# A tibble: 1 × 9
# max_depth learning_rate gamma min_child_weight subsample colsample_bytree nrounds cv_auc cv_auc_sd
# <dbl>         <dbl> <dbl>            <dbl>     <dbl>            <dbl>   <int>  <dbl>     <dbl>
#   1         5           0.1     0                5       0.8                1     300  0.784   0.00313
 

# Create the parameter list for the final model
best_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  tree_method      = "hist",       # keep the same speed settings
  nthread          = N_THREADS,    # as used during tuning
  max_depth = best_parameters$max_depth[[1]],
  eta = best_parameters$learning_rate[[1]],
  gamma = best_parameters$gamma[[1]],
  min_child_weight = best_parameters$min_child_weight[[1]],
  subsample = best_parameters$subsample[[1]],
  colsample_bytree = best_parameters$colsample_bytree[[1]]
)

best_nrounds <- best_parameters$nrounds[[1]]

# Show the 15 best configurations
cv_results_table |>
  head(15)



# 9. CREATE OUT OF FOLD PREDICTIONS --------------------------------------------

# Each observation is predicted by a model that did not see it
# (predicted while in the held out validation fold).
# These predictions are used for calibration and threshold selection.

set.seed(42)

cv_best <- xgb.cv(
  params = best_params,
  data = dtrain,
  folds = xgb_folds,
  nrounds = best_nrounds,
  prediction = TRUE,
  maximize = TRUE,
  verbose = FALSE
)

# Current XGBoost versions
if (!is.null(cv_best$cv_predict$pred)) {
  oof_probability <- as.numeric(cv_best$cv_predict$pred)
  
  # Older XGBoost versions
} else if (!is.null(cv_best$pred)) {
  oof_probability <- as.numeric(cv_best$pred)
  
} else {
  stop("Out of fold predictions were not returned.")
}

stopifnot(
  length(oof_probability) == nrow(x_train)
)


# 10. CALIBRATE THE PREDICTED PROBABILITIES ------------------------------------

# The calibration curve showed the raw probabilities are under confident
# (compressed toward the middle). Isotonic regression fixes this by
# learning a monotone map from raw probability to observed rate.
# It is fitted ONLY on out of fold predictions, so no leakage.
# AUC is unaffected because the map is monotone.
ord <- order(oof_probability)

calibration_fit <- isoreg(
  x = oof_probability[ord],
  y = x_train$PTR_binary[ord]
)

calibrate <- approxfun(
  x = calibration_fit$x,
  y = calibration_fit$yf,
  yleft = min(calibration_fit$yf),
  yright = max(calibration_fit$yf),
  ties = mean
)

oof_calibrated <- calibrate(oof_probability)

# Re-run the check
tibble(
  brier_raw = mean((x_train$PTR_binary - oof_probability)^2),
  brier_calibrated = mean((x_train$PTR_binary - oof_calibrated)^2)
)

# 11. SELECT THE BEST CLASSIFICATION THRESHOLD ---------------------------------

# Chosen on CALIBRATED out of fold predictions, so fold 1 stays unseen

thresholds <- seq(0.01, 0.99, by = 0.01)

f1_by_threshold <- map_dbl(
  thresholds,
  function(threshold) {
    
    predicted_class <- as.integer(
      oof_calibrated >= threshold
    )
    
    tp <- sum(predicted_class == 1 & x_train$PTR_binary == 1)
    fp <- sum(predicted_class == 1 & x_train$PTR_binary == 0)
    fn <- sum(predicted_class == 0 & x_train$PTR_binary == 1)
    
    if (tp == 0) {
      return(0)
    }
    
    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
    
    2 * precision * recall / (precision + recall)
  }
)

best_threshold <- thresholds[
  which.max(f1_by_threshold)
]

best_threshold # 0.37

# Function to calculate classification metrics at different thresholds
compare_thresholds <- function(actual, probability) {
  
  map_dfr(c(0.37,0.45, 0.50, 0.60), function(threshold) {
    
    predicted <- as.integer(probability >= threshold)
    
    tp <- sum(predicted == 1 & actual == 1, na.rm = TRUE)
    tn <- sum(predicted == 0 & actual == 0, na.rm = TRUE)
    fp <- sum(predicted == 1 & actual == 0, na.rm = TRUE)
    fn <- sum(predicted == 0 & actual == 1, na.rm = TRUE)
    
    precision   <- tp / (tp + fp)
    recall      <- tp / (tp + fn)
    specificity <- tn / (tn + fp)
    
    tibble(
      threshold = threshold,
      accuracy = (tp + tn) / (tp + tn + fp + fn),
      precision = precision,
      recall = recall,
      specificity = specificity,
      f1 = 2 * precision * recall / (precision + recall),
      true_positive = tp,
      false_positive = fp,
      false_negative = fn,
      true_negative = tn
    )
  })
}

# Compare thresholds using calibrated out-of-fold predictions
threshold_comparison <- compare_thresholds(
  actual = x_train$PTR_binary,
  probability = oof_calibrated
)

threshold_comparison



# 12. FIT THE FINAL MODEL ------------------------------------------------------

# Fit on all training observations from Folds 2, 3, 4, and 5
set.seed(42)

xg_fit <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = best_nrounds,
  verbose = FALSE
)


# 13. PREDICT FINAL TEST FOLD 1 ------------------------------------------------

# Fold 1 is used for the first time here.
# The same calibration map (fitted on OOF only) is applied
# Use a fixed classification threshold
best_threshold <- 0.50


test_probability_raw <- predict(
  xg_fit,
  dtest
)

test_probability <- calibrate(
  test_probability_raw
)

test_results <- x_test |>
  mutate(
    predicted_probability_raw = test_probability_raw,
    predicted_probability = test_probability,
    predicted_class = if_else(
      predicted_probability >= best_threshold,
      1L,
      0L
    )
  )

summary(test_results$predicted_probability)

table(
  Actual = test_results$PTR_binary,
  Predicted = test_results$predicted_class)


# 14. FINAL TEST AUC -----------------------------------------------------------

test_roc <- roc(
  response = test_results$PTR_binary,
  predictor = test_results$predicted_probability,
  levels = c(0, 1),
  direction = "<",
  quiet = TRUE
)

test_auc <- as.numeric(
  auc(test_roc)
)

test_auc


# 15. FINAL TEST BRIER SCORE ---------------------------------------------------

# Compare raw vs calibrated on the untouched test fold.
# Calibrated should be equal or better. If calibrated is clearly WORSE,
# the calibration overfit: fall back to the raw probabilities.

test_brier_comparison <- tibble(
  brier_raw = mean(
    (test_results$PTR_binary - test_results$predicted_probability_raw)^2
  ),
  brier_calibrated = mean(
    (test_results$PTR_binary - test_results$predicted_probability)^2
  )
)

test_brier_comparison

test_brier <- test_brier_comparison$brier_calibrated


# 16. CONFUSION MATRIX AND CLASSIFICATION METRICS ------------------------------

safe_divide <- function(numerator, denominator) {
  if (denominator == 0) {
    return(NA_real_)
  }
  numerator / denominator
}

# One function computes all metrics at any threshold
metrics_at_threshold <- function(threshold, results) {
  
  pred <- as.integer(
    results$predicted_probability >= threshold
  )
  actual <- results$PTR_binary
  
  tp <- sum(pred == 1 & actual == 1)
  fp <- sum(pred == 1 & actual == 0)
  fn <- sum(pred == 0 & actual == 1)
  tn <- sum(pred == 0 & actual == 0)
  
  tibble(
    threshold = threshold,
    accuracy = (tp + tn) / length(actual),
    recall = safe_divide(tp, tp + fn),
    specificity = safe_divide(tn, tn + fp),
    precision = safe_divide(tp, tp + fp),
    f1_score = safe_divide(2 * tp, 2 * tp + fp + fn)
  )
}

# Confusion matrix at the selected threshold
test_confusion <- table(
  Actual = factor(test_results$PTR_binary, levels = c(0, 1)),
  Predicted = factor(test_results$predicted_class, levels = c(0, 1))
)

test_confusion


# Headline metrics table (probability based metrics + selected threshold)
test_metrics <- metrics_at_threshold(
  best_threshold,
  test_results
) |>
  mutate(
    auc = test_auc,
    brier_score = test_brier,
    .after = threshold
  )

test_metrics



# 17. VARIABLE IMPORTANCE ------------------------------------------------------

xgb.importance(
  model = xg_fit
)

xg_fit |>
  vip(
    num_features = 10)

# 18. SAVE MODEL AND SCORED POSSESSIONS ----------------------------------------

# Honest predictions:
# training rows use out-of-fold predictions
# test rows use predictions from unseen Fold 1
# Use calibrated out-of-fold and test probabilities
train_scored <- train_data |>
  mutate(
    expected_probability = oof_calibrated
  )

test_scored <- test_data |>
  mutate(
    expected_probability = test_probability
  )

# Combine honest predictions
scored_possessions <- bind_rows(
  train_scored,
  test_scored
) |>
  mutate(
    # Positive = player selected the best option more often than expected
    decision_above_expected =
      expected_probability - PTR_binary,
    
    # Positive = defense produced more lower-threat decisions than expected
    defense_above_expected =
      PTR_binary - expected_probability
  )

# Variable importance
importance_table <- xgb.importance(
  model = xg_fit
)

# Save the trained model
xgb.save(
  xg_fit,
  "PTR_xgboost_model.ubj"
)

# Save model settings and results
PTR_model_bundle <- list(
  
  # Exact model configuration
  best_parameters = best_parameters,
  best_nrounds = best_nrounds,
  best_threshold = best_threshold,
  feature_names = colnames(train_matrix),
  xgb_configuration = xgb.config(xg_fit),
  
  # Results
  cv_results_table = cv_results_table,
  test_metrics = test_metrics,
  test_confusion = test_confusion,
  test_brier_comparison = test_brier_comparison,
  importance_table = importance_table,
  scored_possessions = scored_possessions,
  
  # Reproducibility
  xgboost_version =
    as.character(packageVersion("xgboost"))
)

saveRDS(
  PTR_model_bundle,
  "PTR_model_bundle.rds"
)

# Save matrix used for SHAP
saveRDS(
  train_matrix,
  "PTR_train_matrix.rds"
)

message("Model, configuration, results, and SHAP matrix saved.")
