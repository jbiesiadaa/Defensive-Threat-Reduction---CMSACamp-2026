library(tidyverse)
library(dplyr)
library(splines)
library(broom)
library(gtsummary)
library(pROC)
library(mgcv)
library(gt)
library(xgboost)
library(lme4)

analysis_clean <- readRDS("ptr_analysis_clean_502games.rds")


exclude_predictors <- c(
  # Identifiers
  "event_id",
  "player_name",
  "targeted_passing_option_event_id",
  "player_targeted_id",
  "best_option_event_id",
  "best_option_player_id",
  "frame_start",
  "frame_end",
  
  # PTR-construction variables
  "PTR_raw",
  "PTR_status",
  "chose_best_option",
  "player_targeted_xthreat",
  "max_xthreat_all",
  "best_was_tied",
  "is_overreach",
  "no_realistic_option",
  "chosen_pass_realistic",
  "player_targeted_xpass_completion",
  "n_options_counted",
  "n_options_na_completion",
  "engagement_zone",
  
  # Future outcomes
  "pass_outcome",
  "targeted_pass_successful",
  "lead_to_shot",
  "lead_to_goal",
  "any_beaten_by_possession",
  "any_beaten_by_movement",
  "stop_possession_danger",
  "reduce_possession_danger",
  "force_backward",
  "defensive_outcome",
  
  # Filtering variables
  "is_header",
  "hand_pass",
  "is_player_possession_start_matched",
  "is_player_possession_end_matched",
  "short_possession",
  "disruption_possession",
  "has_tracking",
  "attacking_side",
  
  # Variables excluded because of missing values
  "max_chain_length",
  "mean_engagement_speed",
  "min_engagement_distance",
  "defensive_structure",
  "n_defensive_lines",
  
  # Only for targeted
  "pass_distance",
  "pass_direction",
  "pass_range",
  "quick_pass",
  
  # Boolean
  "carrier_stationary",
  "target_stationary",
  "best_option_stationary")


# ==============================================================================
# CREATE PRELIMINARY MODEL DATA
# ==============================================================================

model_data <- analysis_clean |>
  select(
    -any_of(exclude_predictors)
  )



start_versions <- c(
  
  # Cooridnates
  "carrier_x_start",
  "carrier_y_start",
  
  "player_targeted_x_start",
  "player_targeted_y_start",
  
  "best_option_x_start",
  "best_option_y_start",
  
  # Defensive-team shape
  "team_surface_area",
  "team_spread",
  "nearest_surface_area",
  "nearest_spread",
  "dc_defmid_surface_area",
  "dc_defmid_spread",
  
  # Pressure around the carrier
  "separation_start",
  "second_nearest_def_dist",
  "n_within_5m",
  
  # Pressure around the best option
  "nearest_def_dist_best_option",
  "second_nearest_def_dist_best_option",
  "n_within_5m_best_option",
  
  # Pressure around the targeted option
  "nearest_def_dist_targeted",
  "n_within_5m_targeted",
  
  # Distance to goal
  "dist_carrier_to_goal_start",
  "dist_target_to_goal_start",
  "dist_best_to_goal_start",
  
  # Passing distances
  "targeted_pass_distance_start",
  "best_option_pass_distance_start",
  "targeted_vs_best_distance_start",
  
  # Angles to goal
  "angle_carrier_to_goal_start",
  "angle_target_to_goal_start",
  "angle_option_to_goal_start"
)



# END VARIABLES
end_versions <- c(
  
  # Coordinates
  
  "carrier_x_end",
  "carrier_y_end",
  
  "player_targeted_x_end",
  "player_targeted_y_end",
  
  "best_option_x_end",
  "best_option_y_end",
  
  # Defensive-team shape
  "team_surface_area_end",
  "team_spread_end",
  "nearest_surface_area_end",
  "nearest_spread_end",
  "dc_defmid_surface_area_end",
  "dc_defmid_spread_end",
  
  # Pressure around the carrier
  "separation_end",
  "nearest_def_dist_end",
  "second_nearest_def_dist_end",
  "n_within_5m_end",
  
  # Pressure around the best option
  "nearest_def_dist_best_option_end",
  "second_nearest_def_dist_best_option_end",
  "n_within_5m_best_option_end",
  
  # Pressure around the targeted option
  "nearest_def_dist_targeted_end",
  "n_within_5m_targeted_end",
  
  # Distance to goal
  "dist_carrier_to_goal_end",
  "dist_target_to_goal_end",
  "dist_best_to_goal_end",
  
  # Passing distances
  "targeted_pass_distance_end",
  "best_option_pass_distance_end",
  
  # Angles to goal
  "angle_carrier_to_goal_end",
  "angle_target_to_goal_end",
  "angle_option_to_goal_end"
)



# GAIN VARIABLES
# Change between the start and the passing decision


gain_versions <- c(
  # Change in defensive-team shape
  "team_surface_area_gain",
  "team_spread_gain",
  "nearest_surface_area_gain",
  "nearest_spread_gain",
  "dc_defmid_surface_area_gain",
  "dc_defmid_spread_gain",
  
  # Change in pressure around the carrier
  "separation_gain",
  "nearest_def_dist_gain",
  "second_nearest_def_dist_gain",
  "n_within_5m_gain",
  
  # Change in pressure around the best option
  "best_option_nearest_def_dist_gain",
  "best_option_second_nearest_def_dist_gain",
  "best_option_n_within_5m_gain",
  
  # Change in pressure around the targeted option
  "nearest_def_dist_targeted_gain",
  "n_within_5m_targeted_gain",
  
  # Change in passing distances
  "targeted_pass_distance_gain",
  "best_option_pass_distance_gain",
  
  # Carrier movement
  "carrier_move_forward",
  "carrier_move_lateral",
  "carrier_move_dist",
  "carrier_move_angle",
  
  # Target movement
  "target_run_forward",
  "target_run_lateral",
  "target_run_dist",
  "target_run_angle",
  
  # Best-option movement
  "best_option_run_forward",
  "best_option_run_lateral",
  "best_option_run_dist",
  "best_option_run_angle"
)

# end
model_data_end <- model_data |>
  select(-any_of(c(
    start_versions,
    gain_versions
  )))


# start_gain 

model_data_start_gain <- model_data |>
  select(-any_of(c(
    end_versions
  )))



# Exclude Correlations 
# checking their coorelation

library(corrr)

predictor_cols <- model_data_start_gain |> 
  select(where(is.numeric), -matches("PTR|event_id|frame|player_id|team_id")) 

corr_matrix <- correlate(predictor_cols, use  = "pairwise.complete.obs")


View(corr_matrix |> stretch() |> filter(abs(r) > 0.5, x != y) |> arrange(desc(abs(r))))

colnames(model_data_end)




correlations_end <- c(
  # duplicate defender distance
  "separation_end",
  
  # redundant with x/y coordinates
  "dist_carrier_to_goal_end",
  "angle_carrier_to_goal_end",
  
  "dist_target_to_goal_end",
  "angle_target_to_goal_end",
  
  "dist_best_to_goal_end",
  "angle_option_to_goal_end",
  
  # choose spread OR surface area
  "team_surface_area_end",
  "dc_defmid_surface_area_end",
  "nearest_surface_area_end",
  
  # redundant counts
  "n_passing_options",
  
  # derived geometry
  "delta_to_last_defensive_line_start",
  
  # highly related defender metric
  "second_nearest_def_dist_end",
  "second_nearest_def_dist_best_option_end",
  
  # derived from carrier position
  "last_defensive_line_height_start",
  
  "engagement_type_group",  # related to PTR
  
  # coorelated with dc_mid_spread so will just keep defensivemidfielders
  "team_spread_end" ,
  
  # would be True if n_engaments >0 
  "engaged",
  
  "lane_obstruction_diff", # basically calculated from min_dist_to_targeted_lane - min_dist_to_best_option_lane
  
  "lane_count_diff"
)

# Remove variables
model_data_end <- model_data_end |>
  select(-any_of(correlations_end))

colnames(model_data_end)

# Save your data table to your working directory
saveRDS(model_data_end, file = "model_data_end.rds")





Logistic_Model_Data <- readRDS("model_data_end.rds")


colnames(Logistic_Model_Data)





# logis.tic Regression 

# defending team id

Logistic_Model_Data <- Logistic_Model_Data |>
  group_by(match_id) |>
  mutate(
    defending_team_id = if_else(
      team_id == unique(team_id)[1],
      unique(team_id)[2],
      unique(team_id)[1]
    )
  ) |>
  ungroup()


# Group_position_function 

group_position <- function(position) {
  case_when(
    position == "Goalkeeper" ~ "Goalkeeper",
    position %in% c("Center Back", "Left Center Back", "Right Center Back") ~ "Center Back",
    position %in% c("Left Back", "Right Back", "Left Wing Back", "Right Wing Back") ~ "Wide Back",
    position %in% c("Defensive Midfield", "Left Defensive Midfield", "Right Defensive Midfield",
                    "Center Midfield", "Attacking Midfield") ~ "Midfield",
    position %in% c("Left Midfield", "Right Midfield", "Left Winger", "Right Winger") ~ "Winger",
    position %in% c("Center Forward", "Left Forward", "Right Forward") ~ "Forward",
    TRUE ~ "Other"
  )
}


# zero -> optimal pass
#if greater 0 --> not optimal pass

model_data_end_logis <- Logistic_Model_Data |>
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


colnames(model_data_end_logis)








logistic_variable_selection <- c(
  # Match context variables and ball Carrier
  "carrier_x_end", "carrier_y_end", "duration",
  "carrier_position_group", "game_state", "organised_defense",
  "inside_defensive_shape_start", 
  
  # Pressure Context on the Carrier
  "nearest_def_dist", "any_pressure","any_pressing","any_counter_press", "any_recovery_press",
  
  # Possible Variables Affecting Decision 
  "n_passing_options_dangerous_not_difficult", "n_passing_options_line_break",
  
  # Target Option vs Best Option
  "best_option_pass_distance_end", 
  "nearest_def_dist_best_option_end", 
  "n_within_5m_best_option_end", 
  
  # Passing Lane
  "n_defenders_in_best_option_lane", 
  "min_dist_to_best_option_lane",
  
  "dc_defmid_spread_end", 
  
  # response
  "PTR_binary",
  
  # random effects
  "carrier_position_group","player_id", "defending_team_id", "match_id")


model_data_end_logis <- model_data_end_logis |>
  select(any_of(logistic_variable_selection))



# Standarizing the numeric variables
model_data_end_logis_std <- model_data_end_logis |>
  mutate(duration_z = as.numeric(scale(duration)),
         nearest_def_dist_z= as.numeric(scale(nearest_def_dist)),
         n_passing_options_dangerous_not_difficult_z= as.numeric(scale(n_passing_options_dangerous_not_difficult)),
         n_passing_options_line_break_z = as.numeric(scale(n_passing_options_line_break)),
         best_option_pass_distance_end_z = as.numeric(scale(best_option_pass_distance_end)),
         nearest_def_dist_best_option_end_z = as.numeric(scale(nearest_def_dist_best_option_end)),
         n_within_5m_best_option_end_z= as.numeric(scale(n_within_5m_best_option_end)),
         n_defenders_in_best_option_lane_z = as.numeric(scale(n_defenders_in_best_option_lane)),
         min_dist_to_best_option_lane_z = as.numeric(scale(min_dist_to_best_option_lane)),
         dc_defmid_spread_end_z = as.numeric(scale(dc_defmid_spread_end))
         )



# ==============================================================================
# 1. Simple GLM with only linear terms, GAM (added carrier coordinates), GAMM and simple xgboost comparison
# ==============================================================================
set.seed(123)
N_FOLDS <- 5

# Convert player_id to factor (required for the random effect smooth)
model_data_cv <- model_data_end_logis_std |> 
  mutate(player_id = as.factor(player_id),
         defending_team_id = as.factor(defending_team_id),
         carrier_position_group = as.factor(carrier_position_group))

# Assign folds at the match level
match_folds <- model_data_cv |> 
  distinct(match_id) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

# Join fold assignments
model_data_cv <- model_data_cv |> 
  left_join(match_folds, by = "match_id")



#cv_variances <- list()

# 2. COMPARATIVE CROSS-VALIDATION FUNCTION
# ==============================================================================
ptr_cv <- function(x) {
  
  # Split into training and validation sets
  cv_train <- model_data_cv |> filter(fold != x)
  cv_test  <- model_data_cv |> filter(fold == x)
 
  
  logit_fit <- glm(
    PTR_binary ~ 
      duration_z +
      game_state +
      organised_defense +
      inside_defensive_shape_start +
      nearest_def_dist_z +
      any_pressure +
      any_pressing +
      any_counter_press +
      any_recovery_press +
      n_passing_options_dangerous_not_difficult_z +
      n_passing_options_line_break_z +
      best_option_pass_distance_end_z +
      nearest_def_dist_best_option_end_z +
      n_within_5m_best_option_end_z +
      n_defenders_in_best_option_lane_z +
      min_dist_to_best_option_lane_z +
      dc_defmid_spread_end_z,
    family = binomial(link = "logit"), 
    data = cv_train
  )
  

  
  gam_fit <- bam(
    PTR_binary ~ 
      s(carrier_x_end, carrier_y_end, k = 18) + 
      carrier_position_group +
      duration_z +
      game_state +
      organised_defense +
      inside_defensive_shape_start +
      nearest_def_dist_z +
      any_pressure +
      any_pressing +
      any_counter_press +
      any_recovery_press +
      n_passing_options_dangerous_not_difficult_z +
      n_passing_options_line_break_z +
      best_option_pass_distance_end_z +
      nearest_def_dist_best_option_end_z +
      n_within_5m_best_option_end_z +
      n_defenders_in_best_option_lane_z +
      min_dist_to_best_option_lane_z +
      dc_defmid_spread_end_z,
    family = binomial(link = "logit"),
    method = "fREML",
    discrete = TRUE,
    data = cv_train
  )
  
  
  gamm_fit <- bam(
    PTR_binary ~ 
      s(carrier_x_end, carrier_y_end,k = 18) + 
      duration_z +
      carrier_position_group+
      game_state +
      organised_defense +
      inside_defensive_shape_start +
      nearest_def_dist_z +
      any_pressure +
      any_pressing +
      any_counter_press +
      any_recovery_press +
      n_passing_options_dangerous_not_difficult_z +
      n_passing_options_line_break_z +
      best_option_pass_distance_end_z +
      nearest_def_dist_best_option_end_z +
      n_within_5m_best_option_end_z +
      n_defenders_in_best_option_lane_z +
      min_dist_to_best_option_lane_z +
      dc_defmid_spread_end_z +
      # s(carrier_position_group, bs = "re") +
      # s(player_id, bs = "re") + 
      s(player_id, bs = "re"),
      # s(defending_team_id, bs = "re"),   
    family = binomial(link = "logit"),
    method = "fREML",
    discrete = TRUE,
    data = cv_train
  )
  
  # Generate predictions on validation data
  cv_out <- tibble(
    logit_pred  = predict(logit_fit, newdata = cv_test, type = "response"),
    gam_pred    = predict(gam_fit, newdata = cv_test, type = "response"),
    gamm_pred   = predict(gamm_fit, newdata = cv_test, type = "response"),
    test_actual = cv_test$PTR_binary,
    test_fold   = x
  )
  
  return(cv_out)  
} 

  
# Run the map loop across all folds
cv_predictions <- map(1:N_FOLDS, ptr_cv) |> 
  list_rbind()

# Summarize metrics across all folds

cv_results<- cv_predictions |> 
  pivot_longer(
    cols = c(logit_pred, gam_pred, gamm_pred), 
    names_to = "model", 
    values_to = "test_pred"
  ) |> 
  group_by(model, test_fold) |>
  
  # Calculate metrics for each individual fold
  summarize(
    auc = as.numeric(auc(roc(test_actual, test_pred, quiet = TRUE))),
    accuracy = mean(as.integer(test_pred >= 0.5) == test_actual),
    
    # Precision = True Positives / (True Positives + False Positives)
    precision = {predicted_positives <- as.integer(test_pred >= 0.5)
      true_positives <- sum(predicted_positives == 1 & test_actual == 1)
      all_predicted_positives <- sum(predicted_positives == 1)
      
      # Handle cases where no positives are predicted to avoid dividing by zero
      if(all_predicted_positives == 0) 0 else true_positives / all_predicted_positives
    },
    .groups = "drop"
  ) |> 
  group_by(model) |> 
  
  # Summarize Mean and Standard Error across the 5 folds
  summarize(
    cv_auc      = mean(auc),
    se_auc      = sd(auc) / sqrt(N_FOLDS),
    cv_accuracy = mean(accuracy),
    se_accuracy = sd(accuracy) / sqrt(N_FOLDS),
    cv_precision = mean(precision),
    se_precision = sd(precision) / sqrt(N_FOLDS)
  )

# Print final comparative results including Precision
print(cv_results)


# Adding XGboost result from Julia

xgb_row <- tibble(
  model = "XGBoost",
  cv_auc = 0.783,
  cv_accuracy = 0.717,
  cv_precision = 0.719
)

# Combine with your existing CV results
table_data <- cv_results |>
  select(model, cv_auc, cv_accuracy, cv_precision) |>
  mutate(
    model = case_when(
      model == "logit_pred" ~ "Baseline Logistic Regression",
      model == "gam_pred"   ~ "Generalized Additive Model (GAM)",
      model == "gamm_pred"  ~ "Generalized Additive Mixed Model (GAMM)",
      TRUE ~ model
    )
  ) |>
  bind_rows(xgb_row) |>
  arrange(cv_auc)

presentation_table <- table_data |>
  gt() |>
  tab_header(
    title = md("**Comparing predictive performance**")
  ) |>
  cols_label(
    model = md("**Model Architecture**"),
    cv_auc = md("**Mean AUC**"),
    cv_accuracy = md("**Accuracy**"),
    cv_precision = md("**Precision**")
  ) |>
  fmt_percent(
    columns = c(cv_accuracy, cv_precision),
    decimals = 1
  ) |>
  fmt_number(
    columns = cv_auc,
    decimals = 3
  ) |>
  data_color(
    columns = c(cv_auc, cv_accuracy, cv_precision),
    direction = "column",
    palette = "Blues",
    alpha = 0.85
  ) |>
  tab_options(
    heading.title.font.size = px(20),
    column_labels.background.color = "#f9f9f9",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "#333333",
    table_body.border.bottom.color = "#333333",
    table_body.border.bottom.width = px(2),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    data_row.padding = px(12)
  ) |>
  tab_style(
    style = cell_text(color = "#222222"),
    locations = cells_body(columns = model)
  ) 

presentation_table

# Updated 

# Identify max values dynamically for automatic highlighting
max_auc       <- max(table_data$cv_auc, na.rm = TRUE)
max_accuracy  <- max(table_data$cv_accuracy, na.rm = TRUE)
max_precision <- max(table_data$cv_precision, na.rm = TRUE)

presentation_table <- table_data |>
  gt() |>
  tab_header(
    title = md("**Comparing Predictive Performance**")
  ) |>
  cols_label(
    model = md("**Model Architecture**"),
    cv_auc = md("**Mean AUC**"),
    cv_accuracy = md("**Accuracy**"),
    cv_precision = md("**Precision**")
  ) |>
  fmt_percent(
    columns = c(cv_accuracy, cv_precision),
    decimals = 1
  ) |>
  fmt_number(
    columns = cv_auc,
    decimals = 3
  ) |>
  # Bold model names
  tab_style(
    style = cell_text( color = "#222222"),
    locations = cells_body(columns = model)
  ) |>
  # Highlight highest value in Mean AUC
  tab_style(
    style = list(
      cell_fill(color = "#e0f2f1"),
      cell_text(weight = "bold", color = "#004d40")
    ),
    locations = cells_body(
      columns = cv_auc,
      rows = cv_auc == max_auc
    )
  ) |>
  # Highlight highest value in Accuracy
  tab_style(
    style = list(
      cell_fill(color = "#e0f2f1"),
      cell_text(weight = "bold", color = "#004d40")
    ),
    locations = cells_body(
      columns = cv_accuracy,
      rows = cv_accuracy == max_accuracy
    )
  ) |>
  # Highlight highest value in Precision
  tab_style(
    style = list(
      cell_fill(color = "#e0f2f1"),
      cell_text(weight = "bold", color = "#004d40")
    ),
    locations = cells_body(
      columns = cv_precision,
      rows = cv_precision == max_precision
    )
  ) |>
  # Enable subtle alternating row colors
  opt_row_striping() |>
  tab_options(
    table.font.size = 13,
    heading.title.font.size = px(18),
    column_labels.font.weight = "bold",
    column_labels.background.color = "#f4f8f7",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "#1b4943",
    table_body.border.bottom.color = "#1b4943",
    table_body.border.bottom.width = px(2),
    # Accent top border line (Teal)
    table.border.top.color = "#1b4943",
    table.border.top.width = px(4),
    table.border.bottom.color = "transparent",
    # Light gray/teal background for alternate rows
    row.striping.background_color = "#f8faf9",
    data_row.padding = px(10)
  )

# Display table
presentation_table




# Running the model for getting the variable coefficients on entire dataset

final_gamm_fit <- bam(
  PTR_binary ~ 
    s(carrier_x_end, carrier_y_end, k = 15) + 
    duration_z +
    game_state +
    carrier_position_group+
    organised_defense +
    inside_defensive_shape_start +
    nearest_def_dist_z +
    any_pressure +
    any_pressing +
    any_counter_press +
    any_recovery_press +
    n_passing_options_dangerous_not_difficult_z +
    n_passing_options_line_break_z +
    best_option_pass_distance_end_z +
    nearest_def_dist_best_option_end_z +
    n_within_5m_best_option_end_z +
    n_defenders_in_best_option_lane_z +
    min_dist_to_best_option_lane_z +
    dc_defmid_spread_end_z +
    # s(carrier_position_group, bs = "re") +
    # s(player_id, bs = "re") + 
    s(player_id, bs = "re"),
  # s(defending_team_id, bs = "re"),   
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = TRUE,
  data = model_data_cv
)

coef_raw_gamm <- tidy(final_gamm_fit, parametric = TRUE) |> 
  rename(
    Variable = term,
    Beta = estimate,
    SE = std.error,
    p_val = p.value
  ) |> 
  mutate(
    Odds_Ratio = exp(Beta),
    OR_lower   = exp(Beta - 1.96 * SE),
    OR_upper   = exp(Beta + 1.96 * SE)
  ) |> 
  filter(Variable != "(Intercept)")

# Clean variable names, filter for p < 0.05, and arrange globally by Odds Ratio
coef_clean_gamm <- coef_raw_gamm |> 
  mutate(
    Clean_Variable = case_when(
      Variable == "duration_z" ~ "Possession Duration (SD)",
      Variable == "carrier_position_groupForward" ~ "Carrier: Forward vs. Defender",
      Variable == "carrier_position_groupMidfielder" ~ "Carrier: Midfielder vs. Defender",
      Variable == "game_statewinning" ~ "Game State: Trailing vs. Tied",
      Variable == "game_statelosing" ~ "Game State: Leading vs. Tied",
      Variable == "organised_defenseTRUE" ~ "Organized Defensive Block (Yes)",
      Variable == "inside_defensive_shape_startTRUE" ~ "Carrier Inside Defensive Block (Yes)",
      Variable == "dc_defmid_spread_end_z" ~ "Defensive Midfield Spread (SD)",
      Variable == "nearest_def_dist_z" ~ "Distance to Nearest Defender (SD)",
      Variable == "any_pressureTRUE" ~ "Under Any Pressure (Yes)",
      Variable == "any_pressingTRUE" ~ "Under Active Pressing (Yes)",
      Variable == "any_counter_pressTRUE" ~ "Under Counter-Pressing (Yes)",
      Variable == "any_recovery_pressTRUE" ~ "Under Recovery Pressing (Yes)",
      Variable == "n_passing_options_dangerous_not_difficult_z" ~ "Num. Dangerous Passing Options (SD)",
      Variable == "n_passing_options_line_break_z" ~ "Num. Line-Breaking Options (SD)",
      Variable == "best_option_pass_distance_end_z" ~ "Distance to Best Passing Option (SD)",
      Variable == "nearest_def_dist_best_option_end_z" ~ "Defender Distance to Best Option (SD)",
      Variable == "n_within_5m_best_option_end_z" ~ "Opponents within 5m of Best Option (SD)",
      Variable == "n_defenders_in_best_option_lane_z" ~ "No. of Defenders in Best Option Passing Lane (SD)",
      Variable == "min_dist_to_best_option_lane_z" ~ "Min. Defender Distance to Pass Lane (SD)",
      TRUE ~ Variable
    )
  ) |> 
  # Keep only statistically significant predictors (p < 0.05)
  filter(p_val < 0.05) |> 
  # Select final clean columns (no Category variable)
  select(Clean_Variable, Odds_Ratio, OR_lower, OR_upper) |> 
  # Sort globally by Odds Ratio (from highest driver of suboptimal passes down to lowest)
  arrange(desc(Odds_Ratio))

# Generate the Suboptimal Coefficients Table
coefficients_table_suboptimal <- coef_clean_gamm |> 
  gt() |> 
  tab_header(
    title = md("**What Drives Suboptimal Pass Choices?**")
  ) |> 
  cols_label(
    Clean_Variable = md("**Tactical Metric**"),
    Odds_Ratio = md("**Odds Ratio (OR)**"),
    OR_lower = md("**95% CI Lower**"),
    OR_upper = md("**95% CI Upper**")
  ) |> 
  fmt_number(
    columns = c(Odds_Ratio, OR_lower, OR_upper),
    decimals = 2
  ) |> 
  # Green = factors that make players make more OPTIMAL choices (OR < 1)
  # Red = factors that force players to make SUBOPTIMAL choices (OR > 1)
  tab_style(
    style = cell_text(color = "#1b5e20", weight = "bold"),
    locations = cells_body(
      columns = c(Odds_Ratio),
      rows = Odds_Ratio < 1
    )
  ) |> 
  tab_style(
    style = cell_text(color = "#b71c1c", weight = "bold"),
    locations = cells_body(
      columns = c(Odds_Ratio),
      rows = Odds_Ratio > 1
    )
  ) |> 
  tab_options(
    column_labels.background.color = "#e9e9e9",
    column_labels.font.weight = "bold",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table_body.border.bottom.color = "#333333",
    table_body.border.bottom.width = px(2),
    data_row.padding = px(8)
  )

print(coefficients_table_suboptimal)


# ==============================================================================
# SECTION 2: OPTIMAL PASS CHOICES TABLE (LOGIT OBJECT)
# ==============================================================================

# Safely extract fixed parametric coefficients using broom::tidy
coef_raw_gamm <- tidy(final_gamm_fit, parametric = TRUE) |> 
  rename(
    Variable = term,
    Beta_suboptimal = estimate,
    SE = std.error,
    p_val = p.value
  ) |> 
  mutate(
    # Invert the Beta to represent the log-odds of an OPTIMAL pass (0)
    Beta_optimal = -Beta_suboptimal,
    # Calculate the inverted Odds Ratios
    Odds_Ratio = exp(Beta_optimal),
    OR_lower   = exp(Beta_optimal - 1.96 * SE),
    OR_upper   = exp(Beta_optimal + 1.96 * SE)
  ) |> 
  filter(Variable != "(Intercept)")

# Clean variable names, filter for p < 0.05, and arrange globally by Odds Ratio
coef_clean_gamm <- coef_raw_gamm |> 
  mutate(
    Clean_Variable = case_when(
      Variable == "duration_z" ~ "Possession Duration (SD)",
      Variable == "carrier_position_groupForward" ~ "Carrier: Forward vs. Defender",
      Variable == "carrier_position_groupMidfielder" ~ "Carrier: Midfielder vs. Defender",
      Variable == "game_statewinning" ~ "Game State: Trailing vs. Tied",
      Variable == "game_statelosing" ~ "Game State: Leading vs. Tied",
      Variable == "organised_defenseTRUE" ~ "Organized Defensive Block (Yes)",
      Variable == "inside_defensive_shape_startTRUE" ~ "Carrier Inside Defensive Block (Yes)",
      Variable == "dc_defmid_spread_end_z" ~ "Defensive Midfield Spread (SD)",
      Variable == "nearest_def_dist_z" ~ "Distance to Nearest Defender (SD)",
      Variable == "any_pressureTRUE" ~ "Under Any Pressure (Yes)",
      Variable == "any_pressingTRUE" ~ "Under Any Pressing (Yes)",
      Variable == "any_counter_pressTRUE" ~ "Under Counter-Pressing (Yes)",
      Variable == "any_recovery_pressTRUE" ~ "Under Recovery Pressing (Yes)",
      Variable == "n_passing_options_dangerous_not_difficult_z" ~ "Num. Dangerous Passing Options (SD)",
      Variable == "n_passing_options_line_break_z" ~ "Num. Line-Breaking Options (SD)",
      Variable == "best_option_pass_distance_end_z" ~ "Distance to Best Passing Option (SD)",
      Variable == "nearest_def_dist_best_option_end_z" ~ "Defender Distance to Best Option (SD)",
      Variable == "n_within_5m_best_option_end_z" ~ "Opponents within 5m of Best Option (SD)",
      Variable == "n_defenders_in_best_option_lane_z" ~ "No. of Defenders in Best Option Passing Lane (SD)",
      Variable == "min_dist_to_best_option_lane_z" ~ "Min. Defender Distance to Pass Lane (SD)",
      TRUE ~ Variable
    )
  ) |> 
  # Keep only statistically significant predictors (p < 0.05)
  filter(p_val < 0.05) |> 
  # Select final clean columns
  select(Clean_Variable, Odds_Ratio, OR_lower, OR_upper) |> 
  # Sort globally by Odds Ratio (from highest driver of optimal passes down to lowest)
  arrange(desc(Odds_Ratio))

# Isolate top and bottom 5 effects
coef_top_bottom <- bind_rows(
  head(coef_clean_gamm, 5), # Strongest 5 facilitators (OR >> 1)
  tail(coef_clean_gamm, 5)  # Strongest 5 suppressors (OR << 1)
)

# Generate the Optimal Coefficients Table
coefficients_table_optimal <- coef_top_bottom |> 
  gt() |> 
  tab_header(
    title = md("**What Drives Optimal High-Threat Pass Decisions?**"),
    subtitle = md("Metrics predicting a player choosing the *highest-threat* passing option")
  ) |> 
  cols_label(
    Clean_Variable = md("**Tactical Metric**"),
    Odds_Ratio = md("**Odds Ratio (OR)**"),
    OR_lower = md("**95% CI Lower**"),
    OR_upper = md("**95% CI Upper**")
  ) |> 
  fmt_number(
    columns = c(Odds_Ratio, OR_lower, OR_upper),
    decimals = 2
  ) |> 
  # Green = factors driving optimal choice (OR > 1)
  # Red = factors suppressing optimal choice (OR < 1)
  tab_style(
    style = cell_text(color = "#1b5e20", weight = "bold"),
    locations = cells_body(
      columns = c(Odds_Ratio),
      rows = Odds_Ratio > 1
    )
  ) |> 
  tab_style(
    style = cell_text(color = "#b71c1c", weight = "bold"),
    locations = cells_body(
      columns = c(Odds_Ratio),
      rows = Odds_Ratio < 1
    )
  ) |> 
  tab_options(
    column_labels.background.color = "#e9e9e9",
    column_labels.font.weight = "bold",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table_body.border.bottom.color = "#333333",
    table_body.border.bottom.width = px(2),
    data_row.padding = px(8)
  )

print(coefficients_table_optimal)




# Why random intercept?

library(mgcv)

# Variance of the player random intercept
vc <- gam.vcomp(final_gamm_fit)
vc

















# EDAs 



# Heatmaps of Optimal vs non-optimal area

library(ggplot2)

ggplot(model_data_end_logis, aes(x = carrier_x_end, y = carrier_y_end)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.6) +
  facet_wrap(~ PTR_binary, labeller = as_labeller(c(`0` = "Optimal", `1` = "Sub-Optimal"))) +
  scale_fill_viridis_c() +
  theme_minimal(base_size = 13) +
  labs(title = "Where on the Pitch Do Optimal vs. Sub-Optimal Decisions Happen?",
       x = "Pitch X", y = "Pitch Y", fill = "Density")


# Carrier's position and decision

plot(final_gamm_fit, select = 1, scheme = 2, main = "Spatial Decision Surface")



# Number of Events by Position
model_data_end_logis |>
  count(carrier_position_group) |>
  ggplot(aes(x = reorder(carrier_position_group, n), y = n)) +
  geom_col(fill = "#4a7c9e") +
  coord_flip() +
  theme_minimal(base_size = 13) +
  labs(title = "Number of Carrying Events by Position", x = NULL, y = "Count")



# Optimal Pass Rate by Position

library(dplyr)
library(ggplot2)
model_data_end_logis |>
  count(carrier_position_group, PTR_binary) |>
  mutate(PTR_binary = factor(PTR_binary, levels = c(0, 1),
                             labels = c("Optimal", "Sub-Optimal"))) |>
  ggplot(aes(x = reorder(carrier_position_group, n, sum), y = n, fill = PTR_binary)) +
  geom_col(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Optimal" = "#4a7c9e", "Sub-Optimal" = "#c0392b")) +
  theme_minimal(base_size = 13) +
  labs(title = "Optimal Pass Rate by Position",
       x = NULL, y = "% of possession", fill = NULL)




# ##
# 1508 - > Atlanta
# 1498 -> St. Jose Earthquake
# 1504 -> Vancouver
# 1503 -> Philly Union
# 863 -> Columbus Crew
# 1501 - > Real Salt Lake
# 1507 -> Nashville
# 1506 -> New York City Fc
# 2312 -> Charlotte
# 1500 -> New Enegland
# 884 -> DC United
# 1502 -> Toronto
# 885 -> Cincy 
# 862 -> Houston
# 1494 -> Miami 
# 1505 -> Monteral
# 337 -> Orlando
# 883 -> New York Red Bulls
# 1757 -> Austin
# 2906 -> St. Louis
# 336 -> Dallas
# 861 -> Minnesota
# 919 ->  Seatlle
# 1499 -> Colorado Rapids
# 860 -> Portland Timbers
# 1497 -> Sporting Kansas City
# 1495 -> LA Galaxy
# 1496-> Chicago Fire
# 918 -> LAFC














