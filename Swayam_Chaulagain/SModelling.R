# Start modeling glmmTMB 

library(tidyverse)
library(glmmTMB)

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


# ==============================================================================
# PARAMETERIZATION FAMILIES (only features that exist in start/end/gain form)
# Start-only features with no twin (last_defensive_line_height_start,
# delta_to_last_defensive_line_start, inside_defensive_shape_start,
# any_goalside_start, any_close_at_start) stay in BOTH models.
# ==============================================================================
# ==============================================================================
# START VARIABLES
# Initial situation when the player receives possession
# ==============================================================================

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


# ==============================================================================
# END VARIABLES
# Situation at the moment of the passing decision
# ==============================================================================

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


# ==============================================================================
# GAIN VARIABLES
# Change between the start and the passing decision
# ==============================================================================

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






correlations_start_gain <- c(
  # duplicate defender distance
  "separation_start",
  "separation_gain",
  
  # redundant with x/y coordinates
  "dist_carrier_to_goal_start",
  "angle_carrier_to_goal_start",
  
  "dist_target_to_goal_start",
  "angle_target_to_goal_start",
  
  "dist_best_to_goal_start",
  "angle_option_to_goal_start",
  
  # choose spread OR surface area
  "team_surface_area",
  "team_surface_area_gain",
  "dc_defmid_surface_area",
  "dc_defmid_surface_area_gain",
  "nearest_surface_area",
  "nearest_surface_area_gain",
  "team_spread",
  "team_spread_gain",
  
  
  # redundant counts
  "n_passing_options",
  
  # derived geometry
  "delta_to_last_defensive_line_start",
  
  # highly related defender metric
  "second_nearest_def_dist_start",
  "second_nearest_def_dist",
  "second_nearest_def_dist_best_option",
  
  # derived from carrier position
  "last_defensive_line_height_start",
  
  "engagement_type_group", # related to PTR
  
  # would be True if n_engaments >0 
  "engaged",
  
  "lane_obstruction_diff", # basically calculated from min_dist_to_targeted_lane - min_dist_to_best_option_lane
  
  "lane_count_diff"
)



# Remove variables
model_data_start_gain <- model_data_start_gain |>
  select(-any_of(correlations_start_gain))

colnames(model_data_start_gain)



#Looking at numeric columns

model_data_end |>
  select(where(is.numeric)) |>
  colnames()






#.  .....Tier 1 model..............

 

library(tidyverse)
library(glmmTMB)

# ==========================================================
# Prepare data
library(tidyverse)
library(glmmTMB)
library(parallel)

# ==========================================================
# STEP 1: Prepare data
# ==========================================================
model_data_end_cl <- model_data_end |>
  mutate(
    player_id = factor(player_id),
    team_id   = factor(team_id),    # Factor level kept for fixed effect control
    match_id  = factor(match_id),
    across(where(is.character), factor)
  )

# ==========================================================
# STEP 2: Tier 1 predictors
# ==========================================================
t1_vars <- c(
  "duration",
  "channel_start",
  "third_start",
  "game_state",
  "organised_defense",
  "inside_defensive_shape_start",
  "nearest_def_dist",
  "n_passing_options_dangerous_not_difficult",
  "n_passing_options_dangerous_difficult",
  "n_passing_options_line_break",
  "n_passing_options_ahead", 
  "carrier_position"
)

# ==========================================================
# STEP 3: Position helper
# ==========================================================
pos_terms <- function(x, y) {
  paste(
    x,
    y,
    paste0("I(", x, "^2)"),
    paste0("I(", y, "^2)"),
    paste0(x, ":", y),
    sep = " + "
  )
}

# ==========================================================
# STEP 4: Build formula (Optimized Random Effects)
# ==========================================================
# We move team_id to a fixed effect because having only 29 teams 
# with 243k rows was causing optimizer instability (NaN standard errors).
rhs <- paste(
  pos_terms("carrier_x_end", "carrier_y_end"),
  paste(t1_vars, collapse = " + "),
  "team_id",             # Fixed effect: Controls for team style/bias safely
  "(1 | player_id)",     # Random effect: Grouped by the 815 individual players
  sep = " + "
)

f_t1 <- as.formula(paste("PTR ~", rhs))

# ==========================================================
# STEP 5: Fit model with Parallel CPU Processing
# ==========================================================
# Autodetect your CPU cores to drastically speed up the optimizer's math
num_cores <- min(parallel::detectCores() - 1, 4)
cat("Fitting stabilized Tier 1 model using", num_cores, "CPU cores...\n")

fit_t1 <- glmmTMB(
  formula = f_t1,
  data = model_data_end_cl,
  family = beta_family(),
  ziformula = ~1,
  control = glmmTMBControl(
    parallel = num_cores #  cuts down the 25-30 minute wait time
  )
)

# ==========================================================
# STEP 6: Print stabilized diagnostics
# ==========================================================
summary(fit_t1)
cat("AIC:", AIC(fit_t1), "\n")
cat("BIC:", BIC(fit_t1), "\n")




# ==========================================================
# STEP 6: Checking predicted vs observed PTR 
# ==========================================================
model_data_end_cl$predicted <- predict(fit_t1, type = "response")

# Alpha adjusted to 0.05 because 500 games will have ~250,000+ points
# rendering alpha=0.2 a solid block of black ink.
ggplot(model_data_end_cl, aes(x = predicted, y = PTR)) +
  geom_point(alpha = 0.05, color = "midnightblue") +
  geom_smooth(color = "red", method = "gam") +
  theme_minimal() +
  labs(title = "Predicted vs Observed PTR (500 Games)")




# ==========================================================
# STEP 7: 10-Fold Grouped Cross Validation
# ==========================================================


library(rsample)
library(foreach)
library(doParallel)

set.seed(123)
folds <- group_vfold_cv(model_data_end_cl, group = match_id, v = 10)

# Set up parallel cluster using 1 less than your maximum CPU cores
cores <- min(parallel::detectCores() - 1, 10) 
cl <- makeCluster(cores)
registerDoParallel(cl)

cat("\nStarting Parallel 10-Fold CV on", cores, "cores...\n")

# This distributes the 10 folds across your CPU cores to run simultaneously
rmse_vector <- foreach(
  i = 1:10, 
  .packages = c("glmmTMB", "rsample"), 
  .combine = 'c'
) %dopar% {
  
  # Split the data
  train_data <- analysis(folds$splits[[i]])
  test_data  <- assessment(folds$splits[[i]])
  
  # Fit model on training split 
  # Note: ziformula = ~1 keeps the zero-inflation model lightweight
  model <- glmmTMB(
    formula = f_t1,
    data = train_data,
    family = beta_family(),
    ziformula = ~1
  )
  
  # Predict on unseen test data
  preds <- predict(
    model, 
    newdata = test_data, 
    type = "response", 
    allow.new.levels = TRUE
  )
  
  # Return RMSE for this fold
  sqrt(mean((test_data$PTR - preds)^2, na.rm = TRUE))
}

# Always shut down the parallel workers when done!
stopCluster(cl)
registerDoSEQ()

# Print the final results
cat("\n--- CV Results (500 Games) ---\n")
print(rmse_vector)
cat("Mean CV RMSE:", mean(rmse_vector), "\n")

#  print(rmse_vector)
# 0.010379349 0.011313441 0.007328700 0.006847168 0.005753726
# mean(rmse_vector)  0.008324477

# ..........................................................









