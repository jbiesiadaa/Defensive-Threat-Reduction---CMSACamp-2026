# Start modeling glmmTMB 

library(tidyverse)
library(dplyr)
library(splines)
library(broom)
library(gtsummary)
library(pROC)
library(mgcv)

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



# 
# 
# 
# #.  .....Tier 1 model..............
# 
# library(parallel)
# 
# # ==========================================================
# # STEP 1: Prepare data
# # ==========================================================
# model_data_end_cl <- model_data_end |>
#   mutate(
#     player_id = factor(player_id),
#     team_id   = factor(team_id),    # Factor level kept for fixed effect control
#     match_id  = factor(match_id),
#     across(where(is.character), factor)
#   )
# 
# # ==========================================================
# # STEP 2: Tier 1 predictors
# # ==========================================================
# t1_vars <- c(
#   "duration",
#   "channel_start",
#   "third_start",
#   "game_state",
#   "organised_defense",
#   "inside_defensive_shape_start",
#   "n_passing_options_dangerous_not_difficult",
#   "n_passing_options_dangerous_difficult",
#   "n_passing_options_line_break",
#   "n_passing_options_ahead", 
#   "carrier_position"
# )
# 
# # ==========================================================
# # STEP 3: Position helper
# # ==========================================================
# pos_terms <- function(x, y) {
#   paste(
#     x,
#     y,
#     paste0("I(", x, "^2)"),
#     paste0("I(", y, "^2)"),
#     paste0(x, ":", y),
#     sep = " + "
#   )
# }
# 
# # ==========================================================
# # STEP 4: Build formula (Optimized Random Effects)
# # ==========================================================
# rhs <- paste(
#   pos_terms("carrier_x_end", "carrier_y_end"),
#   paste(t1_vars, collapse = " + "),
#   "team_id",             
#   "(1 | player_id)",     # Random effect
#   sep = " + "
# )
# 
# f_t1 <- as.formula(paste("PTR ~", rhs))
# 
# # ==========================================================
# # STEP 5: Fit model with Parallel CPU Processing
# # ==========================================================
# # Autodetect your CPU cores 
# num_cores <- min(parallel::detectCores() - 1, 4)
# cat("Fitting stabilized Tier 1 model using", num_cores, "CPU cores...\n")
# 
# fit_t1 <- glmmTMB(
#   formula = f_t1,
#   data = model_data_end_cl,
#   family = beta_family(),
#   ziformula = ~1,
#   control = glmmTMBControl(
#     parallel = num_cores 
#   )
# )
# 
# # ==========================================================
# # STEP 6: Print stabilized diagnostics
# # ==========================================================
# summary(fit_t1)
# cat("AIC:", AIC(fit_t1), "\n")
# cat("BIC:", BIC(fit_t1), "\n")



# 
# # ==========================================================
# # STEP 6: Checking predicted vs observed PTR 
# # ==========================================================
# model_data_end_cl$predicted <- predict(fit_t1, type = "response")
# 
# # Alpha adjusted to 0.05 because 500 games will have ~250,000+ points
# # rendering alpha=0.2 a solid block of black ink.
# ggplot(model_data_end_cl, aes(x = predicted, y = PTR)) +
#   geom_point(alpha = 0.05, color = "midnightblue") +
#   geom_smooth(color = "red", method = "gam") +
#   theme_minimal() +
#   labs(title = "Predicted vs Observed PTR (500 Games)")
# 
# 
# 
# 
# # ==========================================================
# # STEP 7: 10-Fold Grouped Cross Validation
# # ==========================================================
# 
# 
# library(rsample)
# library(foreach)
# library(doParallel)
# 
# set.seed(123)
# folds <- group_vfold_cv(model_data_end_cl, group = match_id, v = 10)
# 
# # Set up parallel cluster using 1 less than your maximum CPU cores
# cores <- min(parallel::detectCores() - 1, 10) 
# cl <- makeCluster(cores)
# registerDoParallel(cl)
# 
# cat("\nStarting Parallel 10-Fold CV on", cores, "cores...\n")
# 
# # This distributes the 10 folds across your CPU cores to run simultaneously
# rmse_vector <- foreach(
#   i = 1:10, 
#   .packages = c("glmmTMB", "rsample"), 
#   .combine = 'c'
# ) %dopar% {
#   
#   # Split the data
#   train_data <- analysis(folds$splits[[i]])
#   test_data  <- assessment(folds$splits[[i]])
#   
#   # Fit model on training split 
# #   # Note: ziformula = ~1 keeps the zero-inflation model lightweight
# #   model <- glmmTMB(
# #     formula = f_t1,
# #     data = train_data,
# #     family = beta_family(),
# #     ziformula = ~1
# #   )
# #   
# #   # Predict on unseen test data
# #   preds <- predict(
# #     model, 
# #     newdata = test_data, 
# #     type = "response", 
# #     allow.new.levels = TRUE
# #   )
# #   
# #   # Return RMSE for this fold
# #   sqrt(mean((test_data$PTR - preds)^2, na.rm = TRUE))
# # }
# # 
# # # Always shut down the parallel workers when done!
# # stopCluster(cl)
# # registerDoSEQ()
# # 
# # # Print the final results
# # cat("\n--- CV Results (500 Games) ---\n")
# # print(rmse_vector)
# # cat("Mean CV RMSE:", mean(rmse_vector), "\n")
# # 
# # #  print(rmse_vector)
# # # 0.010379349 0.011313441 0.007328700 0.006847168 0.005753726
# # # mean(rmse_vector)  0.008324477
# # 
# # # ..........................................................
# # 
# # 
# # 
# 
# # Updated t1
# # ==========================================================
# # 1. Load Libraries
# # ==========================================================
# library(tidyverse)
# library(glmmTMB)
# library(mgcv)
# library(parallel)
# library(rsample)
# library(foreach)
# library(doParallel)
# 
# # ==========================================================
# # 2. Helper function to group positions
# # ==========================================================
# group_position <- function(position) {
#   case_when(
#     position == "Goalkeeper" ~ "Goalkeeper",
#     position %in% c("Center Back", "Left Center Back", "Right Center Back") ~ "Center Back",
#     position %in% c("Left Back", "Right Back", "Left Wing Back", "Right Wing Back") ~ "Wide Back",
#     position %in% c("Defensive Midfield", "Left Defensive Midfield", "Right Defensive Midfield",
#                     "Center Midfield", "Attacking Midfield") ~ "Midfield",
#     position %in% c("Left Midfield", "Right Midfield", "Left Winger", "Right Winger") ~ "Winger",
#     position %in% c("Center Forward", "Left Forward", "Right Forward") ~ "Forward",
#     TRUE ~ "Other"
#   )
# }
# 
# # ==========================================================
# # 3. Clean, Group, and Scale Data Globally
# # ==========================================================
# model_data_end_cl <- model_data_end |>
#   filter(
#     carrier_position != "Substitute",
#     best_option_position != "Substitute",
#     targeted_position != "Substitute"
#   ) |>
#   mutate(
#     carrier_position_group = factor(group_position(carrier_position)),
#     targeted_position_group = factor(group_position(targeted_position)),
#     best_option_position_group = factor(group_position(best_option_position)),
#     team_id = factor(team_id),
#     match_id = factor(match_id),
#     player_id = factor(player_id),
#     across(where(is.character), factor),
# 
#     # Scale continuous variables globally to avoid prediction mismatches
#     duration_z = as.numeric(scale(duration)),
#     n_options_realistic_z = as.numeric(scale(n_options_realistic)),
#     max_xthreat_realistic_z = as.numeric(scale(max_xthreat_realistic))
#   ) |>
#   droplevels()
# 
# # ==========================================================
# # 4. Generate & Save / Load Match Splits
# # ==========================================================
#   set.seed(42)
#   games <- sample(unique(model_data_end_cl$match_id))
#   n <- length(games)
# 
#   split_ids <- tibble(
#     match_id = games,
#     split = c(rep("train", round(0.8 * n)),
#               rep("valid", round(0.1 * n)),
#               rep("test",  n - round(0.8 * n) - round(0.1 * n)))
#   )
#   saveRDS(split_ids, "match_split502.rds")
# 
# split_ids <- readRDS("match_split502.rds")
# 
# # Partition Training set
# train_data <- model_data_end_cl |>
#   left_join(split_ids, by = "match_id") |>
#   filter(split == "train") |>
#   select(-split) |>
#   droplevels()
# 
# # ==========================================================
# # 5. Model Formula Definition  "Passer & situation"
# # ==========================================================
# f_t1 <- PTR ~
#   s(carrier_x_end, carrier_y_end, k = 15) +  # Smooth terms in GAM Thin plate regression splines
#   duration_z +
#   game_state +
#   organised_defense +
#   inside_defensive_shape_start +
#   n_options_realistic_z +
#   max_xthreat_realistic_z +
#   carrier_position_group +
#   team_id +
#   (1 | player_id)  # random affect for repeated observation
# 
# # ==========================================================
# # 6. Fit Zero-Inflated Beta Model (Parallel CPUs)
# # ==========================================================
# num_cores <- min(parallel::detectCores() - 1, 4)
# cat("Fitting stabilized hybrid model using", num_cores, "CPU cores...\n")
# 
# fit_t1 <- glmmTMB(
#   formula = f_t1,
#   data = train_data,
#   family = beta_family(link = "logit"),
#   ziformula = ~ 1,
#   control = glmmTMBControl(parallel = num_cores),
#   REML = TRUE
# )
# 
# # Results
# summary(fit_t1)
# cat("AIC:", AIC(fit_t1), "\n")
# cat("BIC:", BIC(fit_t1), "\n")
# cat("Hessian Positive Definite:", fit_t1$sdr$pdHess, "\n")
# glmmTMB::diagnose(fit_t1)
# saveRDS(fit_t1, "fit_t1_hybrid.rds")
# 
# # ==========================================================
# # 7. Check Predicted vs Observed PTR
# # ==========================================================
# # Predict on the entire cleaned dataset (now safe because of global scaling)
# model_data_end_cl$predicted <- predict(fit_t1, newdata = model_data_end_cl, type = "response")
# 
# ggplot(model_data_end_cl, aes(x = predicted, y = PTR)) +
#   geom_point(alpha = 0.05, color = "midnightblue") +
#   geom_smooth(color = "red", method = "gam") +
#   theme_minimal() +
#   labs(
#     title = "Predicted vs Observed PTR (500 Games)",
#     x = "Predicted Passing Threat Reduction",
#     y = "Observed PTR"
#   )
# 
# # ==========================================================
# # 8. Parallel 10-Fold Grouped Cross-Validation
# # ==========================================================
# set.seed(123)
# folds <- group_vfold_cv(model_data_end_cl, group = match_id, v = 10)
# 
# cores_cv <- min(parallel::detectCores() - 1, 10)
# cl <- makeCluster(cores_cv)
# registerDoParallel(cl)
# 
# cat("\nStarting Parallel 10-Fold CV on", cores_cv, "cores...\n")
# 
# rmse_vector <- foreach(
#   i = 1:10,
#   .packages = c("glmmTMB", "rsample"),
#   .combine = 'c'
# ) %dopar% {
# 
#   train_fold <- analysis(folds$splits[[i]])
#   test_fold  <- assessment(folds$splits[[i]])
# 
#   model_fold <- glmmTMB(
#     formula = f_t1,
#     data = train_fold,
#     family = beta_family(link = "logit"),
#     ziformula = ~ 1
#   )
# 
#   preds <- predict(
#     model_fold,
#     newdata = test_fold,
#     type = "response",
#     allow.new.levels = TRUE
#   )
# 
#   sqrt(mean((test_fold$PTR - preds)^2, na.rm = TRUE))
# }
# 
# stopCluster(cl)
# registerDoSEQ()
# 
# # Print Final CV Performance Metrics
# cat("\n--- CV Results (500 Games) ---\n")
# print(rmse_vector)
# cat("Mean CV RMSE:", mean(rmse_vector), "\n")
# 
# 
# 
# 
# 












# logistic Regression Using single 80/10/10 split


# defending team id

model_data_end <- model_data_end |>
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

model_data_end_logis <- model_data_end |>
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
  "player_id", "defending_team_id", "match_id")


model_data_end_logis <- model_data_end_logis |>
  select(any_of(logistic_variable_selection))


model_data_end_logis |>
  select(where(is.numeric)) |>
  colnames()


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


# Running Logistic Regression 

# Split the matches

set.seed(42)

games <- sample(unique(model_data_end_logis_std$match_id))
n_games <- length(games)

split_ids <- tibble(
  match_id = games,
  split = c(
    rep("train", round(0.8 * n_games)),
    rep("valid", round(0.1 * n_games)),
    rep("test",n_games -
          round(0.8 * n_games) -
          round(0.1 * n_games))
  )
)

train_data <- model_data_end_logis_std |>
  left_join(split_ids, by="match_id") |>
  filter(split=="train") |>
  select(-split)

valid_data <- model_data_end_logis_std |>
  left_join(split_ids, by="match_id") |>
  filter(split=="valid") |>
  select(-split)

test_data <- model_data_end_logis_std |>
  left_join(split_ids, by="match_id") |>
  filter(split=="test") |>
  select(-split)




# Fitting the base logistic (no_random_effect)

logit_base <- gam(
  
  PTR_binary ~
    s(carrier_x_end, carrier_y_end, k = 15) +
    
    duration_z +
    
    carrier_position_group +
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
  data = train_data
)



# Looking at the coefficients

linear_coefs <- tidy(
  logit_base, 
  parametric = TRUE,     # <--- CRITICAL: Tells tidy() to ignore the spline for a moment
  exponentiate = TRUE, 
  conf.int = TRUE
)

print(linear_coefs, n=23)


# Looking AIC, BIC,deviance, adj.r.squared

glance(logit_base)
gam.check(logit_base)

tidy(logit_base, parametric = FALSE)
plot(logit_base, select = 1, scheme = 2, se = TRUE)  # or for a 3D/contour view of the carrier/optimal/non-optimal




# Predict on Validation 

valid_data$prob <- predict(
  logit_base,
  newdata = valid_data,
  type = "response"
)


# Roc curves

roc_obj <- roc(
  valid_data$PTR_binary,
  valid_data$prob
)

auc(roc_obj)

#.............Area under the curve: 0.754...................................................................





# using the cross validation


library(tidyverse)
library(mgcv)
library(pROC)

# ==============================================================================
# 5-Fold Group Cross-Validation (Grouped by match_id)
# ==============================================================================

# 1. Assign each unique match to one of 5 folds
set.seed(44)
unique_matches <- unique(model_data_end_logis_std$match_id)
n_folds <- 5

fold_assignments <- tibble(
  match_id = sample(unique_matches),
  fold = rep(1:n_folds, length.out = length(unique_matches))
)

# Join the fold assignments back to the main standardized dataset
cv_data <- model_data_end_logis_std |>
  left_join(fold_assignments, by = "match_id")

# Create a vector to store the validation AUC for each fold
cv_auc_results <- numeric(n_folds)

# 2. Loop through the folds
for (i in 1:n_folds) {
  message("--- Fitting Fold ", i, " of ", n_folds, " ---")
  
  # Split into CV Train and CV Validation for this fold
  cv_train <- cv_data |> filter(fold != i)
  cv_valid <- cv_data |> filter(fold == i)
  
  # Fit the base model on this fold's training data
  fit_fold <- gam(
    formula = PTR_binary ~
      s(carrier_x_end, carrier_y_end, k = 15) +
      duration_z +
      carrier_position_group +
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
  
  # Predict on this fold's validation data
  preds <- predict(fit_fold, newdata = cv_valid, type = "response")
  
  # Calculate and store AUC
  roc_fold <- roc(cv_valid$PTR_binary, preds, quiet = TRUE)
  cv_auc_results[i] <- auc(roc_fold)
}

# ==============================================================================
# 3. View the Cross-Validation Results
# ==============================================================================
cat("\n=== Cross-Validation Results ===\n")
for (i in 1:n_folds) {
  cat("Fold", i, "AUC:", round(cv_auc_results[i], 4), "\n")
}
cat("Mean CV AUC:", round(mean(cv_auc_results), 4), 
    " (SD:", round(sd(cv_auc_results), 4), ")\n")



# ...............................................
# Fold 1 AUC: 0.7514 
# Fold 2 AUC: 0.7556 
# Fold 3 AUC: 0.7529 
# Fold 4 AUC: 0.7566 
# Fold 5 AUC: 0.7555
# ...............................................







