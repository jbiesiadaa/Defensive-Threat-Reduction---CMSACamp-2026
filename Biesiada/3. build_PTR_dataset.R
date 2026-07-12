# 3. build_PTR_dataset
# Passing Threat Reduction (PTR): possession-level dataset construction
# from SkillCorner Dynamic Events (player possessions, passing options,on-ball engagements)
# data wrangling and feature engineering steps
# Julia Biesiada Version -> Checking Swayam work and then adding new extensions 

library(tidyverse)
library(jsonlite)
library(gt)
library(dplyr)
library(sportyR)




# Suppression creates the largest average passing threat reduction, while forced backward actions most often make attackers choose below their best available option.


### NEW IDEA ~ JULIA BIESIADA -------------------------------------------------- 

# PHASE 1: Build the PTR (Passing Threat Reduction) analysis dataset

# Compute PTR two ways:
#        PTR_raw       = max(xthreat of ALL options)       - chosen xthreat
#        PTR_realistic = max(xthreat of REALISTIC options) - chosen xthreat
#        Realistic = xpass_completion > 0.68 (SkillCorner's own line-break



# Working with single game -> case study for the EDA 
events    <- read.csv("mls_skillcorner/dynamic_events/match_742721_events.csv")

# ------------------------------------------------------------------------------
# Working with multiple games

folder <- "mls_skillcorner/dynamic_events"

# find all event csv files
files <- list.files(
  path = folder,
  pattern = "_events\\.csv$",
  full.names = TRUE
)

# keep only first 200 games
files_200 <- files[1:200]

# read and combine 200 games
dynamic_200 <- do.call(
  rbind,
  lapply(files_200, function(file) {
    data <- read.csv(file)
    data$source_file <- file
    data$game_id <- gsub("match_|_events.csv", "", basename(file))
    return(data)
  })
)


# Writing it as events
events <- dynamic_200

# Helper: SkillCorner booleans arrive as strings ("True"/"False") or logicals -> transforming them to the logical values
to_bool <- function(x) {
  case_when(
    x %in% c(TRUE,  "TRUE",  "True",  "true")  ~ TRUE,
    x %in% c(FALSE, "FALSE", "False", "false") ~ FALSE,
    TRUE ~ NA
  )
}

events <- dynamic_200


# 1. SPLIT INTO EVENT TABLES
# ------------------------------------------------------------------------------
possessions <- events |> filter(event_type_id == 8)
options     <- events |> filter(event_type_id == 7)
obe         <- events |> filter(event_type_id == 9)

# 2. OPTION SET PER POSSESSION
#    Join on (match_id, associated_player_possession_event_id) -- NOT frame_end.
#    Every passing option comes with a label saying which possession it belongs to, 
#    so I use that label to connect them. 
#    Matching by time instead would just guess based on when things happened, and can link the wrong events together.
# ------------------------------------------------------------------------------
option_summary <- options |>
  filter(!is.na(xthreat)) |>
  mutate(realistic = !is.na(xpass_completion) & xpass_completion > 0.68) |>
  group_by(match_id, associated_player_possession_event_id) |>
  summarise(
    n_options_counted     = n(),
    max_xthreat_all       = max(xthreat, na.rm = TRUE),
    max_xthreat_realistic = ifelse(any(realistic),
                                   max(xthreat[realistic], na.rm = TRUE),
                                   NA_real_),
    # Identify the best realistic option
    best_option_event_id  = ifelse(any(realistic),
                                   event_id[which.max(ifelse(realistic, xthreat, -Inf))][1],
                                   NA_character_),
    best_option_player_id = ifelse(any(realistic),
                                   as.integer(player_id[which.max(ifelse(realistic, xthreat, -Inf))][1]),
                                   NA_integer_),
    # position at the pass moment (the fixused for plots + controls)
    best_option_x_end = ifelse(any(realistic),
                           x_end[which.max(ifelse(realistic, xthreat, -Inf))][1], NA_real_),
    best_option_y_end = ifelse(any(realistic),
                           y_end[which.max(ifelse(realistic, xthreat, -Inf))][1], NA_real_),
    # position when his option-event began (kept ONLY to derive movement)
    best_option_x_start = ifelse(any(realistic),
                                 x_start[which.max(ifelse(realistic, xthreat, -Inf))][1], NA_real_),
    best_option_y_start = ifelse(any(realistic),
                                 y_start[which.max(ifelse(realistic, xthreat, -Inf))][1], NA_real_),
    .groups = "drop"                                
  )

# 3. DEFENSIVE (OBE) FEATURES PER POSSESSION
# ------------------------------------------------------------------------------
obe_summary <- obe |>
  mutate(across(c(stop_possession_danger, reduce_possession_danger,
                  force_backward, pressing_chain, goal_side_start,
                  close_at_player_possession_start, possession_danger,
                  simultaneous_defensive_engagement_same_target, beaten_by_possession, beaten_by_movement),   
                to_bool)) |> # Convert text booleans to real TRUE/FALSE
  group_by(match_id, associated_player_possession_event_id) |>
  summarise(
    # what kind of defensive action?
    n_engagements            = n(), # Number of defensive engagements
    # Identify the type of defensive action
    any_pressing             = any(event_subtype == "pressing",       na.rm = TRUE),
    any_pressure             = any(event_subtype == "pressure",       na.rm = TRUE),
    any_counter_press        = any(event_subtype == "counter_press",  na.rm = TRUE),
    any_recovery_press       = any(event_subtype == "recovery_press", na.rm = TRUE),
    any_other_engagement     = any(event_subtype == "other",          na.rm = TRUE),
    # Pressing chain variables
    in_pressing_chain        = any(pressing_chain, na.rm = TRUE),
    max_chain_length         = suppressWarnings( # suppressWarnings() to avoid annoying warnings when a possession has no valid values, and then we replace the weird result with NA
      max(pressing_chain_length, na.rm = TRUE)),
    #defensive FAILURE flags: never predictors
    any_beaten_by_possession = any(beaten_by_possession, na.rm = TRUE),
    any_beaten_by_movement   = any(beaten_by_movement,   na.rm = TRUE),
    # Defender distance
    min_engagement_distance  = suppressWarnings(
      min(interplayer_distance_min, na.rm = TRUE)), # This finds the closest distance between the defender and the ball carrier
    # Goalside and close at start
    any_goalside_start       = any(goal_side_start, na.rm = TRUE), # This checks whether at least one defender was goalside at the start (Goalside means the defender was between the attacker and the goal)          
    any_close_at_start       = any(close_at_player_possession_start, na.rm = TRUE), # This checks whether a defender was already close when the possession started -> 
    # Engagement speed
    mean_engagement_speed    = mean(speed_avg, na.rm = TRUE), # How intense or fast was the pressure?       
    
    # Simultaneous pressure
    any_simultaneous_same_target = any(                                    
      simultaneous_defensive_engagement_same_target, na.rm = TRUE), # This checks whether more than one defender engaged the same ball carrier at the same time
    
    # Where the press came FROM (defender's location at engagement start)
    ## Location coding: no middle-third flag (both FALSE = middle: adding it
    # is collinear). Wide vs central only: 5 channels too sparse,
    # left/right is symmetric. No raw x/y in linear models (zones encode the
    # nonlinearity); I would savecoords for tree models + heatmaps later
    
    any_engagement_from_attacking_third = any(third_start == "attacking_third", # Did any defensive engagement start in the attacking third?
                                              na.rm = TRUE),               
    any_engagement_from_defensive_third = any(third_start == "defensive_third", # Did any defensive engagement start in the defensive third?
                                              na.rm = TRUE),               
    any_engagement_from_wide            = any(channel_start %in%
                                                c("wide_left", "wide_right"),
                                              na.rm = TRUE),        # Did any defensive engagement start in a wide channel?
    
    #  SkillCorner's own defensive outcome -> VALIDATION ONLY, never predictors
    stop_possession_danger   = any(stop_possession_danger,   na.rm = TRUE),
    reduce_possession_danger = any(reduce_possession_danger, na.rm = TRUE),
    force_backward           = any(force_backward,           na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate( # Clean weird missing values
    max_chain_length        = ifelse(is.infinite(max_chain_length),
                                     NA_real_, max_chain_length),
    min_engagement_distance = ifelse(is.infinite(min_engagement_distance),
                                     NA_real_, min_engagement_distance),
    mean_engagement_speed   = ifelse(is.nan(mean_engagement_speed),
                                     NA_real_, mean_engagement_speed)
  )

# 4. BUILD THE ANALYSIS DATASET (one row = one possession ending in a pass)
# I add the information about: 
#   the pass the attacker chose
#   the passing options available
#   the defensive engagement against the ball carrier
# ------------------------------------------------------------------------------
analysis <- possessions |>
  filter(!is.na(targeted_passing_option_event_id)) |>   # ended with a pass
  select(
    match_id,event_id, player_id, player_name, team_id, # game_id
    targeted_passing_option_event_id,
    
    # tracking-join keys + direction flag
    frame_start, frame_end, attacking_side,
    
    # identity of the player actually targeted and x and y
    player_targeted_id,player_targeted_x_pass,player_targeted_y_pass,
    
    # carrier positions, event coords (distance controls below)
    x_start, y_start, x_end, y_end,
    
    # chosen pass -> What pass did the attacker choose, and how dangerous or difficult was it?
    player_targeted_xthreat, player_targeted_xpass_completion,
    pass_direction, pass_distance, pass_range, pass_outcome,
    quick_pass, one_touch, is_header, hand_pass, 
    
    # pressure on the ball carrier (separation_gain = end - start) -> Did the attacker gain space or lose space during the possession?
    separation_start, separation_end, separation_gain,
    
    # structure -> What defensive shape was the attacker facing?
    organised_defense, defensive_structure, n_defensive_lines,
    inside_defensive_shape_start,
    last_defensive_line_height_start, delta_to_last_defensive_line_start,
    
    # option availability (SkillCorner aggregates) -> How good was the attacker’s passing options?
    n_passing_options, n_passing_options_dangerous_not_difficult,
    n_passing_options_dangerous_difficult, n_passing_options_line_break,
    n_passing_options_ahead,
    
    # context controls (channel_start = the CARRIER's channel here) -> What was the situation when the pass happened?
    team_out_of_possession_phase_type, third_start, channel_start,
    game_state, duration,
    
    # outcomes for predictive validation (10s window) --> NEVER predictors
    lead_to_shot, lead_to_goal,
    
    # data quality -> Can we trust the tracking data for this possession? (Matching SkillCorner tracking data with the start and end of the possession)
    is_player_possession_start_matched, is_player_possession_end_matched
  ) |>
  mutate(across(c(organised_defense, inside_defensive_shape_start,
                  quick_pass, one_touch, is_header,
                  lead_to_shot, lead_to_goal,
                  is_player_possession_start_matched,
                  is_player_possession_end_matched), to_bool)) |> # Convert text booleans to real TRUE/FALSE
  # integer casts AT THE SOURCE (compared to tracking ids/frames later)
  mutate(
    player_targeted_id = as.integer(player_targeted_id),
    frame_start        = as.integer(frame_start),
    frame_end          = as.integer(frame_end)
  ) |>
  left_join(option_summary,
            by = c("match_id",
                   "event_id" = "associated_player_possession_event_id")) |>
  left_join(obe_summary,
            by = c("match_id",
                   "event_id" = "associated_player_possession_event_id")) |>
  mutate(
    # DISTANCES TO GOAL 
    # carrier
    dist_carrier_to_goal_start = sqrt((52.5 - x_start)^2 + (0 - y_start)^2),
    dist_carrier_to_goal_end   = sqrt((52.5 - x_end)^2   + (0 - y_end)^2),
    # best option
    dist_option_to_goal_start  = sqrt((52.5 - best_option_x_start)^2 +
                                        (0 - best_option_y_start)^2),
    dist_option_to_goal_end    = sqrt((52.5 - best_option_x_end)^2 +
                                        (0 - best_option_y_end)^2),
    # targeted (pass moment only)
    dist_target_to_goal        = sqrt((52.5 - player_targeted_x_pass)^2 +
                                        (0 - player_targeted_y_pass)^2),
    
    # DISTANCES BETWEEN Carrier, Best Option and Targeted Option 
    # carrier <-> best option, at both moments (did the option drift away
    # from or toward the carrier during the possession?)
    dist_carrier_to_option_start = sqrt((x_start - best_option_x_start)^2 +
                                          (y_start - best_option_y_start)^2),
    dist_carrier_to_option_end   = sqrt((x_end - best_option_x_end)^2 +
                                          (y_end - best_option_y_end)^2),
    # carrier <-> target (pass moment)
    dist_carrier_to_target       = sqrt((x_end - player_targeted_x_pass)^2 +
                                          (y_end - player_targeted_y_pass)^2),
    # target <-> best option at the pass (how far apart were the chosen and
    # declined destinations? 0 when he took the best option)
    dist_target_to_option        = sqrt((player_targeted_x_pass - best_option_x_end)^2 +
                                          (player_targeted_y_pass - best_option_y_end)^2),
    
    # MOVEMENT: BEST OPTION'S RUN (event window)
    best_option_run_dist    = sqrt((best_option_x_end - best_option_x_start)^2 +
                                     (best_option_y_end - best_option_y_start)^2),
    best_option_run_forward = best_option_x_end - best_option_x_start,  # + = to goal
    best_option_run_lateral = best_option_y_end - best_option_y_start,  # signed
    best_option_run_angle   = ifelse(best_option_run_dist > 1,
                                     atan2(best_option_y_end - best_option_y_start,
                                           best_option_x_end - best_option_x_start) * 180 / pi,
                                     NA_real_),
    
    # MOVEMENT: CARRIER (possession window; = the carry) 
    carrier_move_dist    = sqrt((x_end - x_start)^2 + (y_end - y_start)^2),
    carrier_move_forward = x_end - x_start,                             # + = to goal
    carrier_move_lateral = y_end - y_start,                             # signed
    carrier_move_angle   = ifelse(carrier_move_dist > 1,
                                  atan2(y_end - y_start,
                                        x_end - x_start) * 180 / pi,
                                  NA_real_),
    
    n_engagements = replace_na(n_engagements, 0L),
    engaged       = n_engagements > 0, # Did the defense engage the ball carrier or not?
    
    # After the left_join, unengaged possessions have NA for all OBE
    # No engagement = the action genuinely didn't happen -> FALSE.
    across(any_of(c("any_pressing", "any_pressure", "any_counter_press",
                    "any_recovery_press", "any_other_engagement",
                    "in_pressing_chain", "any_goalside_start",
                    "any_close_at_start", "any_simultaneous_same_target",
                    "any_engagement_from_attacking_third",
                    "any_engagement_from_defensive_third",
                    "any_engagement_from_wide",
                    "any_beaten_by_possession",
                    "any_beaten_by_movement",
                    "stop_possession_danger", "reduce_possession_danger",
                    "force_backward")),
           ~ replace_na(.x, FALSE)),
    
    # Create coach friendly engagement type -> simple label for the type of defensive engagement
    # Two versions of this info exist:
    #   - any_ FLAGS: can overlap  -> use in MODELS
    #   - this LABEL: one per row   -> use in TABLES, never as predictor
    # case_when = priority ladder, first match wins:
    # counter press > recovery press > pressing > pressure > other
    # Never reorder later -- it would change all past tables
    engagement_type_group = case_when(
      !engaged             ~ "No engagement",
      any_counter_press    ~ "Counter press",
      any_recovery_press   ~ "Recovery press",
      any_pressing         ~ "Pressing",
      any_pressure         ~ "Pressure",
      any_other_engagement ~ "Other engagement",
      TRUE                 ~ "Engaged, unknown"   # data-quality canary: ~0 expected
    ),
    
    # Create coach friendly engagement zone -> Did the defense pressure high, middle, or deep?
    engagement_zone = case_when(
      !engaged                            ~ "No engagement",
      any_engagement_from_attacking_third ~ "High engagement",
      any_engagement_from_defensive_third ~ "Deep engagement",
      engaged                             ~ "Middle engagement",
      TRUE                                ~ "Unknown"
    ),
    
    # Passing options existed, but none had xPass completion above 0.68
    # This may indicate defensive denial, so we flag it instead of dropping it.
    no_realistic_option = !is.na(max_xthreat_all) & is.na(max_xthreat_realistic),
    
    # This identifies possessions that were very short 
    # The player had very little time to make a decision
    short_possession = one_touch %in% TRUE | replace_na(duration < 0.5, FALSE),
    
    #     Disruption possessions:
    #     Disruption phases may involve deflections, blocked actions, loose balls,
    #     The player may not have enough control to make a real passing decision.
    #     Since PTR compares the chosen pass to the best available option, I flag
    #     these possessions and we want to remove them from the main model
    disruption_possession = case_when(
      is.na(team_out_of_possession_phase_type) ~ NA,
      team_out_of_possession_phase_type == "disruption" ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Check if chosen pass was realistic
    # NA when xpass_completion is missing (tracking gap): unknown != FALSE.
    #   Descriptive / outcome-side only --> NOT a predictor of PTR.
    chosen_pass_realistic = case_when(
      is.na(player_targeted_xpass_completion) ~ NA,
      player_targeted_xpass_completion > 0.68 ~ TRUE,
      TRUE                                    ~ FALSE
    ),
    
    # This checks whether the ball carrier started in a central zone
    # Wide zones are everything else
    # Was the ball carrier in the center or half space?
    carrier_central = case_when(
      is.na(channel_start) ~ NA,
      channel_start %in% c("center", "half_space_left",
                           "half_space_right") ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # THE METRIC --------------------------------------------------------------
    # This creates the raw version of Passing Threat Reduction -> Swayam version
    # Raw PTR compares the chosen pass to the most dangerous option, even if that option was difficult
    PTR_raw = ifelse(
      !is.na(player_targeted_xthreat) & !is.na(max_xthreat_all),
      pmax(max_xthreat_all - player_targeted_xthreat, 0),
      NA_real_
    ),
    
    # Julia's new improved metric 
    # Did the attacker choose below their best realistic dangerous option?
    PTR = ifelse(
      !is.na(player_targeted_xthreat) & !is.na(max_xthreat_realistic),
      pmax(max_xthreat_realistic - player_targeted_xthreat, 0),
      NA_real_
    ),
    
    # Explain why PTR is present or missing -> This creates a label explaining whether PTR was calculated
    # Why do we have or not have PTR for this possession?
    PTR_status = case_when(
      is.na(max_xthreat_all) ~ "No xThreat option data",
      no_realistic_option    ~ "Options existed, none realistic",
      !is.na(PTR)            ~ "PTR calculated",
      TRUE                   ~ "Chosen pass xthreat missing"
    ),
    
    # Check whether attacker chose the best realistic option -> compares targeted_passing_option_event_id with best_option_event_id
    # Did the attacker pick the best realistic passing option?
    chose_best_option = ifelse(
      is.na(best_option_event_id),
      NA,
      as.character(targeted_passing_option_event_id) ==
        as.character(best_option_event_id)
    ),
    
    # Create defensive outcome label
    # What did the defense achieve on this possession?
    # WHERE TO USE WHAT:
    #   FLAGS (any()) (stop_possession_danger, reduce_possession_danger,
    #          force_backward):
    #     -> validation checks: does high PTR line up with these? 
    #     -> alternative targets: for example logistic model predicting suppression
    #     -> NEVER as predictors of PTR (they measure the outcome we're validating against
    #   LABEL(case_when) (this column, one value per row):
    #     -> group_by tables comparing mean PTR across outcomes
    #     -> figures and summaries
    #     -> NEVER in any model at all
    # case_when = priority ladder, first match wins:
    # stop > suppression > forced backward > engaged > no engagement
    # (strongest defensive result first). Never reorder after results exist
    
    defensive_outcome = case_when(
      stop_possession_danger   ~ "Stop (turnover)",
      reduce_possession_danger ~ "Suppression",
      force_backward           ~ "Forced backward",
      engaged                  ~ "Engaged, no outcome",
      TRUE                     ~ "No engagement"
    )
  )

# Checking for one game
analysis|>
  filter(event_id == "8_172")|>
  select(best_option_run_angle,best_option_run_lateral,best_best_option_run_forward )
  
events|>
  filter(event_id == "8_172")|>
  select(player_targeted_angle_to_goal_start, player_targeted_angle_to_goal_end)
  
# Tracking of changes 
# 07/10/2026
#---- Option_summary
# Added the identity and coordinates of the best realistic passing option start and end
# The code keeps the option event ID, player ID, position at the passing moment,
# and position at the beginning of the option event to measure player movement.

# ---Analysis
# Added frame information, attacking direction, targeted player identity,
# targeted receiver coordinates end frame, and ball carrier coordinates start and end
# Calculation: distance_to_goal by carrier, best option and targeted
# distance between the carrier and best option, carrier and player targeted, player targeted and best option
# movement of the best option player before the pass
# angle of the best option, lateral run

# checking this outcomes
analysis|>
  count(lead_to_goal, lead_to_shot, any_beaten_by_possession, any_beaten_by_movement)


# 5. QUALITY FILTERS -----------------------------------------------------------

# Clean dataset for validation and EDA
n_start <- nrow(analysis)

analysis_clean <- analysis |>
  filter(!is_header %in% TRUE) |>        # remove headers: not normal pass/xThreat logic
  filter(!hand_pass %in% TRUE) |>        # remove hand/throw-in type actions if present
  filter(is_player_possession_end_matched %in% TRUE) |>  # pass moment reliably tracked
  filter(!is.na(PTR))                    # PTR is computable


cat("Possessions:", n_start, "->", nrow(analysis_clean),
    "(removed:", n_start - nrow(analysis_clean), ")\n") # How many was removed by filtering

# Stricter Idea for main models
analysis_model <- analysis_clean |>
  filter(is_player_possession_start_matched %in% TRUE) |> # start features reliable
  filter(!short_possession) |>                            # remove one-touch/very short actions
  filter(!disruption_possession %in% TRUE)                # remove messy/loose-ball actions

# Representation of all changes
cat("Full -> clean -> model:",
    nrow(analysis), "->", nrow(analysis_clean), "->", nrow(analysis_model), "\n")


# COMPLETENESS CHECK -- must print character(0) or do not proceed
# ------------------------------------------------------------------------------
required <- c("frame_start","frame_end","attacking_side","player_targeted_id",
              "x_start","y_start","x_end","y_end",
              "best_option_player_id","best_option_x","best_option_y",
              "dist_carrier_to_goal","dist_option_to_goal",
              "dist_carrier_to_option")
print(setdiff(required, names(analysis_model)))

cat("best_option_player_id coverage:",
    round(mean(!is.na(analysis_model$best_option_player_id)) * 100, 1), "%\n")

events|>
  select(event_id,player_targeted_distance_to_goal_start )

# Saving for future work
saveRDS(analysis_clean, "ptr_analysis_dataset_200.rds")
saveRDS(analysis_model,  "ptr_model_dataset_200.rds")


# Checking
class(analysis_clean$lead_to_shot)   # should say "logical"
table(analysis_clean$lead_to_shot, useNA = "always")
