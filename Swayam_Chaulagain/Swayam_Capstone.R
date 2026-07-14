## Updated analysis dataset code from Julia (Building_PTR_datset)


# 3. build_PTR_dataset
# Passing Threat Reduction (PTR): possession-level dataset construction
# from SkillCorner Dynamic Events (player possessions, passing options,on-ball engagements)
# data wrangling and feature engineering steps
# Julia Biesiada Version -> Checking Swayam work and then adding new extensions 

library(tidyverse)


# Suppression creates the largest average passing threat reduction, while forced backward actions most often make attackers choose below their best available option.


### NEW IDEA ~ JULIA BIESIADA -------------------------------------------------- 

# PHASE 1: Build the PTR (Passing Threat Reduction) analysis dataset

# Compute PTR two ways:
#        PTR_raw       = max(xthreat of ALL options)       - chosen xthreat
#        PTR_realistic = max(xthreat of REALISTIC options) - chosen xthreat
#        Realistic = xpass_completion > 0.68 (SkillCorner's own line-break



# Working with single game -> case study for the EDA 
events    <- read.csv("mls_skillcorner/dynamic_events/match_742721_events.csv")

events <- read.csv("mls_skillcorner/dynamic_events/match_1039803_events.csv")
# ------------------------------------------------------------------------------
# Working with multiple games

folder <- "mls_skillcorner/dynamic_events"

# find all event csv files
files <- list.files(
  path = folder,
  pattern = "_events\\.csv$",
  full.names = TRUE
)

# keep only first 10 games
files_10 <- files[1:10]

# read and combine 200 games
dynamic_10 <- do.call(
  rbind,
  lapply(files_10, function(file) {
    data <- read.csv(file)
    data$source_file <- file
    data$game_id <- gsub("match_|_events.csv", "", basename(file))
    return(data)
  })
)


# Writing it as events
events <- dynamic_10

# Helper: SkillCorner booleans arrive as strings ("True"/"False") or logicals -> transforming them to the logical values
to_bool <- function(x) {
  case_when(
    x %in% c(TRUE,  "TRUE",  "True",  "true")  ~ TRUE,
    x %in% c(FALSE, "FALSE", "False", "false") ~ FALSE,
    TRUE ~ NA
  )
}

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
  mutate(
    match_id = as.character(match_id),
    event_id = as.character(event_id),
    associated_player_possession_event_id =
      as.character(associated_player_possession_event_id),
    
    realistic =
      !is.na(xpass_completion) &
      xpass_completion > 0.68
  ) |>
  
  left_join(
    events |>
      filter(event_type_id == 8) |>
      transmute(
        match_id = as.character(match_id),
        
        associated_player_possession_event_id =
          as.character(event_id),
        
        # Give the joined variable a unique name
        chosen_option_event_id =
          as.character(targeted_passing_option_event_id)
      ) |>
      distinct(
        match_id,
        associated_player_possession_event_id,
        .keep_all = TRUE
      ),
    by = c(
      "match_id",
      "associated_player_possession_event_id"
    )
  ) |>
  
  mutate(
    is_targeted = replace_na(
      event_id == chosen_option_event_id,
      FALSE
    )
  ) |>
  
  group_by(
    match_id,
    associated_player_possession_event_id
  ) |>
  
  arrange(
    desc(xthreat),
    desc(is_targeted),
    event_id,
    .by_group = TRUE
  ) |>
  
  summarise(
    n_options_counted = n(),
    n_options_realistic = sum(realistic),
    n_options_na_completion = sum(is.na(xpass_completion)),
    
    max_xthreat_all = max(xthreat),
    
    max_xthreat_realistic =
      if (any(realistic)) {
        max(xthreat[realistic])
      } else {
        NA_real_
      },
    
    best_was_tied =
      if (any(realistic)) {
        sum(
          realistic &
            xthreat == max(xthreat[realistic])
        ) > 1
      } else {
        NA
      },
    
    best_option_event_id =
      if (any(realistic)) {
        first(event_id[realistic])
      } else {
        NA_character_
      },
    
    best_option_player_id =
      if (any(realistic)) {
        as.integer(first(player_id[realistic]))
      } else {
        NA_integer_
      },
    
    best_option_x_end =
      if (any(realistic)) {
        first(x_end[realistic])
      } else {
        NA_real_
      },
    
    best_option_y_end =
      if (any(realistic)) {
        first(y_end[realistic])
      } else {
        NA_real_
      },
    
    best_option_x_start =
      if (any(realistic)) {
        first(x_start[realistic])
      } else {
        NA_real_
      },
    
    best_option_y_start =
      if (any(realistic)) {
        first(y_start[realistic])
      } else {
        NA_real_
      },
    
    .groups = "drop"
  )

# 3. DEFENSIVE (OBE) FEATURES PER POSSESSION
# ------------------------------------------------------------------------------
obe_summary <- obe |>
  mutate(match_id = as.character(match_id),                          # NEW
         associated_player_possession_event_id =                     # NEW
           as.character(associated_player_possession_event_id)) |>
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


# 4.TARGETED PLAYER'S START POSITION
#      The targeted pass IS an option event, so its start coordinates live in
#      the options table -- same source as best_option_*, so when PTR == 0
#      (same option row) the coordinates match EXACTLY.
# ------------------------------------------------------------------------------
targeted_option_coords <- options |>
  transmute(
    match_id = as.character(match_id),
    targeted_passing_option_event_id = as.character(event_id),
    player_targeted_x_start = x_start,     # where the receiver stood when
    player_targeted_y_start = y_start      # his option event began
  ) |>
  distinct(match_id, targeted_passing_option_event_id, .keep_all = TRUE)


# 5. BUILD THE ANALYSIS DATASET (one row = one possession ending in a pass) --
# I add the information about: 
#   the pass the attacker chose
#   the passing options available
#   the defensive engagement against the ball carrier
analysis <- possessions |>
  filter(!is.na(targeted_passing_option_event_id)) |>   # ended with a pass
  
  # keys as character AT THE SOURCE (all later joins/comparisons depend on it)
  mutate(
    match_id = as.character(match_id),
    event_id = as.character(event_id),
    targeted_passing_option_event_id =
      as.character(targeted_passing_option_event_id)
  ) |>
  
  select(
    ## keys + identity
    match_id, event_id, player_id, player_name, team_id,
    targeted_passing_option_event_id, player_targeted_id,
    
    ## tracking-join keys + direction flag
    frame_start, frame_end, attacking_side,
    
    ## carrier position (possession event window)
    carrier_x_start = x_start,  carrier_y_start = y_start,
    carrier_x_end   = x_end,    carrier_y_end   = y_end,
    
    ## targeted receiver at the pass moment -> RENAMED pass -> end
    player_targeted_x_end = player_targeted_x_pass,
    player_targeted_y_end = player_targeted_y_pass,
    
    ## chosen pass: how dangerous / difficult was it?
    player_targeted_xthreat, player_targeted_xpass_completion,
    pass_direction, pass_distance, pass_range, pass_outcome,
    quick_pass, one_touch, is_header, hand_pass,
    
    ## pressure on the carrier (separation_gain = end - start)
    separation_start, separation_end, separation_gain,
    
    ## defensive shape the attacker was facing
    organised_defense, defensive_structure, n_defensive_lines,
    inside_defensive_shape_start,
    last_defensive_line_height_start, delta_to_last_defensive_line_start,
    
    ## option availability (SkillCorner aggregates)
    n_passing_options, n_passing_options_dangerous_not_difficult,
    n_passing_options_dangerous_difficult, n_passing_options_line_break,
    n_passing_options_ahead,
    
    ## context controls (channel_start = the CARRIER's channel)
    team_out_of_possession_phase_type, third_start, channel_start,
    game_state, duration,
    
    ## outcomes for validation (10s window) -- NEVER predictors
    lead_to_shot, lead_to_goal,
    
    ## data quality: can we trust the tracking for this possession?
    is_player_possession_start_matched, is_player_possession_end_matched
  ) |>
  
  # text booleans -> real TRUE/FALSE
  mutate(across(c(organised_defense, inside_defensive_shape_start,
                  quick_pass, one_touch, is_header,
                  lead_to_shot, lead_to_goal,
                  is_player_possession_start_matched,
                  is_player_possession_end_matched),
                to_bool)) |>
  
  # integer casts at the source (compared against tracking ids/frames later)
  mutate(
    player_targeted_id = as.integer(player_targeted_id),
    frame_start        = as.integer(frame_start),
    frame_end          = as.integer(frame_end)
  ) |>
  
  # options set, defensive engagements, targeted start position
  left_join(option_summary,
            by = c("match_id",
                   "event_id" = "associated_player_possession_event_id")) |>
  left_join(obe_summary,
            by = c("match_id",
                   "event_id" = "associated_player_possession_event_id")) |>
  left_join(targeted_option_coords,
            by = c("match_id", "targeted_passing_option_event_id"),
            relationship = "many-to-one") |>
  
  
  # GEOMETRY -- symmetric for all three actors, at BOTH moments
  #      Goal center = (52.5, 0) in the standardized frame.
  mutate(
    
    ## DISTANCE TO GOAL 
    dist_carrier_to_goal_start = sqrt((52.5 - carrier_x_start)^2 + carrier_y_start^2),
    
    dist_carrier_to_goal_end   = sqrt((52.5 - carrier_x_end)^2   + carrier_y_end^2),
    
    dist_target_to_goal_start  = sqrt((52.5 - player_targeted_x_start)^2 +
                                        player_targeted_y_start^2),
    dist_target_to_goal_end    = sqrt((52.5 - player_targeted_x_end)^2 +
                                        player_targeted_y_end^2),
    
    dist_best_to_goal_start  = sqrt((52.5 - best_option_x_start)^2 +
                                      best_option_y_start^2),
    dist_best_to_goal_end    = sqrt((52.5 - best_option_x_end)^2 +
                                      best_option_y_end^2),
    
    # PASS DISTANCES 
    
    # Distance from carrier to chosen target
    targeted_pass_distance_start = sqrt(
      (player_targeted_x_start - carrier_x_start)^2 +
        (player_targeted_y_start - carrier_y_start)^2),
    
    targeted_pass_distance_end = sqrt(
      (player_targeted_x_end - carrier_x_end)^2 +
        (player_targeted_y_end - carrier_y_end)^2),
    
    targeted_pass_distance_gain =
      targeted_pass_distance_end -
      targeted_pass_distance_start,
    
    
    # Distance from carrier to best realistic option
    best_option_pass_distance_start = sqrt(
      (best_option_x_start - carrier_x_start)^2 +
        (best_option_y_start - carrier_y_start)^2),
    
    best_option_pass_distance_end = sqrt(
      (best_option_x_end - carrier_x_end)^2 +
        (best_option_y_end - carrier_y_end)^2),
    
    best_option_pass_distance_gain =
      best_option_pass_distance_end -
      best_option_pass_distance_start,
    
    
    # Positive = chosen target was farther than best option
    targeted_vs_best_distance_start =
      targeted_pass_distance_start -
      best_option_pass_distance_start,
    
    # PASS OUTCOME
    # Applies only to the targeted pass because the best option was not attempted
    targeted_pass_successful = case_when(
      pass_outcome == "successful" ~ TRUE,
      pass_outcome %in% c("unsuccessful", "offside") ~ FALSE,
      TRUE ~ NA),
    
    ## MOVEMENT (start -> end), same shape for all three players --------------
    # carrier (= the carry; possession window)
    carrier_move_forward = carrier_x_end - carrier_x_start,        # + = to goal
    carrier_move_lateral = carrier_y_end - carrier_y_start,        # signed
    carrier_move_dist    = sqrt(carrier_move_forward^2 + carrier_move_lateral^2),
    carrier_stationary = carrier_move_dist <= 1,
    carrier_move_angle   = ifelse(carrier_stationary, 0,
                                  atan2(carrier_move_lateral,
                                        carrier_move_forward) * 180 / pi),
    
    # targeted receiver's run (option window)
    target_run_forward = player_targeted_x_end - player_targeted_x_start,
    target_run_lateral = player_targeted_y_end - player_targeted_y_start,
    target_run_dist    = sqrt(target_run_forward^2 + target_run_lateral^2),
    target_stationary  = target_run_dist <= 1,
    target_run_angle   = ifelse(target_stationary,0,
                                atan2(target_run_lateral,
                                      target_run_forward) * 180 / pi),
    
    # best option's run (option window)
    best_option_run_forward = best_option_x_end - best_option_x_start,
    best_option_run_lateral = best_option_y_end - best_option_y_start,
    best_option_run_dist    = sqrt(best_option_run_forward^2 + best_option_run_lateral^2),
    best_option_stationary  = best_option_run_dist <= 1 ,
    best_option_run_angle   = ifelse(best_option_stationary,0,
                                     atan2(best_option_run_lateral,
                                           best_option_run_forward) * 180 / pi),
    
    # angle from each player to goal center at end (0 = straight at goal, ±90 = level with it)
    angle_carrier_to_goal_start = atan2(0 - carrier_y_start,
                                        52.5 - carrier_x_start) * 180 / pi,
    angle_carrier_to_goal_end = atan2(0 - carrier_y_end,
                                      52.5 - carrier_x_end) * 180 / pi,
    
    angle_target_to_goal_start = atan2(0 - player_targeted_y_start,
                                       52.5 - player_targeted_x_start) * 180 / pi,
    angle_target_to_goal_end  = atan2(0 - player_targeted_y_end,
                                      52.5 - player_targeted_x_end) * 180 / pi,
    
    angle_option_to_goal_start  = atan2(0 - best_option_y_start,
                                        52.5 - best_option_x_start) * 180 / pi,
    angle_option_to_goal_end  = atan2(0 - best_option_y_end,
                                      52.5 - best_option_x_end) * 180 / pi,
    # DEFENSIVE ENGAGEMENT -----
    n_engagements = replace_na(n_engagements, 0L),
    engaged       = n_engagements > 0,
    
    # unengaged possessions have NA for all OBE flags after the join;
    # no engagement = the action genuinely didn't happen -> FALSE
    across(any_of(c("any_pressing", "any_pressure", "any_counter_press",
                    "any_recovery_press", "any_other_engagement",
                    "in_pressing_chain", "any_goalside_start",
                    "any_close_at_start", "any_simultaneous_same_target",
                    "any_engagement_from_attacking_third",
                    "any_engagement_from_defensive_third",
                    "any_engagement_from_wide",
                    "any_beaten_by_possession", "any_beaten_by_movement",
                    "stop_possession_danger", "reduce_possession_danger",
                    "force_backward")),
           ~ replace_na(.x, FALSE)),
    
    # coach-friendly LABEL (one per row): TABLES ONLY, never a predictor.
    # priority ladder, first match wins -- never reorder after results exist
    engagement_type_group = case_when(
      !engaged             ~ "No engagement",
      any_counter_press    ~ "Counter press",
      any_recovery_press   ~ "Recovery press",
      any_pressing         ~ "Pressing",
      any_pressure         ~ "Pressure",
      any_other_engagement ~ "Other engagement",
      TRUE                 ~ "Engaged, unknown"    # data-quality canary: ~0 expected
    ),
    
    # where did the pressure come from? (high / middle / deep)
    engagement_zone = case_when(
      !engaged                            ~ "No engagement",
      any_engagement_from_attacking_third ~ "High engagement",
      any_engagement_from_defensive_third ~ "Deep engagement",
      engaged                             ~ "Middle engagement",
      TRUE                                ~ "Unknown"
    ),
    
    
    # QUALITY / CONTEXT FLAGS -----
    # options existed but none above the realism threshold
    # (possible defensive denial -> flagged, not dropped)
    no_realistic_option = !is.na(max_xthreat_all) & is.na(max_xthreat_realistic),
    
    # very little time to decide
    short_possession = one_touch %in% TRUE | replace_na(duration < 0.5, FALSE),
    
    # deflections / blocked actions / loose balls: no real passing decision
    disruption_possession = case_when(
      is.na(team_out_of_possession_phase_type)          ~ NA,
      team_out_of_possession_phase_type == "disruption" ~ TRUE,
      TRUE                                              ~ FALSE
    ),
    
    # chosen pass realistic? NA when completion missing (unknown != FALSE)
    # descriptive / outcome-side only -- NOT a predictor of PTR
    chosen_pass_realistic = case_when(
      is.na(player_targeted_xpass_completion)              ~ NA,
      player_targeted_xpass_completion > 0.68 ~ TRUE,
      TRUE                                                 ~ FALSE
    ),
    
    # carrier in a central zone (center or half spaces)
    carrier_central = case_when(
      is.na(channel_start)                                          ~ NA,
      channel_start %in% c("center", "half_space_left",
                           "half_space_right")                      ~ TRUE,
      TRUE                                                          ~ FALSE
    ),
    
    
    # THE METRIC: PASSING THREAT REDUCTION ----
    # raw version: chosen pass vs the most dangerous option, even if difficult
    PTR_raw = ifelse(
      !is.na(player_targeted_xthreat) & !is.na(max_xthreat_all),
      pmax(max_xthreat_all - player_targeted_xthreat, 0),
      NA_real_
    ),
    
    # main version: chosen pass vs the best REALISTIC option
    PTR = ifelse(
      !is.na(player_targeted_xthreat) & !is.na(max_xthreat_realistic),
      pmax(max_xthreat_realistic - player_targeted_xthreat, 0),
      NA_real_
    ),
    
    # why is PTR present or missing for this possession?
    PTR_status = case_when(
      is.na(max_xthreat_all) ~ "No xThreat option data",
      no_realistic_option    ~ "Options existed, none realistic",
      !is.na(PTR)            ~ "PTR calculated",
      TRUE                   ~ "Chosen pass xthreat missing"
    ),
    
    # did the attacker pick the best realistic option?
    # (both ids are character already -- plain comparison)
    chose_best_option = ifelse(
      is.na(best_option_event_id),
      NA,
      targeted_passing_option_event_id == best_option_event_id
    ),
    
    # overreach: threat beat the best realistic option, but completion fell
    # below the realism threshold. Kept in analysis_clean (EDA + discussion);
    # excluded from the model tier only.
    is_overreach = chose_best_option %in% FALSE & PTR == 0 &
      !chosen_pass_realistic %in% TRUE,
    
    # what did the defense achieve? LABEL: tables/figures only, never in models
    # FLAGS (stop/reduce/force_backward): validation targets, never predictors
    # priority ladder, strongest result first -- never reorder after results exist
    defensive_outcome = case_when(
      stop_possession_danger   ~ "Stop (turnover)",
      reduce_possession_danger ~ "Suppression",
      force_backward           ~ "Forced backward",
      engaged                  ~ "Engaged, no outcome",
      TRUE                     ~ "No engagement"
    )
  )

# 6. Saving For Future Work Pre Filter ----------------------------------------
saveRDS(analysis, "analysis_10games_prefilter.rds")

# 7. QUALITY FILTERS -----------------------------------------------------------

# Clean dataset for validation and EDA
n_start <- nrow(analysis)

analysis_clean <- analysis |>
  filter(!is_header %in% TRUE) |>        # remove headers: not normal pass/xThreat logic
  filter(!hand_pass %in% TRUE) |>        # remove hand/throw-in type actions if present
  filter(is_player_possession_end_matched %in% TRUE) |>  # pass moment reliably tracked
  filter(!is.na(PTR))|>                    # PTR is computable
  filter(!is_overreach %in% TRUE)|>          # Removing Gamblers
  filter(is_player_possession_start_matched %in% TRUE) |> # start features reliable
  filter(!short_possession) |>                            # remove one-touch/very short actions
  filter(!disruption_possession %in% TRUE)                # remove messy/loose-ball actions


cat("Possessions:", n_start, "->", nrow(analysis_clean),
    "(removed:", n_start - nrow(analysis_clean), ")\n") # How many was removed by filtering

# 8. Verification --------------------------------------------------------------

# Checking Logic of coordinates
# they should match
analysis_clean|>
  filter(PTR == 0)|>
  select(chose_best_option,player_targeted_x_start, best_option_x_start)


analysis_clean |>
  filter(PTR == 0) |>
  mutate(
    coordinates_exact_match =
      player_targeted_x_start == best_option_x_start &
      player_targeted_y_start == best_option_y_start &
      player_targeted_x_end ==  best_option_x_end &
      player_targeted_y_end == best_option_y_end
  ) |>
  count(coordinates_exact_match)


# they should not match
analysis_clean|>
  filter(PTR > 0)|>
  select(chose_best_option,player_targeted_x_end, best_option_x_end)

# How many we have a best option and chosen PTR = 0 and PTR > 0
table(analysis$chose_best_option)

class(analysis_clean$lead_to_shot)   # should say "logical"
table(analysis_clean$lead_to_shot, useNA = "always")

# checking this outcomes
analysis|>
  count(lead_to_goal, lead_to_shot, any_beaten_by_possession, any_beaten_by_movement)


# Checking for one game
analysis|>
  filter(event_id == "8_172")|>
  select(best_option_run_angle,best_option_run_lateral,best_best_option_run_forward )

events|>
  filter(event_id == "8_172")|>
  select(player_targeted_angle_to_goal_start, player_targeted_angle_to_goal_end)

# 9. Saving for future work ----------------------------------------------------

saveRDS(analysis_clean, "ptr_analysis_dataset_200.rds")

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


# 07/12/2026
# Added Player Targeted coordinates at the start of the event 
# When PTR = 0, the coordinates for player targeted and best option coordinates should match 
# Otherwise it's different
# When we have a situation when two players have max threat the same (chosen player and other option) 
# let's say 0.3 the algorithm will chose the chosen player (targeted) first
# added calculations of distances + geometry 
# PASS DISTANCE:
# - SkillCorner pass_distance was available only for successful passes.
# - Added coordinate-based pass distances so both successful and unsuccessful
#   passes can be retained.
# - Calculated distance from the carrier to:
#     1. The targeted receiver
#     2. The best realistic passing option
# - Added start distance, end distance, and distance change.
# - Added the difference between the targeted-pass distance and the
#   best-option distance.
#
# PASS OUTCOME:
# - Added targeted_pass_successful.
# - This outcome applies only to the targeted pass because the best option
#   was not attempted.
# - Pass outcome should be used for validation or separate analysis, not as
#   a predictor of PTR.
#
# MOVEMENT:
# - Added stationary indicators for the carrier, targeted receiver, and
#   best passing option.
# - A player is classified as stationary when movement distance is <= 1 metre.
# - Previously, stationary players had NA for movement angle.
# - Stationary players now receive an angle of 0.
# - The stationary indicator should be used together with the angle variable.
#
# VARIABLE NAMES:
# - option_run_forward -> best_option_run_forward
# - option_run_lateral -> best_option_run_lateral
# - option_run_dist    -> best_option_run_dist
# - option_run_angle   -> best_option_run_angle
