###############################################################

# Load SkillCorner data and prepare player and ball tracking

# Working with multiple games


library(tidyverse)
library(jsonlite)
library(data.table)
library(dplyr)
library(purrr)
library(stringr)


folder <- "mls_skillcorner/tracking"

# find all tracking files
files <- list.files(
  path = folder,
  pattern = "_tracking(\\.jsonl|\\.json)?$",
  full.names = TRUE
)

# keep first 100 games
files_100 <- files[1:min(10, length(files))]

# read many tracking files
tracking_100 <- map_dfr(files_100, function(file) {
  
  if (str_detect(file, "\\.jsonl$")) {
    data <- stream_in(file(file), verbose = FALSE)
  } else {
    data <- fromJSON(file, flatten = TRUE)
  }
  
  data <- as.data.frame(data)
  
  data$source_file <- file
  
  data$game_id <- basename(file) |>
    str_remove("^match_") |>
    str_remove("_tracking(\\.jsonl|\\.json)?$")
  
  return(data)
})

................................................................................

# 1. Load Data


matchdata <- fromJSON("mls_skillcorner/match_data/match_742721_data.json")
events <- read.csv("mls_skillcorner/dynamic_events/match_742721_events.csv")
tracking <- fromJSON("mls_skillcorner/tracking/match_742721_tracking.json")



# 1. Player lookup table
players_lookup <- matchdata$players |>
  as_tibble() |>
  unnest_wider(player_role, names_sep = "_") |>
  select(
    player_id = id,
    player_name = short_name,
    team_id,
    number,
    position = player_role_name,
    position_group = player_role_position_group,
    position_acronym = player_role_acronym
  )


 # 2. Player tracking data
players <- tracking |>
  select(frame, timestamp, period, player_data) |>
  unnest(player_data) |>
  rename(
    player_x = x,
    player_y = y
  ) |>
  left_join(players_lookup, by = "player_id")


# 3. Ball tracking data
ball <- tracking |>
  select(frame, timestamp, period, ball_data) |>
  unnest_wider(ball_data, names_sep = "_") |>
  transmute(
    frame,
    timestamp,
    period,
    ball_x = ball_data_x,
    ball_y = ball_data_y,
    ball_z = ball_data_z,
    ball_detected = ball_data_is_detected
  )


# 4. Attach attacking team to events
events <- events |>
  left_join(
    players_lookup |>
      select(player_id, attacking_team_id = team_id),
    by = c("player_in_possession_id" = "player_id"))



# Convert tracking to data.table for fast keyed joins ---
setDT(players) 
setDT(events)



# --- 2. Join full player snapshot (all 22 players) to each event's frame_start 
snapshot_start <- events |>
  select(event_id, match_id, frame_start, attacking_team_id,
         x_start, y_start) |>
  left_join(players, by = c("frame_start" = "frame"))


# tag each row as attacker or defender relative to that event
snapshot_start <- snapshot_start |>
  mutate(side = if_else(team_id == attacking_team_id, "attack", "defense"))



# team compactness "how spread out is the defense, left-right, at the start of this event."
compactness <- snapshot_start |>
  filter(side == "defense") |>
  group_by(event_id) |>
  summarise(
    compactness_x_sd = sd(player_x),
    depth_of_block = max(player_x) - min(player_x)
  )


# nearest defender distance / count within radius

proximity <- snapshot_start |>
  filter(side == "defense") |>
  group_by(event_id) |>
  summarise(
    nearest_def_dist = min(sqrt((player_x - x_start)^2 + (player_y - y_start)^2)),
    n_within_5m = sum(sqrt((player_x - x_start)^2 + (player_y - y_start)^2) <= 5)
  )


events <- events |>
  left_join(compactness, by = "event_id") |>
  left_join(gaps, by = "event_id") |>
  left_join(proximity, by = "event_id")
















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




# Initial PTR metric and creating one possession per row data

options_per_frame <- options |>
  filter(!is.na(xthreat)) |> # Keep only options with xThreat
  group_by(match_id, associated_player_possession_event_id) |>
  summarise(
    n_options_counted      = n(), # This counts how many passing options were available for that possession
    max_xthreat_all        = max(xthreat, na.rm = TRUE), # This finds the highest xThreat value among all available passing options
    .groups = "drop"
  )

table(events$xthreat)


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
    match_id, game_id, event_id, player_id, player_name, team_id,
    targeted_passing_option_event_id,
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
                  is_player_possession_start_matched,
                  lead_to_shot, lead_to_goal,
                  is_player_possession_end_matched), to_bool)) |> # Convert text booleans to real TRUE/FALSE
  left_join(options_per_frame, # Join passing option summary
            by = c("match_id",
                   "event_id" = "associated_player_possession_event_id")) |>
  left_join(obe_summary, # Join defensive engagement summary
            by = c("match_id",
                   "event_id" = "associated_player_possession_event_id")) |>
  mutate(
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
    PTR = ifelse(
      !is.na(player_targeted_xthreat) & !is.na(max_xthreat_all),
      pmax(max_xthreat_all - player_targeted_xthreat, 0),
      NA_real_
    ),
  
    # Explain why PTR is present or missing -> This creates a label explaining whether PTR was calculated
    # Why do we have or not have PTR for this possession?
    PTR_status = case_when(
      is.na(max_xthreat_all) ~ "No xThreat option data",
      !is.na(PTR)            ~ "PTR calculated",
      TRUE                   ~ "Chosen pass xthreat missing"
    ),
    
  
    # Create defensive outcome label
    
    defensive_outcome = case_when(
      stop_possession_danger   ~ "Stop (turnover)",
      reduce_possession_danger ~ "Suppression",
      force_backward           ~ "Forced backward",
      engaged                  ~ "Engaged, no outcome",
      TRUE                     ~ "No engagement"
    )
  )
.........................................................















