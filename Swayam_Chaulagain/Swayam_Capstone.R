# Project Start

library(tidyverse)
library(jsonlite)
library(gt)
library(dplyr)
library(sportyR)



# Working with multiple games (Event data)
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





# Working with multiple tracking data

# Choosing the folder
folder <- "mls_skillcorner/tracking"

# Filter out the games that event data is missing
games_to_remove <- c(
  "1066470", "1096007", "1106283", "648779", "648780",
  "649421", "649422", "649433", "649434", "651546",
  "688134", "688136", "708458", "760689", "880422",
  "895807", "907133", "915267"
)

# All files info
files <- list.files(
  path = folder,
  pattern = "_tracking(\\.jsonl|\\.json)?$",
  full.names = TRUE
)

file_info <- tibble(
  file = files,
  game_id = str_extract(basename(files), "\\d+")
)

# Filtering the games with event data
files_keep <- file_info |>
  filter(!game_id %in% games_to_remove)

# Double checking if the games are removed
files_keep |>
  filter(game_id %in% games_to_remove)

# Extracting 10 games
files_10 <- files_keep$file[1:min(10, nrow(files_keep))]

# Saving 
tracking_10 <- map_dfr(files_10, function(file_path) {
  
  if (str_detect(file_path, "\\.jsonl$")) {
    data <- stream_in(file(file_path), verbose = FALSE)
  } else {
    data <- fromJSON(file_path, flatten = TRUE)
  }
  
  data <- as.data.frame(data)
  
  data$source_file <- file_path
  data$game_id <- str_extract(basename(file_path), "\\d+")
  
  return(data)
})

#  Saving 10 games

saveRDS(tracking_10, "tracking_10.rds")







# Working with multiple match data 




# Choosing the folder
folder <- "mls_skillcorner/match_data"

# Filter out the games that event data is missing
games_to_remove <- c(
  "1066470", "1096007", "1106283", "648779", "648780",
  "649421", "649422", "649433", "649434", "651546",
  "688134", "688136", "708458", "760689", "880422",
  "895807", "907133", "915267"
)

# All files info
files <- list.files(
  path = folder,
  pattern = "_data\\.json$",
  full.names = TRUE
)

file_info <- tibble(
  file = files,
  game_id = str_extract(basename(files), "\\d+")
)

# Filtering the games with event data
files_keep <- file_info |>
  filter(!game_id %in% games_to_remove)

# Double checking if the games are removed
files_keep |>
  filter(game_id %in% games_to_remove)

# Extracting 10 games
files_10 <- files_keep$file[1:min(10, nrow(files_keep))]



# read and combine 10 games
match_10 <- do.call(
  rbind,
  lapply(files_10, function(file) {
    data <- fromJSON(file)
    data$source_file <- file
    data$game_id <- gsub("match_|_data.json", "", basename(file))
    return(data)
  })
)

#  Saving 10 games

saveRDS(match_10, "match.rds")



................................................................................




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
    best_option_x = ifelse(any(realistic),
                           x_end[which.max(ifelse(realistic, xthreat, -Inf))][1], NA_real_),
    best_option_y = ifelse(any(realistic),
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
    # distance controls -- MUST live after the joins (need best_option_x/y).
    # Event-coordinate math is safe: both endpoints share the mirrored
    # system; opponent goal fixed at (52.5, 0).
    dist_carrier_to_goal   = sqrt((52.5 - x_end)^2 + (0 - y_end)^2),
    dist_best_option_to_goal    = sqrt((52.5 - best_option_x)^2 +
                                         (0 - best_option_y)^2),
    dist_targeted_player_to_goal = sqrt((52.5 - player_targeted_x_pass)^2 +
                                          (0 - player_targeted_y_pass)^2),
    
    dist_carrier_to_best_option = sqrt((x_end - best_option_x)^2 +
                                         (y_end - best_option_y)^2),
    dist_carrier_to_targeted_player = sqrt( (x_end - player_targeted_x_pass)^2 +
                                              (y_end - player_targeted_y_pass)^2),
    dist_targeted_to_best_option = sqrt((player_targeted_x_pass - best_option_x)^2 +(player_targeted_y_pass - best_option_y)^2),
    # was the best option a RUNNER or a static outlet?
    best_best_option_run_dist    = sqrt((best_option_x - best_option_x_start)^2 +
                                          (best_option_y - best_option_y_start)^2),
    best_best_option_run_forward = best_option_x - best_option_x_start,  # + = ran toward goal
    # Lateral movement of the best option receiver
    best_option_run_lateral =
      best_option_y - best_option_y_start,
    # Absolute lateral movement, regardless of direction
    best_option_run_lateral_abs =
      abs(best_option_y - best_option_y_start),
    # Direction of the receiver's run in degrees
    best_option_run_angle =
      atan2(
        best_option_y - best_option_y_start,
        best_option_x - best_option_x_start
      ) * 180 / pi,
    
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





................................................................................



# Creating Compactness variable from the tracking data and adding it to the event data above


# 1. Player lookup table
players_lookup <- match_10$players |>
  as_tibble() |>
  unnest(players) |>
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


# Adding player_in_possession_id for the "player_possession" event

events <- events |>
  mutate(
    player_in_possession_id = if_else(
      event_type == "player_possession" & is.na(player_in_possession_id),
      player_id,
      player_in_possession_id
    )
  )


#  Attach attacking team id to all the events
events <- events |>
  left_join(
    players_lookup |>
      select(player_id, attacking_team_id = team_id),
    by = c("player_in_possession_id" = "player_id"))



# Getting attacking side of a team in a first and second period
team_direction <- events |>
  filter(team_id == attacking_team_id) |>
  distinct(team_id, period, attacking_side)


# standarizing player's coordinates all at once
players_std <- players |>
  left_join(team_direction, by = c("team_id", "period")) |>
  mutate(
    player_x = if_else(attacking_side == "right_to_left", -player_x, player_x),
    player_y = if_else(attacking_side == "right_to_left", -player_y, player_y)
  ) |>
  select(-attacking_side)





# Convert tables to data.table for fast joins 
setDT(players_std) 
setDT(events)




# Joining full 22 player position to each event's frame_start (snapshot_start)
# Standarizing tracking data to event data "attacking side is always from left to right"

snapshot_start <- events |>
  select(event_id, match_id, frame_start, attacking_team_id, x_start, y_start) |>
  left_join(players_std, by = c("frame_start" = "frame")) |>
  mutate(side = if_else(team_id == attacking_team_id, "attack", "defense")) |>
  filter(position_acronym != "GK")

snapshot_end <- events |>
  select(event_id, match_id, frame_end, attacking_team_id, x_end, y_end) |>
  left_join(players_std, by = c("frame_end" = "frame")) |>
  mutate(side = if_else(team_id == attacking_team_id, "attack", "defense")) |>
  filter(position_acronym != "GK")


# Calculating surface area of the players
polygon_area <- function(x, y) {
  if (length(x) < 3) return(NA_real_)   # need at least 3 points to enclose an area
  ord <- chull(x, y)                     # order of points forming the outer boundary
  x <- x[ord]; y <- y[ord]
  x_next <- c(x[-1], x[1])               # each point's neighbor, wrapping around
  y_next <- c(y[-1], y[1])
  1/2 * abs(sum(x * y_next - x_next * y))  # shoelace formula for polygon area
}



# Team level Compactness (Surface area and spread)

team_compactness_start <- snapshot_start |>
  filter(side == "defense") |>
  group_by(event_id) |>
  summarise(
    team_surface_area = polygon_area(player_x, player_y),
    team_spread = sum((player_x - mean(player_x))^2 + (player_y - mean(player_y))^2),
    team_length = max(player_x) - min(player_x),
    team_width  = max(player_y) - min(player_y))


team_compactness_end <- snapshot_end |>
  filter(side == "defense") |>
  group_by(event_id) |>
  summarise(
    team_surface_area_end = polygon_area(player_x, player_y),
    team_spread_end = sum((player_x - mean(player_x))^2 + (player_y - mean(player_y))^2),
    team_length_end = max(player_x) - min(player_x),
    team_width_end  = max(player_y) - min(player_y)
  )


# Compactness only on the 5 nearest opponent to the ball carrier

dc_ball_near_start <- snapshot_start |>
  filter(side == "defense") |>
  mutate(dist_to_ball_carrier = sqrt((player_x - x_start)^2 + (player_y - y_start)^2)) |>
  group_by(event_id) |>
  slice_min(dist_to_ball_carrier, n = 5) |>
  summarise(
    nearest_surface_area = polygon_area(player_x, player_y),
    nearest_spread = sum((player_x - mean(player_x))^2 + (player_y - mean(player_y))^2)
  )


dc_ball_near_end <- snapshot_end |>
  filter(side == "defense") |>
  mutate(dist_to_ball_carrier = sqrt((player_x - x_end)^2 + (player_y - y_end)^2)) |>
  group_by(event_id) |>
  slice_min(dist_to_ball_carrier, n = 5) |>
  summarise(
    nearest_surface_area_end = polygon_area(player_x, player_y),
    nearest_spread_end = sum((player_x - mean(player_x))^2 + (player_y - mean(player_y))^2)
  )


# Compactness of the defenders and defensive midfielders

dc_def_mid_start <- snapshot_start |>
  filter(side == "defense", position_acronym %in% c("LB","LWB","LCB","CB","RCB","RWB","RB","LDM","DM","RDM", "LM","CM","RM","AM")) |>
  group_by(event_id) |>
  summarise(
    dc_defmid_surface_area = polygon_area(player_x, player_y),
    dc_defmid_spread = sum((player_x - mean(player_x))^2 + (player_y - mean(player_y))^2)
  )


dc_def_mid_end <- snapshot_end |>
  filter(side == "defense", position_acronym %in% c("LB","LWB","LCB","CB","RCB","RWB","RB","LDM","DM","RDM", "LM","CM","RM","AM")) |>
  group_by(event_id) |>
  summarise(
    dc_defmid_surface_area_end = polygon_area(player_x, player_y),
    dc_defmid_spread_end = sum((player_x - mean(player_x))^2 + (player_y - mean(player_y))^2)
  )



# Nearest Defender distance to the ball carrier 
# Nearest 2nd defender to the ball carrier
# No. of defenders in 5m radius


proximity_start <- snapshot_start |>
  filter(side == "defense") |>
  mutate(dist_to_ball_carrier = sqrt((player_x - x_start)^2 + (player_y - y_start)^2)) |>
  group_by(event_id) |>
  summarise(
    nearest_def_dist = min(dist_to_ball_carrier),
    second_nearest_def_dist = sort(dist_to_ball_carrier)[2],
    n_within_5m = sum(dist_to_ball_carrier <= 5)
  )



proximity_end <- snapshot_end |>
  filter(side == "defense") |>
  mutate(dist_to_ball_carrier = sqrt((player_x - x_end)^2 + (player_y - y_end)^2)) |>
  group_by(event_id) |>
  summarise(
    nearest_def_dist_end = min(dist_to_ball_carrier),
    second_nearest_def_dist_end = sort(dist_to_ball_carrier)[2],
    n_within_5m_end = sum(dist_to_ball_carrier <= 5)
  )



# Computing the gain between frame_start vs end

events <- events |>
  left_join(team_compactness_start, by = "event_id") |>
  left_join(dc_ball_near_start, by = "event_id") |>
  left_join(dc_def_mid_start, by = "event_id") |>
  left_join(proximity_start, by = "event_id") |>
  left_join(team_compactness_end, by = "event_id") |>
  left_join(dc_ball_near_end, by = "event_id") |>
  left_join(dc_def_mid_end, by = "event_id") |>
  left_join(proximity_end, by = "event_id") |>
  mutate(
    team_surface_area_gain = team_surface_area_end - team_surface_area,
    nearest_surface_area_gain = nearest_surface_area_end - nearest_surface_area,
    nearest_spread_gain = nearest_spread_end - nearest_spread,
    dc_def_mid_gain = dc_defmid_surface_area - dc_defmid_surface_area_end)








..................................







library(tidyverse)


# Creating Table of games

# Event files
event_files <- tibble(
  event_file = list.files(
    "mls_skillcorner/dynamic_events",
    pattern = "_events\\.csv$",
    full.names = TRUE
  )
) |>
  mutate(game_id = str_extract(basename(event_file), "\\d+"))

# Tracking files
tracking_files <- tibble(
  tracking_file = list.files(
    "mls_skillcorner/tracking",
    pattern = "_tracking(\\.jsonl|\\.json)?$",
    full.names = TRUE
  )
) |>
  mutate(game_id = str_extract(basename(tracking_file), "\\d+"))

# Match files
match_files <- tibble(
  match_file = list.files(
    "mls_skillcorner/match_data",
    pattern = "_data\\.json$",
    full.names = TRUE
  )
) |>
  mutate(game_id = str_extract(basename(match_file), "\\d+"))



# Remove unwanted games

games_to_remove <- c(
  "1066470","1096007","1106283","648779","648780",
  "649421","649422","649433","649434","651546",
  "688134","688136","708458","760689","880422",
  "895807","907133","915267"
)


games <- event_files |>
  inner_join(tracking_files, by="game_id") |>
  inner_join(match_files, by="game_id") |>
  filter(!game_id %in% games_to_remove) |>
  slice(1:10)





process_game <- function(event_file,
                         tracking_file,
                         match_file){
  
  # Read files
  
  events <- read_csv(event_file, show_col_types = FALSE)
  
  tracking <-
    if(str_detect(tracking_file,"jsonl")){
      stream_in(file(tracking_file), verbose = FALSE)
    } else{
      fromJSON(tracking_file)
    }
  
  match <- fromJSON(match_file)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  return(events)
  
}



results <- pmap_dfr(
  games,
  function(game_id,
           event_file,
           tracking_file,
           match_file){
    
    message("Processing ", game_id)
    
    process_game(
      event_file,
      tracking_file,
      match_file
    )
    
  }
)













