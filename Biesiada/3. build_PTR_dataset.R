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


# Working with multiple games

folder <- "mls_skillcorner/dynamic_events"

# find all event csv files
files <- list.files(
  path = folder,
  pattern = "_events\\.csv$",
  full.names = TRUE
)

# keep only first 100 games
files_100 <- files[1:100]

# read and combine 100 games
dynamic_100 <- do.call(
  rbind,
  lapply(files_100, function(file) {
    data <- read.csv(file)
    data$source_file <- file
    data$game_id <- gsub("match_|_events.csv", "", basename(file))
    return(data)
  })
)


# -- Step 1: Passing options aggregated at frame_end----------------------------
# We join at frame_end because player_targeted_xthreat is computed at the moment of the pass (frame_end), reducing timing mismatch

options_per_frame <- dynamic_100 |>
  filter(event_type == "passing_option", !is.na(xthreat)) |>
  group_by(match_id, frame_end) |> # MORE OPTIONS, we added by match_id because we have a lot of games
  summarise(
    max_xthreat_available = max(xthreat, na.rm = TRUE),
    .groups = "drop")

# -- Step 2: Defensive outcomes from on_ball_engagement ------------------------

outcomes_per_possession <- dynamic_100 |>
  filter(event_type == "on_ball_engagement") |>
  group_by(match_id, associated_player_possession_event_id) |>
  summarise(
    stop_possession_danger   = any(stop_possession_danger   == "True", na.rm = TRUE), # Did any defensive engagement stop the possession danger?
    reduce_possession_danger = any(reduce_possession_danger == "True", na.rm = TRUE), # Did defense reduce the danger?
    force_backward           = any(force_backward           == "True", na.rm = TRUE), # Did defense force the attacker backward?
    n_engagements            = n(), # For each possession, summarize what the defense achieved.
    .groups = "drop"
  )

# Step 3: Build analysis_data 
analysis_data <- dynamic_500 |>
  filter(event_type == "player_possession") |>
  select(-stop_possession_danger, -reduce_possession_danger, -force_backward) |>
  left_join(options_per_frame, by = c("match_id", "frame_end")) |>
  left_join(outcomes_per_possession, by = c("match_id",
                                            "event_id" =
                                              "associated_player_possession_event_id")) |>
  mutate(
    PTR = ifelse(
      !is.na(player_targeted_xthreat),
      pmax(max_xthreat_available - player_targeted_xthreat, 0),
      NA_real_),
    defensive_outcome = case_when(
      stop_possession_danger   == TRUE ~ "Interception / Turnover",
      reduce_possession_danger == TRUE ~ "Suppression",
      force_backward           == TRUE ~ "Forced Backward",
      n_engagements > 0               ~ "Engaged, No Outcome",
      TRUE                            ~ "No Engagement"
    ))

# Step 4: Remove empty and all-NA columns 
analysis_data <- analysis_data |>
  select(where(~ sum(!is.na(.)) > 0 & sum(. != "", na.rm = TRUE) > 0))

# Analysis table by Defense outcome
PTR_summary <- analysis_data |>
  filter(!is.na(PTR), n_engagements > 0) |>
  mutate(defensive_outcome = factor(defensive_outcome,
                                    levels = c("Suppression", "Interception / Turnover",
                                               "Forced Backward", "Engaged, No Outcome"))) |>
  group_by(defensive_outcome) |>
  summarise(
    n            = n(),
    mean_PTR     = mean(PTR, na.rm = TRUE),
    pct_positive = mean(PTR > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  )

PTR_summary |>
  gt() |>
  cols_label(
    defensive_outcome = "Defensive Outcome",
    n                 = "N",
    mean_PTR          = "Mean PTR",
    pct_positive      = "% Positive PTR"
  ) |>
  tab_header(
    title    = md("**Passing Threat Reduction (PTR) by Outcome**"),
    subtitle = md(" PTR = (max available xThreat − xThreat of pass chosen)")
  ) |>
  tab_source_note(md(
    "**PTR > 0:** attacker chose below their best available option — defense forced attacker a poor pass decision."
  )) |>
  fmt_number(columns = mean_PTR,     decimals = 5) |>
  fmt_number(columns = pct_positive, decimals = 1) |>
  fmt_integer(columns = n) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = defensive_outcome)
  ) |>
  tab_style(
    style = list(cell_fill(color = "#2d6a4f"),
                 cell_text(color = "white", weight = "bold")),
    locations = cells_body(
      columns = pct_positive,
      rows    = pct_positive == max(PTR_summary$pct_positive, na.rm = TRUE)
    )
  ) |>
  tab_style(
    style = list(cell_fill(color = "#2d6a4f"),
                 cell_text(color = "white", weight = "bold")),
    locations = cells_body(
      columns = mean_PTR,
      rows    = mean_PTR == max(PTR_summary$mean_PTR, na.rm = TRUE)
    )
  ) |>
  opt_row_striping() |>
  tab_options(
    table.font.size               = 13,
    heading.title.font.size       = 16,
    column_labels.font.weight     = "bold",
    table.border.top.color        = "#2d6a4f",
    table.border.top.width        = px(3),
    row.striping.background_color = "#f5f5f5"
  )

# MAIN TAKEWAY: 
# Suppression creates the largest average passing threat reduction, while forced backward actions most often make attackers choose below their best available option.


### NEW IDEA ~ JULIA BIESIADA -------------------------------------------------- 

# PHASE 1: Build the PTR (Passing Threat Reduction) analysis dataset

# Compute PTR two ways:
#        PTR_raw       = max(xthreat of ALL options)       - chosen xthreat
#        PTR_realistic = max(xthreat of REALISTIC options) - chosen xthreat
#        Realistic = xpass_completion > 0.68 (SkillCorner's own line-break


library(tidyverse)


# 0. LOAD GAMES
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
  filter(!is.na(xthreat)) |> # Keep only options with xThreat
  mutate(
    realistic = !is.na(xpass_completion) & xpass_completion > 0.68 # Creating a new column realistic (xpass_completion > 0.68)
  ) |>
  group_by(match_id, associated_player_possession_event_id) |>
  summarise(
    n_options_counted      = n(), # This counts how many passing options were available for that possession
    max_xthreat_all        = max(xthreat, na.rm = TRUE), # This finds the highest xThreat value among all available passing options
    max_xthreat_realistic  = ifelse(any(realistic), # This finds the highest xThreat only among realistic options
                                    max(xthreat[realistic], na.rm = TRUE),
                                    NA_real_),
    
    # who was the most dangerous realistic option? (Question: did the attacker abandon the dangerous lane?)
    # This finds the event_id of the most dangerous realistic passing option
    best_option_event_id = ifelse(
      any(realistic),
      event_id[which.max(ifelse(realistic, xthreat, -Inf))][1],
      NA_character_),
    n_realistic = sum(realistic),
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
                  is_player_possession_end_matched), to_bool)) |> # Convert text booleans to real TRUE/FALSE
  left_join(option_summary, # Join passing option summary
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

# checking this outcomes
analysis|>
  count(lead_to_goal, lead_to_shot, any_beaten_by_possession, any_beaten_by_movement)

# ------------------------------------------------------------------------------
# 5. QUALITY FILTERS

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

# Saving for future work
saveRDS(analysis_clean, "ptr_analysis_dataset_200.rds")
saveRDS(analysis_model,  "ptr_model_dataset_200.rds")



