# Project Start

library(tidyverse)
library(jsonlite)
library(gt)
library(dplyr)


Event_data <- read.csv("mls_skillcorner/dynamic_events/match_649419_events.csv")
Tracking_data <- fromJSON("mls_skillcorner/tracking/match_649419_tracking.json")
Match_data <- fromJSON("mls_skillcorner/match_data/match_649419_data.json")
Meta_data <- fromJSON("mls_skillcorner/metadata/match_649419_info.json")


# finding the missing 18 dynamic events data
files1 <- list.files("mls_skillcorner/dynamic_events", pattern="\\.csv$")
files2 <- list.files("mls_skillcorner/tracking", pattern="\\.json$")


ids_csv <- sub(".*match_([0-9]+)_events\\.csv$", "\\1", basename(files1))
ids_json  <- sub(".*match_([0-9]+)_tracking\\.json$", "\\1", basename(files2))

# Matches that exist in CSV folder but are missing in JSON folder
missing_in_csv <- setdiff(ids_json, ids_csv)
missing_in_csv


table(Event_data$event_type_id)
table(Event_data$event_type)


# off_ball_run

off_ball_mls <- Event_data |>
  filter(event_type_id == 1)


# passing_option 
passing_option <- Event_data |>
  filter(event_type_id ==7)


# player possession
player_possession <- Event_data |>
  filter(event_type_id == 8)


# on ball_engagement
on_ball_engagement <- Event_data |>
  filter(event_type_id == 9)


# remove the columns with all NAs and all empty values
OBE_new <- on_ball_engagement[ , colSums(is.na(on_ball_engagement)) < nrow(on_ball_engagement)]
OBE_new <- OBE_new[, colSums(OBE_new != "" & !is.na(OBE_new)) > 0]


PP_new <- player_possession[ , colSums(is.na(player_possession)) < nrow(player_possession)]
PP_new <- PP_new[, colSums(PP_new != "" & !is.na(PP_new)) > 0]





# left join
merged_eda <- merge(x = Event_data, y = Tracking_data, 
  by.x = c("period", "frame_start"), 
  by.y = c("period", "frame"),      
  all.x = TRUE                                  
)



# Filtering around 47 interesting variables to work on

EDA_subset <- merged_eda |>
  select(
    # Context
    match_id, period, frame_start, frame_end, duration,
    
    # Core Targets
    xthreat, xpass_completion, dangerous, force_backward, 
    reduce_possession_danger, possession_danger, xloss_player_possession_end, end_type, 
    
    # Defensive Shape
    x_start, y_start, x_end, y_end, inside_defensive_shape_start, organised_defense, defensive_structure, 
    n_defensive_lines, last_defensive_line_x_start, last_defensive_line_height_start,
    interplayer_distance_start, interplayer_distance_min,
    
    # Options & Pressure
    n_passing_options, n_passing_options_at_start, n_passing_options_line_break, n_passing_options_last_line_break, 
    n_passing_options_ahead, passing_option_score, distance_to_player_in_possession_start, separation_start,
    n_opponents_ahead_player_in_possession_pass_moment,
    
    # Action Types
    pass_distance, pass_angle, pass_direction, carry, one_touch, quick_pass, speed_avg, forward_momentum, trajectory_angle,
    lead_to_goal, lead_to_shot,
    
    # Tracking Lists 
    ball_data, player_data
    
  )


# Comparing defensive structure vs attacking teams' options 

EDA_subset |>
  filter(inside_defensive_shape_start != "") |>
  group_by(inside_defensive_shape_start) |>
  summarise(
    #count = n(),
    avg_passing_options = mean(n_passing_options, na.rm = TRUE),
    avg_pass_completion = mean(xpass_completion, na.rm = TRUE)*100,
    pct_organized_defense = mean(organised_defense == "True", na.rm = TRUE)*100,
    avg_threat_present   = mean(xthreat, na.rm = TRUE)) |>
  gt() |>
  tab_header(
    title = md("Defensive Block Analysis")) |>
  cols_label(inside_defensive_shape_start = "Inside Defensive Shape",
    #count = "Event Count",
    avg_passing_options = "Avg Passing Options",
    avg_pass_completion = "Avg Pass Completion %",
    pct_organized_defense = "Organized Defense %",
    avg_threat_present = "Avg Threat Present") |>
  fmt_number(columns = c(avg_passing_options), decimals = 2) |>
  fmt_number(columns = c(avg_pass_completion), decimals = 1) |>
  fmt_number(columns = c(pct_organized_defense), decimals = 1) |>
  fmt_number(columns = c(avg_threat_present), decimals = 4) |>
  tab_options(heading.align = "center",
    column_labels.background.color = "bisque")

colnames(Event_data)




table(Event_data$affected_line_break)

table(Event_data$affected_line_breaking_passing_option_dangerous)

table( Event_data$team_in_possession_phase_type,Event_data$team_out_of_possession_phase_type, Event_data$team_id) 


table(Event_data$defensive_structure)



EDA_subset |>
  filter(inside_defensive_shape_start != "") |>
  group_by(inside_defensive_shape_start) |>
  summarise(
    #count = n(),
    avg_passing_options = mean(n_passing_options, na.rm = TRUE),
    avg_pass_completion = mean(xpass_completion, na.rm = TRUE)*100,
    pct_organized_defense = mean(organised_defense == "True", na.rm = TRUE)*100,
    avg_threat_present   = mean(xthreat, na.rm = TRUE)) |>
  gt() |>
  tab_header(
    title = md("Defensive Block Analysis")) |>
  cols_label(inside_defensive_shape_start = "Inside Defensive Shape",
             #count = "Event Count",
             avg_passing_options = "Avg Passing Options",
             avg_pass_completion = "Avg Pass Completion %",
             pct_organized_defense = "Organized Defense %",
             avg_threat_present = "Avg Threat Present") |>
  fmt_number(columns = c(avg_passing_options), decimals = 2) |>
  fmt_number(columns = c(avg_pass_completion), decimals = 1) |>
  fmt_number(columns = c(pct_organized_defense), decimals = 1) |>
  fmt_number(columns = c(avg_threat_present), decimals = 4) |>
  tab_options(heading.align = "center",
              column_labels.background.color = "bisque")

colnames(Event_data)

table(Event_data$player_targeted_xthreat)


View(
Event_data |>
  filter(player_targeted_xthreat > 0.1) |>
  select(time_start, xthreat, player_targeted_xthreat, lead_to_goal, lead_to_shot)

)

  
  
  
  
  
  
  
  
  
