
library(tidyverse)
library(jsonlite)
library(data.table)
library(dplyr)
library(purrr)
library(stringr)


# Working with one specific game

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
  filter(side == "defense", position_acronym %in% c("LB","LWB","LCB","CB","RCB","RWB","RB","LDM","DM","RDM")) |>
  group_by(event_id) |>
  summarise(
    dc_defmid_surface_area = polygon_area(player_x, player_y),
    dc_defmid_spread = sum((player_x - mean(player_x))^2 + (player_y - mean(player_y))^2)
  )


dc_def_mid_end <- snapshot_end |>
  filter(side == "defense", position_acronym %in% c("LB","LWB","LCB","CB","RCB","RWB","RB","LDM","DM","RDM")) |>
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





















