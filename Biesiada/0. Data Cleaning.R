### 0. Data Cleaning 
### Goal: Help to connect matchdata with tracking data

# 0. Loading Library and Data
library(tidyverse)
library(jsonlite)

# matchdata <- () # put your dataset json 
# events <- () #put your events csv
# tracking < - () #put your tracking json

# 1. Making Tracking Data longer so each frame represents 1 player
players_long <- tracking |>
  select(frame, timestamp, period, player_data) |>
  unnest(player_data) |>
  rename(
    player_x = x,
    player_y = y
  )

head(players_long)

# 2. Adding Matchdata info to players id for clean look

#For all 
players_lookup_all <- matchdata$players |>
  as_tibble() |>
  unnest_wider(player_role, names_sep = "_")

# Clean Version (only important variables)
players_lookup_clean <- matchdata$players |>
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


# 3. Joining Tables (tracking with match data) by player id
players_joined <- players_long |>
  left_join(players_lookup_clean, by = "player_id")


# 4. Creating a Ball data 
ball_long <- tracking |>
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

# 5. Adding a Ball frame to Player data for analysis
players_with_ball <- players_joined |>
  left_join(ball_long, by = c("frame", "timestamp", "period"))


# 6. Saving
write_csv(players_joined,"players_joined.csv")
write_csv(ball_long,"ball_long.csv")
