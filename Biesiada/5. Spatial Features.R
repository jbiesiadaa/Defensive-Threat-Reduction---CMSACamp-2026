# 5. Spatial Features 


# Compute defender-geometry features from RAW TRACKING at the pass moment:
#   receiver_pressure : distance of nearest defender to the BEST option
#   lane_coverage     : min distance of any defender to the carrier->option lane
#   n_in_lane_2m      : defenders within 2m of that lane
# escape_gap = def_dist_target - def_dist_best_option
# positive = the attacker passed to a FREER man than the dangerous one
# -> the spatial signature of "forced to the safe outlet"


library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(tidyverse)

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

saveRDS(tracking_10, "tracking_10_idea.rds")

colnames(tracking_10)
colnames(tracking_10$player_data)



# JOIN + STANDARDIZE: events <-> tracking, one game
# Two snapshots: frame_start (reception context) + frame_end (decision moment)
# ==============================================================================
library(jsonlite)
library(tidyverse)
library(data.table)

# 1. Load data
matchdata <- fromJSON("mls_skillcorner/match_data/match_742721_data.json")
events    <- read.csv("mls_skillcorner/dynamic_events/match_742721_events.csv")
tracking  <- fromJSON("mls_skillcorner/tracking/match_742721_tracking.json")

# 2. Player lookup
players_lookup <- matchdata$players |>
  as_tibble() |>
  unnest_wider(player_role, names_sep = "_") |>
  select(player_id = id, player_name = short_name, team_id, number,
         position = player_role_name,
         position_group = player_role_position_group,
         position_acronym = player_role_acronym) |>
  mutate(player_id = as.integer(player_id),
         team_id   = as.integer(team_id))                      # FIX: int ids

# 3. Tracking long
players <- tracking |>
  select(frame, timestamp, period, player_data) |>
  unnest(player_data) |>
  rename(player_x = x, player_y = y) |>
  mutate(player_id = as.integer(player_id)) |>                 # FIX: int ids
  left_join(players_lookup, by = "player_id")

# 4. Ball long (kept for later; not needed for the 7 variables)
ball <- tracking |>
  select(frame, timestamp, period, ball_data) |>
  unnest_wider(ball_data, names_sep = "_") |>
  transmute(frame, timestamp, period,
            ball_x = ball_data_x, ball_y = ball_data_y,
            ball_z = ball_data_z, ball_detected = ball_data_is_detected)

# 5. Carrier id on ALL event types
events <- events |>
  mutate(
    player_in_possession_id = if_else(
      event_type == "player_possession" & is.na(player_in_possession_id),
      player_id, player_in_possession_id),
    player_in_possession_id = as.integer(player_in_possession_id),
    player_targeted_id      = as.integer(player_targeted_id) 
  ) |>
  left_join(players_lookup |>
              select(player_id, attacking_team_id = team_id),
            by = c("player_in_possession_id" = "player_id"))

setDT(players); setDT(events)

# 6. SNAPSHOT BUILDER (one function, both frames -- no duplicated code)
make_snapshot <- function(events, players, frame_col) {
  events |>
    select(event_id, match_id, event_type, attacking_team_id, attacking_side,
           player_in_possession_id,                                # FIX: carrier in
           player_targeted_id,                                    # NEW          
           frame_used = all_of(frame_col),
           x_start, y_start, x_end, y_end) |>
    left_join(players, by = c("frame_used" = "frame"),
              relationship = "many-to-many") |>                    # FIX: declared
    mutate(
      player_x = if_else(attacking_side == "right_to_left", -player_x, player_x),
      player_y = if_else(attacking_side == "right_to_left", -player_y, player_y),  #
      side = if_else(team_id == attacking_team_id, "attack", "defense")
    )|>
  distinct(event_id, player_id, .keep_all = TRUE)                # dup guard
}

snapshot_start <- make_snapshot(events, players, "frame_start")  # reception
snapshot_end   <- make_snapshot(events, players, "frame_end")    # decision (primary)

# 7. NEW: TARGETED PLAYER'S POSITION AT frame_start (from tracking, flipped)
target_pos_start <- snapshot_start |>
  filter(event_type == "player_possession",
         player_id == player_targeted_id,
         !is.na(player_x)) |>
  transmute(event_id = as.character(event_id),
            player_targeted_x_start = player_x,
            player_targeted_y_start = player_y)


# 8. VERIFICATION -- V1 on both snapshots (flip certified vs SkillCorner's xy)
# coverage: how many possessions got a start position?
nrow(target_pos_start)   # expect ~85-95% of pass possessions (~800+ of ~898)

# ratio sanity
cat("rows per event -- start:", round(nrow(snapshot_start) / nrow(events), 1),
    "| end:", round(nrow(snapshot_end) / nrow(events), 1), " (expect ~22)\n")

cat("--- V1, possession rows only ---\n")
snapshot_end |>
  filter(event_type == "player_possession",
         player_id == player_in_possession_id) |>
  summarise(n = n(),
            cor_x = cor(player_x, x_end, use = "complete.obs"),
            cor_y = cor(player_y, y_end, use = "complete.obs")) |>
  print()

snapshot_start |>
  filter(event_type == "player_possession",
         player_id == player_in_possession_id) |>
  summarise(n = n(),
            cor_x = cor(player_x, x_start, use = "complete.obs"),
            cor_y = cor(player_y, y_start, use = "complete.obs")) |>
  print()


# 9. Join to the analysis 

# join the target's start position (one game) onto the analysis dataset
analysis <- analysis |>
  mutate(event_id = as.character(event_id)) |>
  left_join(target_pos_start, by = "event_id")   # target_pos_start already has character event_id

# verify: only game 742721 rows should have values
analysis |>
  summarise(n = n(),
            pct_has_target_start = mean(!is.na(player_targeted_x_start)) * 100)
# expect: ~87% for TRUE (your 783 coverage), 0% for FALSE


# Verify Coordinates x and y for the sitatuon when he have the chose_best_option TRUE
analysis |>
  filter(
    chose_best_option %in% TRUE,
    !is.na(player_targeted_x_pass),
    !is.na(player_targeted_y_pass),
    !is.na(best_option_x_end),
    !is.na(best_option_y_end)
  ) |>
  summarise(
    n = n(),
    cor_x = cor(best_option_x_end, player_targeted_x_pass),
    cor_y = cor(best_option_y_end, player_targeted_y_pass),
    max_x_gap = max(abs(best_option_x_end - player_targeted_x_pass)),
    max_y_gap = max(abs(best_option_y_end - player_targeted_y_pass))
  )

# Checking Logic 
analysis|>
  filter(PTR_raw == 0)|>
  select(chose_best_option,player_targeted_x_pass, best_option_x_end )
