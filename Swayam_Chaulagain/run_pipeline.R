
library(tidyverse)
library(jsonlite)
library(data.table)
library(purrr)
library(dplyr)


to_bool <- function(x) {
  ifelse(
    is.na(x),
    NA,
    x %in% c(TRUE, "True", "TRUE", "true", 1, "1")
  )
}


# Build list of available games
# -------------------------------------------------------------------------

event_files <- tibble(
  event_file = list.files(
    "mls_skillcorner/dynamic_events",
    pattern = "_events\\.csv$",
    full.names = TRUE
  )
) |>
  mutate(
    game_id = str_extract(
      basename(event_file),
      "\\d+"
    )
  )


tracking_files <- tibble(
  tracking_file = list.files(
    "mls_skillcorner/tracking",
    pattern = "_tracking(\\.jsonl|\\.json)?$",
    full.names = TRUE
  )
) |>
  mutate(
    game_id = str_extract(
      basename(tracking_file),
      "\\d+"
    )
  )


match_files <- tibble(
  match_file = list.files(
    "mls_skillcorner/match_data",
    pattern = "_data\\.json$",
    full.names = TRUE
  )
) |>
  mutate(
    game_id = str_extract(
      basename(match_file),
      "\\d+"
    )
  )


# -------------------------------------------------------------------------
# Remove unusable games
# -------------------------------------------------------------------------

games_to_remove <- c(
  "1066470","1096007","1106283","648779","648780",
  "649421","649422","649433","649434","651546",
  "688134","688136","708458","760689","880422",
  "895807","907133","915267"
)


games <- event_files |>
  inner_join(
    tracking_files,
    by = "game_id"
  ) |>
  inner_join(
    match_files,
    by = "game_id"
  ) |>
  filter(
    !game_id %in% games_to_remove
  )


games <- games |>
  slice_head(n = 2)


# -------------------------------------------------------------------------
# Run feature engineering
# -------------------------------------------------------------------------

results <- purrr::pmap_dfr(
  
  games,
  
  function(game_id,
           event_file,
           tracking_file,
           match_file){
    
    message("Processing game ", game_id)
    
    process_game(
      event_file = event_file,
      tracking_file = tracking_file,
      match_file = match_file
    )
    
  }
  
)




# 1. Distribution of minimum distances
results |>
  select(min_dist_to_best_option_lane, min_dist_to_targeted_lane) |>
  summary()

# 2. Distribution of defender counts in each lane
results |>
  count(n_defenders_in_best_option_lane)

results |>
  count(n_defenders_in_targeted_lane)

# 3. The key comparison — is the best option's lane MORE obstructed
#    than the lane to the pass actually chosen?
results |>
  summarise(
    mean_lane_obstruction_diff = mean(lane_obstruction_diff, na.rm = TRUE),
    median_lane_obstruction_diff = median(lane_obstruction_diff, na.rm = TRUE),
    pct_targeted_more_open = mean(lane_obstruction_diff > 0, na.rm = TRUE),
    
    mean_lane_count_diff = mean(lane_count_diff, na.rm = TRUE),
    median_lane_count_diff = median(lane_count_diff, na.rm = TRUE),
    pct_targeted_fewer_defenders = mean(lane_count_diff < 0, na.rm = TRUE)
  )

# 4. Cross-tab: when best option's lane IS blocked but targeted lane ISN'T,
#    is that a large share of cases?
results |>
  mutate(
    best_blocked = n_defenders_in_best_option_lane > 0,
    targeted_blocked = n_defenders_in_targeted_lane > 0
  ) |>
  count(best_blocked, targeted_blocked) |>
  mutate(pct = n / sum(n))


