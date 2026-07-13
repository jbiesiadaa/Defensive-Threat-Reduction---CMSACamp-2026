
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
  slice_head(n = 1)


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











#filter

results <- results |>
  mutate(
    # Did the player choose the best realistic option?
    # Includes tied best options
    chose_best_option = case_when(
      is.na(player_targeted_xthreat) ~ NA,
      is.na(max_xthreat_realistic) ~ NA,
      player_targeted_xthreat == max_xthreat_realistic ~ TRUE,
      TRUE ~ FALSE
    ),

    # Was the chosen pass realistic?
    chosen_pass_realistic = case_when(
      is.na(player_targeted_xpass_completion) ~ NA,
      player_targeted_xpass_completion > 0.68 ~ TRUE,
      TRUE ~ FALSE
    ),

    # No realistic passing option existed
    no_realistic_option = !is.na(max_xthreat_all) &
      is.na(max_xthreat_realistic),

    # Very little time to decide
    short_possession = one_touch %in% TRUE |
      tidyr::replace_na(duration < 0.5, FALSE),

    # Deflections / blocked actions / loose balls
    disruption_possession = case_when(
      is.na(team_out_of_possession_phase_type) ~ NA,
      team_out_of_possession_phase_type == "disruption" ~ TRUE,
      TRUE ~ FALSE
    ),

    # Overreach ("gambler") decisions
    is_overreach = chose_best_option %in% FALSE &
      PTR == 0 &
      !chosen_pass_realistic %in% TRUE
  )


n_start <- nrow(results)

analysis_clean <- results |>
  filter(!is_header %in% TRUE) |>                        # remove headers
  filter(!hand_pass %in% TRUE) |>                        # remove hand/throw-in actions
  filter(is_player_possession_end_matched %in% TRUE) |>  # pass moment reliably tracked
  filter(!is.na(PTR)) |>                                 # PTR is computable
  filter(!is_overreach %in% TRUE) |>                     # remove gamblers
  filter(is_player_possession_start_matched %in% TRUE) |># start features reliable
  filter(!short_possession) |>                           # remove one-touch/very short actions
  filter(!disruption_possession %in% TRUE) |>            # remove disruptions
  filter(!no_realistic_option)                           # remove possessions with no realistic option



analysis_clean_selected <- analysis_clean %>%
  select(
    match_id,
    event_id,
    player_id,
    player_name,
    team_id,
    
    # chosen option
    targeted_passing_option_event_id,
    player_targeted_id,
    player_targeted_xthreat,
    player_targeted_x_start,
    player_targeted_y_start,
    player_targeted_x_pass,
    player_targeted_y_pass,
    
    # best option
    best_option_event_id,
    best_option_player_id,
    best_option_x_start,
    best_option_y_start,
    best_option_x,
    best_option_y,
    max_xthreat_all,
    max_xthreat_realistic,
    best_was_tied,
    
    # decision outcomes
    chose_best_option,
    is_overreach,
    chosen_pass_realistic,
    
    # pass information
    pass_direction,
    pass_distance,
    pass_range,
    pass_outcome,
    player_targeted_xpass_completion,

    
    # pressure/context
    engaged,
    any_pressing,
    any_pressure,
    in_pressing_chain,
    max_chain_length,
    
    # defensive structure
    organised_defense,
    defensive_structure,
    n_defensive_lines,
    inside_defensive_shape_start,
    last_defensive_line_height_start,
    
    # option availability
    n_passing_options,
    n_options_counted,
    n_options_realistic,
    no_realistic_option,
    
    # danger
    lead_to_shot,
    lead_to_goal,
    stop_possession_danger,
    reduce_possession_danger,
    
    # movement
    carrier_move_forward,
    carrier_move_lateral,
    carrier_move_dist,
    target_run_forward,
    target_run_lateral,
    target_run_dist,
    best_option_run_forward,
    best_option_run_lateral,
    best_option_run_dist,
    
    # spacing
    nearest_def_dist,
    second_nearest_def_dist,
    n_within_5m,
    nearest_def_dist_best_option,
    second_nearest_def_dist_best_option,
    n_within_5m_best_option,
    min_dist_to_best_option_lane,
    n_defenders_in_best_option_lane,
    min_dist_to_targeted_lane,
    n_defenders_in_targeted_lane,
    
  )




# checks


analysis_clean |>
  filter(PTR == 0) |>
  summarise(
    n = n(),
    n_chose_best = sum(chose_best_option %in% TRUE, na.rm = TRUE),
    n_overreach = sum(is_overreach %in% TRUE, na.rm = TRUE),
    n_unexplained = sum(
      chose_best_option %in% FALSE &
        is_overreach %in% FALSE,
      na.rm = TRUE
    ),
    n_unexplained_tied = sum(
      chose_best_option %in% FALSE &
        is_overreach %in% FALSE &
        best_was_tied %in% TRUE,
      na.rm = TRUE
    ),
    n_unexplained_not_tied = sum(
      chose_best_option %in% FALSE &
        is_overreach %in% FALSE &
        best_was_tied %in% FALSE,
      na.rm = TRUE
    )
  )
