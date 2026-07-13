# 
# make_option_features <- function(events) {
# 
#   options <- events |>
#     filter(event_type_id == 7)
# 
#   option_features <- options |>
#     filter(!is.na(xthreat)) |>
#     mutate(
#       realistic =
#         !is.na(xpass_completion) &
#         xpass_completion > 0.68
#     ) |>
#     group_by(
#       match_id,
#       associated_player_possession_event_id
#     ) |>
#     summarise(
# 
#       # Number of available passing options
#       n_options_counted = n(),
# 
#       # -----------------------------------------------------------------------
#       # Best available option (highest xThreat)
#       # -----------------------------------------------------------------------
# 
#       max_xthreat_all =
#         max(xthreat, na.rm = TRUE),
# 
#       best_option_event_id =
#         event_id[which.max(xthreat)][1],
# 
#       best_option_player_id =
#         as.integer(
#           player_id[which.max(xthreat)][1]
#         ),
# 
# 
# 
#       best_option_x_start =
#         x_start[which.max(xthreat)][1],
# 
#       best_option_y_start =
#         y_start[which.max(xthreat)][1],
# 
#       best_option_x =
#         x_end[which.max(xthreat)][1],
# 
#       best_option_y =
#         y_end[which.max(xthreat)][1],
# 
# 
# 
# 
#       # Best realistic option
#       # (used only for PTR calculation)
#       # -----------------------------------------------------------------------
#         max_xthreat_realistic =
#           if (any(realistic & !is.na(xthreat))) {
#             max(xthreat[realistic & !is.na(xthreat)], na.rm = TRUE)
#           } else {
#             NA_real_
#           },
# 
# 
#       .groups = "drop"
#     )
# 
# 
# 
# 
# 
# 
#   option_features
# 
# }


make_option_features <- function(events) {

  # which option did the carrier actually target? (needed for the tie-break)
  possessions_targets <- events |>
    filter(event_type_id == 8) |>
    transmute(
      match_id ,
      associated_player_possession_event_id,
      chosen_option_event_id = targeted_passing_option_event_id
    ) |>
    distinct(match_id, associated_player_possession_event_id,
             .keep_all = TRUE)

  option_features <- events |>
    filter(event_type_id == 7, !is.na(xthreat)) |>
    mutate(
      realistic = !is.na(xpass_completion) &
        xpass_completion > 0.68
    ) |>
    left_join(possessions_targets,
              by = c("match_id",
                     "associated_player_possession_event_id")) |>
    mutate(is_targeted =
             replace_na(event_id == chosen_option_event_id, FALSE)) |>
    group_by(match_id, associated_player_possession_event_id) |>
    # highest xThreat first
    arrange(desc(xthreat), desc(is_targeted), event_id, .by_group = TRUE) |>
    summarise(
      n_options_counted     = n(),
      n_options_realistic   = sum(realistic),
      max_xthreat_all       = max(xthreat),
      max_xthreat_realistic = if (any(realistic))
        max(xthreat[realistic]) else NA_real_,
      best_was_tied         = if (any(realistic))
        sum(realistic &
              xthreat == max(xthreat[realistic])) > 1
      else NA,

      # best REALISTIC option -- every field from the same (first) row
      best_option_event_id  = if (any(realistic))
        first(event_id[realistic])  else NA_character_,
      best_option_player_id = if (any(realistic))
        as.integer(first(player_id[realistic]))
      else NA_integer_,
      best_option_x_start   = if (any(realistic))
        first(x_start[realistic])   else NA_real_,
      best_option_y_start   = if (any(realistic))
        first(y_start[realistic])   else NA_real_,
      best_option_x         = if (any(realistic))
        first(x_end[realistic])     else NA_real_,
      best_option_y         = if (any(realistic))
        first(y_end[realistic])     else NA_real_,
      .groups = "drop"
    )

  option_features


}



# targeted option coordinates
make_targeted_option_coords <- function(events) {
  
  targeted_option_coords <- events |> 
    filter(event_type_id == 7) |>
    transmute(
      match_id,
      targeted_passing_option_event_id = event_id,
      player_targeted_x_start = x_start,
      player_targeted_y_start = y_start
    ) |>
    distinct(
      match_id,
      targeted_passing_option_event_id,
      .keep_all = TRUE
    )
  
  targeted_option_coords
  
}








