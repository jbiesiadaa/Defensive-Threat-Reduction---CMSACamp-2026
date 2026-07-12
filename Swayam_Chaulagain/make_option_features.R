make_option_features <- function(events) {
  
  options <- events |>
    filter(event_type_id == 7)
  
  option_features <- options |>
    filter(!is.na(xthreat)) |>
    mutate(
      realistic =
        !is.na(xpass_completion) &
        xpass_completion > 0.68
    ) |>
    group_by(
      match_id,
      associated_player_possession_event_id
    ) |>
    summarise(
      
      # Number of available passing options
      n_options_counted = n(),
      
      # -----------------------------------------------------------------------
      # Best available option (highest xThreat)
      # -----------------------------------------------------------------------
      
      max_xthreat_all =
        max(xthreat, na.rm = TRUE),
      
      best_option_event_id =
        event_id[which.max(xthreat)][1],
      
      best_option_player_id =
        as.integer(
          player_id[which.max(xthreat)][1]
        ),
      
      
      
      best_option_x_start =
        x_start[which.max(xthreat)][1],
      
      best_option_y_start =
        y_start[which.max(xthreat)][1],
      
      best_option_x =
        x_end[which.max(xthreat)][1],
      
      best_option_y =
        y_end[which.max(xthreat)][1],
      

      
      
      # Best realistic option
      # (used only for PTR calculation)
      # -----------------------------------------------------------------------
        max_xthreat_realistic =
          if (any(realistic & !is.na(xthreat))) {
            max(xthreat[realistic & !is.na(xthreat)], na.rm = TRUE)
          } else {
            NA_real_
          },

      
      .groups = "drop"
    )
  
  


  
  option_features
  
}