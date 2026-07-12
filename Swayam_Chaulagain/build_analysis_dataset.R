build_analysis_dataset <- function(events,
                                   option_features,
                                   obe_features,
                                   tracking_features) {
  
  analysis <- events |>
    
    filter(
      event_type_id == 8,
      !is.na(targeted_passing_option_event_id)
    ) |>
    
    left_join(
      option_features,
      by = c(
        "match_id",
        "event_id" =
          "associated_player_possession_event_id"
      )
    ) |>
    
    left_join(
      obe_features,
      by = c(
        "match_id",
        "event_id" =
          "associated_player_possession_event_id"
      )
    ) |>
    
    left_join(
      tracking_features,
      by = c("match_id", "event_id")
    ) |>
    
    mutate(
      player_in_possession_id = if_else(
        event_type == "player_possession" &
          is.na(player_in_possession_id),
        player_id,
        player_in_possession_id
      ),
      
      # ======================================================================
      # Passing Threat Reduction
      # ======================================================================
      
      PTR_raw =
        if_else(
          !is.na(player_targeted_xthreat) &
            !is.na(max_xthreat_all),
          pmax(
            max_xthreat_all -
              player_targeted_xthreat,
            0
          ),
          NA_real_
        ),
      
      PTR =
        if_else(
          !is.na(player_targeted_xthreat) &
            !is.na(max_xthreat_realistic),
          pmax(
            max_xthreat_realistic -
              player_targeted_xthreat,
            0
          ),
          NA_real_
        ),
      
      # ======================================================================
      # Spatial distances
      # ======================================================================
      
      dist_carrier_to_goal =
        sqrt(
          (52.5 - x_end)^2 +
            y_end^2
        ),
      
      dist_best_option_to_goal =
        sqrt(
          (52.5 - best_option_x)^2 +
            best_option_y^2
        ),
      
      dist_targeted_player_to_goal =
        sqrt(
          (52.5 - player_targeted_x_pass)^2 +
            player_targeted_y_pass^2
        ),
      
      dist_carrier_to_best_option =
        sqrt(
          (x_end - best_option_x)^2 +
            (y_end - best_option_y)^2
        ),
      
      dist_carrier_to_targeted_player =
        sqrt(
          (x_end - player_targeted_x_pass)^2 +
            (y_end - player_targeted_y_pass)^2
        ),
      
      dist_targeted_to_best_option =
        sqrt(
          (player_targeted_x_pass - best_option_x)^2 +
            (player_targeted_y_pass - best_option_y)^2
        ),
      
      # ======================================================================
      # Best option movement
      # ======================================================================
      
      best_option_run_dist =
        sqrt(
          (best_option_x - best_option_x_start)^2 +
            (best_option_y - best_option_y_start)^2
        ),
      
      best_option_run_forward =
        best_option_x -
        best_option_x_start,
      
      best_option_run_lateral =
        best_option_y -
        best_option_y_start,
      
      best_option_run_lateral_abs =
        abs(best_option_run_lateral),
      
      best_option_run_angle =
        atan2(
          best_option_y - best_option_y_start,
          best_option_x - best_option_x_start
        ) * 180 / pi,
      
      
      
      # ======================================================================
      # OBE clean-up
      # ======================================================================
      
      
      
      n_engagements =
        replace_na(
          n_engagements,
          0L
        ),
      
      engaged =
        n_engagements > 0
      
    ) |>
    
    mutate(
      
      across(
        any_of(
          c(
            "any_pressing",
            "any_pressure",
            "any_counter_press",
            "any_recovery_press",
            "any_other_engagement",
            "in_pressing_chain",
            "any_goalside_start",
            "any_close_at_start",
            "any_simultaneous_same_target",
            "any_engagement_from_attacking_third",
            "any_engagement_from_defensive_third",
            "any_engagement_from_wide",
            "any_beaten_by_possession",
            "any_beaten_by_movement",
            "stop_possession_danger",
            "reduce_possession_danger",
            "force_backward"
          )
        ),
        ~ replace_na(.x, FALSE)
      )
      
    )
  
  analysis
  
}