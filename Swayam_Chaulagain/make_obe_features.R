make_obe_features <- function(events) {
  
  obe <- events |>
    filter(event_type_id == 9)
  
  obe_features <- obe |>
    mutate(
      across(
        c(
          stop_possession_danger,
          reduce_possession_danger,
          force_backward,
          pressing_chain,
          goal_side_start,
          close_at_player_possession_start,
          possession_danger,
          simultaneous_defensive_engagement_same_target,
          beaten_by_possession,
          beaten_by_movement
        ),
        to_bool
      )
    ) |>
    group_by(match_id, associated_player_possession_event_id) |>
    summarise(
      
      # Number of engagements
      n_engagements = n(),
      
      # Engagement types
      any_pressing       = any(event_subtype == "pressing", na.rm = TRUE),
      any_pressure       = any(event_subtype == "pressure", na.rm = TRUE),
      any_counter_press  = any(event_subtype == "counter_press", na.rm = TRUE),
      any_recovery_press = any(event_subtype == "recovery_press", na.rm = TRUE),
      any_other_engagement = any(event_subtype == "other", na.rm = TRUE),
      
      # Pressing chain
      in_pressing_chain = any(pressing_chain, na.rm = TRUE),
      
      max_chain_length = suppressWarnings(
        max(pressing_chain_length, na.rm = TRUE)
      ),
      
      # Defensive failures
      any_beaten_by_possession =
        any(beaten_by_possession, na.rm = TRUE),
      
      any_beaten_by_movement =
        any(beaten_by_movement, na.rm = TRUE),
      
      # Distance
      min_engagement_distance =
        suppressWarnings(
          min(interplayer_distance_min, na.rm = TRUE)
        ),
      
      # Defensive shape
      any_goalside_start =
        any(goal_side_start, na.rm = TRUE),
      
      any_close_at_start =
        any(close_at_player_possession_start, na.rm = TRUE),
      
      # Pressure speed
      mean_engagement_speed =
        mean(speed_avg, na.rm = TRUE),
      
      # Simultaneous engagement
      any_simultaneous_same_target =
        any(
          simultaneous_defensive_engagement_same_target,
          na.rm = TRUE
        ),
      
      # Engagement origin
      any_engagement_from_attacking_third =
        any(
          third_start == "attacking_third",
          na.rm = TRUE
        ),
      
      any_engagement_from_defensive_third =
        any(
          third_start == "defensive_third",
          na.rm = TRUE
        ),
      
      any_engagement_from_wide =
        any(
          channel_start %in%
            c("wide_left", "wide_right"),
          na.rm = TRUE
        ),
      
      # SkillCorner outcomes
      stop_possession_danger =
        any(stop_possession_danger, na.rm = TRUE),
      
      reduce_possession_danger =
        any(reduce_possession_danger, na.rm = TRUE),
      
      force_backward =
        any(force_backward, na.rm = TRUE),
      
      .groups = "drop"
    ) |>
    mutate(
      
      max_chain_length =
        if_else(
          is.infinite(max_chain_length),
          NA_real_,
          max_chain_length
        ),
      
      min_engagement_distance =
        if_else(
          is.infinite(min_engagement_distance),
          NA_real_,
          min_engagement_distance
        ),
      
      mean_engagement_speed =
        if_else(
          is.nan(mean_engagement_speed),
          NA_real_,
          mean_engagement_speed
        )
      
    )
  
  obe_features
}