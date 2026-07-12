make_tracking_features <- function(events, tracking, match, option_features) {
  
  
  # 1. PLAYER LOOKUP TABLE
  
  players_lookup <- match$players |>
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
  
  
  
  # 2. PLAYER TRACKING DATA
  players <- tracking |>
    select(frame, timestamp, period, player_data) |>
    unnest(player_data) |>
    rename(
      player_x = x,
      player_y = y
    ) |>
    left_join(
      players_lookup,
      by = "player_id"
    )
  
  
  # 3. BALL TRACKING DATA
  
  ball <- tracking |>
    select(frame, timestamp, period, ball_data) |>
    unnest_wider(
      ball_data,
      names_sep = "_"
    ) |>
    transmute(
      frame,
      timestamp,
      period,
      ball_x = ball_data_x,
      ball_y = ball_data_y,
      ball_z = ball_data_z,
      ball_detected = ball_data_is_detected
    )
  
  
  # 4. ATTACKING TEAM IDENTIFICATION
  
  events <- events |>
    mutate(
      player_in_possession_id = if_else(
        event_type == "player_possession" &
          is.na(player_in_possession_id),
        player_id,
        player_in_possession_id
      )
    )
  
  
  events <- events |>
    left_join(
      players_lookup |>
        select(
          player_id,
          attacking_team_id = team_id
        ),
      by = c(
        "player_in_possession_id" = "player_id"
      )
    )
  
  
  # 5. TEAM ATTACKING DIRECTION
  
  team_direction <- events |>
    filter(team_id == attacking_team_id) |>
    distinct(
      team_id,
      period,
      attacking_side
    )
  
  # 6. ATTACH BEST OPTION COORDINATES
  
  events <- events |>
    left_join(
      option_features |>
        select(
          match_id,
          associated_player_possession_event_id,
          best_option_x_start,
          best_option_y_start,
          best_option_x,
          best_option_y
        ),
      by = c(
        "match_id",
        "event_id" = "associated_player_possession_event_id"
      )
    )
  
  
  # 7. STANDARDISE PLAYER COORDINATES
  
  players_std <- players |>
    left_join(
      team_direction,
      by = c(
        "team_id",
        "period"
      )
    ) |>
    mutate(
      player_x = if_else(
        attacking_side == "right_to_left",
        -player_x,
        player_x
      ),
      
      player_y = if_else(
        attacking_side == "right_to_left",
        -player_y,
        player_y
      )
    ) |>
    select(
      -attacking_side
    )
  
  
  
  # 8. CREATE EVENT SNAPSHOTS
  # ===========================================================================
  
  snapshot_start <- events |>
    select(
      event_id,
      match_id,
      frame_start,
      attacking_team_id,
      x_start,
      y_start,
      best_option_x_start,
      best_option_y_start
    ) |>
    left_join(
      players_std,
      by = c(
        "frame_start" = "frame"),
      relationship = "many-to-many"
    ) |>
    mutate(
      side = if_else(
        team_id == attacking_team_id,
        "attack",
        "defense"
      )
    ) |>
    filter(
      position_acronym != "GK"
    )
  
  
  
  snapshot_end <- events |>
    select(
      event_id,
      match_id,
      frame_end,
      attacking_team_id,
      x_end,
      y_end,
      best_option_x,
      best_option_y,
      player_targeted_x_pass,
      player_targeted_y_pass
    ) |>
    left_join(
      players_std,
      by = c(
        "frame_end" = "frame"),
      relationship = "many-to-many"
    ) |>
    mutate(
      side = if_else(
        team_id == attacking_team_id,
        "attack",
        "defense"
      )
    ) |>
    filter(
      position_acronym != "GK"
    )
  
  
  
  # 9. POLYGON AREA FUNCTION
  # ===========================================================================
  
  polygon_area <- function(x, y) {
    
    if(length(x) < 3)
      return(NA_real_)
    
    ord <- chull(x, y)
    
    x <- x[ord]
    y <- y[ord]
    
    x_next <- c(x[-1], x[1])
    y_next <- c(y[-1], y[1])
    
    0.5 *
      abs(
        sum(x * y_next - x_next * y)
      )
  }
  
  
  # 10. DISTANCE FROM POINT TO SEGMENT
  # ===========================================================================
  
  dist_point_to_segment <- function(px, py, ax, ay, bx, by) {
    
    abx <- bx - ax
    aby <- by - ay
    
    seg_len_sq <- abx^2 + aby^2
    
    t <- if_else(
      seg_len_sq == 0,
      0,
      ((px - ax) * abx + (py - ay) * aby) / seg_len_sq
    )
    
    in_segment <- t >= 0 & t <= 1
    
    t_clamped <- pmin(pmax(t, 0), 1)
    
    closest_x <- ax + t_clamped * abx
    closest_y <- ay + t_clamped * aby
    
    dist <- sqrt((px - closest_x)^2 + (py - closest_y)^2)
    
    tibble(dist = dist, in_segment = in_segment)
  }
  
  
  # 11. FULL DEFENSIVE TEAM COMPACTNESS
  # ===========================================================================
  
  team_compactness_start <- snapshot_start |>
    filter(side == "defense") |>
    group_by(match_id, event_id) |>
    summarise(
      
      team_surface_area = polygon_area(player_x, player_y),
      
      team_spread = sum( (player_x - mean(player_x))^2 + (player_y - mean(player_y))^2),
      
      .groups = "drop"
    )
  
  
  
  team_compactness_end <- snapshot_end |>
    filter(side == "defense") |>
    group_by(match_id, event_id) |>
    summarise(
      
      team_surface_area_end =
        polygon_area(player_x, player_y),
      
      team_spread_end =
        sum(
          (player_x - mean(player_x))^2 +
            (player_y - mean(player_y))^2
        ),
      
      .groups = "drop"
    )
  
  
  
  # 12. FIVE NEAREST DEFENDERS COMPACTNESS
  # ===========================================================================
  
  dc_ball_near_start <- snapshot_start |>
    filter(side == "defense") |>
    mutate(
      dist_to_ball_carrier =
        sqrt(
          (player_x - x_start)^2 +
            (player_y - y_start)^2
        )
    ) |>
    group_by(match_id, event_id) |>
    slice_min(
      dist_to_ball_carrier,
      n = 5
    ) |>
    summarise(
      
      nearest_surface_area =
        polygon_area(player_x, player_y),
      
      nearest_spread =
        sum(
          (player_x - mean(player_x))^2 +
            (player_y - mean(player_y))^2
        ),
      
      .groups = "drop"
    )
  
  
  
  dc_ball_near_end <- snapshot_end |>
    filter(side == "defense") |>
    mutate(
      dist_to_ball_carrier =
        sqrt(
          (player_x - x_end)^2 +
            (player_y - y_end)^2
        )
    ) |>
    group_by(match_id, event_id) |>
    slice_min(
      dist_to_ball_carrier,
      n = 5
    ) |>
    summarise(
      
      nearest_surface_area_end =
        polygon_area(player_x, player_y),
      
      nearest_spread_end =
        sum(
          (player_x - mean(player_x))^2 +
            (player_y - mean(player_y))^2
        ),
      
      .groups = "drop"
    )
  
  
  
  # 13. DEFENSIVE UNIT COMPACTNESS
  # ===========================================================================
  
  defensive_positions <- c(
    "LB","LWB",
    "LCB","CB","RCB",
    "RWB","RB",
    "LDM","DM","RDM",
    "LM","CM","RM","AM"
  )
  
  
  dc_def_mid_start <- snapshot_start |>
    filter(
      side == "defense",
      position_acronym %in% defensive_positions
    ) |>
    group_by(match_id, event_id) |>
    summarise(
      
      dc_defmid_surface_area =
        polygon_area(player_x, player_y),
      
      dc_defmid_spread =
        sum(
          (player_x - mean(player_x))^2 +
            (player_y - mean(player_y))^2
        ),
      
      .groups = "drop"
    )
  
  
  
  dc_def_mid_end <- snapshot_end |>
    filter(
      side == "defense",
      position_acronym %in% defensive_positions
    ) |>
    group_by(match_id, event_id) |>
    summarise(
      
      dc_defmid_surface_area_end =
        polygon_area(player_x, player_y),
      
      dc_defmid_spread_end =
        sum(
          (player_x - mean(player_x))^2 +
            (player_y - mean(player_y))^2
        ),
      
      .groups = "drop"
    )
  
  
  
  # 14. DEFENDER PROXIMITY FEATURES — ball carrier
  # ===========================================================================
  
  proximity_start <- snapshot_start |>
    filter(side == "defense") |>
    mutate(
      dist_to_ball_carrier =
        sqrt(
          (player_x - x_start)^2 +
            (player_y - y_start)^2
        )
    ) |>
    group_by(match_id, event_id) |>
    summarise(
      
      nearest_def_dist =
        min(dist_to_ball_carrier),
      
      second_nearest_def_dist =
        sort(dist_to_ball_carrier)[2],
      
      n_within_5m =
        sum(dist_to_ball_carrier <= 5),
      
      .groups = "drop"
    )
  
  
  
  proximity_end <- snapshot_end |>
    filter(side == "defense") |>
    mutate(
      dist_to_ball_carrier =
        sqrt(
          (player_x - x_end)^2 +
            (player_y - y_end)^2
        )
    ) |>
    group_by(match_id, event_id) |>
    summarise(
      
      nearest_def_dist_end =
        min(dist_to_ball_carrier),
      
      second_nearest_def_dist_end =
        sort(dist_to_ball_carrier)[2],
      
      n_within_5m_end =
        sum(dist_to_ball_carrier <= 5),
      
      .groups = "drop"
    )
  
  
  
  # 15. DEFENDER PROXIMITY TO BEST OPTION
  # ===========================================================================
  
  proximity_best_option_start <- snapshot_start |>
    filter(
      side == "defense",
      !is.na(best_option_x_start),
      !is.na(best_option_y_start)
    ) |>
    mutate(
      dist_to_best_option =
        sqrt(
          (player_x - best_option_x_start)^2 +
            (player_y - best_option_y_start)^2
        )
    ) |>
    group_by(match_id, event_id) |>
    summarise(
      
      nearest_def_dist_best_option =
        suppressWarnings(min(dist_to_best_option, na.rm = TRUE)),
      
      second_nearest_def_dist_best_option =
        sort(dist_to_best_option)[2],
      
      n_within_5m_best_option =
        sum(dist_to_best_option <= 5, na.rm = TRUE),
      
      .groups = "drop"
    ) |>
    mutate(
      nearest_def_dist_best_option =
        if_else(is.infinite(nearest_def_dist_best_option), NA_real_, nearest_def_dist_best_option)
    )
  
  
  proximity_best_option_end <- snapshot_end |>
    filter(
      side == "defense",
      !is.na(best_option_x),
      !is.na(best_option_y)
    ) |>
    mutate(
      dist_to_best_option =
        sqrt(
          (player_x - best_option_x)^2 +
            (player_y - best_option_y)^2
        )
    ) |>
    group_by(match_id, event_id) |>
    summarise(
      
      nearest_def_dist_best_option_end =
        suppressWarnings(min(dist_to_best_option, na.rm = TRUE)),
      
      second_nearest_def_dist_best_option_end =
        sort(dist_to_best_option)[2],
      
      n_within_5m_best_option_end =
        sum(dist_to_best_option <= 5, na.rm = TRUE),
      
      .groups = "drop"
    ) |>
    mutate(
      nearest_def_dist_best_option_end =
        if_else(is.infinite(nearest_def_dist_best_option_end), NA_real_, nearest_def_dist_best_option_end)
    )
  
  
  
  # 16. PASSING LANE FEATURES (carrier -> best option, carrier -> targeted player)
  # ===========================================================================
  
  CORRIDOR_WIDTH <- 2  # lane width
  
  passing_lane_features <- snapshot_end |>
    filter(side == "defense") |>
    mutate(
      
      # lane to best option
      lane_to_best_option = dist_point_to_segment(
        player_x, player_y,
        x_end, y_end,
        best_option_x, best_option_y
      ),
      dist_to_best_option_lane = lane_to_best_option$dist,
      in_best_option_lane_bounds = lane_to_best_option$in_segment,
      
      # lane to targeted player
      lane_to_targeted = dist_point_to_segment(
        player_x, player_y,
        x_end, y_end,
        player_targeted_x_pass, player_targeted_y_pass
      ),
      dist_to_targeted_lane = lane_to_targeted$dist,
      in_targeted_lane_bounds = lane_to_targeted$in_segment
      
    ) |>
    group_by(match_id, event_id) |>
    summarise(
      
      # best option lane
      min_dist_to_best_option_lane =
        suppressWarnings(min(dist_to_best_option_lane, na.rm = TRUE)),
      
      n_defenders_in_best_option_lane =
        sum(
          dist_to_best_option_lane <= CORRIDOR_WIDTH &
            in_best_option_lane_bounds,
          na.rm = TRUE
        ),
      
      # targeted player lane
      min_dist_to_targeted_lane =
        suppressWarnings(min(dist_to_targeted_lane, na.rm = TRUE)),
      
      n_defenders_in_targeted_lane =
        sum(
          dist_to_targeted_lane <= CORRIDOR_WIDTH &
            in_targeted_lane_bounds,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    ) |>
    mutate(
      min_dist_to_best_option_lane =
        if_else(is.infinite(min_dist_to_best_option_lane), NA_real_, min_dist_to_best_option_lane),
      min_dist_to_targeted_lane =
        if_else(is.infinite(min_dist_to_targeted_lane), NA_real_, min_dist_to_targeted_lane)
    )
  
  
  
  # 17. FINAL TRACKING FEATURE TABLE
  # ===========================================================================
  
  tracking_features <- list(
    
    team_compactness_start,
    team_compactness_end,
    
    dc_ball_near_start,
    dc_ball_near_end,
    
    dc_def_mid_start,
    dc_def_mid_end,
    
    proximity_start,
    proximity_end,
    
    proximity_best_option_start,
    proximity_best_option_end,
    
    passing_lane_features
    
  ) |>
    purrr::reduce(
      left_join,
      by = c("match_id", "event_id")
    ) |>
    mutate(
      
      team_surface_area_gain = team_surface_area_end - team_surface_area,
      team_spread_gain = team_spread_end - team_spread,
      
      nearest_surface_area_gain = nearest_surface_area_end - nearest_surface_area,
      nearest_spread_gain = nearest_spread_end - nearest_spread,
      
      dc_defmid_surface_area_gain = dc_defmid_surface_area_end - dc_defmid_surface_area,
      dc_defmid_spread_gain = dc_defmid_spread_end - dc_defmid_spread,
      
      nearest_def_dist_gain = nearest_def_dist_end - nearest_def_dist,
      second_nearest_def_dist_gain = second_nearest_def_dist_end - second_nearest_def_dist,
      n_within_5m_gain = n_within_5m_end - n_within_5m,
      
      best_option_nearest_def_dist_gain =
        nearest_def_dist_best_option_end - nearest_def_dist_best_option,
      best_option_second_nearest_def_dist_gain =
        second_nearest_def_dist_best_option_end - second_nearest_def_dist_best_option,
      best_option_n_within_5m_gain =
        n_within_5m_best_option_end - n_within_5m_best_option,
      
      lane_obstruction_diff = min_dist_to_targeted_lane - min_dist_to_best_option_lane,
      lane_count_diff = n_defenders_in_targeted_lane - n_defenders_in_best_option_lane
      
    )
  
  
  return(tracking_features)
  
}