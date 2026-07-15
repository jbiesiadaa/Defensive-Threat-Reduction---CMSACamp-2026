
# TRACKING FEATURES PIPELINE  (v2 -- correct paths + single-game test)
# ------------------------------------------------------------------------------
# Run AFTER the PTR script has produced `analysis`
# (after section 5 "BUILD THE ANALYSIS DATASET", BEFORE the quality filters --
#  the filters are re-run at the bottom, after the tracking join).

analysis <- readRDS("analysis_502games_prefilter.rds")

# Loading Libraries
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(jsonlite)

# must be the PRE-tracking version -- rejoining an already-enriched analysis
# would create .x/.y duplicate columns everywhere
stopifnot(!"team_surface_area" %in% names(analysis))

# Make IDs consistent before processing
analysis <- analysis |>
  mutate(
    match_id = as.character(match_id),
    event_id = as.character(event_id)
  )

# ---- FILE LOCATIONS (matching your actual folders) ---------------------------
# matchdata: mls_skillcorner/match_data/match_742721_data.json
# tracking : mls_skillcorner/tracking/match_742721_tracking.json

tracking_file_for <- function(mid) {
  file.path("mls_skillcorner/tracking",
            paste0("match_", mid, "_tracking.json"))
}

match_file_for <- function(mid) {
  file.path("mls_skillcorner/match_data",
            paste0("match_", mid, "_data.json"))
}

# ==============================================================================
# 0. HELPERS
# ==============================================================================

polygon_area <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3) return(NA_real_)
  ord <- grDevices::chull(x, y)
  x <- x[ord]; y <- y[ord]
  x_next <- c(x[-1], x[1]); y_next <- c(y[-1], y[1])
  0.5 * abs(sum(x * y_next - x_next * y))
}

dist_point_to_segment <- function(px, py, ax, ay, bx, by) {
  abx <- bx - ax
  aby <- by - ay
  seg_len_sq <- abx^2 + aby^2
  t <- ifelse(seg_len_sq == 0, 0, ((px - ax) * abx + (py - ay) * aby) / seg_len_sq)
  in_segment <- t >= 0 & t <= 1
  t_clamped  <- pmin(pmax(t, 0), 1)
  dist <- sqrt((px - (ax + t_clamped * abx))^2 + (py - (ay + t_clamped * aby))^2)
  tibble(dist = dist, in_segment = in_segment)
}

safe_min <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else min(x)
}

second_min <- function(x) {
  x <- sort(x)                       # sort() drops NA
  if (length(x) >= 2) x[2] else NA_real_
}

defensive_positions <- c(
  "LB","LWB","LCB","CB","RCB","RWB","RB",
  "LDM","DM","RDM","LM","CM","RM","AM"
)

CORRIDOR_WIDTH <- 2   # passing-lane width (m)

# ==============================================================================
# 1. SNAPSHOT KEYS -- straight from `analysis`, one row = one possession
# ==============================================================================

snapshot_keys <- analysis |>
  transmute(
    match_id = as.character(match_id),
    event_id = as.character(event_id),
    frame_start = as.integer(frame_start),
    frame_end   = as.integer(frame_end),
    attacking_team_id = as.integer(team_id),   # carrier's team = attacking team
    attacking_side,                            # direction flag for the flip
    carrier_x_start, carrier_y_start,
    carrier_x_end,   carrier_y_end,
    best_option_x_start, best_option_y_start,
    best_option_x_end,   best_option_y_end,
    player_targeted_x_start, player_targeted_y_start,
    player_targeted_x_end,   player_targeted_y_end
  )

# ==============================================================================
# 2. PER-MATCH FUNCTIONS
# ==============================================================================

# ---- 2a. Load one match's tracked players (RAW coordinates, no flip yet) ----
load_match_players <- function(mid) {
  
  mf <- match_file_for(mid)
  tf <- tracking_file_for(mid)
  if (!file.exists(mf) || !file.exists(tf)) {
    warning("Missing match/tracking file for match ", mid, " -- skipped")
    return(NULL)
  }
  
  match_meta <- fromJSON(mf)
  tracking   <- fromJSON(tf)
  
  players_lookup <- match_meta$players |>
    as_tibble() |>
    unnest_wider(player_role, names_sep = "_") |>
    transmute(
      player_id = as.integer(id),
      team_id   = as.integer(team_id),
      position_acronym = player_role_acronym
    )
  
  tracking |>
    as_tibble() |>
    select(frame, player_data) |>
    unnest(player_data) |>
    transmute(
      frame     = as.integer(frame),
      player_id = as.integer(player_id),
      player_x  = x,
      player_y  = y
    ) |>
    left_join(players_lookup, by = "player_id")
}

# ---- 2b. Snapshot: all players at a frame, flipped into the possession's
#          standardized frame (goal at 52.5, 0), keepers dropped --------------
make_snapshot <- function(keys, players, frame_col) {
  keys |>
    inner_join(
      players,
      by = setNames("frame", frame_col),
      relationship = "many-to-many"
    ) |>
    mutate(
      needs_flip = attacking_side == "right_to_left",
      player_x   = if_else(needs_flip, -player_x, player_x),
      player_y   = if_else(needs_flip, -player_y, player_y),
      side       = if_else(team_id == attacking_team_id, "attack", "defense")
    ) |>
    filter(coalesce(position_acronym, "") != "GK")
}

# ---- 2c. Defensive features for one snapshot (compactness + proximities) ----
#          cx/cy = carrier, bx/by = best option, tx/ty = targeted receiver
snapshot_features <- function(snap, cx, cy, bx, by, tx, ty, suffix = "") {
  
  d <- snap |> filter(side == "defense")
  
  team <- d |>
    group_by(event_id) |>
    summarise(
      team_surface_area = polygon_area(player_x, player_y),
      team_spread = sum((player_x - mean(player_x))^2 +
                          (player_y - mean(player_y))^2),
      .groups = "drop"
    )
  
  near5 <- d |>
    mutate(dist_bc = sqrt((player_x - {{cx}})^2 + (player_y - {{cy}})^2)) |>
    group_by(event_id) |>
    slice_min(dist_bc, n = 5, with_ties = FALSE ) |>
    summarise(
      nearest_surface_area = polygon_area(player_x, player_y),
      nearest_spread = sum((player_x - mean(player_x))^2 +
                             (player_y - mean(player_y))^2),
      .groups = "drop"
    )
  
  defmid <- d |>
    filter(position_acronym %in% defensive_positions) |>
    group_by(event_id) |>
    summarise(
      dc_defmid_surface_area = polygon_area(player_x, player_y),
      dc_defmid_spread = sum((player_x - mean(player_x))^2 +
                               (player_y - mean(player_y))^2),
      .groups = "drop"
    )
  
  prox <- d |>
    mutate(dist_bc = sqrt((player_x - {{cx}})^2 + (player_y - {{cy}})^2)) |>
    group_by(event_id) |>
    summarise(
      nearest_def_dist        = safe_min(dist_bc),
      second_nearest_def_dist = second_min(dist_bc),
      n_within_5m             = sum(dist_bc <= 5, na.rm = TRUE),
      .groups = "drop"
    )
  
  prox_best <- d |>
    filter(!is.na({{bx}}), !is.na({{by}})) |>
    mutate(dd = sqrt((player_x - {{bx}})^2 + (player_y - {{by}})^2)) |>
    group_by(event_id) |>
    summarise(
      nearest_def_dist_best_option        = safe_min(dd),
      second_nearest_def_dist_best_option = second_min(dd),
      n_within_5m_best_option             = sum(dd <= 5, na.rm = TRUE),
      .groups = "drop"
    )
  
  prox_targ <- d |>
    filter(!is.na({{tx}}), !is.na({{ty}})) |>
    mutate(dd = sqrt((player_x - {{tx}})^2 + (player_y - {{ty}})^2)) |>
    group_by(event_id) |>
    summarise(
      nearest_def_dist_targeted = safe_min(dd),
      n_within_5m_targeted      = sum(dd <= 5, na.rm = TRUE),
      .groups = "drop"
    )
  
  out <- list(team, near5, defmid, prox, prox_best, prox_targ) |>
    reduce(full_join, by = "event_id")
  
  if (suffix != "") {
    out <- out |> rename_with(~ paste0(.x, suffix), -event_id)
  }
  out
}

# ---- 2d. Passing-lane features (end snapshot only) ---------------------------
lane_features <- function(snap_end) {
  snap_end |>
    filter(side == "defense") |>
    mutate(
      lane_best = dist_point_to_segment(
        player_x, player_y,
        carrier_x_end, carrier_y_end,
        best_option_x_end, best_option_y_end
      ),
      dist_to_best_option_lane   = lane_best$dist,
      in_best_option_lane_bounds = lane_best$in_segment,
      
      lane_targ = dist_point_to_segment(
        player_x, player_y,
        carrier_x_end, carrier_y_end,
        player_targeted_x_end, player_targeted_y_end
      ),
      dist_to_targeted_lane   = lane_targ$dist,
      in_targeted_lane_bounds = lane_targ$in_segment
    ) |>
    group_by(event_id) |>
    summarise(
      min_dist_to_best_option_lane = safe_min(dist_to_best_option_lane),
      n_defenders_in_best_option_lane = sum(
        dist_to_best_option_lane <= CORRIDOR_WIDTH & in_best_option_lane_bounds,
        na.rm = TRUE
      ),
      min_dist_to_targeted_lane = safe_min(dist_to_targeted_lane),
      n_defenders_in_targeted_lane = sum(
        dist_to_targeted_lane <= CORRIDOR_WIDTH & in_targeted_lane_bounds,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}

# ---- 2e. One match end-to-end -------------------------------------------------
compute_match_features <- function(mid, keys_all) {
  
  keys <- keys_all |> filter(match_id == mid)
  if (nrow(keys) == 0) return(NULL)
  
  players <- load_match_players(mid)
  if (is.null(players)) return(NULL)
  
  snap_start <- make_snapshot(keys, players, "frame_start")
  snap_end   <- make_snapshot(keys, players, "frame_end")
  
  feats_start <- snapshot_features(
    snap_start,
    carrier_x_start, carrier_y_start,
    best_option_x_start, best_option_y_start,
    player_targeted_x_start, player_targeted_y_start,
    suffix = ""
  )
  
  feats_end <- snapshot_features(
    snap_end,
    carrier_x_end, carrier_y_end,
    best_option_x_end, best_option_y_end,
    player_targeted_x_end, player_targeted_y_end,
    suffix = "_end"
  )
  
  lanes <- lane_features(snap_end)
  
  list(feats_start, feats_end, lanes) |>
    reduce(full_join, by = "event_id") |>
    mutate(match_id = as.character(mid), .before = 1)
}

# ==============================================================================
# 3. CHECK AVAILABLE MATCH FILES
# ==============================================================================

match_ids <- unique(snapshot_keys$match_id)

file_check <- tibble(
  match_id = match_ids,
  has_match_file = file.exists(match_file_for(match_ids)),
  has_tracking_file = file.exists(tracking_file_for(match_ids))
) |>
  mutate(
    files_available = has_match_file & has_tracking_file
  )

file_check |>
  print(n = Inf)

# CHANGE:
# Only matches with both match data and tracking data will be processed.
available_match_ids <- file_check |>
  filter(files_available) |>
  pull(match_id)

stopifnot(
  "No complete match and tracking files were found" =
    length(available_match_ids) > 0
)

# ==============================================================================
# 4. PROCESS ALL AVAILABLE MATCHES
# ==============================================================================

# CHANGE:
# Multiple matches are now processed first.
# The one-game test was moved to the end of the script.

tracking_features <- map(
  available_match_ids,
  ~ compute_match_features(.x, snapshot_keys),
  .progress = TRUE
) |>
  list_rbind()

stopifnot(
  "No tracking features were created" =
    nrow(tracking_features) > 0
)

dim(tracking_features)

tracking_features |>
  count(match_id) |>
  print(n = Inf)

glimpse(tracking_features)

# ==============================================================================
# 5. CREATE GAIN AND LANE VARIABLES
# ==============================================================================

tracking_features <- tracking_features |>
  mutate(
    # Defensive-team changes
    team_surface_area_gain =
      team_surface_area_end - team_surface_area,
    
    team_spread_gain =
      team_spread_end - team_spread,
    
    # Five nearest defenders
    nearest_surface_area_gain =
      nearest_surface_area_end - nearest_surface_area,
    
    nearest_spread_gain =
      nearest_spread_end - nearest_spread,
    
    # Defensive and midfield players
    dc_defmid_surface_area_gain =
      dc_defmid_surface_area_end - dc_defmid_surface_area,
    
    dc_defmid_spread_gain =
      dc_defmid_spread_end - dc_defmid_spread,
    
    # Pressure around the carrier
    nearest_def_dist_gain =
      nearest_def_dist_end - nearest_def_dist,
    
    second_nearest_def_dist_gain =
      second_nearest_def_dist_end - second_nearest_def_dist,
    
    n_within_5m_gain =
      n_within_5m_end - n_within_5m,
    
    # Pressure around the best option
    best_option_nearest_def_dist_gain =
      nearest_def_dist_best_option_end -
      nearest_def_dist_best_option,
    
    best_option_second_nearest_def_dist_gain =
      second_nearest_def_dist_best_option_end -
      second_nearest_def_dist_best_option,
    
    best_option_n_within_5m_gain =
      n_within_5m_best_option_end -
      n_within_5m_best_option,
    
    # Pressure around the targeted player
    nearest_def_dist_targeted_gain =
      nearest_def_dist_targeted_end -
      nearest_def_dist_targeted,
    
    n_within_5m_targeted_gain =
      n_within_5m_targeted_end -
      n_within_5m_targeted,
    
    # Targeted lane compared with best-option lane
    lane_obstruction_diff =
      min_dist_to_targeted_lane -
      min_dist_to_best_option_lane,
    
    lane_count_diff =
      n_defenders_in_targeted_lane -
      n_defenders_in_best_option_lane
  )

# ==============================================================================
# 6. CHECK ONE ROW PER POSSESSION
# ==============================================================================

duplicate_keys <- tracking_features |>
  count(match_id, event_id) |>
  filter(n > 1)

stopifnot(
  "Tracking features must have one row per possession" =
    nrow(duplicate_keys) == 0
)

# ==============================================================================
# 7. JOIN TRACKING FEATURES INTO ANALYSIS
# ==============================================================================

analysis <- analysis |>
  left_join(
    tracking_features,
    by = c("match_id", "event_id")
  ) |>
  mutate(
    # CHANGE:
    # TRUE means the possession has the main tracking features.
    has_tracking =
      !is.na(nearest_def_dist) &
      !is.na(team_surface_area)
  )

# Check that the join did not create extra rows
stopifnot(
  !anyDuplicated(
    analysis[c("match_id", "event_id")]
  )
)

cat(
  "Tracking coverage:",
  round(100 * mean(analysis$has_tracking), 1),
  "%\n"
)

# ==============================================================================
# 8. ADD PLAYER POSITIONS FROM MATCH LINEUP
# ==============================================================================

# CHANGE:
# Add the registered lineup position for the carrier,
# targeted player, and best passing option.

position_lookup <- map(
  unique(analysis$match_id),
  function(mid) {
    
    mf <- match_file_for(mid)
    
    if (!file.exists(mf)) return(NULL)
    
    fromJSON(mf)$players |>
      as_tibble() |>
      unnest_wider(player_role, names_sep = "_") |>
      transmute(
        match_id = as.character(mid),
        player_id = as.integer(id),
        position = player_role_name
      )
  }
) |>
  list_rbind() |>
  distinct(match_id, player_id, .keep_all = TRUE)


analysis <- analysis |>
  mutate(
    player_id = as.integer(player_id),
    player_targeted_id = as.integer(player_targeted_id),
    best_option_player_id = as.integer(best_option_player_id)
  ) |>
  
  # Carrier position
  left_join(
    position_lookup |>
      rename(carrier_position = position),
    by = c("match_id", "player_id")
  ) |>
  
  # Targeted-player position
  left_join(
    position_lookup |>
      rename(
        player_targeted_id = player_id,
        targeted_position = position
      ),
    by = c("match_id", "player_targeted_id")
  ) |>
  
  # Best-option position
  left_join(
    position_lookup |>
      rename(
        best_option_player_id = player_id,
        best_option_position = position
      ),
    by = c("match_id", "best_option_player_id")
  )

# ==============================================================================
# 9. CHECK TRACKING COVERAGE BY MATCH
# ==============================================================================

analysis |>
  group_by(match_id) |>
  summarise(
    possessions = n(),
    possessions_with_tracking = sum(has_tracking),
    tracking_coverage = round(
      100 * mean(has_tracking),
      1
    ),
    .groups = "drop"
  ) |>
  print(n = Inf)


# ==============================================================================
# 10. APPLY QUALITY FILTERS
# ==============================================================================

n_start <- nrow(analysis)

analysis_clean <- analysis |>
  filter(!is_header %in% TRUE) |> # remove headers: not normal pass/xThreat logic
  filter(!hand_pass %in% TRUE) |>  # remove hand/throw-in type actions if present
  filter(is_player_possession_end_matched %in% TRUE) |> # pass moment reliably tracked
  filter(!is.na(PTR)) |> # PTR is computable
  filter(!is_overreach %in% TRUE) |> # Removing Gamblers Situations
  filter(is_player_possession_start_matched %in% TRUE) |> # start features reliable
  filter(!short_possession) |> # remove one-touch/very short actions
  filter(!disruption_possession %in% TRUE)|> # remove messy/loose-ball actions
  filter(!is.na(organised_defense)) # remove NA for organised defesne

cat(
  "Possessions:",
  n_start,
  "->",
  nrow(analysis_clean),
  "| Removed:",
  n_start - nrow(analysis_clean),
  "\n"
)

# ==============================================================================
# 11. FINAL CHECKS
# ==============================================================================

cat(
  "Tracking coverage after filtering:",
  round(
    100 * mean(analysis_clean$has_tracking),
    1
  ),
  "%\n"
)

# Tracking defender distance should agree with separation_start
cat(
  "Correlation:",
  round(
    cor(
      analysis_clean$nearest_def_dist,
      analysis_clean$separation_start,
      use = "complete.obs"
    ),
    3
  ),
  "\n"
)

# When the targeted player is the best option,
# the two passing lanes should be the same.
analysis_clean |>
  filter(
    PTR == 0,
    has_tracking,
    chose_best_option %in% TRUE
  ) |>
  summarise(
    max_lane_distance_difference =
      max(abs(lane_obstruction_diff), na.rm = TRUE),
    
    max_lane_count_difference =
      max(abs(lane_count_diff), na.rm = TRUE)
  ) |>
  print()

# Check reasonable ranges
analysis_clean |>
  filter(has_tracking) |>
  summarise(
    max_team_area =
      max(team_surface_area, na.rm = TRUE),
    
    min_defender_distance =
      min(nearest_def_dist, na.rm = TRUE),
    
    max_defenders_within_5m =
      max(n_within_5m, na.rm = TRUE),
    
    negative_areas =
      sum(team_surface_area < 0, na.rm = TRUE)
  ) |>
  print()


# ==============================================================================
# 12. CHECK MISSING VALUES
# ==============================================================================

na_summary <- analysis_clean |>
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) |>
  mutate(
    pct_missing = round(
      100 * n_missing / nrow(analysis_clean),
      2
    )
  ) |>
  arrange(desc(n_missing))

na_summary |>
  print(n = 20)

table(
  analysis_clean$pass_outcome,
  useNA = "ifany"
)

target_pass<-analysis_clean|>
  select(targeted_pass_successful)

analysis_clean |>
  filter(is.na(targeted_pass_successful)) |>
  count(pass_outcome)

# Check tracking_features has 50 columns
ncol(tracking_features)

# Check all tracking variables were added to analysis
setdiff(
  names(tracking_features),
  names(analysis_clean)
)

# character(0) means no tracking variables are missing from analysis_clean.

# ==============================================================================
# 13. SAVE FINAL MULTI-GAME DATASET
# ==============================================================================

saveRDS(
  analysis_clean,
  "ptr_analysis_clean_502games.rds"
)


# ==============================================================================
# 14. OPTIONAL: TEST ONE GAME
# ==============================================================================

# CHANGE:
# The single-game test is now last and is optional.
# This automatically selects the first available match.
# Select the first available match
test_id <- available_match_ids[1]

# Or choose a specific match
# test_id <- "1039803"


# Tracking features for one game
test_features <- tracking_features |>
  filter(match_id == test_id)

# Full analysis before quality filters
test_analysis <- analysis |>
  filter(match_id == test_id)

# Final analysis after quality filters
test_analysis_clean <- analysis_clean |>
  filter(match_id == test_id)


# Check number of rows
cat(
  "Test match:", test_id,
  "\nTracking features:", nrow(test_features),
  "\nAnalysis:", nrow(test_analysis),
  "\nAnalysis clean:", nrow(test_analysis_clean),
  "\n"
)


# Check tracking coverage
test_analysis |>
  summarise(
    possessions = n(),
    with_tracking = sum(has_tracking),
    tracking_coverage = round(
      100 * mean(has_tracking),
      1
    )
  ) |>
  print()


# Check reasonable tracking values
test_features |>
  summarise(
    max_team_area =
      max(team_surface_area, na.rm = TRUE),
    
    min_defender_distance =
      min(nearest_def_dist, na.rm = TRUE),
    
    max_defenders_within_5m =
      max(n_within_5m, na.rm = TRUE),
    
    pct_missing = round(
      100 * mean(is.na(team_surface_area)),
      1
    )
  ) |>
  print()


# Check all 50 tracking columns exist
ncol(test_features)

setdiff(
  names(test_features),
  names(test_analysis_clean)
)

