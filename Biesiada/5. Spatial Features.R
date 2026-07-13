
# TRACKING FEATURES PIPELINE  (v2 -- correct paths + single-game test)
# ------------------------------------------------------------------------------
# Run AFTER the PTR script has produced `analysis`
# (after section 5 "BUILD THE ANALYSIS DATASET", BEFORE the quality filters --
#  the filters are re-run at the bottom, after the tracking join).
#

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(jsonlite)

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
# 3. SINGLE-GAME TEST: match 742721  -- RUN THIS FIRST
# ==============================================================================

test_features <- compute_match_features("742721", snapshot_keys)

# 3a. did it produce roughly one row per possession?
cat("Feature rows:", nrow(test_features),
    "| possessions in analysis:",
    sum(snapshot_keys$match_id == "742721"), "\n")
glimpse(test_features)

# 3b. numbers in a sensible range?
test_features |>
  summarise(
    max_team_area = max(team_surface_area, na.rm = TRUE),   # < 7140 (105x68)
    min_def_dist  = min(nearest_def_dist,  na.rm = TRUE),   # >= 0
    max_within_5m = max(n_within_5m,       na.rm = TRUE),   # <= 10
    pct_missing   = round(mean(is.na(team_surface_area)) * 100, 1)
  ) |>
  print()

# 3c. COORDINATE-FRAME CHECK: tracked carrier vs event carrier position.
#     Median pos_error must be well under ~2 m. If it is pitch-sized,
#     STOP -- the flip convention is wrong; don't run the full loop.
players_test <- load_match_players("742721")

carrier_check <- analysis |>
  filter(as.character(match_id) == "742721") |>
  transmute(event_id,
            player_id = as.integer(player_id),
            frame_start = as.integer(frame_start),
            attacking_side,
            carrier_x_start, carrier_y_start) |>
  left_join(players_test, by = c("frame_start" = "frame", "player_id")) |>
  mutate(
    player_x = if_else(attacking_side == "right_to_left", -player_x, player_x),
    player_y = if_else(attacking_side == "right_to_left", -player_y, player_y),
    pos_error = sqrt((player_x - carrier_x_start)^2 +
                       (player_y - carrier_y_start)^2)
  )

print(summary(carrier_check$pos_error))

rm(players_test)

# >>> ONLY CONTINUE PAST THIS POINT IF 3a-3c LOOK GOOD <<<

# ==============================================================================
# 4. FULL LOOP OVER ALL MATCHES
# ==============================================================================

match_ids <- unique(snapshot_keys$match_id)

tracking_features <- map(
  match_ids,
  ~ compute_match_features(.x, snapshot_keys),
  .progress = TRUE
) |>
  list_rbind()

stopifnot(
  "No tracking files found -- check the two *_file_for() paths at the top" =
    nrow(tracking_features) > 0
)

# gains + lane comparisons
tracking_features <- tracking_features |>
  mutate(
    team_surface_area_gain = team_surface_area_end - team_surface_area,
    team_spread_gain       = team_spread_end - team_spread,
    
    nearest_surface_area_gain = nearest_surface_area_end - nearest_surface_area,
    nearest_spread_gain       = nearest_spread_end - nearest_spread,
    
    dc_defmid_surface_area_gain = dc_defmid_surface_area_end - dc_defmid_surface_area,
    dc_defmid_spread_gain       = dc_defmid_spread_end - dc_defmid_spread,
    
    nearest_def_dist_gain        = nearest_def_dist_end - nearest_def_dist,
    second_nearest_def_dist_gain = second_nearest_def_dist_end - second_nearest_def_dist,
    n_within_5m_gain             = n_within_5m_end - n_within_5m,
    
    best_option_nearest_def_dist_gain =
      nearest_def_dist_best_option_end - nearest_def_dist_best_option,
    best_option_second_nearest_def_dist_gain =
      second_nearest_def_dist_best_option_end - second_nearest_def_dist_best_option,
    best_option_n_within_5m_gain =
      n_within_5m_best_option_end - n_within_5m_best_option,
    
    nearest_def_dist_targeted_gain =
      nearest_def_dist_targeted_end - nearest_def_dist_targeted,
    n_within_5m_targeted_gain =
      n_within_5m_targeted_end - n_within_5m_targeted,
    
    lane_obstruction_diff = min_dist_to_targeted_lane - min_dist_to_best_option_lane,
    lane_count_diff       = n_defenders_in_targeted_lane - n_defenders_in_best_option_lane
  )

# uniqueness guard: exactly one feature row per possession
dup_keys <- tracking_features |> count(match_id, event_id) |> filter(n > 1)
stopifnot("tracking_features must be 1 row per possession" = nrow(dup_keys) == 0)

# ==============================================================================
# 5. JOIN INTO `analysis` -- BEFORE the quality filters
# ==============================================================================

analysis <- analysis |>
  mutate(match_id = as.character(match_id),
         event_id = as.character(event_id)) |>
  left_join(tracking_features, by = c("match_id", "event_id")) |>
  mutate(has_tracking = !is.na(nearest_def_dist))

stopifnot(!anyDuplicated(analysis[c("match_id", "event_id")]))  # no fan-out

cat("Tracking coverage in analysis:",
    round(100 * mean(analysis$has_tracking), 1), "%\n")

# ==============================================================================
# 6. QUALITY FILTERS (your section 6, unchanged) -> analysis_clean
# ==============================================================================

n_start <- nrow(analysis)

analysis_clean <- analysis |>
  filter(!is_header %in% TRUE) |>
  filter(!hand_pass %in% TRUE) |>
  filter(is_player_possession_end_matched %in% TRUE) |>
  filter(!is.na(PTR)) |>
  filter(!is_overreach %in% TRUE) |>
  filter(is_player_possession_start_matched %in% TRUE) |>
  filter(!short_possession) |>
  filter(!disruption_possession %in% TRUE)

cat("Possessions:", n_start, "->", nrow(analysis_clean),
    "(removed:", n_start - nrow(analysis_clean), ")\n")

# ==============================================================================
# 7. FINAL VERIFICATION
# ==============================================================================

cat("Tracking coverage in analysis_clean:",
    round(100 * mean(analysis_clean$has_tracking), 1), "%\n")

# defenders in the same frame as the carrier? expect clearly positive r
cat("cor(nearest_def_dist, separation_start) =",
    round(cor(analysis_clean$nearest_def_dist,
              analysis_clean$separation_start,
              use = "complete.obs"), 3), "\n")

# when the carrier chose the best option, both lanes are the same segment
analysis_clean |>
  filter(PTR == 0, has_tracking, chose_best_option %in% TRUE) |>
  summarise(
    max_lane_dist_diff  = max(abs(lane_obstruction_diff), na.rm = TRUE),  # ~0
    max_lane_count_diff = max(abs(lane_count_diff),       na.rm = TRUE)   # 0
  ) |>
  print()

# range sanity
analysis_clean |>
  filter(has_tracking) |>
  summarise(
    max_team_area = max(team_surface_area, na.rm = TRUE),
    min_def_dist  = min(nearest_def_dist,  na.rm = TRUE),
    max_within_5m = max(n_within_5m,       na.rm = TRUE),
    n_neg_area    = sum(team_surface_area < 0, na.rm = TRUE)
  ) |>
  print()

# ==============================================================================
# 8. SAVE
# ==============================================================================

saveRDS(analysis_clean, "ptr_analysis_dataset_200_with_tracking.rds")

# Checking NA
# pass_distance is only calculated for successful passes
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

na_summary|>
  print(n = 10)

table(analysis_clean$pass_outcome)