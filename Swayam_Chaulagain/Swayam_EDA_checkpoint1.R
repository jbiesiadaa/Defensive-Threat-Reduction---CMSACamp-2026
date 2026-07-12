library(tidyverse)
library(jsonlite)
library(gt)
library(dplyr)
library(sportyR)



Event_data <- read.csv("mls_skillcorner/dynamic_events/match_649419_events.csv")



# Working with multiple games

folder <- "mls_skillcorner/dynamic_events"

# find all event csv files
files <- list.files(
  path = folder,
  pattern = "_events\\.csv$",
  full.names = TRUE
)

# keep only first 50 games
files_50 <- files[1:50]

# read and combine 10 games
dynamic_50 <- do.call(
  rbind,
  lapply(files_50, function(file) {
    data <- read.csv(file)
    data$source_file <- file
    data$game_id <- gsub("match_|_events.csv", "", basename(file))
    return(data)
  })
)





# Step 1: Passing options aggregated at frame_end
# We join at frame_end because player_targeted_xthreat is computed at the moment of the pass (frame_end), reducing timing mismatch

options_per_frame <- dynamic_50 |>
  filter(event_type == "passing_option", !is.na(xthreat)) |>
  group_by(frame_end) |>
  summarise(
    max_xthreat_available = max(xthreat, na.rm = TRUE),
    .groups = "drop"
  )



# ── Step 2: Defensive outcomes from on_ball_engagement

outcomes_per_possession <- dynamic_50 |>
  filter(event_type == "on_ball_engagement") |>
  group_by(match_id, associated_player_possession_event_id) |>
  summarise(
    stop_possession_danger   = any(stop_possession_danger   == "True", na.rm = TRUE),
    reduce_possession_danger = any(reduce_possession_danger == "True", na.rm = TRUE),
    force_backward           = any(force_backward           == "True", na.rm = TRUE),
    n_engagements            = n(),
    .groups = "drop"
  )


# Step 3: Build analysis_data 
analysis_data <- dynamic_50 |>
  filter(event_type == "player_possession") |>
  select(-stop_possession_danger, -reduce_possession_danger, -force_backward) |>
  left_join(options_per_frame,       by = "frame_end") |>
  left_join(outcomes_per_possession, by = c("match_id",
                                            "event_id" =
                                              "associated_player_possession_event_id")) |>
  mutate(
    # DOS: how far below their best available option did the attacker go?
    DOS = ifelse(
      !is.na(player_targeted_xthreat),
      max_xthreat_available - player_targeted_xthreat,
      NA_real_),
    
    # Defensive outcome — what did the defense achieve on this possession?
    defensive_outcome = case_when(
      stop_possession_danger   == TRUE ~ "Interception / Turnover",
      reduce_possession_danger == TRUE ~ "Suppression",
      force_backward           == TRUE ~ "Forced Backward",
      n_engagements > 0               ~ "Engaged, No Outcome",
      TRUE                            ~ "No Engagement"
    )
  )

# Step 4: Remove empty and all-NA columns 
analysis_data <- analysis_data |>
  select(where(~ sum(!is.na(.)) > 0 & sum(. != "", na.rm = TRUE) > 0))


analysis_data |>
  summarise(
    n_negative = sum(
      (max_xthreat_available - player_targeted_xthreat) < 0,
      na.rm = TRUE
    )
  )



# Analysis table by Defense outcome
dos_summary <- analysis_data |>
  filter(!is.na(DOS), n_engagements > 0) |>
  mutate(defensive_outcome = factor(defensive_outcome,
                                    levels = c("Suppression", "Interception / Turnover",
                                               "Forced Backward", "Engaged, No Outcome"))) |>
  group_by(defensive_outcome) |>
  summarise(
    n            = n(),
    mean_DOS     = mean(DOS, na.rm = TRUE),
    pct_positive = mean(DOS > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  )

dos_summary |>
  gt() |>
  cols_label(
    defensive_outcome = "Defensive Outcome",
    n                 = "N",
    mean_DOS          = "Mean PTR",
    pct_positive      = "% Positive PTR"
  ) |>
  tab_header(
    title    = md("**Passing Threat Reduction (PTR) by Outcome**"),
    subtitle = md(" PTR = (max available xThreat − xThreat of pass chosen)")
  ) |>
  tab_source_note(md(
    "**PTR > 0:** attacker chose below their best available option — defense forced attacker a poor pass decision."
  )) |>
  fmt_number(columns = mean_DOS,     decimals = 5) |>
  fmt_number(columns = pct_positive, decimals = 1) |>
  fmt_integer(columns = n) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = defensive_outcome)
  ) |>
  tab_style(
    style = list(cell_fill(color = "#2d6a4f"),
                 cell_text(color = "white", weight = "bold")),
    locations = cells_body(
      columns = pct_positive,
      rows    = pct_positive == max(dos_summary$pct_positive, na.rm = TRUE)
    )
  ) |>
  tab_style(
    style = list(cell_fill(color = "#2d6a4f"),
                 cell_text(color = "white", weight = "bold")),
    locations = cells_body(
      columns = mean_DOS,
      rows    = mean_DOS == max(dos_summary$mean_DOS, na.rm = TRUE)
    )
  ) |>
  opt_row_striping() |>
  tab_options(
    table.font.size               = 13,
    heading.title.font.size       = 16,
    column_labels.font.weight     = "bold",
    table.border.top.color        = "#2d6a4f",
    table.border.top.width        = px(3),
    row.striping.background_color = "#f5f5f5"
  )






View(dynamic_50 |>
  select(match_id, associated_player_possession_event_id, n_passing_options))






























# EDA 1: xThreat heatmap of passing options on pitch 
passing_options_pos <- dynamic_50 |>
  filter(event_type == "passing_option",
         !is.na(xthreat),
         !is.na(x_start), !is.na(y_start))

geom_soccer("FIFA") +
  stat_summary_hex(data = passing_options_pos,
                   aes(x = x_start, y = y_start, z = xthreat),
                   fun = mean, bins = 20, alpha = 0.5) +
  scale_fill_gradient2(
    mid      = "skyblue",
    high     = "red",
    name     = "Mean xThreat") +
  labs(title    = "Where do high-threat passing options appear?") +
  theme_void()




# EDA 2: DOS heatmap on pitch
dos_spatial <- analysis_data |>
  filter(!is.na(DOS), !is.na(x_start), !is.na(y_start))

geom_soccer("FIFA") +
  stat_summary_hex(
    data = dos_spatial,
    aes(x = x_start, y = y_start, z = DOS),
    fun = mean, bins = 25, alpha = 0.5
  ) +
  scale_fill_gradient2(
    low      = "skyblue",
    mid      = "white",
    high     = "red",
    midpoint = 0,
    name     = "Mean DOS"
  ) +
  labs(
    title    = "Mean DOS by pitch zone",
    subtitle = "Red = defense forced suboptimal choice | Skyblue = attacker found best option freely"
  ) +
  theme_void()





