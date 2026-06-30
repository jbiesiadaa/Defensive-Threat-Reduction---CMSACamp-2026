library(tidyverse)
library(jsonlite)
library(gt)
library(dplyr)
library(sportyR)


Event_data <- read.csv("mls_skillcorner/dynamic_events/match_649419_events.csv")




#  Passing options at frame_end 
options_per_frame <- Event_data |>
  filter(event_type == "passing_option", !is.na(xthreat)) |>
  group_by(frame_end) |>
  summarise(
    max_xthreat_available  = max(xthreat, na.rm = TRUE),
    mean_xthreat_available = mean(xthreat, na.rm = TRUE),
    n_options              = n(),
    n_dangerous_easy       = sum(dangerous == "True" &
                                   difficult_pass_target == "False", na.rm = TRUE),
    n_dangerous_difficult  = sum(dangerous == "True" &
                                   difficult_pass_target == "True",  na.rm = TRUE),
    # Passing options that are both dangerous and easy to execute
    #Danger option ratio
    DOR                    = n_dangerous_easy / pmax(n_options, 1),
    .groups = "drop"
  )



# Pressure features from on_ball_engagement at frame_start
pressure_per_frame <- Event_data |>
  filter(event_type == "on_ball_engagement") |>
  group_by(frame_start) |>
  summarise(n_pressers = n(),
            has_press             = any(event_subtype %in% c("pressing",
                                                     "counter_press",
                                                     "pressure",
                                                     "recovery_press"),
                                        na.rm = TRUE),
            pressing_chain_active = any(pressing_chain == "True", na.rm = TRUE),
            max_chain_length      = suppressWarnings(max(pressing_chain_length, na.rm = TRUE)),
            max_chain_length      = ifelse(is.infinite(max_chain_length), NA, max_chain_length),
            .groups = "drop")





# Off-ball runs at frame_start 
runs_per_frame <- Event_data |>
  filter(event_type == "off_ball_run") |>
  group_by(frame_start) |>
  summarise(
    n_runs             = n(),
    n_dangerous_runs   = sum(dangerous == "True", na.rm = TRUE),
    has_line_break_run = any(event_subtype == "run_ahead_of_the_ball"),
    has_pulling_run    = any(event_subtype %in% c("pulling_wide",
                                                  "pulling_half_space")),
    .groups = "drop"
  )



#  Defensive outcomes from on_ball_engagement via event_id 
outcomes_per_possession <- Event_data |>
  filter(event_type == "on_ball_engagement") |>
  group_by(associated_player_possession_event_id) |>
  summarise(stop_possession_danger   = any(stop_possession_danger   == "True", na.rm = TRUE),
    reduce_possession_danger = any(reduce_possession_danger == "True", na.rm = TRUE),
    force_backward           = any(force_backward           == "True", na.rm = TRUE),
    .groups = "drop"
  )





# Join and Build analysis_data 
analysis_data <- Event_data |>
  filter(event_type == "player_possession") |>
  
  # Remove raw outcome columns before join to avoid .x/.y conflicts
  select(-stop_possession_danger, -reduce_possession_danger, -force_backward) |>
  
  # Join aggregated tables
  left_join(options_per_frame,       by = "frame_end") |>
  left_join(pressure_per_frame,      by = "frame_start") |>
  left_join(runs_per_frame,          by = "frame_start") |>
  left_join(outcomes_per_possession, by = c("event_id" = 
                                              "associated_player_possession_event_id")) |>
  
  mutate(DOS = ifelse(!is.na(player_targeted_xthreat),
                 pmax(max_xthreat_available - player_targeted_xthreat, 0),
                 NA_real_),
    
    # Defensive outcome label
    defensive_outcome = case_when(
      stop_possession_danger   == TRUE ~ "Interception",
      reduce_possession_danger == TRUE ~ "Suppression",
      force_backward           == TRUE ~ "Forced backward",
      TRUE                             ~ "No effect"),
    
    # Pressure label
    pressure_type = case_when(
      pressing_chain_active == TRUE ~ "Pressing chain",
      has_press             == TRUE ~ "Individual press",
      n_pressers            >  0   ~ "Engagement only",
      TRUE                         ~ "No pressure")
  )

# Remove empty and all-NA columns
analysis_data <- analysis_data |>
  select(where(~ {
    non_na    <- sum(!is.na(.))
    non_empty <- sum(. != "", na.rm = TRUE)
    non_na > 0 & non_empty > 0
  }))




# EDA 1: xThreat heatmap of passing options on pitch 
passing_options_pos <- Event_data |>
  filter(event_type == "passing_option",
         !is.na(xthreat),
         !is.na(x_start), !is.na(y_start))

geom_soccer("EPL") +
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

geom_soccer("EPL") +
  stat_summary_hex(
    data = dos_spatial,
    aes(x = x_start, y = y_start, z = DOS),
    fun = mean, bins = 20, alpha = 0.5
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




# Build summary table
dos_summary <- analysis_data |>
  filter(!is.na(DOS)) |>
  mutate(
    defensive_outcome = factor(defensive_outcome,
                               levels = c("Interception", "Suppression", 
                                          "Forced backward", "No effect"))
  ) |>
  group_by(defensive_outcome) |>
  summarise(
    n             = n(),
    mean_DOS      = mean(DOS, na.rm = TRUE),
#   median_DOS    = median(DOS, na.rm = TRUE),
    pct_positive  = mean(DOS > 0, na.rm = TRUE) * 100,
#    mean_DOR      = mean(DOR, na.rm = TRUE),
#    mean_n_opts   = mean(n_passing_options, na.rm = TRUE),
#    mean_n_danger = mean(n_passing_options_dangerous_not_difficult, 
    .groups = "drop"
  )

# Build gt table
dos_summary |>
  gt() |>
  
  # Column labels
  cols_label(
    defensive_outcome = "Defensive Outcome",
    n                 = "N",
    mean_DOS          = "Mean DOS",
#    median_DOS        = "Median DOS",
    pct_positive      = "% Positive DOS",
#    mean_DOR          = "Mean DOR",
#    mean_n_opts       = "Avg Options",
#    mean_n_danger     = "Avg Dangerous\nEasy Options"
  ) |>
  
  # Format numbers
  fmt_number(columns = c(mean_DOS), decimals = 5) |>
#  fmt_number(columns = c(mean_n_opts, mean_n_danger),     decimals = 2) |>
  fmt_number(columns = pct_positive,                      decimals = 1) |>
  
  # Title
  tab_header(
    title    = md("**Defensive Opportunity Suppression (DOS) by Outcome**"),
    subtitle = md("DOS = max available xThreat − xThreat of pass chosen")
  ) |>
  
  # Source note explaining metrics
  tab_source_note(
    source_note = md(
      "**High DOS** means the attacker had a dangerous option available but chose something far less threatening
       **% Positive DOS:** Share of events where defense forced a suboptimal decision.  "
    )
  ) |>
  
  # Highlight the pct_positive column
  tab_style(
    style     = cell_fill(color = "#E1F5EE"),
    locations = cells_body(columns = pct_positive)
  ) |>
  
  # Bold the outcome column
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = defensive_outcome)
  ) |>
  
  # Color pct_positive cells by value
  data_color(
    columns = pct_positive,
    fn      = scales::col_numeric(
      palette = c("#ffffff", "#2d6a4f"),
      domain  = c(0, 100)
    )
  ) |>
  
  # Color mean_DOS cells
  data_color(
    columns = mean_DOS,
    fn      = scales::col_numeric(
      palette = c("#ffffff", "#2d6a4f"),
      domain  = c(0, max(dos_summary$mean_DOS))
    )
  ) |>
  
  tab_options(
    table.font.size         = 13,
    heading.title.font.size = 16,
    column_labels.font.weight = "bold",
    table.border.top.color  = "#2d6a4f",
    table.border.top.width  = px(3)
  )








folder <- "mls_skillcorner/dynamic_events"

# find all event csv files
files <- list.files(
  path = folder,
  pattern = "_events\\.csv$",
  full.names = TRUE
)

# keep only first 50 games
files_10 <- files[1:50]

# read and combine 10 games
dynamic_10 <- do.call(
  rbind,
  lapply(files_10, function(file) {
    data <- read.csv(file)
    data$source_file <- file
    data$game_id <- gsub("match_|_events.csv", "", basename(file))
    return(data)
  })
)






