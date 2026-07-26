## 10. PTR Situation
## Julia Biesiada
## Creating two situations PTR = 0 and PTR = 1 to represent on the field

library(tidyverse)
library(jsonlite)
library(sportyR)
library(patchwork)
library(grid)


# 0. Creating a frame ----------------------------------------------------------
analysis_clean <- readRDS("ptr_analysis_clean_502games.rds")

analysis_clean|>
  select(match_id,frame_start, frame_end, PTR, pass_outcome, player_id, 
         player_targeted_id, best_option_player_id)|>
  filter(PTR > 0.3)



analysis_clean <- analysis_clean |>
  mutate(PTR_binary = as.integer(PTR > 0))

# 1. SETTINGS ------------------------------------------------------------------


FONT     <- "sans"
LOGO_DIR <- "teams_logo"

good_match <- 1066465
good_frame <- 2672

bad_match  <- 1066465
bad_frame  <- 31477

PITCH_L <- 105
PITCH_W <- 68

COL_ATT     <- "#1A6BD6"
COL_DEF     <- "#F03A50"
COL_BEST    <- "#42C978"
COL_TARGET  <- "#FFD000"
COL_CARRIER <- "#111111"



# 2. TEAM ABBREVIATIONS --------------------------------------------------------


team_abbr_lookup <- c(
  "1508" = "ATL", "1498" = "SJE", "1504" = "VAN", "1503" = "PHI",
  "863"  = "CLB", "1501" = "RSL", "1507" = "NSH", "1506" = "NYC",
  "2312" = "CLT", "1500" = "NER", "884"  = "DCU", "1502" = "TOR",
  "885"  = "CIN", "862"  = "HOU", "1494" = "MIA", "1505" = "MTL",
  "337"  = "ORL", "883"  = "NYR", "1757" = "ATX", "2906" = "STL",
  "336"  = "DAL", "861"  = "MIN", "919"  = "SEA", "1499" = "COL",
  "860"  = "POR", "1497" = "SKC", "1495" = "LAG", "1496" = "CHI",
  "918"  = "LAFC"
)

team_abbr <- function(team_id) {
  result <- unname(team_abbr_lookup[as.character(team_id)])
  if (is.na(result)) as.character(team_id) else result
}



# 3. DRAW PITCH ------------------------------------------------------------------


draw_pitch <- function(line_color = "white", line_width = 0.65) {
  
  half_length <- PITCH_L / 2
  half_width  <- PITCH_W / 2
  
  make_arc <- function(center_x, center_y, radius, start_angle, end_angle, n = 100) {
    angle <- seq(start_angle, end_angle, length.out = n)
    tibble(
      x = center_x + radius * cos(angle),
      y = center_y + radius * sin(angle)
    )
  }
  
  penalty_angle <- acos((16.5 - 11) / 9.15)
  
  list(
    annotate("rect",
             xmin = -half_length - 4, xmax = half_length + 4,
             ymin = -half_width - 4,  ymax = half_width + 4,
             fill = "#4C9A5E", color = NA),
    
    annotate("rect",
             xmin = -half_length, xmax = half_length,
             ymin = -half_width,  ymax = half_width,
             fill = "#5CAF6E", color = line_color, linewidth = line_width),
    
    annotate("segment",
             x = 0, xend = 0, y = -half_width, yend = half_width,
             color = line_color, linewidth = line_width),
    
    geom_path(data = make_arc(0, 0, 9.15, 0, 2 * pi), aes(x, y),
              inherit.aes = FALSE, color = line_color, linewidth = line_width),
    
    annotate("point", x = 0, y = 0, color = line_color, size = 1),
    
    annotate("rect",
             xmin = -half_length, xmax = -half_length + 16.5,
             ymin = -20.16, ymax = 20.16,
             fill = NA, color = line_color, linewidth = line_width),
    
    annotate("rect",
             xmin = half_length - 16.5, xmax = half_length,
             ymin = -20.16, ymax = 20.16,
             fill = NA, color = line_color, linewidth = line_width),
    
    annotate("rect",
             xmin = -half_length, xmax = -half_length + 5.5,
             ymin = -9.16, ymax = 9.16,
             fill = NA, color = line_color, linewidth = line_width),
    
    annotate("rect",
             xmin = half_length - 5.5, xmax = half_length,
             ymin = -9.16, ymax = 9.16,
             fill = NA, color = line_color, linewidth = line_width),
    
    annotate("point", x = -half_length + 11, y = 0,
             color = line_color, size = 1),
    annotate("point", x = half_length - 11, y = 0,
             color = line_color, size = 1),
    
    geom_path(data = make_arc(-half_length + 11, 0, 9.15,
                              -penalty_angle, penalty_angle),
              aes(x, y), inherit.aes = FALSE,
              color = line_color, linewidth = line_width),
    
    geom_path(data = make_arc(half_length - 11, 0, 9.15,
                              pi - penalty_angle, pi + penalty_angle),
              aes(x, y), inherit.aes = FALSE,
              color = line_color, linewidth = line_width),
    
    annotate("rect",
             xmin = -half_length - 2.4, xmax = -half_length,
             ymin = -3.66, ymax = 3.66,
             fill = NA, color = line_color, linewidth = line_width),
    
    annotate("rect",
             xmin = half_length, xmax = half_length + 2.4,
             ymin = -3.66, ymax = 3.66,
             fill = NA, color = line_color, linewidth = line_width)
  )
}

# 4. DRAW ONE PTR FRAME --------------------------------------------------------

plot_ptr_frame <- function(match_id_use, frame_use, panel_title) {
  
  example_row <- analysis_clean |>
    filter(match_id == match_id_use, frame_end == frame_use) |>
    slice(1)
  
  if (nrow(example_row) == 0) {
    stop("No possession found for the selected match and frame.")
  }
  
  carrier_id  <- as.character(example_row$player_id[1])
  targeted_id <- as.character(example_row$player_targeted_id[1])
  best_id     <- as.character(example_row$best_option_player_id[1])
  same_player <- targeted_id == best_id
  
  matchdata <- fromJSON(
    paste0("mls_skillcorner/match_data/match_", match_id_use, "_data.json")
  )
  tracking <- fromJSON(
    paste0("mls_skillcorner/tracking/match_", match_id_use, "_tracking.json")
  )
  
  players_lookup <- matchdata$players |>
    as_tibble() |>
    transmute(player_id = as.character(id), team_id = as.character(team_id))
  
  frame_players <- tracking |>
    select(frame, player_data) |>
    unnest(player_data) |>
    transmute(
      frame,
      player_id = as.character(player_id),
      player_x  = as.numeric(x),
      player_y  = as.numeric(y)
    ) |>
    filter(frame == frame_use) |>
    left_join(players_lookup, by = "player_id") |>
    filter(!is.na(player_x), !is.na(player_y), !is.na(team_id))
  
  ball_xy <- tracking |>
    filter(frame == frame_use) |>
    pull(ball_data) |>
    as_tibble() |>
    transmute(ball_x = as.numeric(x), ball_y = as.numeric(y)) |>
    filter(!is.na(ball_x), !is.na(ball_y)) |>
    slice(1)
  
  attacking_team_id <- frame_players |>
    filter(player_id == carrier_id) |> pull(team_id) |> first()
  
  if (length(attacking_team_id) == 0 || is.na(attacking_team_id)) {
    stop("Carrier team could not be identified.")
  }
  
  defending_team_id <- frame_players |>
    distinct(team_id) |> filter(team_id != attacking_team_id) |>
    pull(team_id) |> first()
  
  team_location <- frame_players |>
    group_by(team_id) |>
    summarise(mean_x = mean(player_x), .groups = "drop")
  
  att_mean_x <- team_location$mean_x[team_location$team_id == attacking_team_id]
  def_mean_x <- team_location$mean_x[team_location$team_id == defending_team_id]
  
  if (length(att_mean_x) && length(def_mean_x) && att_mean_x > def_mean_x) {
    frame_players <- frame_players |>
      mutate(player_x = -player_x, player_y = -player_y)
    ball_xy <- ball_xy |> mutate(ball_x = -ball_x, ball_y = -ball_y)
  }
  
  carrier_xy  <- frame_players |> filter(player_id == carrier_id)  |> slice(1)
  targeted_xy <- frame_players |> filter(player_id == targeted_id) |> slice(1)
  best_xy     <- frame_players |> filter(player_id == best_id)     |> slice(1)
  
  if (nrow(ball_xy) == 0) {
    ball_xy <- carrier_xy |> transmute(ball_x = player_x, ball_y = player_y)
  }
  
  plot_players <- frame_players |>
    mutate(
      side = case_when(
        team_id == attacking_team_id ~ "Attacking team",
        team_id == defending_team_id ~ "Defending team",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(side), player_x >= 8)
  
  played_line <- tibble(
    x = carrier_xy$player_x[1], y = carrier_xy$player_y[1],
    xend = targeted_xy$player_x[1], yend = targeted_xy$player_y[1]
  )
  
  best_line <- if (same_player) {
    tibble(x = numeric(), y = numeric(), xend = numeric(), yend = numeric())
  } else {
    tibble(
      x = carrier_xy$player_x[1], y = carrier_xy$player_y[1],
      xend = best_xy$player_x[1], yend = best_xy$player_y[1]
    )
  }
  
  ggplot() +
    draw_pitch() +
    
    geom_point(
      data = plot_players,
      aes(x = player_x, y = player_y, color = side),
      size = 5
    ) +
    
    geom_segment(
      data = best_line,
      aes(x = x, y = y, xend = xend, yend = yend),
      color = "black", linewidth = 1.1, linetype = "dashed",
      arrow = arrow(length = unit(0.2, "cm"), type = "closed")
    ) +
    
    geom_segment(
      data = played_line,
      aes(x = x, y = y, xend = xend, yend = yend),
      color = "black", linewidth = 1.2,
      arrow = arrow(length = unit(0.2, "cm"), type = "closed")
    ) +
    
    geom_point(
      data = ball_xy,
      aes(x = ball_x, y = ball_y),
      shape = 21, size = 2.5, fill = "white", color = "black", stroke = 1
    ) +
    
    scale_color_manual(
      values = c("Attacking team" = COL_ATT, "Defending team" = COL_DEF),
      guide = "none"
    ) +
    
    coord_fixed(
      xlim = c(8, PITCH_L / 2 + 4),
      ylim = c(-PITCH_W / 2 - 2, PITCH_W / 2 + 2),
      expand = FALSE
    ) +
    
    labs(title = panel_title) +
    
    theme_void(base_family = FONT) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 18,
                                margin = margin(b = 8)),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(6, 6, 4, 6)
    )
}

plot_ptr0 <- plot_ptr_frame(good_match, good_frame, "PTR = 0")
plot_ptr1 <- plot_ptr_frame(bad_match,  bad_frame,  "PTR = 1")

plot_ptr0
plot_ptr1

# Save PTR Clean Seprate 
# PTR 0 
ggsave(
  filename = "PTR = 0.png",
  plot = plot_ptr0,
  width = 12.8,
  height = 7.2,
  units = "in",
  dpi = 150,
  bg = "white"
)

# PTR = 1
ggsave(
  filename = "PTR = 1.png",
  plot = plot_ptr1,
  width = 12.8,
  height = 7.2,
  units = "in",
  dpi = 150,
  bg = "white"
)



legend_panel <- ggplot() +
  
  # Attacking team
  annotate("point", x = 0.12, y = 0.85, size = 3, color = COL_ATT) +
  annotate("text", x = 0.18, y = 0.85, label = "Attacking team",
           hjust = 0, size = 3.5, family = FONT) +
  
  # Defending team
  annotate("point", x = 0.12, y = 0.65, size = 4, color = COL_DEF) +
  annotate("text", x = 0.18, y = 0.65, label = "Defending team",
           hjust = 0, size = 3.5, family = FONT) +
  
  # Pass played
  annotate("segment", x = 0.08, xend = 0.16, y = 0.45, yend = 0.45,
           color = "black", linewidth = 0.9,
           arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
  annotate("text", x = 0.18, y = 0.45, label = "Pass played",
           hjust = 0, size = 3.5, family = FONT) +
  
  # Best available
  annotate("segment", x = 0.08, xend = 0.16, y = 0.25, yend = 0.25,
           color = "black", linewidth = 0.8, linetype = "dashed",
           arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
  annotate("text", x = 0.18, y = 0.25, label = "Best available",
           hjust = 0, size = 3.5, family = FONT) +
  
  coord_cartesian(
    xlim = c(0, 1),
    ylim = c(0, 1),
    clip = "off",
    expand = FALSE
  ) +
  theme_void()

legend_panel

# Save Legend
ggsave(
  filename = "legend_panel_transparent.png",
  plot = legend_panel,
  width = 3,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "transparent"
)

final_plot <- (plot_ptr0 + plot_ptr1) / legend_panel +
  plot_layout(heights = c(1, 0.10))

final_plot

# Best option for printing: vector PDF
ggsave(
  "ptr_comparison_final.pdf",
  plot = final_plot,
  width = 14,
  height = 5.6,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

# High-resolution PNG backup
ggsave(
  "ptr_comparison_final.png",
  plot = final_plot,
  width = 14,
  height = 5.6,
  units = "in",
  dpi = 600,
  device = ragg::agg_png,
  bg = "white",
  limitsize = FALSE
)

# install.packages("svglite")  # only once

library(svglite)
ggsave(
  "ptr_comparison_final.svg",
  plot = final_plot,
  width = 14,
  height = 5.6,
  units = "in",
  device = svglite::svglite,
  bg = "white")
