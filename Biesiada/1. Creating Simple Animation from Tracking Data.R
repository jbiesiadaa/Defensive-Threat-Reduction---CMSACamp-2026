## 1. Creating Simple Animation from Tracking Data
## Julia Biesiada
## Creating Visual for tracking players and ball

## 0. Loading Data 
library(tidyverse)
library(jsonlite)
library(sportyR)
library(gganimate)
library(gifski)

# Basic Data
matchdata <- fromJSON("mls_skillcorner/match_data/match_742721_data.json")
events <- read.csv("mls_skillcorner/dynamic_events/match_742721_events.csv")
tracking <- fromJSON("mls_skillcorner/tracking/match_742721_tracking.json")

# Reading Data for the Plot -> 0. Data Cleaning Created
players_joined <- read.csv("players_joined.csv")
ball_long <- read.csv("ball_long.csv") 


## 1. Filtering for frames where we have a shot
shots <- events |>
  filter(end_type == "shot") |>
  select(
    event_id,
    event_type,
    event_subtype,
    frame_start,
    frame_end,
    end_type,
    lead_to_goal
  ) |>
  arrange(frame_start)

# View shot
shots

## 2. Modifying Pitch Size from SportyR Package code go to Github SportyR "R/features-soccer.R")

# ------------------------------------------------------------------
# draw_skillcorner_pitch()
#
# Draws a soccer pitch scaled to ANY legal field size (e.g. each MLS
# stadium has slightly different dimensions). Pass in the real
# length/width of the specific field you're plotting and everything
# (outline, zoom crops) adjusts automatically.
#
#
# ------------------------------------------------------------------
draw_skillcorner_pitch <- function(pitch_length = 105, pitch_width = 68,
                                   zoom = "full", overlap = 5) {
  
  # --- Adding Protection for the pitch length and width converting to numbers ---
  pitch_length <- as.numeric(pitch_length)
  pitch_width <- as.numeric(pitch_width)
  
  # --- Adding Protection for the Zoom ---
  zoom <- match.arg(zoom, choices = c("full", "left", "right"))
  
  # ---- Outer boundary: this is the only part that scales with the field ----
  x_min <- -pitch_length / 2
  x_max <-  pitch_length / 2
  y_min <- -pitch_width / 2
  y_max <-  pitch_width / 2
  
  x_margin <- pitch_length * 0.03
  y_margin <- pitch_width * 0.05
  
  # ---- Fixed markings (identical on every legal pitch) ----
  penalty_depth      <- 16.5
  penalty_half_width <- 20.16
  six_depth          <- 5.5
  six_half_width     <- 9.16
  goal_depth         <- 2.5
  goal_half_width    <- 3.66
  center_circle_r    <- 9.15
  penalty_spot_dist  <- 11
  penalty_arc_r      <- 9.15
  
  left_penalty_line  <- x_min + penalty_depth
  right_penalty_line <- x_max - penalty_depth
  left_six_line      <- x_min + six_depth
  right_six_line     <- x_max - six_depth
  left_pen_spot      <- x_min + penalty_spot_dist
  right_pen_spot     <- x_max - penalty_spot_dist
  
  line_size      <- 0.45
  goal_line_size <- 0.55
  spot_size      <- 1.2
  
  center_circle <- data.frame(
    x = center_circle_r * cos(seq(0, 2 * pi, length.out = 200)),
    y = center_circle_r * sin(seq(0, 2 * pi, length.out = 200))
  )
  
  theta <- acos((penalty_depth - penalty_spot_dist) / penalty_arc_r)
  
  left_arc <- data.frame(
    x = left_pen_spot + penalty_arc_r * cos(seq(-theta, theta, length.out = 100)),
    y = penalty_arc_r * sin(seq(-theta, theta, length.out = 100))
  )
  
  right_arc <- data.frame(
    x = right_pen_spot + penalty_arc_r * cos(seq(pi - theta, pi + theta, length.out = 100)),
    y = penalty_arc_r * sin(seq(pi - theta, pi + theta, length.out = 100))
  )
  
  p <- ggplot() +
    
    annotate(
      "rect",
      xmin = x_min - goal_depth - 2,
      xmax = x_max + goal_depth + 2,
      ymin = y_min - 2,
      ymax = y_max + 2,
      fill = "#0b5f00",
      color = NA
    ) +
    
    # Outer pitch
    annotate("segment", x = x_min, xend = x_max, y = y_min, yend = y_min, color = "white", linewidth = line_size) +
    annotate("segment", x = x_min, xend = x_max, y = y_max, yend = y_max, color = "white", linewidth = line_size) +
    annotate("segment", x = x_min, xend = x_min, y = y_min, yend = y_max, color = "white", linewidth = line_size) +
    annotate("segment", x = x_max, xend = x_max, y = y_min, yend = y_max, color = "white", linewidth = line_size) +
    
    # Halfway line
    annotate("segment", x = 0, xend = 0, y = y_min, yend = y_max, color = "white", linewidth = line_size) +
    
    # Center circle and spot
    geom_path(data = center_circle, aes(x = x, y = y), color = "white", linewidth = line_size) +
    annotate("point", x = 0, y = 0, color = "white", size = spot_size) +
    
    # Left penalty box
    annotate("segment", x = x_min, xend = left_penalty_line, y = penalty_half_width, yend = penalty_half_width, color = "white", linewidth = line_size) +
    annotate("segment", x = left_penalty_line, xend = left_penalty_line, y = penalty_half_width, yend = -penalty_half_width, color = "white", linewidth = line_size) +
    annotate("segment", x = left_penalty_line, xend = x_min, y = -penalty_half_width, yend = -penalty_half_width, color = "white", linewidth = line_size) +
    
    # Right penalty box
    annotate("segment", x = x_max, xend = right_penalty_line, y = penalty_half_width, yend = penalty_half_width, color = "white", linewidth = line_size) +
    annotate("segment", x = right_penalty_line, xend = right_penalty_line, y = penalty_half_width, yend = -penalty_half_width, color = "white", linewidth = line_size) +
    annotate("segment", x = right_penalty_line, xend = x_max, y = -penalty_half_width, yend = -penalty_half_width, color = "white", linewidth = line_size) +
    
    # Left six-yard box
    annotate("segment", x = x_min, xend = left_six_line, y = six_half_width, yend = six_half_width, color = "white", linewidth = line_size) +
    annotate("segment", x = left_six_line, xend = left_six_line, y = six_half_width, yend = -six_half_width, color = "white", linewidth = line_size) +
    annotate("segment", x = left_six_line, xend = x_min, y = -six_half_width, yend = -six_half_width, color = "white", linewidth = line_size) +
    
    # Right six-yard box
    annotate("segment", x = x_max, xend = right_six_line, y = six_half_width, yend = six_half_width, color = "white", linewidth = line_size) +
    annotate("segment", x = right_six_line, xend = right_six_line, y = six_half_width, yend = -six_half_width, color = "white", linewidth = line_size) +
    annotate("segment", x = right_six_line, xend = x_max, y = -six_half_width, yend = -six_half_width, color = "white", linewidth = line_size) +
    
    # Goals
    annotate("segment", x = x_min, xend = x_min - goal_depth, y = goal_half_width, yend = goal_half_width, color = "white", linewidth = goal_line_size) +
    annotate("segment", x = x_min - goal_depth, xend = x_min - goal_depth, y = goal_half_width, yend = -goal_half_width, color = "white", linewidth = goal_line_size) +
    annotate("segment", x = x_min - goal_depth, xend = x_min, y = -goal_half_width, yend = -goal_half_width, color = "white", linewidth = goal_line_size) +
    
    annotate("segment", x = x_max, xend = x_max + goal_depth, y = goal_half_width, yend = goal_half_width, color = "white", linewidth = goal_line_size) +
    annotate("segment", x = x_max + goal_depth, xend = x_max + goal_depth, y = goal_half_width, yend = -goal_half_width, color = "white", linewidth = goal_line_size) +
    annotate("segment", x = x_max + goal_depth, xend = x_max, y = -goal_half_width, yend = -goal_half_width, color = "white", linewidth = goal_line_size) +
    
    # Penalty spots
    annotate("point", x = left_pen_spot, y = 0, color = "white", size = spot_size) +
    annotate("point", x = right_pen_spot, y = 0, color = "white", size = spot_size) +
    
    # Penalty arcs
    geom_path(data = left_arc, aes(x = x, y = y), color = "white", linewidth = line_size) +
    geom_path(data = right_arc, aes(x = x, y = y), color = "white", linewidth = line_size) +
    
    theme_void()
  
  # ------------------------------------------------------------------
  # Zoom: now relative to whatever pitch_length / pitch_width you passed
  # in, instead of hardcoded numbers. Left/right always split at the
  # true halfway line (x = 0), so this works for any field size.
  # ------------------------------------------------------------------
  full_xlim <- c(x_min - goal_depth - x_margin, x_max + goal_depth + x_margin)
  full_ylim <- c(y_min - y_margin, y_max + y_margin)
  
  if (zoom == "left") {
    p <- p + coord_fixed(
      xlim = c(full_xlim[1], overlap),
      ylim = full_ylim,
      clip = "off"
    )
  } else if (zoom == "right") {
    p <- p + coord_fixed(
      xlim = c(-overlap, full_xlim[2]),
      ylim = full_ylim,
      clip = "off"
    )
  } else {
    p <- p + coord_fixed(
      xlim = full_xlim,
      ylim = full_ylim,
      clip = "off"
    )
  }
  
  return(p)
}

# ------------------------------------------------------------------
# Example usage with different field sizes
# draw_skillcorner_pitch(pitch_length = 105, pitch_width = 68, zoom = "full")
# draw_skillcorner_pitch(pitch_length = 100, pitch_width = 64, zoom = "right", overlap = 0)
# ------------------------------------------------------------------
draw_skillcorner_pitch(pitch_length = 105, pitch_width = 68, zoom = "full")
draw_skillcorner_pitch(pitch_length = 110, pitch_width = 70, zoom = "left")
draw_skillcorner_pitch(pitch_length = 100, pitch_width = 64, zoom = "right", overlap = 0)


# My project:

draw_skillcorner_pitch(
  pitch_length = matchdata$pitch_length,
  pitch_width = matchdata$pitch_width,
  zoom = "full"
)


## 3. Creating Animation

## 3.1.2 Full action starts here ( I verify by chekcing when the frame starts and leads_to_goal)
frame_min <- 45300

## 3.1.2 Players freeze here
player_freeze_frame <- 45386

## 3.1.3 Ball continues until here
frame_max <- 45399

## 3.2 Players moving from 45300 to 45386
players_moving <- players_joined |>
  filter(frame >= frame_min, frame <= player_freeze_frame)

## 3.3 Frames after the freeze
freeze_frames <- tibble(frame = (player_freeze_frame + 1):frame_max)

## 3.4 Players frozen at frame 45386, repeated for later frames
players_frozen <- players_joined |>
  filter(frame == player_freeze_frame) |>
  select(-frame) |>
  crossing(freeze_frames)

## 3.5.1 Combine moving + frozen players
players_anim_final <- bind_rows(players_moving, players_frozen)

## 3.5.2 Combine moving + ball
ball_anim_final <- ball_long |>
  filter(frame >= frame_min, frame <= frame_max) |>
  filter(!is.na(ball_x), !is.na(ball_y))

## 3.6 Creating a Team Label
home_id <- matchdata$home_team$id
away_id <- matchdata$away_team$id

home_name <- matchdata$home_team$short_name
away_name <- matchdata$away_team$short_name

players_anim_final <- players_anim_final |>
  mutate(
    team_label = case_when(
      team_id == home_id ~ home_name,
      team_id == away_id ~ away_name,
      TRUE ~ paste0("Team ", team_id)
    )
  )

## 3.7 Animation Code
## 3.7 Animation Code
anim_full_action <- draw_skillcorner_pitch(
  pitch_length = matchdata$pitch_length,
  pitch_width = matchdata$pitch_width,
  zoom = "full"
) +
  geom_point(
    data = players_anim_final,
    aes(
      x = player_x,
      y = player_y,
      color = team_label,
      alpha = as.factor(is_detected)
    ),
    size = 4
  ) +
  geom_point(
    data = ball_anim_final,
    aes(x = ball_x, y = ball_y),
    color = "white",
    size = 2
  ) +
  scale_alpha_manual(
    values = c("FALSE" = 0.35, "TRUE" = 1),
    guide = "none"
  ) +
  labs(
    title = "Full Goal Action",
    subtitle = "Frame: {current_frame}",
    caption = "Note: Players not detected by tracking are shown with lower opacity.",
    color = "Team"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      face = "italic",
      size = 12,
      color = "gray40",
      hjust = 0.5
    ),
    plot.caption = element_text(
      size = 10,
      hjust = 0.5,
      color = "gray40"
    )
  ) +
  transition_manual(frame)

# View Animation
anim_full_action

## 3.8 Saving Animation
animate(
  anim_full_action,
  fps = 10,
  width = 1200,
  height = 800,
  res = 150,
  end_pause = 10,
  renderer = gifski_renderer("full_goal_action_clean.gif")
)


## 4. Checking Detection for Goalie

# Goal Action Check -> Goalkeeper
frames_check <- players_joined|>
  filter(frame > 45299 & frame < 45383)
#filter(position == "Goalkeeper")|>
#filter(is_detected == "TRUE")

frames_check

# NO Goalie is detected for goal action

matchdata$home_team$short_name
matchdata$away_team$short_name
