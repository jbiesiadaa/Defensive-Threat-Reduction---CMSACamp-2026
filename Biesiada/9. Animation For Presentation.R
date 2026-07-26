# 9.Animation For Presentation
# Julia Biesiada
# To show the study case - I have already selected from analysis clean

# analysis_clean|>
#  select(match_id,frame_start, frame_end, PTR, pass_outcome, player_id, 
#         player_targeted_id, best_option_player_id)|>
#  filter(PTR > 0.35)

library(tidyverse)
library(jsonlite)
library(sportyR)
library(gganimate)
library(gifski)

matchdata <- fromJSON("mls_skillcorner/match_data/match_1066465_data.json")
events <- read.csv("mls_skillcorner/dynamic_events/match_1066465_events.csv")
tracking <- fromJSON("mls_skillcorner/tracking/match_1066465_tracking.json")


# SkillCorner Field
draw_skillcorner_pitch <- function(pitch_length = 110, pitch_width = 68,
                                   zoom = "left") {
  
  x_min <- -pitch_length / 2
  x_max <-  pitch_length / 2
  y_min <- -pitch_width / 2
  y_max <-  pitch_width / 2
  
  penalty_depth <- 16.5
  penalty_half_width <- 20.16
  
  six_depth <- 5.5
  six_half_width <- 9.16
  
  goal_depth <- 2.5
  goal_half_width <- 3.66
  
  center_circle_r <- 9.15
  penalty_spot_dist <- 11
  penalty_arc_r <- 9.15
  
  left_penalty_line <- x_min + penalty_depth
  right_penalty_line <- x_max - penalty_depth
  
  left_six_line <- x_min + six_depth
  right_six_line <- x_max - six_depth
  
  left_pen_spot <- x_min + penalty_spot_dist
  right_pen_spot <- x_max - penalty_spot_dist
  
  # thinner lines
  line_size <- 0.45
  goal_line_size <- 0.55
  spot_size <- 1.2
  
  center_circle <- data.frame(
    x = center_circle_r * cos(seq(0, 2*pi, length.out = 200)),
    y = center_circle_r * sin(seq(0, 2*pi, length.out = 200))
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
  
  if (zoom == "left") {
    p <- p + coord_fixed(
      xlim = c(x_min - goal_depth - 2, 20),
      ylim = c(-30, 30),
      clip = "on"
    )
  } else if (zoom == "right") {
    p <- p + coord_fixed(
      xlim = c(20, x_max + goal_depth + 2),
      ylim = c(-22, 22),
      clip = "off"
    )
  } else {
    p <- p + coord_fixed(
      xlim = c(x_min - goal_depth - 2, x_max + goal_depth + 2),
      ylim = c(y_min - 2, y_max + 2),
      clip = "off"
    )
  }
  
  return(p)
}

# 1. Making Tracking Data longer so each frame represents 1 player
players_long <- tracking |>
  select(frame, timestamp, period, player_data) |>
  unnest(player_data) |>
  rename(
    player_x = x,
    player_y = y
  )

head(players_long)

# 2. Adding Matchdata info to players id for clean look

#For all 
players_lookup_all <- matchdata$players |>
  as_tibble() |>
  unnest_wider(player_role, names_sep = "_")

# Clean Version (only important variables)
players_lookup_clean <- matchdata$players |>
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


# 3. Joining Tables (tracking with match data) by player id
players_joined <- players_long |>
  left_join(players_lookup_clean, by = "player_id")


# 4. Creating a Ball data 
ball_long <- tracking |>
  select(frame, timestamp, period, ball_data) |>
  unnest_wider(ball_data, names_sep = "_") |>
  transmute(
    frame,
    timestamp,
    period,
    ball_x = ball_data_x,
    ball_y = ball_data_y,
    ball_z = ball_data_z,
    ball_detected = ball_data_is_detected
  )

# 5. Adding a Ball frame to Player data for analysis
players_with_ball <- players_joined |>
  left_join(ball_long, by = c("frame", "timestamp", "period"))

# 6. TEAM ABBREVIATIONS --------------------------------------------------------

team_abbreviations <- c(
  "1508" = "ATL",
  "1498" = "SJ",
  "1504" = "VAN",
  "1503" = "PHI",
  "863"  = "CLB",
  "1501" = "RSL",
  "1507" = "NSH",
  "1506" = "NYC",
  "2312" = "CLT",
  "1500" = "NE",
  "884"  = "DC",
  "1502" = "TOR",
  "885"  = "CIN",
  "862"  = "HOU",
  "1494" = "MIA",
  "1505" = "MTL",
  "337"  = "ORL",
  "883"  = "NYRB",
  "1757" = "ATX",
  "2906" = "STL",
  "336"  = "DAL",
  "861"  = "MIN",
  "919"  = "SEA",
  "1499" = "COL",
  "860"  = "POR",
  "1497" = "SKC",
  "1495" = "LA",
  "1496" = "CHI",
  "918"  = "LAFC"
)



# 7. PLAYER LOOKUP
players_lookup_clean <- matchdata$players |>
  as_tibble() |>
  unnest_wider(player_role, names_sep = "_") |>
  transmute(
    
    # Both player IDs must have the same type
    player_id = as.character(id),
    
    player_name = short_name,
    
    # Convert team ID to character for the abbreviation lookup
    team_id = as.character(team_id),
    
    # Add team abbreviation
    team_abbr = unname(
      team_abbreviations[as.character(team_id)]
    ),
    
    number,
    position = player_role_name,
    position_group = player_role_position_group,
    position_acronym = player_role_acronym
  )



# 8. JOIN PLAYER INFORMATION

players_joined <- players_long |>
  mutate(
    player_id = as.character(player_id)
  ) |>
  left_join(
    players_lookup_clean,
    by = "player_id"
  )



# 9.BALL DATA ------------------------------------------------------------------

ball_long <- tracking |>
  select(frame, timestamp, period, ball_data) |>
  unnest_wider(ball_data, names_sep = "_") |>
  transmute(
    frame,
    timestamp,
    period,
    ball_x = ball_data_x,
    ball_y = ball_data_y,
    ball_z = ball_data_z,
    ball_detected = ball_data_is_detected
  )



# 10. ACTION FRAMES ------------------------------------------------------------


# CHANGE HERE: start of the action
frame_min <- 31460

# CHANGE HERE: frame right before the pass decision
frame_max <- 31477


# 11. PITCH SIZE ---------------------------------------------------------------


pitch_length <- as.numeric(matchdata$pitch_length)
pitch_width  <- as.numeric(matchdata$pitch_width)


# 12. FILTER PLAYER AND BALL DATA ----------------------------------------------


players_anim_final <- players_joined |>
  filter(
    frame >= frame_min,
    frame <= frame_max,
    !is.na(player_x),
    !is.na(player_y),
    !is.na(team_abbr)
  )


ball_anim_final <- ball_long |>
  filter(
    frame >= frame_min,
    frame <= frame_max,
    !is.na(ball_x),
    !is.na(ball_y)
  )



# 13. COLORS FOR THE TWO TEAMS -------------------------------------------------


teams_in_match <- players_anim_final |>
  distinct(team_abbr) |>
  pull(team_abbr)

# CHANGE HERE: first and second team colors
team_colors <- c(
  "CLT" = "#2F80ED",  # Charlotte blue
  "NSH" = "#F2994A"   # Nashville orange
)



# 12. CREATE ANIMATION ---------------------------------------------------------


anim_full_action <- draw_skillcorner_pitch(
  pitch_length = pitch_length,
  pitch_width = pitch_width,
  zoom = "left"   # CHANGE HERE if you want "right" or full
) +
  geom_point(
    data = players_anim_final,
    aes(
      x = player_x,
      y = player_y,
      color = team_abbr
    ),
    size = 4
  ) +
  geom_point(
    data = ball_anim_final,
    aes(
      x = ball_x,
      y = ball_y
    ),
    color = "white",
    size = 2.2
  ) +
  scale_color_manual(
    values = team_colors
  ) +
  labs(
    color = "Team"
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold")
  ) +
  transition_manual(frame)

anim_full_action



# 13. RENDER ANIMATION ---------------------------------------------------------

animation_fast <- animate(
  anim_full_action,
  
  # CHANGE HERE: higher fps makes the action faster
  fps = 10,
  
  # CHANGE HERE: total animation duration
  duration = 8,

  width = 1280,
  height = 720,
  end_pause = 24,
  renderer = gifski_renderer(
    loop = TRUE
  )
)

animation_fast



# 14. SAVE ANIMATION------------------------------------------------------------

anim_save(
  "game_moment.gif",
  animation = animation_fast)