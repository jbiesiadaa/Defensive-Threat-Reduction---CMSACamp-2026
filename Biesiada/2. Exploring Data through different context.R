# 2. Exploring Data through different context
# Julia Biesiada


library(tidyverse)
library(ggplot2)

# folder where your event CSV files are
folder <- "mls_skillcorner/dynamic_events"

# find all event csv files
files <- list.files(
  path = folder,
  pattern = "_events\\.csv$",
  full.names = TRUE
)

# keep only first 10 games
files_10 <- files[1:10]

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


# Checking if everything is good
dim(dynamic_10)
unique(dynamic_10$game_id)
table(dynamic_10$event_type)

## 2. Rewriting it 
events <- dynamic_10


# ------------------------------------------------------------------------------
## Chosen pass
## 3. When the player had the ball and passed, what kind of pass did they choose, and how dangerous was that pass?
## What pass did the player actually choose? 

actual_pass_eda <- events |>
  filter(event_type == "player_possession") |>
  filter(!is.na(targeted_passing_option_event_id)) |> # This keeps only player possessions where the player attempted a pass
  select(
    player_targeted_xthreat, # How dangerous the chosen pass was
    player_targeted_xpass_completion, # How likely the chosen pass was to be completed
    pass_direction, # direction of the pass
    pass_distance, # distance of the pass
    pass_range, # category of pass distance
    quick_pass, # TRUE if the possession lasts less than 1 second and ends with a pass but the player possession was not a one-touch. Else FALSE.
    one_touch, # TRUE if the player possession consisted of just one touch.Else FALSE.
    separation_start, # Distance (in metres) from the closest opponent at the start of event
    pass_ahead)
  
# Summary
summary(actual_pass_eda)

# Type of passes -> Direction 
# What pass directions happen most often?
actual_pass_eda |>
  count(pass_direction, sort = TRUE)

# Pass Range -> short,medium
# Are most passes short, medium, or long?
actual_pass_eda |>
  count(pass_range, sort = TRUE)

# Avg threat by pass direction:
# Which pass direction is most dangerous on average?
actual_pass_eda |>
  group_by(pass_direction) |>
  summarise(
    count = n(),
    avg_xthreat = mean(player_targeted_xthreat, na.rm = TRUE),
    avg_xpass = mean(player_targeted_xpass_completion, na.rm = TRUE) * 100, # Pass completion probability if the player attempted a pass at the end of his possession
    avg_distance = mean(pass_distance, na.rm = TRUE),
    avg_separation = mean(separation_start, na.rm = TRUE)
  )


# Avg threat by pass range
# Are short, medium, or long passes more dangerous?
actual_pass_eda |>
  group_by(pass_range) |>
  summarise(
    count = n(),
    avg_xthreat = mean(player_targeted_xthreat, na.rm = TRUE),
    avg_xpass = mean(player_targeted_xpass_completion, na.rm = TRUE) * 100,
    avg_separation = mean(separation_start, na.rm = TRUE)
  )

# Simple Plot
hist(
  actual_pass_eda$player_targeted_xthreat,
  main = "Actual Pass xThreat",
  xlab = "xThreat of chosen pass"
)

# Most passes have low xThreat, and only a few passes are very dangerous.
# Because the data is the right skewed by creating a category with quantile
# Is this pass in the top 25 % of dangeorus passes?

#  Was this pass in the top 25 percent of dangerous passes?
actual_pass_eda <- actual_pass_eda |>
  mutate(
    high_xthreat = player_targeted_xthreat >= quantile(
      player_targeted_xthreat,
      0.75,
      na.rm = TRUE
    )
  )

# Explore Quick Pass
# Are quick passes more or less dangerous than non quick passes?
actual_pass_eda |>
  group_by(quick_pass) |>
  summarise(
    count = n(),
    avg_xthreat = mean(player_targeted_xthreat, na.rm = TRUE),
    avg_xpass = mean(player_targeted_xpass_completion, na.rm = TRUE) * 100,
    avg_separation = mean(separation_start, na.rm = TRUE)
  )


# Explore Pass Ahead
actual_pass_eda |>
  group_by(pass_ahead) |>
  summarise(
    count = n(),
    avg_xthreat = mean(player_targeted_xthreat, na.rm = TRUE),
    avg_xpass = mean(player_targeted_xpass_completion, na.rm = TRUE) * 100,
    avg_separation = mean(separation_start, na.rm = TRUE)
  )

# Explore One Touch 
# Are one touch passes more rushed, harder, or more dangerous?
actual_pass_eda |>
  group_by(one_touch) |>
  summarise(
    count = n(),
    avg_xthreat = mean(player_targeted_xthreat, na.rm = TRUE),
    avg_xpass = mean(player_targeted_xpass_completion, na.rm = TRUE) * 100,
    avg_separation = mean(separation_start, na.rm = TRUE)
  )


# Explore Separation at Start 
actual_pass_eda |>
  summarise(
    avg_separation = mean(separation_start, na.rm = TRUE),
    min_separation = min(separation_start, na.rm = TRUE),
    max_separation = max(separation_start, na.rm = TRUE)
  )

# How much space that they have when they begin

# Creating a Simple groups for space separation
actual_pass_eda <- actual_pass_eda |>
  mutate(
    separation_group = case_when(
      separation_start < 3 ~ "tight pressure",
      separation_start < 7 ~ "moderate pressure",
      separation_start >= 7 ~ "more space",
      TRUE ~ "missing"
    )
  )

# Comparing what was the avg_xthreat and avg_xpass in this space context

actual_pass_eda |>
  group_by(separation_group) |>
  summarise(
    count = n(),
    avg_xthreat = mean(player_targeted_xthreat, na.rm = TRUE),
    avg_xpass = mean(player_targeted_xpass_completion, na.rm = TRUE) * 100
  )

### Summary: 
# I filtered the data to only Player Possession events where the player attempted a pass, 
# then explored what kind of pass they chose and how dangerous that pass was.

# I looked at:

# pass danger: player_targeted_xthreat -> tells how dangerous the chosen pass was 
# pass difficulty: player_targeted_xpass_completion -> tells how likely the pass was to be completed
# pass type: pass_direction, pass_distance, and pass_range describe whether the pass was forward, backward, short, medium, long, etc.
# decision speed: quick_pass and one_touch describe whether the player made the pass quickly or with only one touch
# Space/pressure context: separation_start tells how much space the player had from the closest opponent at the start of the possession

# ------------------------------------------------------------------------------
## 4. What options were available? 
# Player Possession gives the actual pass chosen. Passing Option gives the full list of possible options.
# Before the player passed, what passing options were available, and did the player choose a dangerous or line breaking option?
# What could they have done?

# SkillCorner says every Passing Option is linked to one Player Possession,
# and if the player passes, the Player Possession row also stores which Passing Option was targeted

available_options_eda <- events |>
  filter(event_type == "player_possession") |>
  filter(!is.na(targeted_passing_option_event_id)) |> # One row equals one player possession where the player passed
  select(
    # ID
    match_id,
    event_id,
    targeted_passing_option_event_id, # This tells us which option became the actual pass
    
    # available options summary
    n_passing_options, # Total number of passing options
    n_passing_options_dangerous_difficult,  # Number of options that were dangerous + hard
    n_passing_options_dangerous_not_difficult, # Number of options that were dangerous + easy
    n_passing_options_not_dangerous_not_difficult, # Number of options that were not dangerous + easy
    n_passing_options_not_dangerous_difficult, # Number of options that were not dangerous + hard
    n_passing_options_first_line_break, # Number of options that could break the first defensive line
    n_passing_options_second_last_line_break, #  Number of options that could break the second defensive line
    n_passing_options_last_line_break, # Number of options that could break the last defensive line
    n_passing_options_ahead, # Number of options ahead of the ball carrier
    
    # options early or late in the possession
    n_passing_options_at_start,
    n_passing_options_at_end,
    n_passing_options_ahead_at_start,
    n_passing_options_ahead_at_end,
    
    # chosen pass details
    interplayer_distance, # How far the targeted player was from the passer
    interplayer_angle, # The angle of the pass relative to the direction of attack
    player_targeted_xthreat, # How dangerous the chosen pass was
    player_targeted_xpass_completion, # How likely the chosen pass was to be completed
    player_targeted_difficult_pass_target, # Whether the chosen target was difficult.
    
    # chosen pass line break
    first_line_break,
    second_last_line_break,
    last_line_break
  )


## 4.1 How many options did players usually have?
available_options_eda |>
  summarise(
    count = n(),
    avg_options = mean(n_passing_options, na.rm = TRUE),
    avg_dangerous_and_easy_options = mean(n_passing_options_dangerous_not_difficult, na.rm = TRUE),
    avg_dangerous_and_hard_options = mean(n_passing_options_dangerous_difficult, na.rm = TRUE),
    avg_not_dangerous_and_easy_options = mean(n_passing_options_not_dangerous_not_difficult, na.rm = TRUE),
    avg_not_dangerous_and_hard_options = mean(n_passing_options_not_dangerous_difficult, na.rm = TRUE),
    avg_second_line_break_options = mean(n_passing_options_second_last_line_break, na.rm = TRUE),
    avg_last_line_break_options = mean(n_passing_options_last_line_break, na.rm = TRUE),
    avg_options_ahead = mean(n_passing_options_ahead, na.rm = TRUE)
  )

## 4.2 What type of option player had 

# Did the player have any dangerous option?
# Did the player have a dangerous easy option?
# Did the player have a dangerous hard option?


available_options_eda <- available_options_eda |>
  mutate(
    had_dangerous_option =
      n_passing_options_dangerous_not_difficult > 0 |
      n_passing_options_dangerous_difficult > 0,
    
    had_dangerous_easy_option =
      n_passing_options_dangerous_not_difficult > 0,
    
    had_dangerous_hard_option =
      n_passing_options_dangerous_difficult > 0
  )

## 4.3 If they had a dangerous option, was the chosen pass more dangerous?

# a) Any dangerous option 
# If the player had any dangerous option, was the chosen pass more dangerous?

available_options_eda |>
  group_by(had_dangerous_option) |>
  summarise(
    count = n(),
    avg_chosen_xthreat = mean(player_targeted_xthreat, na.rm = TRUE),
    avg_chosen_xpass = mean(player_targeted_xpass_completion, na.rm = TRUE) * 100,
    avg_target_distance = mean(interplayer_distance, na.rm = TRUE)
  )

# When the player had at least one dangerous option available, the pass they chose was much more dangerous, but much harder to complete
# More threat, lower completion probability

# b) Dangerous and easy option! 
# If the player had a dangerous but realistic option, was the chosen pass more dangerous?
available_options_eda |>
  group_by(had_dangerous_easy_option) |>
  summarise(
    count = n(),
    avg_chosen_xthreat = mean(player_targeted_xthreat, na.rm = TRUE),
    avg_chosen_xpass = mean(player_targeted_xpass_completion, na.rm = TRUE) * 100,
    avg_target_distance = mean(interplayer_distance, na.rm = TRUE)
  )

# When the player had a dangerous but realistic option, the chosen pass was more threatening and still fairly completable
# the best attacking opportunities
# The player had a dangerous option that was actually realistic to play.

# c) Dangerous but hard option
available_options_eda |>
  group_by(had_dangerous_hard_option) |>
  summarise(
    count = n(),
    avg_chosen_xthreat = mean(player_targeted_xthreat, na.rm = TRUE),
    avg_chosen_xpass = mean(player_targeted_xpass_completion, na.rm = TRUE) * 100,
    avg_target_distance = mean(interplayer_distance, na.rm = TRUE)
  )

# When the player had a dangerous but difficult option, the chosen pass was the most threatening, but also much harder to complete
# high reward, low completion probability

# MAIN TAKEWAY: 
# The average target distance is around 14 to 17 meters. It does not change as much as xThreat or xPass.
# So the main difference is not just distance. 
# The danger probably comes from where the target is, whether the pass breaks lines, and whether it moves the ball into dangerous space



# Visual
danger_easy_graph <- available_options_eda |>
  group_by(had_dangerous_easy_option) |>
  summarise(
    count = n(),
    avg_chosen_xthreat = mean(player_targeted_xthreat, na.rm = TRUE),
    avg_chosen_xpass = mean(player_targeted_xpass_completion, na.rm = TRUE) * 100,
    avg_target_distance = mean(interplayer_distance, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    dangerous_easy_option = ifelse(
      had_dangerous_easy_option,
      "Dangerous easy option available",
      "No dangerous easy option"
    )
  )

ggplot(
  danger_easy_graph,
  aes(x = dangerous_easy_option, y = avg_chosen_xthreat)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = round(avg_chosen_xthreat, 3)),
    vjust = -0.4,
    size = 4
  ) +
  labs(
    title = "Chosen Pass Threat When a Dangerous Easy Option Was Available",
    subtitle = "Passes became more threatening when the ball carrier had a realistic dangerous option",
    x = "",
    y = "Average chosen pass xThreat"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(size = 10)
  )


# Total number of dangerous easy vs dangerous hard options
available_options_eda |>
summarise(
  total_dangerous_easy_options = sum(n_passing_options_dangerous_not_difficult, na.rm = TRUE),
  total_dangerous_hard_options = sum(n_passing_options_dangerous_difficult, na.rm = TRUE)
)


available_options_eda |>
  summarise(
    possessions_with_dangerous_easy = sum(n_passing_options_dangerous_not_difficult > 0, na.rm = TRUE),
    possessions_with_dangerous_hard = sum(n_passing_options_dangerous_difficult > 0, na.rm = TRUE),
    total_pass_possessions = n()
  )

# How often did the player have at least one dangerous easy option or dangerous hard option?

## 4.4 If the player had a last line breaking option,
## did they actually choose a line breaking pass?

available_options_eda <- available_options_eda |>
  mutate(
    
    # TRUE if the ball carrier had at least one available passing option
    # that could break the opponent's last defensive line
    had_last_line_break_option =
      n_passing_options_last_line_break > 0,
    
    # TRUE if the ball carrier had at least one option that could break
    # any defensive line: first line, second last line, or last line
    had_any_line_break_option =
      n_passing_options_first_line_break > 0 |
      n_passing_options_second_last_line_break > 0 |
      n_passing_options_last_line_break > 0,
    
    # TRUE if the pass the player actually chose broke at least one line
    chosen_line_break =
      first_line_break == "True" |
      second_last_line_break == "True" |
      last_line_break == "True"
  )

# Did the player actually choose a pass that broke a defensive line?

available_options_eda |>
  group_by(had_last_line_break_option) |>
  summarise(
    count = n(),
    avg_chosen_xthreat = mean(player_targeted_xthreat, na.rm = TRUE),
    avg_chosen_xpass = mean(player_targeted_xpass_completion, na.rm = TRUE) * 100,
    pct_chosen_line_break = mean(chosen_line_break, na.rm = TRUE) * 100
  )

# Interpretation: When the ball carrier had a pass available that could break the last defensive line, they were much more likely to choose a line breaking pass. 
# These passes created more threat, but they were also riskier and harder to complete.

# Any line breaking option available?
available_options_eda |>
  group_by(had_any_line_break_option) |>
  summarise(
    count = n(),
    avg_chosen_xthreat = mean(player_targeted_xthreat, na.rm = TRUE),
    avg_chosen_xpass = mean(player_targeted_xpass_completion, na.rm = TRUE) * 100,
    pct_chosen_line_break = mean(chosen_line_break, na.rm = TRUE) * 100
  )


# Interpretation:
# When there was no line breaking option available, the player never chose a line breaking pass. That makes sense.
# When there was at least one line breaking option available, the player chose a line breaking pass 47% of the time.


# ------------------------------------------------------------------------------
## 5.1 Defensive organization context
## Goal: check whether the defense was organized,
## and how that affected the chosen pass threat and line breaking.


to_bool <- function(x) {
  case_when(
    x %in% c(TRUE, "TRUE", "True", "true") ~ TRUE,
    x %in% c(FALSE, "FALSE", "False", "false") ~ FALSE,
    TRUE ~ NA
  )
}

defense_eda <- events |>
  filter(event_type == "player_possession") |>
  filter(!is.na(targeted_passing_option_event_id)) |>
  select(
    match_id,
    event_id,
    targeted_passing_option_event_id,
    
    # Main defensive organization variables
    organised_defense,
    defensive_structure,
    n_defensive_lines,
    
    # Is the player inside the opponent defensive shape?
    inside_defensive_shape_start,
    inside_defensive_shape_end,
    
    # Last defensive line context
    last_defensive_line_height_start,
    last_defensive_line_height_end,
    last_defensive_line_height_gain,
    delta_to_last_defensive_line_start,
    delta_to_last_defensive_line_end,
    delta_to_last_defensive_line_gain,
    
    # Opponent pressure around the target
    separation_start,
    separation_end,
    n_player_targeted_opponents_within_5m_start,
    n_player_targeted_opponents_within_5m_end,
    n_player_targeted_opponents_ahead_start,
    n_player_targeted_opponents_ahead_end,
    
    # Chosen pass outcome
    player_targeted_xthreat,
    player_targeted_xpass_completion,
    
    # Line break outcome
    first_line_break,
    second_last_line_break,
    last_line_break
  ) |>
  mutate(
    across(
      c(
        organised_defense,
        inside_defensive_shape_start,
        inside_defensive_shape_end,
        first_line_break,
        second_last_line_break,
        last_line_break
      ),
      to_bool
    ),
    
    # Did the chosen pass break any defensive line?
    # coalesce changes missing values to FALSE inside this calculation
    chosen_line_break =
      coalesce(first_line_break, FALSE) |
      coalesce(second_last_line_break, FALSE) |
      coalesce(last_line_break, FALSE),
    
    # Was the target already behind the last defensive line?
    target_behind_last_line_start =
      delta_to_last_defensive_line_start < 0,
    
    # Was the target closely defended?
    target_close_to_opponent_start =
      separation_start <= 5,
    
    # Was there at least one opponent within 5m of the target?
    target_has_opponent_within_5m_start =
      n_player_targeted_opponents_within_5m_start > 0
  )

## 5.2 When the defense was organized, were chosen passes less threatening?
defense_eda |>
  group_by(organised_defense) |>
  summarise(
    count = n(),
    avg_chosen_xthreat = mean(player_targeted_xthreat, na.rm = TRUE),
    avg_chosen_xpass = mean(player_targeted_xpass_completion, na.rm = TRUE) * 100,
    avg_last_line_height = mean(last_defensive_line_height_start, na.rm = TRUE),
    avg_separation = mean(separation_start, na.rm = TRUE),
    pct_inside_shape = mean(inside_defensive_shape_start, na.rm = TRUE) * 100,
    pct_chosen_line_break = mean(chosen_line_break, na.rm = TRUE) * 100
  )

# When the defense was organized, the chosen passes were much less threatening.
# The average chosen xThreat dropped from 0.0179 to 0.00451.
# At the same time, pass completion probability increased from 77.8% to 90.6%.
# This suggests that organized defense may reduce attacking threat by forcing players
# into safer, less dangerous passes instead of high threat passes.




### ----------------------------------------------------------------------------------------------------
## 6 On Ball Engagement (Defender)




### Extra: Understatement of Player Possesion and Player Options Relationship-------------------------
# Step 1: Split the data
player_possession <- events |>
  filter(event_type == "player_possession")

passing_options <- events |>
  filter(event_type == "passing_option")

# Step 2: Count how many passing options each possession had
options_per_possession <- passing_options |>
  group_by(match_id, associated_player_possession_event_id) |>
  summarise(
    n_options_from_passing_option_rows = n(),
    .groups = "drop"
  )

options_per_possession


# Step 3: Join those options back to Player Possession

check_connection <- player_possession |>
  filter(!is.na(targeted_passing_option_event_id)) |>
  select(
    match_id,
    event_id,
    targeted_passing_option_event_id,
    n_passing_options
  ) |>
  left_join(
    options_per_possession,
    by = c(
      "match_id",
      "event_id" = "associated_player_possession_event_id"
    )
  )

# View
View(check_connection)

# Table Shows:
# event_id = the player possession
# targeted_passing_option_event_id = the option they chose
# n_passing_options = SkillCorner summary of options
# n_options_from_passing_option_rows = options we counted manually


# ONE EXAMPLE Possession

one_example <- check_connection |>
  filter(n_options_from_passing_option_rows > 1) |>
  slice(1)

example_match <- one_example$match_id
example_possession <- one_example$event_id
chosen_option <- one_example$targeted_passing_option_event_id

# Now show all options for that one possession:

passing_options |>
  filter(
    match_id == example_match,
    associated_player_possession_event_id == example_possession
  ) |>
  mutate(
    chosen_option = as.character(event_id) == as.character(chosen_option)
  ) |>
  select(
    event_id,
    associated_player_possession_event_id,
    chosen_option,
    player_name,
    xthreat,
    xpass_completion,
    dangerous,
    difficult_pass_target,
    passing_option_score
  ) |>
  arrange(desc(chosen_option), desc(xthreat))

# associated_player_possession_event_id -> Those are all options available during one ball possession.
# One row should have:  chosen_option == TRUE -> That is the pass the player actually chose.
# Connection: player_possession$event_id = passing_options$associated_player_possession_event_id
# Passing Option rows are more detailed because they describe each possible option, not just the final chosen pass

# Project: 
# Player Possession tells you the actual decision.
# Passing Options tell you the alternatives.





