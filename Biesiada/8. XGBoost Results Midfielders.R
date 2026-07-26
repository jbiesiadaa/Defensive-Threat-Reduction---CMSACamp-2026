# 8. XGBoost Results Midfielders
# Julia Biesiada
# Midfielders Decisions (role and players)

library(tidyverse)
library(gt)
library(tidytext)


# MIDFIELDER DECISIONS ABOVE/BELOW EXPECTED — SPLIT BY ROLE --------------------

# Settings
MIN_PASSES  <- 200
MIN_MATCHES <- 5

# Load saved results
bundle <- readRDS("PTR_model_bundle.rds")

scored_possessions <- bundle$scored_possessions
analysis_clean <- readRDS("ptr_analysis_clean_502games.rds")


# 1. Player-name lookup --------------------------------------------------------

player_names <- analysis_clean |>
  transmute(
    player_id = as.character(player_id),
    player_name,
  ) |>
  filter(!is.na(player_name)) |>
  distinct(player_id, .keep_all = TRUE)


# 2. Midfield possessions with role --------------------------------------------

# Role is assigned per POSSESSION, not per player:
# a player who plays both roles is evaluated within each role separately

midfield_possessions <- scored_possessions |>
  mutate(player_id = as.character(player_id),
         team_id = as.character(team_id)) |>
  filter(
    carrier_position %in% c(
      "Defensive Midfield",
      "Left Defensive Midfield",
      "Right Defensive Midfield",
      "Center Midfield",
      "Attacking Midfield"
    )
  ) |>
  mutate(
    mid_role = if_else(
      carrier_position %in% c(
        "Defensive Midfield",
        "Left Defensive Midfield",
        "Right Defensive Midfield"
      ),
      "Defensive Midfielders",
      "Attacking / Central Midfielders"
    )
  )

# 3. ROLE-LEVEL COMPARISON TABLE


role_comparison <- midfield_possessions |>
  group_by(mid_role) |>
  summarise(
    n_possessions = n(),
    n_players = n_distinct(player_id),
    
    # Rate of lower-threat decisions
    actual_rate = mean(PTR_binary, na.rm = TRUE),
    expected_rate = mean(expected_probability, na.rm = TRUE),
    
    # Positive = selected the best option more often than expected
    decision_above_expected = expected_rate - actual_rate,
    
    .groups = "drop"
  )

role_comparison_table <- role_comparison |>
  gt() |>
  tab_header(
    title = md("**Passing Decisions by Midfield Role**"),
    subtitle = "Actual versus model-expected lower-threat decision rates"
  ) |>
  cols_label(
    mid_role = "Midfield Role",
    n_possessions = "Possessions",
    n_players = "Players",
    actual_rate = "Actual Lower-Threat Rate",
    expected_rate = "Expected Lower-Threat Rate",
    decision_above_expected = "Best-Option Selection Above Expected"
  ) |>
  fmt_number(
    columns = c(n_possessions, n_players),
    decimals = 0
  ) |>
  fmt_percent(
    columns = c(
      actual_rate,
      expected_rate,
      decision_above_expected
    ),
    decimals = 1
  ) |>
  data_color(
    columns = decision_above_expected,
    palette = c("#B23A48", "white", "#2E7D32"),
    domain = c(
      -max(abs(role_comparison$decision_above_expected)),
      max(abs(role_comparison$decision_above_expected))
    )
  ) |>
  tab_source_note(
    source_note = md(
      "**Positive values** indicate that the highest-threat realistic option was selected more often than expected."
    )
  ) |>
  tab_options(
    column_labels.background.color = "#F1F1F1",
    column_labels.font.weight = "bold",
    data_row.padding = px(10),
    table.font.size = px(13)
  )

role_comparison_table


# 4. PLAYER-LEVEL RESULTS WITHIN EACH ROLE -------------------------------------


midfielder_ptr <- midfield_possessions |>
  group_by(player_id,team_id, mid_role) |>
  summarise(
    n_possessions = n(),
    n_matches = n_distinct(match_id),
    
    actual_rate = mean(PTR_binary, na.rm = TRUE),
    expected_rate = mean(expected_probability, na.rm = TRUE),
    
    # Positive = best option selected more often than expected
    decision_above_expected = expected_rate - actual_rate,
    
    # Approximate uncertainty using each possession's predicted probability
    se = sqrt(
      sum(
        expected_probability *
          (1 - expected_probability),
        na.rm = TRUE
      )
    ) / n_possessions,
    
    .groups = "drop"
  ) |>
  left_join(player_names, by = "player_id") |>
  filter(
    n_possessions >= MIN_PASSES,
    n_matches >= MIN_MATCHES,
    !is.na(player_name)
  )



# 5. FUNCTION TO SELECT TOP/BOTTOM 3 AND CREATE A PLOT -------------------------

make_role_plot <- function(role_name, plot_title) {
  
  role_data <- midfielder_ptr |>
    filter(mid_role == role_name)
  
  plot_data <- bind_rows(
    role_data |>
      slice_max(
        decision_above_expected,
        n = 3,
        with_ties = FALSE
      ),
    
    role_data |>
      slice_min(
        decision_above_expected,
        n = 3,
        with_ties = FALSE
      )
  ) |>
    distinct(player_id, .keep_all = TRUE) |>
    mutate(
      group = if_else(
        decision_above_expected >= 0,
        "Above expected",
        "Below expected"
      ),
      
      player_name = forcats::fct_reorder(
        player_name,
        decision_above_expected
      )
    )
  
  ggplot(
    plot_data,
    aes(
      x = decision_above_expected,
      y = player_name,
      fill = group
    )
  ) +
    geom_col(width = 0.65) +
    
    # Zero represents performance exactly as expected
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.6,
      color = "grey40"
    ) +
    
    # Add percentage labels
    geom_text(
      aes(
        label = scales::percent(
          decision_above_expected,
          accuracy = 0.1
        ),
        hjust = if_else(
          decision_above_expected >= 0,
          -0.35,
          1.35
        )
      ),
      size = 4,
      fontface = "bold"
    ) +
    
    scale_fill_manual(
      values = c(
        "Above expected" = "#2E7D32",
        "Below expected" = "#B23A48"
      )
    ) +
    
    scale_x_continuous(
      labels = scales::label_percent(accuracy = 1),
      expand = expansion(mult = c(0.25, 0.25))
    ) +
    
    coord_cartesian(
      xlim = c(-0.20, 0.20),
      clip = "off"
    ) +
    
    labs(
      title = plot_title,
      subtitle = paste0(
        "Top and bottom 3 | minimum ",
        MIN_PASSES,
        " possessions and ",
        MIN_MATCHES,
        " matches"
      ),
      x = "Best-option selection above expected",
      y = NULL,
      fill = NULL,
      caption =
        "Positive = highest-threat option selected more often than expected."
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      
      # Center title and subtitle
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        hjust = 0.5
      ),
      
      plot.title.position = "plot",
      plot.margin = margin(10, 45, 10, 10)
    )
}

# 6. CREATE THE TWO SEPARATE PLOTS ---------------------------------------------


attacking_midfield_plot <- make_role_plot(
  role_name = "Attacking / Central Midfielders",
  plot_title = "Attacking and Central Midfield Decisions"
)

defensive_midfield_plot <- make_role_plot(
  role_name = "Defensive Midfielders",
  plot_title = "Defensive Midfield Decisions"
)

attacking_midfield_plot
defensive_midfield_plot



# 7. SAVE RESULTS --------------------------------------------------------------

saveRDS(
  midfielder_ptr,
  "PTR_midfielder_by_role.rds"
)

saveRDS(
  role_comparison,
  "PTR_midfield_role_comparison.rds"
)

ggsave(
  "attacking_central_midfielders.png",
  attacking_midfield_plot,
  width = 9,
  height = 5.5,
  dpi = 300
)

ggsave(
  "defensive_midfielders.png",
  defensive_midfield_plot,
  width = 9,
  height = 5.5,
  dpi = 300
)

gtsave(
  role_comparison_table,
  "midfield_role_comparison.png"
)


# Creating a Table -------------------------------------------------------------


library(tidyverse)
library(gt)
library(scales)
library(purrr)

# MIDFIELDER DECISIONS ABOVE/BELOW EXPECTED — SPLIT BY ROLE --------------------

library(tidyverse)
library(gt)

# Settings
MIN_PASSES  <- 200
MIN_MATCHES <- 5


# 0. LOAD SAVED RESULTS --------------------------------------------------------

bundle <- readRDS("PTR_model_bundle.rds")

scored_possessions <- bundle$scored_possessions
analysis_clean <- readRDS("ptr_analysis_clean_502games.rds")


# 1. TEAM ABBREVIATION LOOKUP --------------------------------------------------

team_lookup <- tribble(
  ~team_id, ~team_abbr,
  "1495",   "LAG",
  "1508",   "ATL",
  "1498",   "SJE",
  "1504",   "VAN",
  "1503",   "PHI",
  "863",    "CLB",
  "1501",   "RSL",
  "1507",   "NSH",
  "1506",   "NYC",
  "2312",   "CLT",
  "1500",   "NER",
  "884",    "DCU",
  "1502",   "TOR",
  "885",    "CIN",
  "862",    "HOU",
  "1494",   "MIA",
  "1505",   "MTL",
  "337",    "ORL",
  "883",    "NYR",
  "1757",   "ATX",
  "2906",   "STL",
  "336",    "DAL",
  "861",    "MIN",
  "919",    "SEA",
  "1499",   "COL",
  "860",    "POR",
  "1497",   "SKC"
)


# 2. PLAYER-NAME LOOKUP --------------------------------------------------------

player_names <- analysis_clean |>
  transmute(
    player_id = as.character(player_id),
    player_name = stringr::str_squish(player_name)
  ) |>
  filter(
    !is.na(player_name),
    player_name != ""
  ) |>
  distinct(player_id, .keep_all = TRUE)


# 3. MIDFIELD POSSESSIONS WITH ROLE --------------------------------------------

# Role is assigned per possession.
# Players who appear in multiple roles are evaluated separately within each role.

midfield_possessions <- scored_possessions |>
  mutate(
    player_id = as.character(player_id),
    team_id = as.character(team_id)
  ) |>
  filter(
    carrier_position %in% c(
      "Defensive Midfield",
      "Left Defensive Midfield",
      "Right Defensive Midfield",
      "Center Midfield",
      "Attacking Midfield"
    )
  ) |>
  mutate(
    mid_role = if_else(
      carrier_position %in% c(
        "Defensive Midfield",
        "Left Defensive Midfield",
        "Right Defensive Midfield"
      ),
      "Defensive Midfielders",
      "Attacking / Central Midfielders"
    )
  )


# 4. ROLE-LEVEL COMPARISON TABLE -----------------------------------------------

role_comparison <- midfield_possessions |>
  group_by(mid_role) |>
  summarise(
    n_possessions = n(),
    n_players = n_distinct(player_id),
    
    # Rate of lower-threat decisions
    actual_rate = mean(
      PTR_binary,
      na.rm = TRUE
    ),
    
    expected_rate = mean(
      expected_probability,
      na.rm = TRUE
    ),
    
    # Positive = highest-threat option selected more often than expected
    decision_above_expected =
      expected_rate - actual_rate,
    
    .groups = "drop"
  )


role_comparison_table <- role_comparison |>
  gt() |>
  
  tab_header(
    title = md("**Passing Decisions by Midfield Role**"),
    subtitle = "Actual versus model-expected lower-threat decision rates"
  ) |>
  
  cols_label(
    mid_role = "Midfield Role",
    n_possessions = "Possessions",
    n_players = "Players",
    actual_rate = "Actual Lower-Threat Rate",
    expected_rate = "Expected Lower-Threat Rate",
    decision_above_expected =
      "Best-Option Selection Above Expected"
  ) |>
  
  fmt_number(
    columns = c(
      n_possessions,
      n_players
    ),
    decimals = 0
  ) |>
  
  fmt_percent(
    columns = c(
      actual_rate,
      expected_rate,
      decision_above_expected
    ),
    decimals = 1
  ) |>
  
  data_color(
    columns = decision_above_expected,
    palette = c(
      "#B23A48",  # negative values: red
      "white",    # zero: white
      "#2E7D32"   # positive values: green
    ),
    domain = c(-max(abs(role_comparison$decision_above_expected)), 
      max(abs(role_comparison$decision_above_expected))) # making it equal depending on the situation 
  ) |>
  
  tab_source_note(
    source_note = md(
      paste0(
        "**Positive values** indicate that the highest-threat ",
        "realistic option was selected more often than expected."
      )
    )
  ) |>
  
  tab_options(
    column_labels.background.color = "#F1F1F1",
    column_labels.font.weight = "bold",
    data_row.padding = px(10),
    table.font.size = px(13)
  )

role_comparison_table


# 5. PLAYER-LEVEL RESULTS WITHIN EACH ROLE -------------------------------------

midfielder_ptr <- midfield_possessions |>
  group_by(
    player_id,
    team_id,
    mid_role
  ) |>
  summarise(
    n_possessions = n(),
    n_matches = n_distinct(match_id),
    
    actual_rate = mean(
      PTR_binary,
      na.rm = TRUE
    ),
    
    expected_rate = mean(
      expected_probability,
      na.rm = TRUE
    ),
    
    # Positive = best option selected more often than expected
    decision_above_expected =
      expected_rate - actual_rate,
    
    # Approximate uncertainty
    se = sqrt(
      sum(
        expected_probability *
          (1 - expected_probability),
        na.rm = TRUE
      )
    ) / n_possessions,
    
    .groups = "drop"
  ) |>
  
  left_join(
    player_names,
    by = "player_id"
  ) |>
  
  left_join(
    team_lookup,
    by = "team_id"
  ) |>
  
  mutate(
    # Use team ID if abbreviation is missing
    team_abbr = coalesce(
      team_abbr,
      team_id
    ),
    
    # Label used in plots
    player_team_label = paste0(
      player_name,
      " (",
      team_abbr,
      ")"
    )
  ) |>
  
  filter(
    n_possessions >= MIN_PASSES,
    n_matches >= MIN_MATCHES,
    !is.na(player_name)
  )


# Check results
midfielder_ptr |>
  select(
    player_id,
    player_name,
    team_id,
    team_abbr,
    mid_role,
    n_possessions,
    decision_above_expected
  ) |>
  arrange(
    mid_role,
    desc(decision_above_expected)
  )


# 6. FUNCTION TO SELECT TOP/BOTTOM 3 AND CREATE PLOT ---------------------------

make_role_plot <- function(role_name, plot_title) {
  
  role_data <- midfielder_ptr |>
    filter(mid_role == role_name)
  
  plot_data <- bind_rows(
    
    role_data |>
      slice_max(
        decision_above_expected,
        n = 3,
        with_ties = FALSE
      ),
    
    role_data |>
      slice_min(
        decision_above_expected,
        n = 3,
        with_ties = FALSE
      )
    
  ) |>
    distinct(
      player_id,
      team_id,
      .keep_all = TRUE
    ) |>
    mutate(
      group = if_else(
        decision_above_expected >= 0,
        "Above expected",
        "Below expected"
      ),
      
      player_team_label = forcats::fct_reorder(
        player_team_label,
        decision_above_expected
      )
    )
  
  ggplot(
    plot_data,
    aes(
      x = decision_above_expected,
      y = player_team_label,
      fill = group
    )
  ) +
    
    geom_col(
      width = 0.65
    ) +
    
    # Zero = exactly as expected
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.6,
      color = "grey40"
    ) +
    
    geom_text(
      aes(
        label = scales::percent(
          decision_above_expected,
          accuracy = 0.1
        ),
        
        hjust = if_else(
          decision_above_expected >= 0,
          -0.35,
          1.35
        )
      ),
      size = 4,
      fontface = "bold"
    ) +
    
    scale_fill_manual(
      values = c(
        "Above expected" = "#2E7D32",
        "Below expected" = "#B23A48"
      )
    ) +
    
    scale_x_continuous(
      labels = scales::label_percent(
        accuracy = 1
      ),
      expand = expansion(
        mult = c(0.25, 0.25)
      )
    ) +
    
    coord_cartesian(
      xlim = c(-0.20, 0.20),
      clip = "off"
    ) +
    
    labs(
      title = plot_title,
      
      subtitle = paste0(
        "Top and bottom 3 | minimum ",
        MIN_PASSES,
        " possessions and ",
        MIN_MATCHES,
        " matches"
      ),
      
      x = "Best-option selection above expected",
      y = NULL,
      fill = NULL,
      
      caption = paste0(
        "Positive = highest-threat realistic option ",
        "selected more often than expected."
      )
    ) +
    
    theme_minimal(
      base_size = 13
    ) +
    
    theme(
      legend.position = "top",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.text.y = element_text(
        face = "bold"
      ),
      
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      ),
      
      plot.subtitle = element_text(
        hjust = 0.5
      ),
      
      plot.title.position = "plot",
      
      plot.margin = margin(
        10,
        45,
        10,
        10
      )
    )
}


# 7. CREATE THE TWO ROLE PLOTS --------------------------------------------------

attacking_midfield_plot <- make_role_plot(
  role_name = "Attacking / Central Midfielders",
  plot_title = "Attacking and Central Midfielder Decisions"
)

defensive_midfield_plot <- make_role_plot(
  role_name = "Defensive Midfielders",
  plot_title = "Defensive Midfielder Decisions"
)

attacking_midfield_plot
defensive_midfield_plot




## #
# ATTACKING/CENTRAL MIDFIELDER DECISION-QUALITY TABLE --------------------------

library(tidyverse)
library(gt)
library(purrr)

# Settings
MIN_PASSES  <- 200
MIN_MATCHES <- 5

PHOTO_DIR <- file.path(getwd(), "player_photos")

dir.create(
  PHOTO_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)

# Load saved results
bundle <- readRDS("PTR_model_bundle.rds")

scored_possessions <- bundle$scored_possessions
analysis_clean <- readRDS("ptr_analysis_clean_502games.rds")


# 1. TEAM LOOKUP ---------------------------------------------------------------

team_lookup <- tribble(
  ~team_id, ~team_abbr, ~team_name,
  "1508",   "ATL",      "Atlanta United",
  "1498",   "SJE",      "San Jose Earthquakes",
  "1504",   "VAN",      "Vancouver Whitecaps",
  "1503",   "PHI",      "Philadelphia Union",
  "863",    "CLB",      "Columbus Crew",
  "1501",   "RSL",      "Real Salt Lake",
  "1507",   "NSH",      "Nashville SC",
  "1506",   "NYC",      "New York City FC",
  "2312",   "CLT",      "Charlotte FC",
  "1500",   "NER",      "New England Revolution",
  "884",    "DCU",      "D.C. United",
  "1502",   "TOR",      "Toronto FC",
  "885",    "CIN",      "FC Cincinnati",
  "862",    "HOU",      "Houston Dynamo",
  "1494",   "MIA",      "Inter Miami",
  "1505",   "MTL",      "CF Montréal",
  "337",    "ORL",      "Orlando City",
  "883",    "NYR",      "New York Red Bulls",
  "1757",   "ATX",      "Austin FC",
  "2906",   "STL",      "St. Louis City SC",
  "336",    "DAL",      "FC Dallas",
  "861",    "MIN",      "Minnesota United",
  "919",    "SEA",      "Seattle Sounders",
  "1499",   "COL",      "Colorado Rapids",
  "860",    "POR",      "Portland Timbers",
  "1497",   "SKC",      "Sporting Kansas City",
  "1495",   "LAG",      "LA Galaxy",
  "1496",   "CHI",      "Chicago Fire",
  "918",    "LAFC",     "Los Angeles FC"
)


# 2. PLAYER FULL-NAME LOOKUP ---------------------------------------------------

player_names <- analysis_clean |>
  transmute(
    player_id = as.character(player_id),
    player_name = stringr::str_squish(player_name)
  ) |>
  filter(
    !is.na(player_name),
    player_name != ""
  ) |>
  distinct(
    player_id,
    .keep_all = TRUE
  )


# 3. ATTACKING AND CENTRAL MIDFIELD POSSESSIONS --------------------------------

attacking_midfield_possessions <- scored_possessions |>
  mutate(
    player_id = as.character(player_id),
    team_id = as.character(team_id)
  ) |>
  filter(
    carrier_position %in% c(
      "Center Midfield",
      "Attacking Midfield"
    )
  )


# 4. PLAYER-LEVEL ACTUAL VERSUS EXPECTED RESULTS -------------------------------

attacking_midfielder_ptr <- attacking_midfield_possessions |>
  group_by(
    player_id,
    team_id
  ) |>
  summarise(
    n_possessions = n(),
    n_matches = n_distinct(match_id),
    
    # PTR_binary = 1 means lower-threat decision
    actual_lower_threat_rate = mean(
      PTR_binary,
      na.rm = TRUE
    ),
    
    expected_lower_threat_rate = mean(
      expected_probability,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) |>
  mutate(
    # Convert to best-option selection rates
    actual_best_option =
      1 - actual_lower_threat_rate,
    
    expected_best_option =
      1 - expected_lower_threat_rate,
    
    # Positive = selected best option more often than expected
    difference =
      actual_best_option - expected_best_option
  ) |>
  left_join(
    player_names,
    by = "player_id"
  ) |>
  left_join(
    team_lookup,
    by = "team_id"
  ) |>
  mutate(
    team_abbr = coalesce(
      team_abbr,
      team_id
    )
  ) |>
  filter(
    n_possessions >= MIN_PASSES,
    n_matches >= MIN_MATCHES,
    !is.na(player_name)
  )


# 5. SELECT TOP AND BOTTOM THREE -----------------------------------------------

top_three <- attacking_midfielder_ptr |>
  slice_max(
    difference,
    n = 3,
    with_ties = FALSE
  ) |>
  arrange(desc(difference)) |>
  mutate(
    section = "TOP 3 — ABOVE EXPECTED",
    rank = row_number()
  )

bottom_three <- attacking_midfielder_ptr |>
  slice_min(
    difference,
    n = 3,
    with_ties = FALSE
  ) |>
  arrange(desc(difference)) |>
  mutate(
    section = "BOTTOM 3 — BELOW EXPECTED",
    rank = row_number()
  )

# 6. PREPARE TABLE DATA --------------------------------------------------------

decision_table_data <- bind_rows(
  top_three,
  bottom_three
) |>
  mutate(
    section = factor(
      section,
      levels = c(
        "TOP 3 — ABOVE EXPECTED",
        "BOTTOM 3 — BELOW EXPECTED"
      )
    ),
    
    # Images must be named using player ID:
    # player_photos/12345.png
    photo = file.path(
      PHOTO_DIR,
      paste0(player_id, ".png")
    )
  ) |>
  select(
    section,
    rank,
    photo,
    player_name,
    team_abbr,
    n_possessions,
    actual_best_option,
    expected_best_option,
    difference
  )


# Check that all selected photos exist
photo_check <- decision_table_data |>
  transmute(
    player_name,
    photo,
    photo_exists = file.exists(photo)
  )

photo_check

#############
library(gt)
library(dplyr)
library(purrr)
library(scales)
library(base64enc)

max_difference <- max(abs(decision_table_data$difference), na.rm = TRUE)

attacking_decision_table <- decision_table_data |>
  select(-any_of("player_html")) |>
  gt(groupname_col = "section", row_group_as_column = FALSE) |>
  cols_hide(rank) |>
  row_group_order(groups = c("TOP 3 — ABOVE EXPECTED", "BOTTOM 3 — BELOW EXPECTED")) |>
  
  tab_header(
    title = md("**ATTACKING MIDFIELDER DECISION QUALITY**"),
    subtitle = md(paste0(
      "Did they pick the best available pass? · ",
      "Min. ", MIN_PASSES, " passes, ", MIN_MATCHES, " matches"
    ))
  ) |>
  
  cols_label(
    photo = "",
    player_name = "PLAYER",
    team_abbr = "TEAM",
    n_possessions = "PASSES",
    actual_best_option = "ACTUAL",
    expected_best_option = "EXPECTED",
    difference = "DIFF"
  ) |>
  
  text_transform(
    locations = cells_body(columns = photo),
    fn = function(paths) {
      map_chr(paths, function(p) {
        if (!file.exists(p)) return("")
        b64 <- base64enc::base64encode(p)
        paste0(
          "<img src='data:image/png;base64,", b64,
          "' style='width:50px;height:50px;object-fit:cover;",
          "object-position:top center;display:block;margin:0 auto;'>"
        )
      })
    }
  ) |>
  
  fmt_number(columns = n_possessions, decimals = 0, use_seps = TRUE) |>
  fmt_percent(columns = c(actual_best_option, expected_best_option), decimals = 1) |>
  fmt_percent(columns = difference, decimals = 1, force_sign = TRUE) |>
  
  data_color(
    columns = difference,
    fn = scales::col_numeric(
      palette = c("#C0392B", "#E8837B", "#FFFFFF", "#7FC08A", "#1E8449"),
      domain  = c(-max_difference, max_difference)
    ),
    autocolor_text = FALSE
  ) |>
  
  tab_style(
    style = cell_text(weight = "bold", color = "#1A1A1A"),
    locations = cells_body(columns = difference)
  ) |>
  
  tab_style(
    style = cell_text(weight = "bold", size = px(12), color = "#172B4D"),
    locations = cells_body(columns = player_name)
  ) |>
  
  tab_style(
    style = cell_text(weight = "bold", size = px(11), color = "#6B7280"),
    locations = cells_body(columns = team_abbr)
  ) |>
  
  tab_style(
    style = list(cell_fill("#E4F2E7"),
                 cell_text(color = "#1E8449", weight = "bold", size = px(13))),
    locations = cells_row_groups(groups = "TOP 3 — ABOVE EXPECTED")
  ) |>
  tab_style(
    style = list(cell_fill("#F8E4E4"),
                 cell_text(color = "#C0392B", weight = "bold", size = px(13))),
    locations = cells_row_groups(groups = "BOTTOM 3 — BELOW EXPECTED")
  ) |>
  
  tab_style(
    style = cell_borders(sides = c("top", "bottom"), color = "#22354D", weight = px(1.5)),
    locations = cells_row_groups()
  ) |>
  
  tab_style(
    style = cell_text(weight = "bold", size = px(10), color = "#8A94A6"),
    locations = cells_column_labels()
  ) |>
  
  tab_style(
    style = css(`text-align` = "center !important"),
    locations = cells_column_labels(columns = everything())
  ) |>
  
  cols_align("center", columns = everything()) |>
  
  cols_width(
    photo ~ px(60),
    player_name ~ px(125),
    team_abbr ~ px(45),
    n_possessions ~ px(68),
    actual_best_option ~ px(78),
    expected_best_option ~ px(88),
    difference ~ px(76)
  ) |>
  
  tab_options(
    table.width = px(540),
    table.font.names = c("Roboto Condensed", "Helvetica Neue", "Arial", "sans-serif"),
    table.font.size = px(11),
    table.border.top.color = "#22354D",
    table.border.top.width = px(2),
    table.border.bottom.color = "#22354D",
    table.border.bottom.width = px(2),
    
    heading.title.font.size = px(17),
    heading.subtitle.font.size = px(10),
    heading.align = "center",
    heading.border.bottom.style = "none",
    
    column_labels.background.color = "#F2F3F5",
    column_labels.padding = px(4),
    column_labels.border.bottom.color = "#22354D",
    column_labels.border.bottom.width = px(1.5),
    
    data_row.padding = px(5),
    row_group.padding = px(5),
    
    table_body.hlines.color = "#EDEFF2"
  )

attacking_decision_table

# Save
gtsave(
  attacking_decision_table,
  filename = "attacking_midfielder_decision_quality.png",
  vwidth = 1100,
  vheight = 900,
  zoom = 3,
  expand = 5
)


gtsave(
  attacking_decision_table,
  filename = "attacking_midfielder_decision_quality_poster.png",
  vwidth = 1800,
  vheight = 1200,
  zoom = 4,
  expand = 15
)

