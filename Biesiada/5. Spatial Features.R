# 5. Spatial Features 


# Compute defender-geometry features from RAW TRACKING at the pass moment:
#   receiver_pressure : distance of nearest defender to the BEST option
#   lane_coverage     : min distance of any defender to the carrier->option lane
#   n_in_lane_2m      : defenders within 2m of that lane
# escape_gap = def_dist_target - def_dist_best_option
# positive = the attacker passed to a FREER man than the dangerous one
# -> the spatial signature of "forced to the safe outlet"


library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)



# Choosing the folder
folder <- "mls_skillcorner/tracking"

# Filter out the games that event data is missing
games_to_remove <- c(
  "1066470", "1096007", "1106283", "648779", "648780",
  "649421", "649422", "649433", "649434", "651546",
  "688134", "688136", "708458", "760689", "880422",
  "895807", "907133", "915267"
)

# All files info
files <- list.files(
  path = folder,
  pattern = "_tracking(\\.jsonl|\\.json)?$",
  full.names = TRUE
)

file_info <- tibble(
  file = files,
  game_id = str_extract(basename(files), "\\d+")
)

# Filtering the games with event data
files_keep <- file_info |>
  filter(!game_id %in% games_to_remove)

# Double checking if the games are removed
files_keep |>
  filter(game_id %in% games_to_remove)

# Extracting 10 games
files_10 <- files_keep$file[1:min(10, nrow(files_keep))]

# Saving 
tracking_10 <- map_dfr(files_10, function(file_path) {
  
  if (str_detect(file_path, "\\.jsonl$")) {
    data <- stream_in(file(file_path), verbose = FALSE)
  } else {
    data <- fromJSON(file_path, flatten = TRUE)
  }
  
  data <- as.data.frame(data)
  
  data$source_file <- file_path
  data$game_id <- str_extract(basename(file_path), "\\d+")
  
  return(data)
})

#  Saving 10 games

saveRDS(tracking_10, "tracking_10_idea.rds")

colnames(tracking_10)
colnames(tracking_10$player_data)

