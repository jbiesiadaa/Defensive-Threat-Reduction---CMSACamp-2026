process_game <- function(event_file,
                         tracking_file,
                         match_file){
  
  # Read files
  
  events <- read_csv(
    event_file,
    show_col_types = FALSE
  )
  
  tracking <-
    if (str_detect(tracking_file, "jsonl")) {
      stream_in(file(tracking_file), verbose = FALSE)
    } else {
      fromJSON(tracking_file)
    }
  
  match <- fromJSON(match_file)
  
  
  
  # Feature engineering
  #-------------------------------------------------------
  
  option_features <-
    make_option_features(events)
  
  targeted_option_coords <-
    make_targeted_option_coords(events)
  
  obe_features <-
    make_obe_features(events)
  
  tracking_features <-
    make_tracking_features(
      events,
      tracking,
      match,
      option_features,
      targeted_option_coords
    )
  
  
  
  # Build final possession dataset
  #-------------------------------------------------------
  
  analysis <-
    build_analysis_dataset(
      events,
      option_features,
      targeted_option_coords,
      obe_features,
      tracking_features
    )
  
  
  
  analysis
  
}