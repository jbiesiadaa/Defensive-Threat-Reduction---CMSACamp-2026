
library(tidyverse)
library(jsonlite)
library(data.table)
library(purrr)
library(dplyr)


to_bool <- function(x) {
  ifelse(
    is.na(x),
    NA,
    x %in% c(TRUE, "True", "TRUE", "true", 1, "1")
  )
}


# Build list of available games
# -------------------------------------------------------------------------

event_files <- tibble(
  event_file = list.files(
    "mls_skillcorner/dynamic_events",
    pattern = "_events\\.csv$",
    full.names = TRUE
  )
) |>
  mutate(
    game_id = str_extract(
      basename(event_file),
      "\\d+"
    )
  )


tracking_files <- tibble(
  tracking_file = list.files(
    "mls_skillcorner/tracking",
    pattern = "_tracking(\\.jsonl|\\.json)?$",
    full.names = TRUE
  )
) |>
  mutate(
    game_id = str_extract(
      basename(tracking_file),
      "\\d+"
    )
  )


match_files <- tibble(
  match_file = list.files(
    "mls_skillcorner/match_data",
    pattern = "_data\\.json$",
    full.names = TRUE
  )
) |>
  mutate(
    game_id = str_extract(
      basename(match_file),
      "\\d+"
    )
  )


# -------------------------------------------------------------------------
# Remove unusable games
# -------------------------------------------------------------------------

games_to_remove <- c(
  "1066470","1096007","1106283","648779","648780",
  "649421","649422","649433","649434","651546",
  "688134","688136","708458","760689","880422",
  "895807","907133","915267"
)


games <- event_files |>
  inner_join(
    tracking_files,
    by = "game_id"
  ) |>
  inner_join(
    match_files,
    by = "game_id"
  ) |>
  filter(
    !game_id %in% games_to_remove
  )


games <- games |>
  slice_head(n = 2)


# -------------------------------------------------------------------------
# Run feature engineering
# -------------------------------------------------------------------------

results <- purrr::pmap_dfr(
  
  games,
  
  function(game_id,
           event_file,
           tracking_file,
           match_file){
    
    message("Processing game ", game_id)
    
    process_game(
      event_file = event_file,
      tracking_file = tracking_file,
      match_file = match_file
    )
    
  }
  
)






......






# checks

# 
# results |>
#   filter(PTR == 0) |>
#   mutate(
#     # compute inline in case these aren't columns yet
#     chose_best_option = ifelse(
#       is.na(best_option_event_id), NA,
#       as.character(targeted_passing_option_event_id) ==
#         as.character(best_option_event_id)
#     ),
#     chosen_pass_realistic = player_targeted_xpass_completion > 0.68,
#     is_overreach = chose_best_option %in% FALSE &
#       !chosen_pass_realistic %in% TRUE
#   ) |>
#   summarise(
#     n              = n(),
#     n_chose_best   = sum(chose_best_option %in% TRUE),
#     n_overreach    = sum(is_overreach %in% TRUE),       # gamblers, filtered later
#     n_tied         = sum(best_was_tied %in% TRUE),
#     n_unexplained  = sum(chose_best_option %in% FALSE &
#                            !is_overreach %in% TRUE),      # expect 0
#     max_gap        = max(dist_targeted_to_best_option[chose_best_option %in% TRUE],
#                          na.rm = TRUE)                  # expect exactly 0
#   )
# 
# 
# suspects <- results |>
#   filter(PTR == 0) |>
#   mutate(chose_best_option = ifelse(
#     is.na(best_option_event_id), NA,
#     as.character(targeted_passing_option_event_id) ==
#       as.character(best_option_event_id))) |>
#   filter(chose_best_option %in% FALSE,
#          player_targeted_xpass_completion > 0.68)
# 
# 
# 
# suspects |>
#   mutate(
#     gap = player_targeted_xthreat - max_xthreat_realistic,
#     bucket = case_when(
#       gap > 0  ~ "C: chosen xthreat ABOVE realistic max (pmax clamped)",
#       gap == 0 ~ "T: exact tie -- tie-break should have caught it (bug!)",
#       gap < 0  ~ "?: chosen BELOW max yet PTR==0 -- impossible, inspect"
#     )
#   ) |>
#   count(bucket)
# 
# # and the size of the gaps for the C rows:
# suspects |>
#   mutate(gap = player_targeted_xthreat - max_xthreat_realistic) |>
#   filter(gap > 0) |>
#   summarise(n = n(),
#             min_gap = min(gap), median_gap = median(gap), max_gap = max(gap))
# 
# 
# 
# 
# 
# 
# 
# # load only what's needed: option rows from your two games
# opts2 <- bind_rows(
#   read.csv("mls_skillcorner/dynamic_events/match_1039803_events.csv"),
#   read.csv("mls_skillcorner/dynamic_events/match_1039803_events.csv")
# ) |>
#   filter(event_type_id == 7) |>
#   select(match_id, event_id, opt_xthreat = xthreat,
#          opt_xpass = xpass_completion)
# 
# # align key types with suspects (adjust direction if needed)
# opts2 <- opts2 |>
#   mutate(match_id = as.character(match_id),
#          event_id = as.character(event_id))
# 
# suspects |>
#   mutate(match_id = as.character(match_id),
#          targeted_passing_option_event_id =
#            as.character(targeted_passing_option_event_id)) |>
#   left_join(opts2,
#             by = c("match_id",
#                    "targeted_passing_option_event_id" = "event_id")) |>
#   mutate(bucket = case_when(
#     is.na(opt_xthreat) & is.na(opt_xpass)
#     ~ "1: option row missing / xthreat NA -> filtered before ranking",
#     is.na(opt_xpass) | opt_xpass <= 0.68
#     ~ "2: NOT realistic on option side -> ineligible to win the tie",
#     TRUE
#     ~ "3: realistic + tied but lost -> TRUE is_targeted bug"
#   )) |>
#   count(bucket)
# 
# 
# 
# opts2 |> count(match_id, event_id) |> filter(n > 1)
# 
# # 2. do the match_id values even overlap? (join-miss detector)
# setdiff(unique(as.character(suspects$match_id)), unique(opts2$match_id))
# # any output here = suspects' match_id isn't in opts2 -> bucket 1 was join failure
# 
# # 3. eyeball raw key formats side by side
# suspects |> distinct(match_id) |> print()
# opts2    |> distinct(match_id) |> print()
# 
# 
# 
# 
# # rebuild opts2 with the CORRECT two file paths (verify both filenames!)
# opts2 <- bind_rows(
#   read.csv("mls_skillcorner/dynamic_events/match_1039803_events.csv"),
#   read.csv("mls_skillcorner/dynamic_events/match_1039804_events.csv")
# ) |>
#   filter(event_type_id == 7) |>
#   transmute(match_id = as.character(match_id),
#             event_id = as.character(event_id),
#             opt_xthreat = xthreat,
#             opt_xpass   = xpass_completion)
# 
# # guards: no dupes, both games present -- don't proceed unless both pass
# stopifnot(
#   nrow(opts2 |> count(match_id, event_id) |> filter(n > 1)) == 0,
#   length(setdiff(as.character(unique(suspects$match_id)),
#                  unique(opts2$match_id))) == 0
# )
# 
# # rerun the bucket diagnostic
# suspects |>
#   mutate(match_id = as.character(match_id),
#          targeted_passing_option_event_id =
#            as.character(targeted_passing_option_event_id)) |>
#   left_join(opts2,
#             by = c("match_id",
#                    "targeted_passing_option_event_id" = "event_id")) |>
#   mutate(bucket = case_when(
#     is.na(opt_xthreat)
#     ~ "1: option xthreat NA -> filtered before ranking",
#     is.na(opt_xpass) | opt_xpass <= 0.68
#     ~ "2: not realistic on option side -> ineligible",
#     TRUE
#     ~ "3: realistic + tied but lost -> is_targeted bug"
#   )) |>
#   count(bucket)
# 
# 
# # full option rows WITH the association column (reuse the two correct CSVs)
# opts_full <- bind_rows(
#   read.csv("mls_skillcorner/dynamic_events/match_1039803_events.csv"),
#   read.csv("mls_skillcorner/dynamic_events/match_1039804_events.csv")
# ) |>
#   filter(event_type_id == 7) |>
#   transmute(match_id  = as.character(match_id),
#             option_id = as.character(event_id),
#             assoc_id  = as.character(associated_player_possession_event_id),
#             opt_xthreat = xthreat)
# 
# diag <- suspects |>
#   transmute(match_id    = as.character(match_id),
#             poss_id     = as.character(event_id),
#             targeted_id = as.character(targeted_passing_option_event_id)) |>
#   # find the targeted option's row and read ITS association
#   left_join(opts_full,
#             by = c("match_id", "targeted_id" = "option_id"))
# 
# diag |>
#   mutate(verdict = case_when(
#     is.na(assoc_id)        ~ "A1: targeted option has NA association",
#     assoc_id != poss_id    ~ "A2: targeted option associated with a DIFFERENT possession",
#     assoc_id == poss_id    ~ "B: in the right group -> ID comparison failed (format)"
#   )) |>
#   count(verdict)
# 
# # if any B rows, inspect the raw strings:
# diag |>
#   filter(assoc_id == poss_id) |>
#   transmute(targeted_id,
#             n_char = nchar(targeted_id),
#             has_ws = targeted_id != trimws(targeted_id))
# 
# 
# 
# 
# 
# ...
# 
# 
# diag2 <- suspects |>
#   transmute(match_id = as.character(match_id),
#             poss_id  = as.character(event_id),
#             targeted_id = as.character(targeted_passing_option_event_id),
#             poss_xthreat = player_targeted_xthreat,   # possession side
#             max_real     = max_xthreat_realistic) |>
#   left_join(opts_full,
#             by = c("match_id", "targeted_id" = "option_id"))
# 
# diag2 |>
#   mutate(
#     opt_vs_max  = opt_xthreat - max_real,       # tied IN THE RANKING?
#     opt_vs_poss = opt_xthreat - poss_xthreat,   # do the two tables agree?
#     verdict = case_when(
#       opt_vs_max < 0  ~ "NOT a tie in-table: option-side xthreat below max (tables disagree)",
#       opt_vs_max == 0 ~ "TRUE in-table tie that still lost -> genuine sort bug",
#       opt_vs_max > 0  ~ "option-side ABOVE max?! -> ranking itself broken, inspect"
#     )
#   ) |>
#   count(verdict)
# 
# # and the size of the disagreement, if any
# diag2 |>
#   mutate(d = abs(opt_xthreat - poss_xthreat)) |>
#   summarise(n_differ = sum(d > 0, na.rm = TRUE),
#             median_d = median(d[d > 0], na.rm = TRUE),
#             max_d    = max(d, na.rm = TRUE))
# 
