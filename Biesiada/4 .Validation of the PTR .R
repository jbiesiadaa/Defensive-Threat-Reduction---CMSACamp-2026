# Julia Biesiada
# 4.Validation of the PTR 
# Validation of the PTR (Passing Threat Reduction) metric
# Each check -> passed/failed

# Most checks use analysis_clean because analysis_model is only a subset with less observations
# Statistical patterns can change after filtering, so they are rechecked on analysis_model in Check 10
# Checks 6–7 are exploratory results, not pass/fail validation.

library(tidyverse)

analysis_clean <- readRDS("ptr_analysis_dataset_200.rds") # broad sample
analysis_model <- readRDS("ptr_model_dataset_200.rds") # strict sample

# Helper: SkillCorner booleans arrive as strings ("True"/"False") or logicals -> transforming them to the logical values
to_bool <- function(x) {
  case_when(
    x %in% c(TRUE,  "TRUE",  "True",  "true")  ~ TRUE,
    x %in% c(FALSE, "FALSE", "False", "false") ~ FALSE,
    TRUE ~ NA
  )
}

# Converting to Boolean
analysis_clean <- analysis_clean |>
  mutate(
    lead_to_shot = to_bool(lead_to_shot),
    lead_to_goal = to_bool(lead_to_goal)
  )

# 1. What is the DISTRIBUTION -> know your outcome
# What we expect: heavy spike at 0 (~50%), long right tail
# ------------------------------------------------------------------------------
summary(analysis_clean$PTR)
cat("Share PTR > 0:", round(mean(analysis_clean$PTR > 0) * 100, 1), "%\n")

ggplot(analysis_clean, aes(PTR)) +
  geom_histogram(bins = 60) +
  labs(title = "PTR distribution", x = "PTR", y = "Count") +
  theme_minimal()

# PTR is zero-heavy and strongly right-skewed. About 45.8% of possessions have PTR > 0,
# meaning threat reduction happens often, but most values are small with a few large cases

# 2.INTERNAL CONSISTENCY -- the metric agrees with itself
# Expect: mean PTR EXACTLY 0 when the attacker chose the best option
# Anything else = a bug in the option join
# ------------------------------------------------------------------------------
analysis_clean |>
  filter(!is.na(chose_best_option)) |>
  group_by(chose_best_option) |>
  summarise(n = n(), mean_PTR = mean(PTR), .groups = "drop") |>
  print()

# Passed: when the attacker chose the best realistic option, mean PTR is exactly 0
# This confirms the option join and PTR formula are working correctly


# 3. CONSTRUCT VALIDITY -- PTR vs SkillCorner's independent outcome flags
# Expect: Suppression and Forced backward clearly above Engaged-no-outcome and No engagement. 
# ------------------------------------------------------------------------------
analysis_clean |>
  group_by(defensive_outcome) |>
  summarise(
    n            = n(),
    mean_PTR     = mean(PTR),
    median_PTR   = median(PTR),
    pct_positive = mean(PTR > 0) * 100,
    .groups = "drop"
  ) |>
  arrange(desc(mean_PTR)) |>
  print()

# Passed: Suppression and Forced backward have higher mean PTR than ordinary engagements and no engagement
# PTR agrees with SkillCorner's defensive outcome labels


# 4. ENGAGEMENT CONTRAST
# Expect: engaged possessions show higher mean PTR than unengaged
# ------------------------------------------------------------------------------
analysis_clean |>
  group_by(engaged) |>
  summarise(n = n(), mean_PTR = mean(PTR),
            pct_positive = mean(PTR > 0) * 100, .groups = "drop") |>
  print()

# Passed: engaged possessions have higher mean PTR than unengaged possessions
# This supports the core idea that defensive engagement is associated with threat reduction

# 5. RAW vs REALISTIC --> does the 0.68 filter matter?
# Expect: moderate correlation (~0.3-0.5). If ~1, the filter changes nothing;
# if very low, check the filter isn't broken.
# ------------------------------------------------------------------------------
cat("cor(PTR_raw, PTR):",
    round(cor(analysis_clean$PTR_raw, analysis_clean$PTR,
              use = "complete.obs"), 3), "\n")

# Comparing mean_PTR_raw and mean_PTR
analysis_clean |>
  group_by(defensive_outcome) |>
  summarise(mean_PTR_raw = mean(PTR_raw, na.rm = TRUE),
            mean_PTR     = mean(PTR), .groups = "drop") |>
  print()

# Passed: PTR_raw and PTR are moderately correlated, not identical
# The 0.68 realism filter matters because PTR_raw is much larger when difficult options are included

# 6. PTR BY ENGAGEMENT TYPE --> which pressure forces bad decisions?
# No expectation: this is our first RESULT.
# Note: Pressure is single action by player, pressing is a chain of pressure by players
# ------------------------------------------------------------------------------
analysis_clean |>
  group_by(engagement_type_group) |>
  summarise(n = n(), mean_PTR = mean(PTR),
            pct_positive = mean(PTR > 0) * 100, .groups = "drop") |>
  arrange(desc(mean_PTR)) |>
  print()

# Exploratory result: recovery press and pressure show the highest mean PTR
# This suggests some engagement types are more associated with threat reduction than others

# 7. PTR BY ENGAGEMENT ZONE -> high vs middle vs deep press?
# No expectation: this is our first RESULT. 
# ------------------------------------------------------------------------------
analysis_clean |>
  group_by(engagement_zone) |>
  summarise(n = n(), mean_PTR = mean(PTR),
            pct_positive = mean(PTR > 0) * 100, .groups = "drop") |>
  arrange(desc(mean_PTR)) |>
  print()

# Exploratory result: deep engagements have the highest mean PTR, while high engagements are lowest
# This may be because defensive actions closer to goal suppress more dangerous options

# 8. ROBUSTNESS --> do results survive excluding short possesions 
# Expect: The ordering unchanged after dropping short possessions
# ------------------------------------------------------------------------------
analysis_clean |>
  filter(!short_possession) |>
  group_by(defensive_outcome) |>
  summarise(n = n(), mean_PTR = mean(PTR), .groups = "drop") |>
  arrange(desc(mean_PTR)) |>
  print()

# Passed: the outcome ordering stays the same after removing short possessions
# This suggests the validation is not driven only by one-touch or very short actions


# 9. DATA-QUALITY CANARIES -- pipeline bug detectors
# EXPECT: both = 0.
# ------------------------------------------------------------------------------
cat("'Engaged, unknown':",
    sum(analysis_clean$engagement_type_group == "Engaged, unknown"), "\n",
    "carrier_central NAs:", sum(is.na(analysis_clean$carrier_central)), "\n")

# Passed: no "Engaged, unknown" cases and no missing carrier_central values
# This suggests the engagement labels and channel coding are working correctly

# 10. MODEL-TIER VALIDATION -- re-verify what subsetting CAN change
#--------------------------------------------------------------------------------

# 10a. Zero share in the model tier (motivates the zero-inflated beta)
cat("Share PTR > 0 (model tier):",
    round(mean(analysis_model$PTR > 0) * 100, 1), "%\n")

# 10b. Construct validity in the model tier
analysis_model |>
  group_by(defensive_outcome) |>
  summarise(n = n(), mean_PTR = mean(PTR),
            pct_positive = mean(PTR > 0) * 100, .groups = "drop") |>
  arrange(desc(mean_PTR)) |>
  print()

# 10c. Engagement contrast in the model tier
analysis_model |>
  group_by(engaged) |>
  summarise(n = n(), mean_PTR = mean(PTR), .groups = "drop") |>
  print()

# Passed: the stricter model sample keeps the same main patterns
# Suppression and Forced backward remain highest, and engaged possessions still have higher PTR than unengaged  


# 11. PREDICTIVE VALIDITY
analysis_clean |>
  filter(engaged) |>
  mutate(PTR_band = case_when(
    PTR == 0                    ~ "zero",
    PTR <= median(PTR[PTR > 0]) ~ "low",
    TRUE                        ~ "high"
  )) |>
  group_by(PTR_band) |>
  summarise(n = n(),
            pct_lead_to_shot = mean(lead_to_shot %in% TRUE) * 100,
            .groups = "drop") |>
  print()


analysis_clean |>
  filter(engaged) |>
  mutate(
    PTR_band = case_when(
      PTR == 0 ~ "zero",
      PTR <= median(PTR[PTR > 0], na.rm = TRUE) ~ "low",
      TRUE ~ "high"
    )
  ) |>
  group_by(PTR_band) |>
  summarise(
    n = n(),
    mean_PTR = mean(PTR, na.rm = TRUE),
    mean_chosen_xT = mean(player_targeted_xthreat, na.rm = TRUE),
    mean_best_realistic_xT = mean(max_xthreat_realistic, na.rm = TRUE),
    pct_lead_to_shot = mean(lead_to_shot %in% TRUE, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# 12. FAILURE CONTRAST --> PTR should be LOW when the defender was beaten or not ( i mean because they are closing the )
# The flag can be canceled in specific situations, such as interceptions, possession regain, or valid defensive positioning.
analysis_clean |>
  filter(engaged) |>
  mutate(any_beaten = any_beaten_by_possession | any_beaten_by_movement) |>
  group_by(any_beaten) |>
  summarise(n = n(), mean_PTR = mean(PTR),
            pct_positive = mean(PTR > 0) * 100, .groups = "drop") |>
  print()



# TBD WORKING ON IT: Wilcoxon
# FORMAL TEST: engaged vs unengaged PTR
# Because PTR is zero-heavy and right-skewed, use a Wilcoxon rank-sum test instead of a normal t-test. With large n, p-values will be very small,
# so interpret this as a supporting check, not the main evidence
# Are PTR values generally higher for engaged possessions than unengaged possessions?

wilcox.test(analysis_model$PTR[analysis_model$engaged],
            analysis_model$PTR[!analysis_model$engaged],
            alternative = "greater", exact = FALSE)

# APENDIX
# PTR passes internal consistency checks and aligns with SkillCorner defensive outcome labels, especially Suppression (reduce_danger) and Forced backward 
# Because these labels come from the same SkillCorner tracking/event data, this is supporting validation, not fully independent proof
# Future work should compare PTR with video coding or coach-labeled examples
