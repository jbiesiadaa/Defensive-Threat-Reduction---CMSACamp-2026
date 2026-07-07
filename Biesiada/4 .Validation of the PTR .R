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


# 1. What is the DISTRIBUTION -> know your outcome
# What we expect: heavy spike at 0 (~50%), long right tail
# ------------------------------------------------------------------------------
summary(analysis_clean$PTR)
cat("Share PTR > 0:", round(mean(analysis_clean$PTR > 0) * 100, 1), "%\n")

ggplot(analysis_clean, aes(PTR)) +
  geom_histogram(bins = 60) +
  labs(title = "PTR distribution", x = "PTR", y = "Count") +
  theme_minimal()

# Checking how many PTR = 0 and PTR > 0
analysis_clean |>
  mutate(PTR_group = case_when(
    PTR == 0 ~ "PTR = 0",
    PTR > 0  ~ "PTR > 0",
    TRUE     ~ "Missing"
  )) |>
  count(PTR_group)

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
# CONDITIONAL VERSION:
# Restrict to possessions where a dangerous realistic option existed.
# This makes the comparison fairer because all rows had meaningful danger available,
# but opportunity level is still not perfectly equal across bands
analysis_clean |>
  filter(engaged, max_xthreat_realistic > 0.02) |>
  mutate(PTR_band = case_when(
    PTR == 0                                  ~ "zero (took the danger)",
    PTR <= median(PTR[PTR > 0], na.rm = TRUE) ~ "low",
    TRUE                                      ~ "high (declined the danger)"
  )) |>
  group_by(PTR_band) |>
  summarise(n = n(),
            mean_PTR = mean(PTR, na.rm = TRUE),
            mean_best_realistic_xT = mean(max_xthreat_realistic, na.rm = TRUE),
            mean_chosen_xT = mean(player_targeted_xthreat, na.rm = TRUE),
            pct_lead_to_shot = mean(lead_to_shot %in% TRUE, na.rm = TRUE) * 100,
            .groups = "drop"
  )

# When a dangerous realistic option existed, what happened if the attacker took the danger versus declined the danger?
# Full Story: dangerous option available -> chosen pass threat -> shot outcome

# Interpretation:
# When PTR was zero, the possession led to a shot 24.7% of the time
# These are cases where the attacker took the dangerous option or selected an even more dangerous pass

# When PTR was high, the possession led to a shot only 12.9% of the time even though the average best realistic option was very dangerous
# This suggests that high PTR captures defensive suppression: the defense was associated with pushing the attacker away from a dangerous pass choice

# Metric PTR, claims to measure something real: how much threat did the defense make the attacker give up
# In fact, high PTR possessions had the most dangerous realistic options available, but the chosen passes were much less dangerous and led to fewer shots.
# This supports the interpretation of PTR as a defensive threat-suppression metric
  
  
# 12. FAILURE CONTRAST - > Even when the defender was beaten, did the attacker still choose a less dangerous pass?
# beaten_by_possession = beaten on the ball — the carrier got past him
# beaten_by_movement = beaten by the run — the receiver escaped him before the pass arrived

analysis_clean |>
  filter(engaged) |>
  mutate(any_beaten = any_beaten_by_possession | any_beaten_by_movement) |>
  group_by(any_beaten_by_possession, any_beaten_by_movement) |>
  summarise(n = n(), mean_PTR = mean(PTR),
            pct_positive = mean(PTR > 0) * 100, .groups = "drop") |>
  print()

# RESULT: deviation rate falls as defense fails more completely:
#   47% (not beaten) -> 33% (by movement) -> 23% (by possession) -> 11% (both, n=9 too small)

# INTERPRETATION:
# This check should be judged mainly by pct_positive, not mean_PTR.
# When defenders are beaten, they are less often associated with positive PTR,
# meaning they less often force the attacker away from the best realistic option.

# The mean PTR can still be higher for beaten cases because these events often
# happen in high-threat situations, where the possible PTR ceiling is larger.

# Future defender profiles should combine PTR with beaten rate:
# high PTR + low beaten = strong suppression;
# high PTR + high beaten = risky pressure.




# EXTRA EVALUATION -------------------------------------------------------------
# FUTURE DEFENDER EVALUATION -- three dimensions:
# 1. Force rate : pct_positive_PTR    How often does the defender help force a less dangerous pass?
# 2. Risk rate  : pct_beaten          How often does the defender get bypassed by possession or movement?
# 3. Damage rate: pct_lead_to_shot    How often does the possession become dangerous after the engagement?
#
# Profiles:
#   Best              : high force, low beaten, low shots
#   Risky presser     : high force, high beaten, high shots
#   Safe containment  : low force, low beaten, low shots
#   Beaten-but-covered: high beaten rate, low shot rate
#     This player may look bad because they get beaten, but the actual damage is low.
#     This profile is hard to see with simple counting stats and may reveal
#     undervalued defenders.
#
# NOTE:
# These rates depend on role, team style, and field zone, so compare players
# within position groups. A stronger future version would adjust for context using model residuals

# -----------------------------------------------------------------------------------------------------------
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
