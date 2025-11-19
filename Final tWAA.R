library(dplyr)
library(xgboost)

# ============================================================================
# CONFIGURATION
# ============================================================================

INPUT_FILE <- "C:/Users/ecloaner/Downloads/TunnellOrientedPitchingModel/statcast_data/statcast_with_tunneling_full.rds"
OUTPUT_DIR <- "C:/Users/ecloaner/Downloads/TunnellOrientedPitchingModel/statcast_data"


# ============================================================================
# LOAD DATA
# ============================================================================

cat("Loading data...\n")
df <- readRDS(INPUT_FILE)
cat(sprintf(" Loaded %s pitches\n\n", format(nrow(df), big.mark = ",")))

# Arsenal Scores


# Arsenal-level aggregation
arsenal_scores <- df %>%
  filter(!is.na(VRA_KDE), !is.na(HRA_KDE), !is.na(VAA_KDE), !is.na(HAA_KDE)) %>%
  group_by(pitcher, player_name, game_year) %>%
  summarize(
    n = n(),
    n_pitch_types = n_distinct(pitch_type),
    pitch_mix = paste(sort(unique(pitch_type)), collapse = ", "),
    
    # Average KDE scores across all pitches in arsenal
    avg_VRA_KDE = mean(VRA_KDE, na.rm = TRUE),
    avg_HRA_KDE = mean(HRA_KDE, na.rm = TRUE),
    avg_VAA_KDE = mean(VAA_KDE, na.rm = TRUE),
    avg_HAA_KDE = mean(HAA_KDE, na.rm = TRUE),
    
    # RELEASE SIMILARITY 
    release_similarity_arsenal = (avg_VRA_KDE + avg_HRA_KDE) / 2,
    
    # APPROACH DIVERGENCE 
    approach_divergence_arsenal = 1 - ((avg_VAA_KDE + avg_HAA_KDE) / 2),
    
    # TUNNEL QUALITY
    tunnel_quality_arsenal = (release_similarity_arsenal + approach_divergence_arsenal) / 2,
    
    # Actual outcomes
    actual_rv = mean(delta_run_exp, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  filter(n >= 500)  # Minimum 500 pitches per season

cat(sprintf("Calculated arsenal scores for %s pitcher-seasons\n\n",
            format(nrow(arsenal_scores), big.mark = ",")))

# Show score distributions
cat("Arsenal Score Distributions:\n")
cat(sprintf("  release_similarity_arsenal: Mean = %.3f, SD = %.3f, Range = %.3f-%.3f\n",
            mean(arsenal_scores$release_similarity_arsenal),
            sd(arsenal_scores$release_similarity_arsenal),
            min(arsenal_scores$release_similarity_arsenal),
            max(arsenal_scores$release_similarity_arsenal)))
cat(sprintf("  approach_divergence_arsenal: Mean = %.3f, SD = %.3f, Range = %.3f-%.3f\n",
            mean(arsenal_scores$approach_divergence_arsenal),
            sd(arsenal_scores$approach_divergence_arsenal),
            min(arsenal_scores$approach_divergence_arsenal),
            max(arsenal_scores$approach_divergence_arsenal)))
cat(sprintf("  tunnel_quality_arsenal: Mean = %.3f, SD = %.3f, Range = %.3f-%.3f\n\n",
            mean(arsenal_scores$tunnel_quality_arsenal),
            sd(arsenal_scores$tunnel_quality_arsenal),
            min(arsenal_scores$tunnel_quality_arsenal),
            max(arsenal_scores$tunnel_quality_arsenal)))

# Pitch Type Avgs

pitch_type_scores <- df %>%
  filter(!is.na(VRA_KDE), !is.na(HRA_KDE), !is.na(VAA_KDE), !is.na(HAA_KDE)) %>%
  group_by(pitcher, player_name, game_year, pitch_type) %>%
  summarize(
    n = n(),
    
    # Average tunneling metrics for this pitch type
    avg_tunnel_quality_for_pitch = mean((VRA_KDE + HRA_KDE + (1 - VAA_KDE) + (1 - HAA_KDE)) / 4, na.rm = TRUE),
    avg_release_sim_for_pitch = mean((VRA_KDE + HRA_KDE) / 2, na.rm = TRUE),
    avg_approach_div_for_pitch = mean(1 - ((VAA_KDE + HAA_KDE) / 2), na.rm = TRUE),
    
    # Individual KDE components
    avg_VRA_KDE = mean(VRA_KDE, na.rm = TRUE),
    avg_HRA_KDE = mean(HRA_KDE, na.rm = TRUE),
    avg_VAA_KDE = mean(VAA_KDE, na.rm = TRUE),
    avg_HAA_KDE = mean(HAA_KDE, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  filter(n >= 100)

cat(sprintf("Calculated pitch-type scores for %s pitcher-pitch type combinations\n\n",
            format(nrow(pitch_type_scores), big.mark = ",")))


#  Modeling



cat("Joining arsenal and pitch-type scores...\n")

# Join all tunneling metrics to pitch-level data
model_data <- df %>%
  inner_join(
    arsenal_scores %>% select(pitcher, game_year, 
                              tunnel_quality_arsenal,
                              release_similarity_arsenal,
                              approach_divergence_arsenal),
    by = c("pitcher", "game_year")
  ) %>%
  inner_join(
    pitch_type_scores %>% select(pitcher, game_year, pitch_type,
                                 avg_tunnel_quality_for_pitch,
                                 avg_release_sim_for_pitch,
                                 avg_approach_div_for_pitch),
    by = c("pitcher", "game_year", "pitch_type")
  ) %>%
  filter(!is.na(delta_run_exp))

cat(sprintf("Prepared %s pitches with full tunneling metrics\n\n",
            format(nrow(model_data), big.mark = ",")))


# Train xRV


cat("Training XGBoost xRV model...\n")
cat("(Predicting run value from tunneling features only)\n\n")

# Features: Arsenal-level + pitch-type-level tunneling
features <- c(
  "tunnel_quality_arsenal",
  "release_similarity_arsenal",
  "approach_divergence_arsenal",
  "avg_tunnel_quality_for_pitch",
  "avg_release_sim_for_pitch",
  "avg_approach_div_for_pitch"
)

target <- "delta_run_exp"

# Prepare training data
train_data <- model_data %>%
  select(all_of(c(features, target))) %>%
  filter(complete.cases(.))

X <- as.matrix(train_data[, features])
y <- train_data[[target]]

dtrain <- xgb.DMatrix(data = X, label = y)

# Train model
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 5,
  eta = 0.03,
  subsample = 0.8,
  colsample_bytree = 0.8
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  verbose = 0
)

# Model performance
xrv_pred <- predict(xgb_model, X)
r_squared <- cor(xrv_pred, y)^2
rmse <- sqrt(mean((xrv_pred - y)^2))

cat(sprintf("Model trained\n"))
cat(sprintf("  R²: %.4f (%.2f%% variance explained)\n", r_squared, r_squared * 100))
cat(sprintf("  RMSE: %.4f\n\n", rmse))

# Feature importance
importance <- xgb.importance(
  feature_names = features,
  model = xgb_model
)

cat("Feature Importance:\n")
print(importance)
cat("\n")


# CALCULATE xRV AND RUNS SAVED


cat("Generating expected run values from tunneling...\n")

# Add xRV predictions to model data
model_data$xrv_tunneling <- predict(xgb_model, as.matrix(select(model_data, all_of(features))))

# Aggregate by pitcher-season
pitcher_performance <- model_data %>%
  group_by(pitcher, player_name, game_year) %>%
  summarize(
    n_pitches_modeled = n(),
    
    # Actual vs expected
    actual_rv_per_pitch = mean(delta_run_exp, na.rm = TRUE),
    xrv_per_pitch = mean(xrv_tunneling, na.rm = TRUE),
    
    # Difference (negative = beats expectations = good)
    rv_diff_per_pitch = actual_rv_per_pitch - xrv_per_pitch,
    
    # Runs saved (negative diff = runs saved)
    runs_saved_per_100 = -rv_diff_per_pitch * 100,
    runs_saved_total = -rv_diff_per_pitch * n_pitches_modeled,
    
    .groups = 'drop'
  )

cat("xRV calculated\n\n")


# T+ Score




cat("Calculating Tunneling+...\n\n")

# Join everything together
# arsenal_scores has 'n' (total pitches in dataset)
# pitcher_performance has 'n_pitches_modeled' (pitches used in modeling)
# Keep both but rename arsenal_scores 'n' to 'n_pitches'
complete_analysis <- arsenal_scores %>%
  rename(n_pitches = n) %>%
  left_join(pitcher_performance, by = c("pitcher", "player_name", "game_year"))

cat(sprintf(" Joined datasets: %d pitcher-seasons\n", nrow(complete_analysis)))
cat("Columns after join:\n")
print(names(complete_analysis))
cat("\n")

# Check if n_pitches exists
if ("n_pitches" %in% names(complete_analysis)) {
  cat(" n_pitches column exists\n\n")
} else {
  cat("✗ ERROR: n_pitches column does NOT exist!\n")
  cat("Something went wrong with the rename\n\n")
  stop("Missing n_pitches column")
}


# Calculate mean and SD of runs saved
runs_saved_mean <- mean(complete_analysis$runs_saved_per_100, na.rm = TRUE)
runs_saved_sd <- sd(complete_analysis$runs_saved_per_100, na.rm = TRUE)

cat(sprintf("Runs Saved Distribution:\n"))
cat(sprintf("  Mean: %.3f runs per 100 pitches\n", runs_saved_mean))
cat(sprintf("  SD: %.3f runs per 100 pitches\n\n", runs_saved_sd))

complete_analysis <- complete_analysis %>%
  mutate(
    # Z-score of runs saved
    runs_saved_zscore = (runs_saved_per_100 - runs_saved_mean) / runs_saved_sd,
    
    # TUNNELING+ = Normalized runs prevented
  
    tunneling_plus = 100 + (runs_saved_zscore * 10),
    
    # TOTAL RUNS SAVED (for this pitcher's actual season)
 
    
    # TUNNELING WINS ABOVE AVERAGE (tWAA) - CUMULATIVE
    tunneling_WAA = runs_saved_total / 10,
    
    # TUNNELING WINS ABOVE AVERAGE per 162 games (tWAA/162) - Rate State
   
    tunneling_WAA_per162 = (runs_saved_per_100 - runs_saved_mean) * 25 / 10,
    
    # Percentiles
    tunnel_quality_pct = percent_rank(tunnel_quality_arsenal) * 100,
    release_similarity_pct = percent_rank(release_similarity_arsenal) * 100,
    approach_divergence_pct = percent_rank(approach_divergence_arsenal) * 100,
    tunneling_plus_pct = percent_rank(tunneling_plus) * 100
  )

cat("Tunneling+ Statistics (Runs Prevented, normalized):\n")
cat(sprintf("  Mean: %.1f (by design)\n", mean(complete_analysis$tunneling_plus, na.rm = TRUE)))
cat(sprintf("  SD: %.1f (by design)\n", sd(complete_analysis$tunneling_plus, na.rm = TRUE)))
cat(sprintf("  Range: %.1f to %.1f\n\n", 
            min(complete_analysis$tunneling_plus, na.rm = TRUE),
            max(complete_analysis$tunneling_plus, na.rm = TRUE)))

cat("Tunneling Wins Above Average (tWAA) Statistics:\n\n")

cat("  CUMULATIVE (tWAA) - Total wins for actual season:\n")
cat(sprintf("    Mean: %.3f wins\n", mean(complete_analysis$tunneling_WAA, na.rm = TRUE)))
cat(sprintf("    SD: %.2f wins\n", sd(complete_analysis$tunneling_WAA, na.rm = TRUE)))
cat(sprintf("    Range: %.2f to +%.2f wins\n", 
            min(complete_analysis$tunneling_WAA, na.rm = TRUE),
            max(complete_analysis$tunneling_WAA, na.rm = TRUE)))
cat(sprintf("    Top 10%%: +%.2f wins or more\n", 
            quantile(complete_analysis$tunneling_WAA, 0.9, na.rm = TRUE)))
cat(sprintf("    Best pitcher: +%.2f wins from tunneling\n\n",
            max(complete_analysis$tunneling_WAA, na.rm = TRUE)))

cat("  RATE STAT (tWAA/162) - Wins per full season:\n")
cat(sprintf("    Mean: %.3f wins (centered at 0 by design)\n", mean(complete_analysis$tunneling_WAA_per162, na.rm = TRUE)))
cat(sprintf("    SD: %.2f wins\n", sd(complete_analysis$tunneling_WAA_per162, na.rm = TRUE)))
cat(sprintf("    Range: %.2f to +%.2f wins per 162\n", 
            min(complete_analysis$tunneling_WAA_per162, na.rm = TRUE),
            max(complete_analysis$tunneling_WAA_per162, na.rm = TRUE)))
cat(sprintf("    Elite (top 10%%): +%.2f wins or more per 162\n", 
            quantile(complete_analysis$tunneling_WAA_per162, 0.9, na.rm = TRUE)))
cat(sprintf("    Poor (bottom 10%%): %.2f wins or less per 162\n\n", 
            quantile(complete_analysis$tunneling_WAA_per162, 0.1, na.rm = TRUE)))

# Show what each 10 point difference means in actual runs
runs_per_10_points <- 10 * runs_saved_sd / 10  # Simplifies to just runs_saved_sd

cat("What Tunneling+ Means:\n")
cat(sprintf("  Each 10 points = %.3f runs per 100 pitches\n", runs_per_10_points))
cat(sprintf("  Tunneling+ 110 (elite):  Prevents %.2f more runs per 100 vs average\n", runs_per_10_points))
cat(sprintf("  Tunneling+ 100 (average): Baseline\n"))
cat(sprintf("  Tunneling+ 90 (poor):    Prevents %.2f fewer runs per 100 vs average\n\n", runs_per_10_points))

# Over a season
cat("Over 2500 pitches (full season):\n")
cat(sprintf("  Elite (110+) prevents %.1f more runs than average\n", runs_per_10_points * 25))
cat(sprintf("  Elite vs Poor (110 vs 90) = %.1f run difference\n", runs_per_10_points * 2 * 25))
cat(sprintf("  Win value: ~%.1f wins\n\n", (runs_per_10_points * 2 * 25) / 10))

# Show distribution
cat("Distribution:\n")
cat(sprintf("  Elite (110+):        %d pitchers (%.1f%%) - Prevent +1 SD more runs\n",
            sum(complete_analysis$tunneling_plus >= 110, na.rm = TRUE),
            100 * mean(complete_analysis$tunneling_plus >= 110, na.rm = TRUE)))
cat(sprintf("  Above Avg (100-110): %d pitchers (%.1f%%)\n",
            sum(complete_analysis$tunneling_plus >= 100 & complete_analysis$tunneling_plus < 110, na.rm = TRUE),
            100 * mean(complete_analysis$tunneling_plus >= 100 & complete_analysis$tunneling_plus < 110, na.rm = TRUE)))
cat(sprintf("  Below Avg (90-100):  %d pitchers (%.1f%%)\n",
            sum(complete_analysis$tunneling_plus >= 90 & complete_analysis$tunneling_plus < 100, na.rm = TRUE),
            100 * mean(complete_analysis$tunneling_plus >= 90 & complete_analysis$tunneling_plus < 100, na.rm = TRUE)))
cat(sprintf("  Poor (<90):          %d pitchers (%.1f%%) - Prevent -1 SD fewer runs\n\n",
            sum(complete_analysis$tunneling_plus < 90, na.rm = TRUE),
            100 * mean(complete_analysis$tunneling_plus < 90, na.rm = TRUE)))

# Show relationship between quality and runs prevented
cor_quality_runs <- cor(complete_analysis$tunnel_quality_arsenal, 
                        complete_analysis$runs_saved_per_100, 
                        use = "complete.obs")

cat(sprintf("Correlation between tunnel_quality and runs_prevented: %.3f\n", cor_quality_runs))


# Quartile Analysis



# Create quartiles based on TUNNELING+ (effect on xRV)
tunneling_plus_breaks <- quantile(complete_analysis$tunneling_plus,
                                  probs = c(0, 0.25, 0.5, 0.75, 1),
                                  na.rm = TRUE)

complete_analysis <- complete_analysis %>%
  mutate(
    tunneling_plus_quartile = case_when(
      tunneling_plus <= tunneling_plus_breaks[2] ~ "Q1 (Bottom 25%)",
      tunneling_plus <= tunneling_plus_breaks[3] ~ "Q2 (25-50%)",
      tunneling_plus <= tunneling_plus_breaks[4] ~ "Q3 (50-75%)",
      TRUE ~ "Q4 (Top 25%)"
    ),
    tunneling_plus_quartile_num = case_when(
      tunneling_plus <= tunneling_plus_breaks[2] ~ 1,
      tunneling_plus <= tunneling_plus_breaks[3] ~ 2,
      tunneling_plus <= tunneling_plus_breaks[4] ~ 3,
      TRUE ~ 4
    )
  )

# Summarize by Tunneling+ quartile
tunneling_plus_quartile_summary <- complete_analysis %>%
  group_by(tunneling_plus_quartile) %>%
  summarize(
    n_pitchers = n(),
    
    # Tunneling+ score (effect on xRV)
    avg_tunneling_plus = mean(tunneling_plus, na.rm = TRUE),
    
    # Underlying tunnel quality scores
    avg_tunnel_quality = mean(tunnel_quality_arsenal, na.rm = TRUE),
    avg_release_similarity = mean(release_similarity_arsenal, na.rm = TRUE),
    avg_approach_divergence = mean(approach_divergence_arsenal, na.rm = TRUE),
    
    # Performance
    avg_actual_rv = mean(actual_rv_per_pitch, na.rm = TRUE),
    avg_xrv = mean(xrv_per_pitch, na.rm = TRUE),
    avg_runs_saved_per_100 = mean(runs_saved_per_100, na.rm = TRUE),
    median_runs_saved_per_100 = median(runs_saved_per_100, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_tunneling_plus))

cat("Quartile Summary (by Tunneling+ Effect):\n\n")
print(tunneling_plus_quartile_summary)
cat("\n")

# Calculate difference
top_quartile <- tunneling_plus_quartile_summary %>% filter(grepl("Q4", tunneling_plus_quartile))
bottom_quartile <- tunneling_plus_quartile_summary %>% filter(grepl("Q1", tunneling_plus_quartile))

tunneling_plus_diff <- top_quartile$avg_tunneling_plus - bottom_quartile$avg_tunneling_plus
runs_diff_per_100 <- top_quartile$avg_runs_saved_per_100 - bottom_quartile$avg_runs_saved_per_100
runs_diff_per_season <- runs_diff_per_100 * 25  # 2500 pitches

cat(sprintf("Top Quartile vs Bottom Quartile (by Tunneling+ Effect):\n"))
cat(sprintf("  Tunneling+: %.1f vs %.1f (diff: %.1f)\n",
            top_quartile$avg_tunneling_plus,
            bottom_quartile$avg_tunneling_plus,
            tunneling_plus_diff))
cat(sprintf("  Tunnel Quality: %.3f vs %.3f (diff: %.3f)\n",
            top_quartile$avg_tunnel_quality,
            bottom_quartile$avg_tunnel_quality,
            top_quartile$avg_tunnel_quality - bottom_quartile$avg_tunnel_quality))
cat(sprintf("  Runs saved per 100: %.2f vs %.2f (diff: %.2f)\n",
            top_quartile$avg_runs_saved_per_100,
            bottom_quartile$avg_runs_saved_per_100,
            runs_diff_per_100))
cat(sprintf("  Over 2500 pitches: %.1f runs\n", runs_diff_per_season))
cat(sprintf("  Win value: ~%.1f wins\n\n", runs_diff_per_season / 10))

# SD


# Group by Tunneling+ (effect on xRV)
complete_analysis <- complete_analysis %>%
  mutate(
    tunneling_plus_tier = case_when(
      tunneling_plus >= 110 ~ "Elite (110+, +1 SD effect)",
      tunneling_plus >= 100 ~ "Above Average (100-110)",
      tunneling_plus >= 90 ~ "Below Average (90-100)",
      TRUE ~ "Poor (<90, -1 SD effect)"
    )
  )

tunneling_plus_tier_summary <- complete_analysis %>%
  group_by(tunneling_plus_tier) %>%
  summarize(
    n_pitchers = n(),
    pct_of_total = n() / nrow(complete_analysis) * 100,
    
    # Effect score
    avg_tunneling_plus = mean(tunneling_plus, na.rm = TRUE),
    avg_runs_saved_per_100 = mean(runs_saved_per_100, na.rm = TRUE),
    
    # Underlying quality
    avg_tunnel_quality = mean(tunnel_quality_arsenal, na.rm = TRUE),
    avg_release_similarity = mean(release_similarity_arsenal, na.rm = TRUE),
    avg_approach_divergence = mean(approach_divergence_arsenal, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_tunneling_plus))

cat("Tunneling+ Tier Summary (by Effect on xRV):\n\n")
print(tunneling_plus_tier_summary)
cat("\n")

# Elite vs Poor difference
elite <- tunneling_plus_tier_summary %>% filter(grepl("Elite", tunneling_plus_tier))
poor <- tunneling_plus_tier_summary %>% filter(grepl("Poor", tunneling_plus_tier))

elite_poor_diff <- elite$avg_runs_saved_per_100 - poor$avg_runs_saved_per_100

cat(sprintf("Elite (110+) vs Poor (<90) Effect:\n"))
cat(sprintf("  Tunneling+: %.1f vs %.1f\n", elite$avg_tunneling_plus, poor$avg_tunneling_plus))
cat(sprintf("  Tunnel Quality: %.3f vs %.3f\n", elite$avg_tunnel_quality, poor$avg_tunnel_quality))
cat(sprintf("  Runs saved per 100: %.2f vs %.2f\n", elite$avg_runs_saved_per_100, poor$avg_runs_saved_per_100))
cat(sprintf("  Difference: %.2f runs per 100 pitches\n", elite_poor_diff))
cat(sprintf("  Over 2500 pitches: %.1f runs per season\n", elite_poor_diff * 25))
cat(sprintf("  Win value: ~%.1f wins\n\n", elite_poor_diff * 25 / 10))

# Save



# Debug
cat("Columns in complete_analysis:\n")
print(names(complete_analysis))
cat("\n")

# Main analysis file
write.csv(complete_analysis,
          file.path(OUTPUT_DIR, "tunneling_analysis_complete.csv"),
          row.names = FALSE)
cat(" Saved: tunneling_analysis_complete.csv\n")

# Quartile summary (by Tunneling+ effect)
write.csv(tunneling_plus_quartile_summary,
          file.path(OUTPUT_DIR, "tunneling_plus_quartile_summary.csv"),
          row.names = FALSE)
cat(" Saved: tunneling_plus_quartile_summary.csv\n")

# Tier summary (by Tunneling+ effect)
write.csv(tunneling_plus_tier_summary,
          file.path(OUTPUT_DIR, "tunneling_plus_tier_summary.csv"),
          row.names = FALSE)
cat(" Saved: tunneling_plus_tier_summary.csv\n")

# Pitch-type scores
write.csv(pitch_type_scores,
          file.path(OUTPUT_DIR, "pitch_type_tunneling_scores.csv"),
          row.names = FALSE)
cat(" Saved: pitch_type_tunneling_scores.csv\n")

# Top tunnelers (by Tunneling+ effect on xRV)
# Note: Excluding n_pitches from selection due to naming conflicts
top_tunnelers <- complete_analysis %>%
  arrange(desc(tunneling_plus)) %>%
  select(player_name, game_year, pitch_mix, 
         n_pitch_types,
         # Tunneling+ = effect on xRV
         tunneling_plus, tunneling_WAA, tunneling_WAA_per162,
         runs_saved_per_100, runs_saved_total,
         # Underlying quality scores
         tunnel_quality_arsenal, release_similarity_arsenal, approach_divergence_arsenal,
         tunnel_quality_pct, tunneling_plus_pct,
         # Performance
         actual_rv_per_pitch, xrv_per_pitch,
         tunneling_plus_tier, tunneling_plus_quartile)

write.csv(head(top_tunnelers, 100),
          file.path(OUTPUT_DIR, "top_100_tunnelers.csv"),
          row.names = FALSE)
cat(" Saved: top_100_tunnelers.csv\n\n")


# Final


cat("TOP 10 RUN PREVENTERS (by Tunneling+):\n")
cat("(Tunneling+ = normalized runs prevented per 100 pitches)\n")
cat("(tWAA = Cumulative wins from tunneling this season)\n")
cat("(tWAA/162 = Wins per full season rate stat)\n\n")
print(head(top_tunnelers %>%
             select(player_name, game_year, tunneling_plus, 
                    tunneling_WAA, tunneling_WAA_per162,
                    runs_saved_per_100, tunnel_quality_arsenal), 10))

cat("\n\nKEY FINDINGS:\n\n")
cat("What is Tunneling+?\n")
cat("  • Direct measure of RUNS PREVENTED by tunneling\n")
cat("  • Normalized to 100 scale (SD = 10) for easy comparison\n")
cat("  • Based on: actual_rv vs xrv_tunneling\n")
cat(sprintf("  • Each 10 points = %.3f runs per 100 pitches\n", runs_per_10_points))
cat("  • Higher Tunneling+ = More runs prevented\n\n")

cat("What is Tunneling WAA (tWAA)?\n")
cat("  • CUMULATIVE: Total wins from tunneling THIS season\n")
cat("  • Based on actual pitches thrown (varies by pitcher)\n")
cat("  • Example: tWAA = +2.5 means gained 2.5 wins from tunneling\n\n")

cat("What is tWAA/162?\n")
cat("  • RATE STAT: Wins per full season (2500 pitches)\n")
cat("  • Comparable across pitchers regardless of workload\n")
cat("  • Like ERA vs ERA+, this normalizes to full season\n\n")

cat("Scale:\n")
cat(sprintf("  Tunneling+ 110 = Prevents %.2f runs per 100 vs average pitcher\n", runs_per_10_points))
cat("  Tunneling+ 100 = Average run prevention (baseline)\n")
cat(sprintf("  Tunneling+ 90  = Prevents %.2f FEWER runs per 100 vs average\n\n", runs_per_10_points))

cat("Score Distributions:\n")
cat(sprintf("  Runs Saved: %.3f ± %.3f runs per 100 pitches\n",
            runs_saved_mean, runs_saved_sd))
cat(sprintf("  Tunnel Quality: %.3f - %.3f (0-1 scale)\n",
            min(complete_analysis$tunnel_quality_arsenal, na.rm = TRUE),
            max(complete_analysis$tunnel_quality_arsenal, na.rm = TRUE)))
cat(sprintf("  Release Similarity: %.3f - %.3f (0-1 scale)\n",
            min(complete_analysis$release_similarity_arsenal, na.rm = TRUE),
            max(complete_analysis$release_similarity_arsenal, na.rm = TRUE)))
cat(sprintf("  Approach Divergence: %.3f - %.3f (0-1 scale)\n\n",
            min(complete_analysis$approach_divergence_arsenal, na.rm = TRUE),
            max(complete_analysis$approach_divergence_arsenal, na.rm = TRUE)))

cat("Impact of Tunneling:\n")
cat(sprintf("  xRV Model R²: %.4f (%.2f%% of run value variance)\n", r_squared, r_squared * 100))
cat(sprintf("  Top 25%% vs Bottom 25%%: %.1f runs per season\n", runs_diff_per_season))
cat(sprintf("  Elite (110+) vs Poor (<90): %.1f runs per season (~%.1f wins)\n\n",
            elite_poor_diff * 25, elite_poor_diff * 25 / 10))

cat("Tunneling+ Distribution:\n")
cat(sprintf("  Elite (110+):        %d pitchers (%.1f%%) - Elite run prevention\n",
            sum(complete_analysis$tunneling_plus >= 110, na.rm = TRUE),
            100 * mean(complete_analysis$tunneling_plus >= 110, na.rm = TRUE)))
cat(sprintf("  Above Avg (100-110): %d pitchers (%.1f%%) - Above average prevention\n",
            sum(complete_analysis$tunneling_plus >= 100 & complete_analysis$tunneling_plus < 110, na.rm = TRUE),
            100 * mean(complete_analysis$tunneling_plus >= 100 & complete_analysis$tunneling_plus < 110, na.rm = TRUE)))
cat(sprintf("  Below Avg (90-100):  %d pitchers (%.1f%%) - Below average prevention\n",
            sum(complete_analysis$tunneling_plus >= 90 & complete_analysis$tunneling_plus < 100, na.rm = TRUE),
            100 * mean(complete_analysis$tunneling_plus >= 90 & complete_analysis$tunneling_plus < 100, na.rm = TRUE)))
cat(sprintf("  Poor (<90):          %d pitchers (%.1f%%) - Poor prevention\n\n",
            sum(complete_analysis$tunneling_plus < 90, na.rm = TRUE),
            100 * mean(complete_analysis$tunneling_plus < 90, na.rm = TRUE)))

cat("Quality vs Prevention:\n")
cat(sprintf("  Correlation (quality → runs): r = %.3f\n", cor_quality_runs))
cat("  (Shows how well quality metrics predict actual run prevention)\n\n")

cat("Files Created:\n")
cat("  • tunneling_analysis_complete.csv - All pitcher-seasons\n")
cat("      - tunneling_plus = RUNS PREVENTED (100-scale)\n")
cat("      - runs_saved_per_100 = raw runs prevented\n")
cat("      - tunnel_quality, release_similarity, approach_divergence (0-1 scale)\n")
cat("  • tunneling_plus_quartile_summary.csv - By run prevention quartile\n")
cat("  • tunneling_plus_tier_summary.csv - Elite vs Poor run preventers\n")
cat("  • pitch_type_tunneling_scores.csv - Individual pitch metrics\n")
cat("  • top_100_tunnelers.csv - Top 100 run preventers\n\n")
