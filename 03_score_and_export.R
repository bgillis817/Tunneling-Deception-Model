# 03_score_and_export.R
# ===========================================================================
# SHAP-based Tunneling Metrics Pipeline
# Adapted for flat repo structure (bgillis817/Tunneling-Deception-Model)
#
# Outputs go to repo root — column names match existing app.R exactly
#
# Methodology (from capstone):
#   1. Train XGBoost on ALL available features predicting delta_run_exp
#   2. Extract SHAP values for every pitch
#   3. Isolate the 4 tunneling KDE features' SHAP contributions
#   4. Aggregate per-pitcher-season -> runs saved per 100 pitches
#   5. Standardize -> Tunneling+ (100 +/- 10 scale)
#   6. Scale to wins -> tWAA (cumulative) and tWAA/162 (rate)
#   7. Build sequential pitch-pair matrices from SHAP contributions
#   8. Export CSVs to repo root for the Shiny app
# ===========================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(xgboost)
  library(readr)
})

# ---------------------------------------------------------------------------
# CONFIG
# ---------------------------------------------------------------------------
INPUT_FILE <- "statcast_combined/statcast_with_tunneling_full.rds"
OUTPUT_DIR <- "."
MIN_PITCHES <- 48
RUNS_PER_WIN <- 10

# ---------------------------------------------------------------------------
# LOAD DATA
# ---------------------------------------------------------------------------
cat("Loading tunneling data...\n")
df <- readRDS(INPUT_FILE)
cat(sprintf("  %s pitches\n\n", format(nrow(df), big.mark = ",")))

# ---------------------------------------------------------------------------
# STEP 1: PREPARE FULL FEATURE SET FOR XGBOOST
# ---------------------------------------------------------------------------
tunneling_features <- c("VRA_KDE", "HRA_KDE", "VAA_KDE", "HAA_KDE")

context_features <- c(
  "plate_x", "plate_z",
  "release_speed", "release_spin_rate",
  "release_extension", "release_pos_x", "release_pos_z",
  "pfx_x", "pfx_z",
  "ax", "az", "vx0", "vz0",
  "spin_axis",
  "balls", "strikes"
)

all_features <- c(context_features, tunneling_features)
target <- "delta_run_exp"

required_cols <- c(all_features, target, "pitcher", "player_name",
                   "game_year", "pitch_type", "stand",
                   "game_pk", "at_bat_number", "pitch_number")

model_data <- df %>%
  select(any_of(required_cols)) %>%
  filter(complete.cases(.))

cat(sprintf("  Complete cases for modeling: %s pitches\n",
            format(nrow(model_data), big.mark = ",")))

valid_features <- all_features[sapply(all_features, function(f) {
  f %in% names(model_data) && sd(model_data[[f]], na.rm = TRUE) > 0
})]

cat(sprintf("  Features with variance: %d / %d\n\n", length(valid_features), length(all_features)))

# ---------------------------------------------------------------------------
# STEP 2: TRAIN XGBOOST
# ---------------------------------------------------------------------------
cat("Training XGBoost model...\n")

X <- as.matrix(model_data[, valid_features])
y <- model_data[[target]]
dtrain <- xgb.DMatrix(data = X, label = y)

xgb_model <- xgb.train(
  params = list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = 5,
    eta = 0.03,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  data = dtrain,
  nrounds = 500,
  verbose = 0
)

preds <- predict(xgb_model, X)
r2 <- cor(preds, y)^2
rmse_val <- sqrt(mean((preds - y)^2))
cat(sprintf("  R2: %.4f | RMSE: %.4f | Features: %d\n\n", r2, rmse_val, length(valid_features)))

importance <- xgb.importance(feature_names = valid_features, model = xgb_model)
cat("Top 10 features by gain:\n")
print(head(importance, 10))
cat("\n")

# ---------------------------------------------------------------------------
# STEP 3: EXTRACT SHAP VALUES
# ---------------------------------------------------------------------------
cat("Extracting SHAP values (this may take a few minutes)...\n")

shap_values <- predict(xgb_model, dtrain, predcontrib = TRUE)
shap_df <- as.data.frame(shap_values)
names(shap_df) <- c(valid_features, "BIAS")

cat(sprintf("  SHAP matrix: %s rows x %d columns\n\n",
            format(nrow(shap_df), big.mark = ","), ncol(shap_df)))

# ---------------------------------------------------------------------------
# STEP 4: ISOLATE TUNNELING SHAP CONTRIBUTIONS
# ---------------------------------------------------------------------------
tunneling_shap_cols <- intersect(tunneling_features, names(shap_df))
cat("Tunneling SHAP features found:", paste(tunneling_shap_cols, collapse = ", "), "\n")

model_data$tunneling_shap_total <- rowSums(shap_df[, tunneling_shap_cols, drop = FALSE])

for (col in tunneling_shap_cols) {
  model_data[[paste0("shap_", col)]] <- shap_df[[col]]
}

tunneling_importance <- importance %>% filter(Feature %in% tunneling_shap_cols)
cat("\nTunneling feature importance (gain):\n")
print(tunneling_importance)

total_tunneling_gain <- sum(tunneling_importance$Gain)
cat("\nSignal breakdown:\n")
for (i in seq_len(nrow(tunneling_importance))) {
  cat(sprintf("  %s: %.4f (%.1f%%)\n",
              tunneling_importance$Feature[i],
              tunneling_importance$Gain[i],
              100 * tunneling_importance$Gain[i] / total_tunneling_gain))
}

v_gain <- sum(tunneling_importance$Gain[tunneling_importance$Feature %in% c("VRA_KDE", "VAA_KDE")])
h_gain <- sum(tunneling_importance$Gain[tunneling_importance$Feature %in% c("HRA_KDE", "HAA_KDE")])
r_gain <- sum(tunneling_importance$Gain[tunneling_importance$Feature %in% c("VRA_KDE", "HRA_KDE")])
a_gain <- sum(tunneling_importance$Gain[tunneling_importance$Feature %in% c("VAA_KDE", "HAA_KDE")])
cat(sprintf("\n  Vertical: %.1f%% | Horizontal: %.1f%%\n", 100*v_gain/total_tunneling_gain, 100*h_gain/total_tunneling_gain))
cat(sprintf("  Release:  %.1f%% | Approach:   %.1f%%\n\n", 100*r_gain/total_tunneling_gain, 100*a_gain/total_tunneling_gain))

# ---------------------------------------------------------------------------
# STEP 5: AGGREGATE TO PITCHER-SEASON LEVEL
# ---------------------------------------------------------------------------
cat("Aggregating to pitcher-season level...\n")

pitcher_shap <- model_data %>%
  group_by(pitcher, player_name, game_year) %>%
  summarize(
    n_pitches = n(),
    n_pitch_types = n_distinct(pitch_type),
    total_tunneling_runs_saved = -sum(tunneling_shap_total, na.rm = TRUE),
    tunneling_runs_saved_per100 = -(mean(tunneling_shap_total, na.rm = TRUE) * 100),
    vra_runs_saved_per100 = -(mean(shap_VRA_KDE, na.rm = TRUE) * 100),
    hra_runs_saved_per100 = -(mean(shap_HRA_KDE, na.rm = TRUE) * 100),
    vaa_runs_saved_per100 = -(mean(shap_VAA_KDE, na.rm = TRUE) * 100),
    haa_runs_saved_per100 = -(mean(shap_HAA_KDE, na.rm = TRUE) * 100),
    vra_runs_saved_total = -(sum(shap_VRA_KDE, na.rm = TRUE)),
    hra_runs_saved_total = -(sum(shap_HRA_KDE, na.rm = TRUE)),
    vaa_runs_saved_total = -(sum(shap_VAA_KDE, na.rm = TRUE)),
    haa_runs_saved_total = -(sum(shap_HAA_KDE, na.rm = TRUE)),
    release_similarity_runs_saved_total = -(sum(shap_VRA_KDE + shap_HRA_KDE, na.rm = TRUE)),
    approach_divergence_runs_saved_total = -(sum(shap_VAA_KDE + shap_HAA_KDE, na.rm = TRUE)),
    release_similarity_runs_saved_per100 = -(mean(shap_VRA_KDE + shap_HRA_KDE, na.rm = TRUE) * 100),
    approach_divergence_runs_saved_per100 = -(mean(shap_VAA_KDE + shap_HAA_KDE, na.rm = TRUE) * 100),
    avg_VRA_KDE = mean(VRA_KDE, na.rm = TRUE),
    avg_HRA_KDE = mean(HRA_KDE, na.rm = TRUE),
    avg_VAA_KDE = mean(VAA_KDE, na.rm = TRUE),
    avg_HAA_KDE = mean(HAA_KDE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_pitches >= MIN_PITCHES)

cat(sprintf("  %d pitcher-seasons (>= %d pitches)\n\n", nrow(pitcher_shap), MIN_PITCHES))

# ---------------------------------------------------------------------------
# STEP 6: TUNNELING+ AND tWAA
# ---------------------------------------------------------------------------
mean_r100 <- mean(pitcher_shap$tunneling_runs_saved_per100, na.rm = TRUE)
sd_r100 <- sd(pitcher_shap$tunneling_runs_saved_per100, na.rm = TRUE)

cat(sprintf("League tunneling: Mean = %.5f R/100, SD = %.5f R/100\n\n", mean_r100, sd_r100))

scores <- pitcher_shap %>%
  mutate(
    runs_saved_z_score = (tunneling_runs_saved_per100 - mean_r100) / sd_r100,
    tunneling_plus = 100 + (runs_saved_z_score * 10),
    expected_runs = mean_r100 * (n_pitches / 100),
    runs_saved_above_avg = total_tunneling_runs_saved - expected_runs,
    runs_above_avg_per_100 = tunneling_runs_saved_per100 - mean_r100,
    tunneling_WAA = runs_saved_above_avg / RUNS_PER_WIN,
    tunneling_WAA_per162 = (runs_above_avg_per_100 * 25) / RUNS_PER_WIN,
    release_similarity = (avg_VRA_KDE + avg_HRA_KDE) / 2,
    approach_divergence = 1 - ((avg_VAA_KDE + avg_HAA_KDE) / 2),
    tunnel_quality = (release_similarity + approach_divergence) / 2,
    tunneling_plus_pct = percent_rank(tunneling_plus) * 100,
    tunneling_WAA_pct = percent_rank(tunneling_WAA) * 100,
    tunneling_WAA_per162_pct = percent_rank(tunneling_WAA_per162) * 100,
    tunnel_quality_pct = percent_rank(tunnel_quality) * 100,
    release_similarity_pct = percent_rank(release_similarity) * 100,
    approach_divergence_pct = percent_rank(approach_divergence) * 100
  ) %>%
  arrange(desc(tunneling_plus))

cat("Tunneling+ distribution:\n")
cat(sprintf("  Mean: %.1f | SD: %.1f\n", mean(scores$tunneling_plus), sd(scores$tunneling_plus)))
cat(sprintf("  Range: %.1f to %.1f\n\n", min(scores$tunneling_plus), max(scores$tunneling_plus)))

# ---------------------------------------------------------------------------
# STEP 7: SEQUENTIAL PITCH-PAIR MATRICES (SHAP-based)
# ---------------------------------------------------------------------------
cat("Building pitch-pair sequencing matrices...\n")

# Pitch-type level — column names match app.R expectations
pitch_type_shap <- model_data %>%
  group_by(pitcher, player_name, game_year, pitch_type) %>%
  summarize(
    n = n(),
    avg_tunnel_quality_for_pitch = -(mean(tunneling_shap_total, na.rm = TRUE) * 100),
    avg_release_sim_for_pitch = -(mean(shap_VRA_KDE + shap_HRA_KDE, na.rm = TRUE) * 100),
    avg_approach_div_for_pitch = -(mean(shap_VAA_KDE + shap_HAA_KDE, na.rm = TRUE) * 100),
    avg_VRA_KDE = mean(VRA_KDE, na.rm = TRUE),
    avg_HRA_KDE = mean(HRA_KDE, na.rm = TRUE),
    avg_VAA_KDE = mean(VAA_KDE, na.rm = TRUE),
    avg_HAA_KDE = mean(HAA_KDE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n >= 50)

# Build pitch pairs using lag()
pairs_data <- model_data %>%
  arrange(pitcher, game_year, game_pk, at_bat_number, pitch_number) %>%
  group_by(pitcher, game_year) %>%
  mutate(
    prev_pitch_type = lag(pitch_type),
    prev_tunneling_shap = lag(tunneling_shap_total)
  ) %>%
  ungroup() %>%
  filter(!is.na(prev_pitch_type))

# League-wide matrix
league_matrix <- pairs_data %>%
  group_by(prev_pitch_type, pitch_type) %>%
  summarize(
    n_pairs = n(),
    runs_per_100 = -(mean(tunneling_shap_total, na.rm = TRUE) * 100),
    .groups = "drop"
  ) %>%
  filter(n_pairs >= 100)

cat(sprintf("  League matrix: %d pitch-pair types\n", nrow(league_matrix)))

# Pitcher-specific matrices — column names match app.R expectations
# (first_pitch, second_pitch, xrv_per_100, xrv_total)
pitcher_matrix <- pairs_data %>%
  group_by(pitcher, player_name, game_year, prev_pitch_type, pitch_type) %>%
  summarize(
    n_pairs = n(),
    xrv_per_100 = -(mean(tunneling_shap_total, na.rm = TRUE) * 100),
    xrv_total = -(sum(tunneling_shap_total, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  rename(first_pitch = prev_pitch_type, second_pitch = pitch_type) %>%
  filter(n_pairs >= 20)

cat(sprintf("  Pitcher matrices: %s pair entries\n\n",
            format(nrow(pitcher_matrix), big.mark = ",")))

# ---------------------------------------------------------------------------
# STEP 8: EXPORT TO REPO ROOT
# Column names match existing app.R exactly:
#   scores.csv: tunnel_quality_arsenal, release_similarity_arsenal,
#               approach_divergence_arsenal, runs_saved_per_100
#   pitcher_pitch_pair_combos_all.csv: first_pitch, second_pitch,
#               xrv_per_100, xrv_total
#   pitch_type_tunneling_scores.csv: avg_tunnel_quality_for_pitch,
#               avg_release_sim_for_pitch, avg_approach_div_for_pitch
# ---------------------------------------------------------------------------
cat("Exporting to repo root...\n")

scores_export <- scores %>%
  transmute(
    player_name, pitcher, game_year, n_pitches,
    tunneling_plus, tunneling_WAA, tunneling_WAA_per162,
    # App column names with _arsenal suffix
    tunnel_quality_arsenal = tunnel_quality,
    release_similarity_arsenal = release_similarity,
    approach_divergence_arsenal = approach_divergence,
    runs_saved_per_100 = tunneling_runs_saved_per100,
    runs_above_avg_per_100,
    total_tunneling_runs_saved, runs_saved_above_avg,
    vra_runs_saved_total, hra_runs_saved_total,
    vaa_runs_saved_total, haa_runs_saved_total,
    vra_runs_saved_per100, hra_runs_saved_per100,
    vaa_runs_saved_per100, haa_runs_saved_per100,
    release_similarity_runs_saved_total, approach_divergence_runs_saved_total,
    release_similarity_runs_saved_per100, approach_divergence_runs_saved_per100,
    avg_VRA_KDE, avg_HRA_KDE, avg_VAA_KDE, avg_HAA_KDE,
    tunneling_plus_pct, tunneling_WAA_pct, tunneling_WAA_per162_pct,
    tunnel_quality_pct, release_similarity_pct, approach_divergence_pct
  )

write_csv(scores_export, file.path(OUTPUT_DIR, "scores.csv"))
cat("  scores.csv\n")

write_csv(pitch_type_shap, file.path(OUTPUT_DIR, "pitch_type_tunneling_scores.csv"))
cat("  pitch_type_tunneling_scores.csv\n")

write_csv(league_matrix, file.path(OUTPUT_DIR, "pitch_pair_xrv_matrix.csv"))
cat("  pitch_pair_xrv_matrix.csv\n")

write_csv(pitcher_matrix, file.path(OUTPUT_DIR, "pitcher_pitch_pair_combos_all.csv"))
cat("  pitcher_pitch_pair_combos_all.csv\n")

write_csv(head(scores_export, 100), file.path(OUTPUT_DIR, "top_100_tunnelers.csv"))
cat("  top_100_tunnelers.csv\n")

metadata <- list(
  r_squared = r2,
  rmse = rmse_val,
  n_features = length(valid_features),
  n_pitcher_seasons = nrow(scores),
  min_pitches = MIN_PITCHES,
  runs_per_win = RUNS_PER_WIN,
  tunneling_mean_r100 = mean_r100,
  tunneling_sd_r100 = sd_r100,
  updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC")
)
writeLines(
  paste(names(metadata), metadata, sep = ": "),
  file.path(OUTPUT_DIR, "model_metadata.txt")
)
cat("  model_metadata.txt\n")

writeLines(
  format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC"),
  file.path(OUTPUT_DIR, "last_updated.txt")
)
cat("  last_updated.txt\n")

# ---------------------------------------------------------------------------
# SUMMARY
# ---------------------------------------------------------------------------
cat(sprintf("\n=== DONE ===\n"))
cat(sprintf("  Pitcher-seasons scored: %d\n", nrow(scores)))
cat(sprintf("  Model R2: %.4f (full model, %d features)\n", r2, length(valid_features)))
cat(sprintf("  Tunneling SD: %.5f runs/100 pitches\n", sd_r100))
cat(sprintf("  Tunneling+ range: %.1f to %.1f\n",
            min(scores$tunneling_plus), max(scores$tunneling_plus)))

cat("\nTop 10 by Tunneling+:\n")
print(
  scores %>%
    select(player_name, game_year, tunneling_plus, tunneling_WAA,
           tunnel_quality, tunneling_runs_saved_per100) %>%
    head(10),
  n = 10
)
