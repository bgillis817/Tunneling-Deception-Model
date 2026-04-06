# 02_compute_tunneling.R
# Calculates pitch angles (VRA, HRA, VAA, HAA) and KDE tunneling scores
# Adapted for flat repo structure (bgillis817/Tunneling-Deception-Model)
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(data.table)
})

# ---------------------------------------------------------------------------
# CONFIG — flat structure, temp data in statcast_combined/
# ---------------------------------------------------------------------------
INPUT_FILE  <- "statcast_combined/statcast_combined_latest.rds"
OUTPUT_FILE <- "statcast_combined/statcast_with_tunneling_full.rds"

# ---------------------------------------------------------------------------
# ANGLE CALCULATIONS (unchanged from your tunnelingstatcast.R)
# ---------------------------------------------------------------------------
add_pitch_angles <- function(df) {
  df %>%
    mutate(
      vy_s = -sqrt(vy0^2 - 2 * ay * (60.5 - release_extension - 50)),
      t_s  = (vy_s - vy0) / ay,
      vz_s = vz0 - az * t_s,
      VRA  = -atan(vz_s / vy_s) * (180 / pi),
      vx_s = vx0 - ax * t_s,
      HRA  = -atan(vx_s / vy_s) * (180 / pi),
      yf   = 17 / 12,
      vy_f = -sqrt(vy0^2 - (2 * ay * (50 - yf))),
      t    = (vy_f - vy0) / ay,
      vz_f = vz0 + (az * t),
      vaa_rad = atan2(vz_f, vy_f),
      VAA  = (180 + (vaa_rad * 180 / pi)) * -1,
      vx_f = vx0 + (ax * t),
      HAA  = -atan(vx_f / vy_f) * (180 / pi)
    ) %>%
    select(-vy_s, -t_s, -vz_s, -vx_s, -yf, -vaa_rad, -vx_f, -vz_f)
}

# ---------------------------------------------------------------------------
# KDE SCORING (unchanged from your tunnelingstatcast.R)
# ---------------------------------------------------------------------------
add_kde_scores_fast <- function(df, features = c("VRA", "HRA", "VAA", "HAA")) {
  dt <- as.data.table(df)
  for (feature in features) dt[[paste0(feature, "_KDE")]] <- NA_real_

  dt[, group_key := paste(game_year, pitcher, stand, sep = "_")]
  groups <- unique(dt$group_key)
  n_groups <- length(groups)

  cat(sprintf("  Processing %s groups...\n", format(n_groups, big.mark = ",")))
  start_time <- Sys.time()
  pb <- txtProgressBar(min = 0, max = n_groups, style = 3, width = 50)

  for (i in seq_along(groups)) {
    group_data <- dt[group_key == groups[i]]
    pitch_types <- unique(group_data$pitch_type)
    if (length(pitch_types) <= 1) { setTxtProgressBar(pb, i); next }

    for (pt in pitch_types) {
      current_idx <- which(group_data$pitch_type == pt)
      other_idx   <- which(group_data$pitch_type != pt)
      if (length(other_idx) < 5) next

      for (feature in features) {
        other_vals <- group_data[[feature]][other_idx]
        other_vals <- other_vals[!is.na(other_vals)]
        if (length(other_vals) < 5) next

        current_vals <- group_data[[feature]][current_idx]
        valid <- !is.na(current_vals)
        if (!any(valid)) next

        h <- 1.06 * sd(other_vals) * length(other_vals)^(-1/5)

        kde_vals <- sapply(current_vals[valid], function(val) {
          mean(dnorm(val, mean = other_vals, sd = h))
        })

        global_idx <- which(dt$group_key == groups[i] &
                              dt$pitch_type == pt &
                              !is.na(dt[[feature]]))
        if (length(global_idx) == length(kde_vals)) {
          dt[[paste0(feature, "_KDE")]][global_idx] <- kde_vals
        }
      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  cat(sprintf("\n  Complete in %.1f minutes\n\n", elapsed))

  dt[, group_key := NULL]
  as.data.frame(dt)
}

# ---------------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------------
cat("Loading combined Statcast data...\n")
df <- readRDS(INPUT_FILE)
cat(sprintf("  Loaded %s pitches\n\n", format(nrow(df), big.mark = ",")))

# Filter to complete cases for tunneling columns
required <- c("vy0", "ay", "release_extension", "vz0", "az",
              "vx0", "ax", "game_year", "pitcher", "stand",
              "pitch_type", "player_name")
df <- df %>% filter(complete.cases(across(all_of(required))))
cat(sprintf("  Complete cases: %s pitches\n\n", format(nrow(df), big.mark = ",")))

# Process year by year (keeps memory manageable)
years <- sort(unique(df$game_year))
all_results <- list()

for (year in years) {
  cat(sprintf("=== %d ===\n", year))
  df_year <- df %>% filter(game_year == year)
  cat(sprintf("  %s pitches\n", format(nrow(df_year), big.mark = ",")))

  cat("  Calculating angles...\n")
  df_year <- add_pitch_angles(df_year)

  cat("  Calculating KDE scores...\n")
  df_year <- add_kde_scores_fast(df_year)

  all_results[[as.character(year)]] <- df_year
}

# Combine and save
df_combined <- bind_rows(all_results)
saveRDS(df_combined, OUTPUT_FILE)
cat(sprintf("\nSaved: %s (%s pitches)\n",
            OUTPUT_FILE, format(nrow(df_combined), big.mark = ",")))
