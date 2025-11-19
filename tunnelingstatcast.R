RDS_PATH <- "C:/Users/ecloaner/Downloads/TunnellOrientedPitchingModel/statcast_data/statcast_2023_2025_combined.rds"
OUTPUT_DIR <- "C:/Users/ecloaner/Downloads/TunnellOrientedPitchingModel/statcast_data"


suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(data.table)
})


# Functions


add_pitch_angles <- function(df) {
  df %>%
    mutate(
      vy_s = -sqrt(vy0^2 - 2 * ay * (60.5 - release_extension - 50)),
      t_s = (vy_s - vy0) / ay,
      vz_s = vz0 - az * t_s,
      VRA = -atan(vz_s / vy_s) * (180 / pi),
      vx_s = vx0 - ax * t_s,
      HRA = -atan(vx_s / vy_s) * (180 / pi),
      yf = 17/12,
      vy_f = -sqrt(vy0^2 - (2 * ay * (50 - yf))),
      t = (vy_f - vy0) / ay,
      vz_f = vz0 + (az * t),
      vaa_rad = atan2(vz_f, vy_f),
      VAA = (180 + (vaa_rad * 180 / pi)) * -1,
      vx_f = vx0 + (ax * t),
      HAA = -atan(vx_f / vy_f) * (180 / pi)
    ) %>%
    select(-vy_s, -t_s, -vz_s, -vx_s, -yf, -vaa_rad, -vx_f, -vz_f)
}

add_kde_scores_fast <- function(df, features = c('VRA', 'HRA', 'VAA', 'HAA')) {
  dt <- as.data.table(df)
  
  for (feature in features) {
    dt[[paste0(feature, '_KDE')]] <- NA_real_
  }
  
  dt[, group_key := paste(game_year, pitcher, stand, sep = "_")]
  groups <- unique(dt$group_key)
  n_groups <- length(groups)
  
  cat(paste("  Processing", format(n_groups, big.mark = ","), "groups...\n\n"))
  
  start_time <- Sys.time()
  
  # Create progress bar
  pb <- txtProgressBar(min = 0, max = n_groups, style = 3, width = 50)
  
  for (i in seq_along(groups)) {
    group_data <- dt[group_key == groups[i]]
    pitch_types <- unique(group_data$pitch_type)
    
    if (length(pitch_types) <= 1) {
      setTxtProgressBar(pb, i)
      next
    }
    
    for (pt in pitch_types) {
      current_idx <- which(group_data$pitch_type == pt)
      other_idx <- which(group_data$pitch_type != pt)
      
      if (length(other_idx) < 5) next
      
      for (feature in features) {
        other_vals <- group_data[[feature]][other_idx]
        other_vals <- other_vals[!is.na(other_vals)]
        
        if (length(other_vals) < 5) next
        
        current_vals <- group_data[[feature]][current_idx]
        valid_current <- !is.na(current_vals)
        
        if (!any(valid_current)) next
        
        h <- 1.06 * sd(other_vals) * length(other_vals)^(-1/5)
        
        kde_vals <- sapply(current_vals[valid_current], function(val) {
          mean(dnorm(val, mean = other_vals, sd = h))
        })
        
        global_idx <- which(dt$group_key == groups[i] & 
                              dt$pitch_type == pt & 
                              !is.na(dt[[feature]]))
        
        if (length(global_idx) == length(kde_vals)) {
          dt[[paste0(feature, '_KDE')]][global_idx] <- kde_vals
        }
      }
    }
    
    # Update progress bar
    setTxtProgressBar(pb, i)
    
    # Print time estimates every 5%
    if (i %% max(1, round(n_groups * 0.05)) == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      
      if (i > 0 && elapsed > 0.5) {  # Only show after 30 seconds
        rate <- i / elapsed
        remaining <- (n_groups - i) / rate
        pct <- round(100 * i / n_groups)
        
        # Print on new line to not interfere with progress bar
        cat(sprintf("\n    %d%% complete | Elapsed: %.1f min | Remaining: %.1f min\n", 
                    pct, elapsed, remaining))
      }
    }
  }
  
  close(pb)
  
  elapsed_total <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  cat(sprintf("\n\n  Complete in %.1f minutes\n\n", elapsed_total))
  
  dt[, group_key := NULL]
  return(as.data.frame(dt))
}


# Load all statcast data


cat("Loading full dataset...\n")
df_all <- readRDS(RDS_PATH)
cat(paste("Loaded", format(nrow(df_all), big.mark = ","), "pitches\n\n"))

# Filter to required columns
required_cols <- c('vy0', 'ay', 'release_extension', 'vz0', 'az', 
                   'vx0', 'ax', 'game_year', 'pitcher', 'stand', 
                   'pitch_type', 'player_name')

df_all <- df_all %>% filter(complete.cases(across(all_of(required_cols))))
cat(paste("good", format(nrow(df_all), big.mark = ","), "complete pitches\n\n"))


# Year by Year


years <- unique(df_all$game_year)
cat("Years to process:", paste(years, collapse = ", "), "\n\n")

all_results <- list()

for (year in years) {
  cat("====================================================\n")
  cat("  PROCESSING", year, "\n")
  cat("====================================================\n\n")
  
  # Filter to year
  df_year <- df_all %>% filter(game_year == year)
  cat(paste("Year", year, ":", format(nrow(df_year), big.mark = ","), "pitches\n\n"))
  
  # Calculate angles
  cat("Calculating angles...\n")
  df_year <- add_pitch_angles(df_year)
  cat(" Done\n\n")
  
  # Calculate KDE
  cat("Calculating KDE scores...\n")
  df_year <- add_kde_scores_fast(df_year)
  
  # Save year results
  year_file <- file.path(OUTPUT_DIR, paste0("tunneling_", year, ".rds"))
  saveRDS(df_year, year_file)
  cat(paste("Saved:", year_file, "\n\n"))
  
  all_results[[as.character(year)]] <- df_year
}


# Combo years



df_combined <- bind_rows(all_results)
cat(paste("Combined:", format(nrow(df_combined), big.mark = ","), "pitches\n\n"))

# Save combined
combined_file <- file.path(OUTPUT_DIR, "statcast_with_tunneling_full.rds")
saveRDS(df_combined, combined_file)
cat(paste("Saved:", combined_file, "\n\n"))



leaderboard <- df_combined %>%
  group_by(pitcher, player_name, pitch_type, game_year) %>%
  summarize(
    n = n(),
    avg_VRA = mean(VRA, na.rm = TRUE),
    avg_HRA = mean(HRA, na.rm = TRUE),
    avg_VAA = mean(VAA, na.rm = TRUE),
    avg_HAA = mean(HAA, na.rm = TRUE),
    avg_VRA_KDE = mean(VRA_KDE, na.rm = TRUE),
    avg_HRA_KDE = mean(HRA_KDE, na.rm = TRUE),
    avg_VAA_KDE = mean(VAA_KDE, na.rm = TRUE),
    avg_HAA_KDE = mean(HAA_KDE, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(n >= 100) %>%
  mutate(
    tunneling_score = (avg_VRA_KDE + avg_HRA_KDE + avg_VAA_KDE + avg_HAA_KDE) / 4
  ) %>%
  arrange(desc(tunneling_score))

cat(paste("good", nrow(leaderboard), "entries\n\n"))

# Save leaderboard
leaderboard_file <- file.path(OUTPUT_DIR, "tunneling_leaderboard_full.csv")
write.csv(leaderboard, leaderboard_file, row.names = FALSE)
cat(paste("Saved:", leaderboard_file, "\n\n"))


cat("Files created:\n")
for (year in years) {
  cat(paste("  -", year, ":", paste0("tunneling_", year, ".rds"), "\n"))
}
cat(paste("  - Combined:", "statcast_with_tunneling_full.rds\n"))
cat(paste("  - Leaderboard:", "tunneling_leaderboard_full.csv\n\n"))

cat("Top 20 tunnelers:\n")
print(head(leaderboard, 20))

cat("\n\nDone\n")
cat("Total pitches analyzed:", format(nrow(df_combined), big.mark = ","), "\n\n")