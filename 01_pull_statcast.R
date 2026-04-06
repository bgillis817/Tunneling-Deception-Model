# 01_pull_statcast.R
# Pulls Statcast data — full seasons on first run, incremental after that
# Adapted for flat repo structure (bgillis817/Tunneling-Deception-Model)
# ============================================================================

library(dplyr)
library(readr)
library(httr)
library(lubridate)

# ---------------------------------------------------------------------------
# CONFIG — all paths relative to repo root (flat structure)
# ---------------------------------------------------------------------------
BASE_DIR   <- "statcast_raw"
OUTPUT_DIR <- "statcast_combined"

dir.create(BASE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Which seasons to cover
# Historical seasons have fixed end dates; current year uses today's date
# New seasons (2026, 2027, ...) are added automatically
SEASONS <- list(
  "2023" = list(start = "2023-03-30", end = "2023-11-01"),
  "2024" = list(start = "2024-03-20", end = "2024-10-30"),
  "2025" = list(start = "2025-03-27", end = "2025-11-01")
)

# Dynamically add current year if it's not already in the list
current_year <- as.character(format(Sys.Date(), "%Y"))
if (!current_year %in% names(SEASONS)) {
  season_start <- paste0(current_year, "-03-20")
  season_end   <- format(Sys.Date(), "%Y-%m-%d")
  SEASONS[[current_year]] <- list(start = season_start, end = season_end)
  cat(sprintf("Auto-added %s season: %s to %s\n", current_year, season_start, season_end))
} else {
  # If we're in a known season that's still ongoing, pull through today
  known_end <- as.Date(SEASONS[[current_year]]$end)
  if (Sys.Date() < known_end) {
    SEASONS[[current_year]]$end <- format(Sys.Date(), "%Y-%m-%d")
  }
}

# ---------------------------------------------------------------------------
# DOWNLOAD FUNCTION (same logic as your statcast.R, just cleaner)
# ---------------------------------------------------------------------------
download_statcast_day <- function(date, output_dir, max_retries = 3) {
  date_str <- format(as.Date(date), "%Y-%m-%d")
  filename <- file.path(output_dir, paste0("sc_", date_str, ".csv"))

  # Skip if already downloaded
  if (file.exists(filename)) {
    return(list(success = TRUE, message = "Cached", rows = 0, skipped = TRUE))
  }

  url <- paste0(
    "https://baseballsavant.mlb.com/statcast_search/csv?",
    "all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=",
    "&hfNewZones=&hfGT=R%7CPO%7CS%7C=&hfSea=",
    format(as.Date(date), "%Y"),
    "%7C&hfSit=&player_type=pitcher",
    "&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=",
    "&game_date_gt=", date_str,
    "&game_date_lt=", date_str,
    "&team=&position=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=",
    "&min_pitches=0&min_results=0&group_by=name&sort_col=pitches",
    "&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details"
  )

  for (attempt in 1:max_retries) {
    result <- tryCatch({
      resp <- GET(url, timeout(300), user_agent("Mozilla/5.0"))
      if (status_code(resp) != 200) stop(paste("HTTP", status_code(resp)))

      txt <- content(resp, "text", encoding = "UTF-8")
      if (nchar(txt) < 100) return(list(success = TRUE, message = "No games", rows = 0, skipped = FALSE))

      data <- read_csv(txt, show_col_types = FALSE)
      if (nrow(data) > 0) {
        write_csv(data, filename)
        return(list(success = TRUE, message = "OK", rows = nrow(data), skipped = FALSE))
      } else {
        return(list(success = TRUE, message = "No games", rows = 0, skipped = FALSE))
      }
    }, error = function(e) {
      if (attempt < max_retries) { Sys.sleep(6); return(NULL) }
      return(list(success = FALSE, message = e$message, rows = NA, skipped = FALSE))
    })
    if (!is.null(result)) return(result)
  }
  list(success = FALSE, message = "Max retries", rows = NA, skipped = FALSE)
}

# ---------------------------------------------------------------------------
# DOWNLOAD EACH SEASON (skips dates already on disk)
# ---------------------------------------------------------------------------
for (season in names(SEASONS)) {
  cat(sprintf("\n=== Season %s ===\n", season))

  season_dir <- file.path(BASE_DIR, season)
  dir.create(season_dir, showWarnings = FALSE)

  dates <- seq(
    as.Date(SEASONS[[season]]$start),
    as.Date(SEASONS[[season]]$end),
    by = "day"
  )

  new_pitches <- 0
  skipped     <- 0

  for (d in dates) {
    res <- download_statcast_day(as.Date(d, origin = "1970-01-01"), season_dir)
    if (isTRUE(res$skipped)) {
      skipped <- skipped + 1
    } else if (res$success && !is.na(res$rows) && res$rows > 0) {
      new_pitches <- new_pitches + res$rows
      cat(sprintf("  %s: %s pitches\n", format(as.Date(d, origin = "1970-01-01"), "%Y-%m-%d"),
                  format(res$rows, big.mark = ",")))
      Sys.sleep(6)
    } else {
      Sys.sleep(2)
    }
  }

  cat(sprintf("  Skipped (cached): %d days | New pitches: %s\n",
              skipped, format(new_pitches, big.mark = ",")))
}

# ---------------------------------------------------------------------------
# COMBINE INTO SINGLE RDS PER YEAR + ONE COMBINED FILE
# ---------------------------------------------------------------------------
problem_cols <- c("game_type", "if_fielding_alignment", "of_fielding_alignment",
                  "spin_dir", "spin_rate_deprecated", "break_angle_deprecated",
                  "break_length_deprecated", "tfs_deprecated", "tfs_zulu_deprecated")

all_years <- list()

for (season in names(SEASONS)) {
  cat(sprintf("\nCombining %s CSVs...\n", season))
  csv_files <- list.files(file.path(BASE_DIR, season), pattern = "^sc_.*\\.csv$", full.names = TRUE)

  if (length(csv_files) == 0) { cat("  No files, skipping\n"); next }

  year_data <- lapply(csv_files, function(f) {
    tryCatch({
      df <- read_csv(f, show_col_types = FALSE, progress = FALSE)
      for (col in problem_cols) {
        if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
      }
      df
    }, error = function(e) NULL)
  })

  year_data <- bind_rows(year_data[!sapply(year_data, is.null)])
  year_data$game_year <- as.integer(season)

  cat(sprintf("  %s: %s pitches\n", season, format(nrow(year_data), big.mark = ",")))
  all_years[[season]] <- year_data
}

combined <- bind_rows(all_years)

years_present <- sort(unique(combined$game_year))
years_label <- paste0(min(years_present), "_", max(years_present))
combined_path <- file.path(OUTPUT_DIR, paste0("statcast_", years_label, "_combined.rds"))
saveRDS(combined, combined_path)

latest_path <- file.path(OUTPUT_DIR, "statcast_combined_latest.rds")
saveRDS(combined, latest_path)

cat(sprintf("\nSaved combined: %s pitches (%s) -> %s\n",
            format(nrow(combined), big.mark = ","),
            paste(years_present, collapse = ", "),
            combined_path))
