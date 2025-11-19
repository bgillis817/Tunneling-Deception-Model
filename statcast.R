library(dplyr)
library(readr)
library(httr)
library(lubridate)


# CONFIGURATION

base_dir <- "C:/Users/ecloaner/Downloads/TunnellOrientedPitchingModel/statcast_data"

# Create directories
if (!dir.exists(base_dir)) {
  dir.create(base_dir, recursive = TRUE)
}

for (year in 2023:2025) {
  year_dir <- file.path(base_dir, as.character(year))
  if (!dir.exists(year_dir)) {
    dir.create(year_dir)
  }
}


# Download from statcast


download_statcast_direct <- function(date, output_dir, max_retries = 3) {
  
  date_str <- format(as.Date(date), "%Y-%m-%d")
  filename <- file.path(output_dir, paste0("sc_", date_str, ".csv"))
  
  # Build Baseball Savant URL
  url <- paste0(
    "https://baseballsavant.mlb.com/statcast_search/csv?",
    "all=true",
    "&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7CPO%7CS%7C=&hfSea=",
    format(as.Date(date), "%Y"),
    "%7C&hfSit=&player_type=pitcher",
    "&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=",
    date_str,
    "&game_date_lt=",
    date_str,
    "&team=&position=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=&min_pitches=0",
    "&min_results=0&group_by=name&sort_col=pitches",
    "&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details"
  )
  
  retry_count <- 0
  
  while (retry_count < max_retries) {
    tryCatch({
      
      # Download with timeout
      response <- GET(
        url,
        timeout(300),  # 5 minute timeout
        user_agent("Mozilla/5.0")
      )
      
      # Check if successful
      if (status_code(response) != 200) {
        stop(paste("HTTP", status_code(response)))
      }
      
      # Get content
      content_text <- content(response, "text", encoding = "UTF-8")
      
      # Check if empty (no games) - just skip silently, no marker files
      if (nchar(content_text) < 100) {
        return(list(success = TRUE, message = "No games", rows = 0))
      }
      
      # Parse CSV
      data <- read_csv(content_text, show_col_types = FALSE)
      
      if (nrow(data) > 0) {
        # Save to file
        write_csv(data, filename)
        return(list(success = TRUE, message = "Success", rows = nrow(data)))
      } else {
        # No data, just skip
        return(list(success = TRUE, message = "No games", rows = 0))
      }
      
    }, error = function(e) {
      retry_count <<- retry_count + 1
      
      if (retry_count < max_retries) {
        Sys.sleep(30)  # Wait before retry
        return(NULL)  # Continue to next retry
      } else {
        cat(sprintf("     Error after %d retries: %s\n", max_retries, e$message))
        return(list(success = FALSE, message = e$message, rows = NA))
      }
    })
  }
  
  return(list(success = FALSE, message = "Max retries exceeded", rows = NA))
}


download_date_range <- function(start_date, end_date, output_dir) {
  
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  total_dates <- length(dates)
  
  cat(sprintf("\nDownloading %d days: %s to %s\n\n", 
              total_dates, start_date, end_date))
  
  success_count <- 0
  error_count <- 0
  nogames_count <- 0
  total_pitches <- 0
  
  for (i in seq_along(dates)) {
    date <- dates[i]
    date_str <- format(date, "%Y-%m-%d")
    
    # Download (no skip logic)
    result <- download_statcast_direct(date, output_dir)
    
    # Update counters
    if (result$success) {
      if (!is.na(result$rows) && result$rows > 0) {
        success_count <- success_count + 1
        total_pitches <- total_pitches + result$rows
        cat(sprintf("[%d/%d] %s -  %s pitches\n", 
                    i, total_dates, date_str, format(result$rows, big.mark = ",")))
      } else {
        nogames_count <- nogames_count + 1
        # Only show no-games message every 10 days
        if (i %% 10 == 0 || i == total_dates) {
          cat(sprintf("[%d/%d] Processed %d no-game days\n", i, total_dates, nogames_count))
        }
      }
    } else {
      error_count <- error_count + 1
      cat(sprintf("[%d/%d] %s - Failed\n", i, total_dates, date_str))
    }
    
    # Rate limiting
    if (result$success && !is.na(result$rows) && result$rows > 0) {
      Sys.sleep(6)  # 6 seconds between successful downloads
    } else {
      Sys.sleep(2)  # 2 seconds for no-game days
    }
  }
  
  # Summary
  cat("\n=== DOWNLOAD SUMMARY ===\n")
  cat(sprintf("  Total dates: %d\n", total_dates))
  cat(sprintf("  Successful: %d\n", success_count))
  cat(sprintf("  No games: %d\n", nogames_count))
  cat(sprintf("  Errors: %d\n", error_count))
  cat(sprintf("  Total pitches: %s\n\n", format(total_pitches, big.mark = ",")))
}


download_season <- function(year, output_dir) {
  
  
  # Define date ranges
  if (year == 2023) {
    start_date <- "2023-03-30"
    end_date <- "2023-11-01"
  } else if (year == 2024) {
    start_date <- "2024-03-20"
    end_date <- "2024-10-30"
  } else if (year == 2025) {
    start_date <- "2025-03-27"
    end_date <- "2025-11-01"  # World Series ended Nov 1
  } else {
    start_date <- paste0(year, "-03-28")
    end_date <- paste0(year, "-11-01")
  }
  
  cat(sprintf("Date range: %s to %s\n", start_date, end_date))
  
  download_date_range(start_date, end_date, output_dir)
}



combine_csvs <- function(input_dir, output_file) {
  
  cat(sprintf("\nCombining CSVs from: %s\n", basename(input_dir)))
  
  # Get all CSV files
  csv_files <- list.files(input_dir, pattern = "^sc_.*\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    warning("No CSV files found")
    return(NULL)
  }
  
  cat(sprintf("Found %d CSV files\n", length(csv_files)))
  
  # Read in chunks
  chunk_size <- 30
  chunks <- split(csv_files, ceiling(seq_along(csv_files) / chunk_size))
  
  all_data <- list()
  
  # Columns that often have type mismatches
  problem_cols <- c("game_type", "if_fielding_alignment", "of_fielding_alignment", 
                    "spin_dir", "spin_rate_deprecated", "break_angle_deprecated",
                    "break_length_deprecated", "tfs_deprecated", "tfs_zulu_deprecated")
  
  for (i in seq_along(chunks)) {
    cat(sprintf("  Reading chunk %d/%d...\n", i, length(chunks)))
    
    chunk_data <- lapply(chunks[[i]], function(file) {
      tryCatch({
        df <- read_csv(file, show_col_types = FALSE, progress = FALSE)
        
        # Convert problematic columns to character to avoid type conflicts
        for (col in problem_cols) {
          if (col %in% names(df)) {
            df[[col]] <- as.character(df[[col]])
          }
        }
        
        return(df)
      }, error = function(e) {
        warning(paste("Error reading", basename(file), "-", e$message))
        return(NULL)
      })
    })
    
    # Filter out NULL entries before binding
    chunk_data <- chunk_data[!sapply(chunk_data, is.null)]
    
    if (length(chunk_data) > 0) {
      all_data[[i]] <- bind_rows(chunk_data)
    } else {
      all_data[[i]] <- NULL
    }
  }
  
  # Remove NULL chunks
  all_data <- all_data[!sapply(all_data, is.null)]
  
  if (length(all_data) == 0) {
    warning("No data successfully read!")
    return(NULL)
  }
  
  # Combine all
  combined <- bind_rows(all_data)
  
  cat(sprintf(" Combined: %s pitches\n", format(nrow(combined), big.mark = ",")))
  
  # Save
  saveRDS(combined, output_file)
  cat(sprintf(" Saved: %s\n\n", basename(output_file)))
  
  return(combined)
}

# Main execution


SKIP_DOWNLOAD <- FALSE

if (!SKIP_DOWNLOAD) {
  
  # Download 2023
  download_season(2023, file.path(base_dir, "2023"))
  
  # Download 2024
  download_season(2024, file.path(base_dir, "2024"))
  
  # Download 2025 
  download_season(2025, file.path(base_dir, "2025"))
  
} else {
  cat("Skipping download (SKIP_DOWNLOAD = TRUE)\n\n")
}

# Csv to RDS



yearly_data <- list()

# Combine 2023
data_2023 <- combine_csvs(
  file.path(base_dir, "2023"),
  file.path(base_dir, "statcast_2023.rds")
)
if (!is.null(data_2023)) {
  data_2023$game_year <- 2023
  yearly_data[["2023"]] <- data_2023
}

# Combine 2024
data_2024 <- combine_csvs(
  file.path(base_dir, "2024"),
  file.path(base_dir, "statcast_2024.rds")
)
if (!is.null(data_2024)) {
  data_2024$game_year <- 2024
  yearly_data[["2024"]] <- data_2024
}

# Combine 2025
data_2025 <- combine_csvs(
  file.path(base_dir, "2025"),
  file.path(base_dir, "statcast_2025.rds")
)
if (!is.null(data_2025)) {
  data_2025$game_year <- 2025
  yearly_data[["2025"]] <- data_2025
}

# Combine 23-25


if (length(yearly_data) > 0) {
  
  all_data <- bind_rows(yearly_data)
  
  cat(sprintf(" Total: %s pitches from %d years\n\n", 
              format(nrow(all_data), big.mark = ","),
              length(yearly_data)))
  
  # Summary
  year_summary <- all_data %>%
    group_by(game_year) %>%
    summarize(
      pitches = n(),
      games = n_distinct(game_pk),
      pitchers = n_distinct(pitcher),
      .groups = 'drop'
    )
  
  print(year_summary)
  cat("\n")
  
  # Save combined
  combined_file <- file.path(base_dir, "statcast_2023_2025_combined.rds")
  saveRDS(all_data, combined_file)
  cat(sprintf("Saved: %s\n", basename(combined_file)))
  cat(sprintf("  Size: %.1f MB\n\n", file.info(combined_file)$size / (1024^2)))
  

  # Validation PROCESS_BY_YEAR.R
  
  # Check columns required by PROCESS_BY_YEAR.R
  required_cols <- c('vy0', 'ay', 'release_extension', 'vz0', 'az', 
                     'vx0', 'ax', 'game_year', 'pitcher', 'stand', 
                     'pitch_type', 'player_name')
  
  cat("Checking columns required for tunneling:\n")
  for (col in required_cols) {
    if (col %in% names(all_data)) {
      pct_present <- 100 * mean(!is.na(all_data[[col]]))
      if (pct_present >= 99) {
        cat(sprintf("   %s: %.1f%%\n", col, pct_present))
      } else {
        cat(sprintf("  ️  %s: %.1f%% (some missing)\n", col, pct_present))
      }
    } else {
      cat(sprintf("  %s: MISSING\n", col))
    }
  }
  cat("\n")
  
  # Check how many complete cases
  complete_cases <- all_data %>% 
    filter(complete.cases(across(all_of(required_cols))))
  
  cat(sprintf("Complete cases for tunneling: %s (%.1f%%)\n\n", 
              format(nrow(complete_cases), big.mark = ","),
              100 * nrow(complete_cases) / nrow(all_data)))
  
  # Check handedness distribution
  if ("stand" %in% names(all_data)) {
    cat("Batter handedness:\n")
    stand_table <- table(all_data$stand, useNA = "always")
    print(stand_table)
    cat("\n")
  }
  
  # Check unique pitchers
  cat(sprintf("Unique pitchers: %d\n\n", n_distinct(all_data$pitcher)))
  
} else {
  stop("No data to combine")
}


cat("Files created:\n")
cat("  • statcast_2023.rds\n")
cat("  • statcast_2024.rds\n")
cat("  • statcast_2025.rds\n")
cat("  • statcast_2023_2025_combined.rds  ← Use this with PROCESS_BY_YEAR.R\n\n")
