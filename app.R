# ============================================================================
# TUNNELING ANALYSIS SHINY APP - UPDATED VERSION
# ============================================================================
# Interactive dashboard for exploring pitch tunneling metrics
# Updated with: tWAA/tWAA per 162, revised About section, no pitch minimum
# Pitch Types tab simplified to show only table
# NEW: Pitch Pairs tab with individual pitcher matrices
# ============================================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)

# ============================================================================
# LOAD DATA
# ============================================================================

# Load arsenal-level data from GitHub or local file
data_url <- "https://raw.githubusercontent.com/bgillis817/Tunneling-Deception-Model/refs/heads/main/scores.csv"

# Try to load from URL, fall back to local if needed
tryCatch({
  data <- read.csv(data_url)
  cat("✓ Arsenal data loaded from GitHub\n")
}, error = function(e) {
  cat("⚠ Could not load from GitHub, trying local file...\n")
  data <- read.csv("scores.csv")
  cat("✓ Arsenal data loaded from local file\n")
})

# Load pitch type-level data
pitch_type_url <- "https://raw.githubusercontent.com/bgillis817/Tunneling-Deception-Model/refs/heads/main/pitch_type_tunneling_scores.csv"

tryCatch({
  pitch_type_data <- read.csv(pitch_type_url)
  cat("✓ Pitch type data loaded from GitHub\n")
}, error = function(e) {
  cat("⚠ Could not load pitch type data from GitHub, trying local file...\n")
  pitch_type_data <- read.csv("pitch_type_tunneling_scores.csv")
  cat("✓ Pitch type data loaded from local file\n")
})

# Load pitch pair data
pitch_pair_url <- "https://raw.githubusercontent.com/bgillis817/Tunneling-Deception-Model/refs/heads/main/pitcher_pitch_pair_combos_all.csv"

tryCatch({
  pitch_pair_data <- read.csv(pitch_pair_url)
  cat("✓ Pitch pair data loaded from GitHub\n")
}, error = function(e) {
  cat("⚠ Could not load pitch pair data from GitHub, trying local file...\n")
  pitch_pair_data <- read.csv("pitcher_pitch_pair_combos_all.csv")
  cat("✓ Pitch pair data loaded from local file\n")
})

# Pitch type name mapping
pitch_names <- c(
  "CH" = "Changeup",
  "CU" = "Curveball",
  "FC" = "Cutter",
  "EP" = "Eephus",
  "FO" = "Forkball",
  "FF" = "Four-Seam Fastball",
  "KN" = "Knuckleball",
  "KC" = "Knuckle-curve",
  "SC" = "Screwball",
  "SI" = "Sinker",
  "SL" = "Slider",
  "SV" = "Slurve",
  "FS" = "Splitter",
  "ST" = "Sweeper",
  "FA" = "Fastball",
  "CS" = "Slow Curve"
)

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      .navbar { background-color: #2C3E50; }
      .navbar-default .navbar-brand { color: #ECF0F1; }
      .well { background-color: #ECF0F1; }
      .metric-box {
        background-color: white;
        border-radius: 5px;
        padding: 15px;
        margin: 10px 0;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .metric-value {
        font-size: 24px;
        font-weight: bold;
        color: #2C3E50;
      }
      .metric-label {
        font-size: 14px;
        color: #7F8C8D;
      }
      .about-section {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 5px;
        margin-bottom: 20px;
      }
      .about-section h3 {
        color: #2C3E50;
        border-bottom: 2px solid #3498DB;
        padding-bottom: 10px;
        margin-bottom: 15px;
      }
      .about-section h4 {
        color: #34495E;
        margin-top: 20px;
        margin-bottom: 10px;
      }
      .metric-explanation {
        background-color: white;
        padding: 15px;
        border-left: 4px solid #3498DB;
        margin: 10px 0;
      }
      .formula-box {
        background-color: #f0f0f0;
        padding: 10px;
        border-radius: 3px;
        font-family: monospace;
        margin: 10px 0;
      }
    "))
  ),
  
  # Title
  titlePanel(
    div(
      style = "background-color: #2C3E50; color: white; padding: 20px; margin: -15px -15px 20px -15px;",
      h1("tWAA/Tunneling+ Model", style = "margin: 0;"),
      p("Quantifying the Impact of Deception", 
        style = "margin: 5px 0 0 0; font-size: 16px;")
    )
  ),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4(" Filters"),
      
      # Year filter
      selectInput("year", "Season:",
                  choices = c("All", sort(unique(data$game_year), decreasing = TRUE)),
                  selected = "All"),
      
      # Search by name
      textInput("search_name", "Search Pitcher:", ""),
      
      hr(),
      
      h4(" Display Options"),
      
      # Metric selection
      selectInput("primary_metric", "Primary Metric:",
                  choices = c("Tunneling+" = "tunneling_plus",
                              "tWAA" = "tunneling_WAA",
                              "tWAA/162" = "tunneling_WAA_per162",
                              "Tunnel Quality" = "tunnel_quality_arsenal",
                              "Release Similarity" = "release_similarity_arsenal",
                              "Approach Divergence" = "approach_divergence_arsenal"),
                  selected = "tunneling_plus"),
      
      hr(),
      
      # Download button
      downloadButton("download_data", "Download Filtered Data", 
                     style = "width: 100%;")
    ),
    
    # Main panel
    mainPanel(
      width = 9,
      
      # Tab panels
      tabsetPanel(
        type = "tabs",
        
        # Overview Tab
        tabPanel("Overview",
                 br(),
                 fluidRow(
                   column(3, div(class = "metric-box", 
                                 uiOutput("metric_pitchers"))),
                   column(3, div(class = "metric-box",
                                 uiOutput("metric_avg_tunneling"))),
                   column(3, div(class = "metric-box",
                                 uiOutput("metric_elite"))),
                   column(3, div(class = "metric-box",
                                 uiOutput("metric_poor")))
                 ),
                 hr(),
                 h3("Distribution of Tunneling Metrics"),
                 plotlyOutput("distribution_plot", height = "400px"),
                 hr(),
                 h3("Tunneling+ vs Tunnel Quality"),
                 plotlyOutput("scatter_plot", height = "400px")
        ),
        
        # Leaderboard Tab
        tabPanel(" Leaderboard",
                 br(),
                 h3("Top Tunnelers"),
                 DTOutput("leaderboard_table")
        ),
        
        # Pitch Type Analysis Tab
        tabPanel("Pitch Types",
                 br(),
                 selectInput("league_pitch_type_filter", "Filter by Pitch Type:",
                             choices = c("All" = "all",
                                         "4-Seam FB" = "FF",
                                         "Sinker" = "SI",
                                         "Cutter" = "FC",
                                         "Slider" = "SL",
                                         "Curveball" = "CU",
                                         "Changeup" = "CH",
                                         "Splitter" = "FS",
                                         "Knuckle Curve" = "KC",
                                         "Sweeper" = "ST",
                                         "Slurve" = "SV"),
                             selected = "all"),
                 hr(),
                 h3("League-Wide Pitch Type Scores"),
                 DTOutput("league_pitch_type_table")
        ),
        
        # NEW: Pitch Pairs Tab
        tabPanel("Pitch Pairs",
                 br(),
                 h3("Pitch Pair Tunneling Analysis"),
                 p("Search for a pitcher to view their pitch pair matrix showing tunneling effectiveness for each back-to-back pitch combination."),
                 hr(),
                 fluidRow(
                   column(12,
                          textInput("pitch_pair_search", "Search Pitcher:", "", width = "100%"),
                          uiOutput("pitch_pair_pitcher_select")
                   )
                 ),
                 hr(),
                 h4("Pitch Pair Runs Saved Matrix"),
                 p("Read as: After throwing [First Pitch], the tunneling value of throwing [Second Pitch]"),
                 p("Positive (Red) = runs saved (good tunneling) | Negative (Blue) = runs allowed (poor tunneling)"),
                 plotOutput("pitch_pair_matrix_plot", height = "600px"),
                 hr(),
                 h4("Pitch Pair Details Table"),
                 DTOutput("pitch_pair_table"),
                 hr(),
                 fluidRow(
                   column(6,
                          downloadButton("download_pitch_pair_matrix", "Download Matrix (PNG)",
                                         style = "width: 100%;")
                   ),
                   column(6,
                          downloadButton("download_pitch_pair_data", "Download Data (CSV)",
                                         style = "width: 100%;")
                   )
                 )
        ),
        
        # Pitcher Detail Tab
        tabPanel(" Individual Pitcher Focus",
                 br(),
                 selectInput("selected_pitcher", "Select Pitcher:",
                             choices = NULL),
                 hr(),
                 uiOutput("pitcher_detail")
        ),
        
        # Compare Tab
        tabPanel("Compare",
                 br(),
                 fluidRow(
                   column(6, 
                          selectInput("compare_pitcher1", "Pitcher 1:",
                                      choices = NULL)),
                   column(6,
                          selectInput("compare_pitcher2", "Pitcher 2:",
                                      choices = NULL))
                 ),
                 hr(),
                 uiOutput("comparison_output")
        ),
        
        # About Tab
        tabPanel(" About",
                 br(),
                 
                 # Main Introduction
                 div(class = "about-section",
                     h3("Tunneling Metrics Explained"),
                     
                     h4("Arsenal-Level Metrics (One score per pitcher per season)"),
                     
                     div(class = "metric-explanation",
                         h5("Release Similarity Arsenal"),
                         p("Measures how similar all of the pitcher's pitches look at the release point."),
                         tags$ul(
                           tags$li(strong("Range:"), "0 to 1 (higher = more similar at release)"),
                           tags$li(strong("Good example:"), "A pitcher whose FB, SL, and CH all come from the same arm slot"),
                           tags$li(strong("Calculation:"), "Average of release similarity scores across ALL pitch pairs")
                         )
                     ),
                     
                     div(class = "metric-explanation",
                         h5("Approach Divergence Arsenal"),
                         p("Measures how much the pitches separate as they approach the plate."),
                         tags$ul(
                           tags$li(strong("Range:"), "0 to 1+ (higher = more separation at plate)"),
                           tags$li(strong("Good example:"), "Pitches that look identical early but break differently late"),
                           tags$li(strong("Calculation:"), "Average of approach divergence across all pitch pairs")
                         )
                     ),
                     
                     div(class = "metric-explanation",
                         h5("Tunnel Quality Arsenal"),
                         p("The overall tunneling effectiveness - the 'magic' score."),
                         tags$ul(
                           tags$li(strong("Calculation:"), "Release Similarity × Approach Divergence"),
                           tags$li(strong("Logic:"), "Best tunneling = similar release + different approaches"),
                         )
                     ),
                     
                     # Level 2
                     div(class = "about-section",
                         h4("Pitch-Pair Metrics (One score per pitch combination)"),
                         p("These measure specific two-pitch combinations (FF/SL, CH/FF, SI/CU, etc.)"),
                         
                         tags$strong("Release Similarity Components:"),
                         tags$ol(
                           tags$li(strong("VRA (Vertical Release Angle)"), "- How similar is the vertical trajectory at release?"),
                           tags$li(strong("HRA (Horizontal Release Angle)"), "- How similar is the horizontal trajectory at release?")
                         ),
                         p("These are calculated using", strong("Kernel Density Estimation (KDE)"), "which measures how much the distributions of angles overlap."),
                         
                         tags$strong("Approach Divergence Components:"),
                         tags$ol(start = 3,
                                 tags$li(strong("VAA (Vertical Approach Angle)"), "- How different are the vertical angles at the plate?"),
                                 tags$li(strong("HAA (Horizontal Approach Angle)"), "- How different are the horizontal angles at the plate?")
                         ),
                         
                         p(strong("Classic example:"), "Skenes SI/SL combo - looks the same early, separates dramatically late")
                     ),
                     
                     # The Modeling Approach
                     div(class = "about-section",
                         h3(" The Modeling Approach"),
                         p("This analysis uses a predictive modeling framework to isolate the effect of tunneling on pitcher performance:"),
                         
                         h5("Step 1: Train the xRV (Expected Run Value) Model"),
                         p("An XGBoost model learns the relationship between tunneling quality and expected outcomes."),
                         
                         div(class = "formula-box",
                             "Input Features (6 total):",
                             tags$ul(
                               tags$li("tunnel_quality_arsenal - Overall arsenal tunneling"),
                               tags$li("release_similarity_arsenal - Arsenal release similarity"),
                               tags$li("approach_divergence_arsenal - Arsenal approach divergence"),
                               tags$li("avg_tunnel_quality_for_pitch - This pitch's tunneling quality"),
                               tags$li("avg_release_sim_for_pitch - This pitch's release similarity"),
                               tags$li("avg_approach_div_for_pitch - This pitch's approach divergence")
                             ),
                             "Target: actual_rv (the run value that actually occurred)"
                         ),
                         
                         h5("Step 2: Generate Expected Run Values"),
                         p("For every pitch, the model predicts: 'Based on tunneling alone, what run value should this pitch generate?'"),
                         
                         h5("Step 3: Calculate Tunneling+ Score"),
                         p("Pitchers are scored based on how their actual performance compares to tunneling-based expectations:"),
                         tags$ul(
                           tags$li("If actual_rv < xrv → Pitcher beats expectations (elite tunneler)"),
                           tags$li("If actual_rv > xrv → Pitcher underperforms (poor tunneler)"),
                           tags$li("If actual_rv ≈ xrv → Pitcher performs as expected")
                         ),
                         
                         div(class = "formula-box",
                             "Z-Score Normalization:",
                             tags$br(),
                             "tunnel_quality_zscore = (tunnel_quality - mean) / sd",
                             tags$br(),
                             "tunneling_plus = 100 + (tunnel_quality_zscore × 10)"
                         )
                     ),
                     
                     # Key Metrics
                     div(class = "about-section",
                         h3(" Key Metrics Explained"),
                         
                         div(class = "metric-explanation",
                             h5("Tunneling+"),
                             p("Normalized runs prevented per 100 pitches, scaled to 100 = league average."),
                             tags$ul(
                               tags$li(strong("100:"), "League average"),
                               tags$li(strong("110:"), "Elite (1 SD above average)"),
                               tags$li(strong("90:"), "Poor (1 SD below average)"),
                               tags$li(strong("Range:"), "Typically 70-130")
                             )
                         ),
                         
                         div(class = "metric-explanation",
                             h5("tWAA (Tunneling Wins Above Average)"),
                             p("Cumulative wins added through tunneling over the course of a season."),
                             tags$ul(
                               tags$li("Positive values = pitcher adds wins via tunneling"),
                               tags$li("Negative values = pitcher loses wins due to poor tunneling"),
                               tags$li("Accounts for total pitch volume")
                             )
                         ),
                         
                         div(class = "metric-explanation",
                             h5("tWAA/162 (Tunneling Wins Above Average per 162 Games)"),
                             p("Rate statistic normalizing tWAA to a full season."),
                             tags$ul(
                               tags$li("Better for comparing pitchers with different workloads"),
                               tags$li("Shows tunneling value independent of playing time"),
                               tags$li("Useful for identifying elite tunneling talent")
                             )
                         )
                     ),
                     
                     # Resources
                     div(class = "about-section",
                         h3("📚 Resources that Guided my work"),
                         tags$ul(
                           tags$li(tags$a(href = "https://medium.com/@maxwellresnick/quantifying-pitch-tunneling-acc0cfcdff02", 
                                          target = "_blank",
                                          "Quantifying Pitch Tunneling - Maxwell Resnick")),
                           tags$li(tags$a(href = "https://blogs.fangraphs.com/introducing-the-kirby-index-a-new-way-to-quantify-command/",
                                          target = "_blank",
                                          "The Kirby Index: Quantifying Command - FanGraphs")),
                           tags$li(tags$a(href = "https://blogs.fangraphs.com/a-visual-primer-on-horizontal-approach-angle-haa/",
                                          target = "_blank",
                                          "Visual Primer on Horizontal Approach Angle - FanGraphs"))
                         )
                     )
                 )
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    df <- data
    
    # Filter by year
    if (input$year != "All") {
      df <- df %>% filter(game_year == as.numeric(input$year))
    }
    
    # Search by name
    if (input$search_name != "") {
      df <- df %>% filter(grepl(input$search_name, player_name, ignore.case = TRUE))
    }
    
    # Sort by primary metric
    df <- df %>% arrange(desc(.data[[input$primary_metric]]))
    
    return(df)
  })
  
  # Update pitcher selections for detail/compare tabs
  observe({
    pitcher_choices <- filtered_data() %>%
      arrange(player_name) %>%
      mutate(label = paste0(player_name, " (", game_year, ")")) %>%
      pull(label)
    
    updateSelectInput(session, "selected_pitcher", choices = pitcher_choices)
    updateSelectInput(session, "compare_pitcher1", choices = pitcher_choices)
    updateSelectInput(session, "compare_pitcher2", choices = pitcher_choices)
  })
  
  # Reactive list of pitchers matching search for pitch pairs
  pitch_pair_pitcher_choices <- reactive({
    search_term <- input$pitch_pair_search
    
    # Start with all pitchers
    all_pitchers <- pitch_pair_data %>%
      select(player_name, game_year) %>%
      distinct() %>%
      arrange(player_name, desc(game_year)) %>%
      mutate(label = paste0(player_name, " (", game_year, ")"))
    
    if (!is.null(search_term) && search_term != "") {
      # Filter by search term
      all_pitchers <- all_pitchers %>%
        filter(grepl(search_term, player_name, ignore.case = TRUE))
    }
    
    return(all_pitchers$label)
  })
  
  # Render the pitcher selection dropdown for pitch pairs
  output$pitch_pair_pitcher_select <- renderUI({
    choices <- pitch_pair_pitcher_choices()
    
    if (length(choices) == 0) {
      return(p("No pitchers found matching search.", style = "color: #999;"))
    }
    
    selectizeInput("pitch_pair_pitcher", "Select Pitcher:",
                   choices = choices,
                   selected = choices[1],
                   options = list(maxOptions = 5000))
  })
  
  # Reactive pitch pair data for selected pitcher
  selected_pitcher_pairs <- reactive({
    req(input$pitch_pair_pitcher)
    
    # Extract pitcher name and year
    pitcher_info <- strsplit(input$pitch_pair_pitcher, " \\(")[[1]]
    pitcher_name <- pitcher_info[1]
    pitcher_year <- as.numeric(gsub("\\)", "", pitcher_info[2]))
    
    # Filter data - NO FLIP needed, data is already in "runs saved" convention
    # Positive = runs saved = good, Negative = runs allowed = bad
    pitcher_pairs <- pitch_pair_data %>%
      filter(player_name == pitcher_name, 
             game_year == pitcher_year)
    
    return(pitcher_pairs)
  })
  
  # ========== OVERVIEW TAB ==========
  
  output$metric_pitchers <- renderUI({
    n <- nrow(filtered_data())
    div(
      div(class = "metric-value", n),
      div(class = "metric-label", "Pitchers")
    )
  })
  
  output$metric_avg_tunneling <- renderUI({
    avg <- mean(filtered_data()$tunneling_plus, na.rm = TRUE)
    div(
      div(class = "metric-value", sprintf("%.1f", avg)),
      div(class = "metric-label", "Avg Tunneling+")
    )
  })
  
  output$metric_elite <- renderUI({
    n_elite <- sum(filtered_data()$tunneling_plus >= 110, na.rm = TRUE)
    pct <- 100 * n_elite / nrow(filtered_data())
    div(
      div(class = "metric-value", sprintf("%d (%.1f%%)", n_elite, pct)),
      div(class = "metric-label", "Elite (110+)")
    )
  })
  
  output$metric_poor <- renderUI({
    n_poor <- sum(filtered_data()$tunneling_plus < 90, na.rm = TRUE)
    pct <- 100 * n_poor / nrow(filtered_data())
    div(
      div(class = "metric-value", sprintf("%d (%.1f%%)", n_poor, pct)),
      div(class = "metric-label", "Poor (<90)")
    )
  })
  
  output$distribution_plot <- renderPlotly({
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = tunneling_plus)) +
      geom_histogram(binwidth = 5, fill = "#3498DB", color = "white", alpha = 0.8) +
      geom_vline(xintercept = 100, linetype = "dashed", color = "red", size = 1) +
      geom_vline(xintercept = 110, linetype = "dashed", color = "green", size = 1) +
      geom_vline(xintercept = 90, linetype = "dashed", color = "orange", size = 1) +
      labs(title = "Tunneling+ Distribution",
           x = "Tunneling+",
           y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$scatter_plot <- renderPlotly({
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = tunnel_quality_arsenal, y = tunneling_plus,
                        text = paste0(player_name, " (", game_year, ")"))) +
      geom_point(alpha = 0.6, size = 3, color = "#3498DB") +
      geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
      labs(title = "Tunnel Quality vs Tunneling+",
           x = "Tunnel Quality (0-1 scale)",
           y = "Tunneling+ (100 scale)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # ========== LEADERBOARD TAB ==========
  
  output$leaderboard_table <- renderDT({
    df <- filtered_data()
    
    # Select key columns
    display_cols <- c("player_name", "game_year", "tunneling_plus", 
                      "tunneling_WAA", "tunneling_WAA_per162",
                      "tunnel_quality_arsenal", "release_similarity_arsenal", 
                      "approach_divergence_arsenal", "runs_saved_per_100")
    
    # Keep only columns that exist
    display_cols <- display_cols[display_cols %in% names(df)]
    
    df_display <- df[, display_cols]
    
    # Rename for display
    col_names <- c(
      player_name = "Player",
      game_year = "Year",
      tunneling_plus = "Tunneling+",
      tunneling_WAA = "tWAA",
      tunneling_WAA_per162 = "tWAA/162",
      tunnel_quality_arsenal = "Tunnel Quality",
      release_similarity_arsenal = "Release Sim",
      approach_divergence_arsenal = "Approach Div",
      runs_saved_per_100 = "Runs/100"
    )
    
    names(df_display) <- col_names[names(df_display)]
    
    datatable(df_display,
              options = list(pageLength = 50, scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = 3:ncol(df_display), digits = 2)
  })
  
  # ========== PITCH PAIRS TAB ==========
  
  # Generate the pitch pair matrix plot
  pitch_pair_matrix_plot_func <- function() {
    pitcher_pairs <- selected_pitcher_pairs()
    
    if (is.null(pitcher_pairs) || nrow(pitcher_pairs) == 0) {
      return(NULL)
    }
    
    # Get pitcher info for title
    pitcher_info <- strsplit(input$pitch_pair_pitcher, " \\(")[[1]]
    pitcher_name <- pitcher_info[1]
    pitcher_year <- gsub("\\)", "", pitcher_info[2])
    
    # Add full pitch names
    pitcher_pairs <- pitcher_pairs %>%
      mutate(
        first_pitch_name = ifelse(is.na(pitch_names[first_pitch]), first_pitch, pitch_names[first_pitch]),
        second_pitch_name = ifelse(is.na(pitch_names[second_pitch]), second_pitch, pitch_names[second_pitch]),
        first_pitch_label = paste0(first_pitch_name, "\n(", first_pitch, ")"),
        second_pitch_label = paste0(second_pitch_name, "\n(", second_pitch, ")")
      )
    
    # Get unique pitch types for this pitcher
    pitch_types <- unique(c(pitcher_pairs$first_pitch, pitcher_pairs$second_pitch))
    pitch_labels <- sapply(pitch_types, function(x) {
      name <- pitch_names[x]
      if (is.na(name)) name <- x
      paste0(name, "\n(", x, ")")
    })
    
    # Create factors with proper ordering
    pitcher_pairs <- pitcher_pairs %>%
      mutate(
        first_pitch_label = factor(first_pitch_label, levels = rev(pitch_labels)),
        second_pitch_label = factor(second_pitch_label, levels = pitch_labels)
      )
    
    # Create the heatmap
    # Data is in "runs saved" convention: positive = good, negative = bad
    # Red (high/positive) = runs saved = good
    # Blue (low/negative) = runs allowed = bad
    p <- ggplot(pitcher_pairs, aes(x = second_pitch_label, y = first_pitch_label, fill = xrv_per_100)) +
      geom_tile(color = "white", size = 1) +
      geom_text(aes(label = sprintf("%.3f\n(n=%d)", xrv_per_100, n_pairs)), 
                color = "black", size = 3.5, lineheight = 0.8) +
      scale_fill_gradient2(
        low = "#2E86AB",
        mid = "white", 
        high = "#C1121F",
        midpoint = 0,
        name = "Runs Saved\nper 100"
      ) +
      labs(
        title = paste0(pitcher_name, " (", pitcher_year, ") - Pitch Pair Tunneling Matrix"),
        subtitle = "Tunneling effectiveness by back-to-back pitch sequence",
        x = "Second Pitch",
        y = "First Pitch",
        caption = "Positive (Red) = runs saved (good tunneling) | Negative (Blue) = runs allowed (poor tunneling)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
        plot.caption = element_text(size = 9, hjust = 0.5, color = "gray50"),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.title = element_text(face = "bold"),
        panel.grid = element_blank()
      )
    
    return(p)
  }
  
  output$pitch_pair_matrix_plot <- renderPlot({
    pitch_pair_matrix_plot_func()
  })
  
  output$pitch_pair_table <- renderDT({
    pitcher_pairs <- selected_pitcher_pairs()
    
    if (is.null(pitcher_pairs) || nrow(pitcher_pairs) == 0) {
      return(NULL)
    }
    
    # Select and rename columns for display
    display_df <- pitcher_pairs %>%
      mutate(
        first_pitch_name = ifelse(is.na(pitch_names[first_pitch]), first_pitch, pitch_names[first_pitch]),
        second_pitch_name = ifelse(is.na(pitch_names[second_pitch]), second_pitch, pitch_names[second_pitch])
      ) %>%
      select(
        first_pitch, first_pitch_name,
        second_pitch, second_pitch_name,
        n_pairs, xrv_per_100, xrv_total
      ) %>%
      arrange(desc(xrv_per_100))  # Best (most positive) at top
    
    colnames(display_df) <- c(
      "1st Pitch", "1st Pitch Name",
      "2nd Pitch", "2nd Pitch Name",
      "Count", "Runs Saved/100", "Total Runs Saved"
    )
    
    datatable(display_df,
              options = list(pageLength = 20, scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = c("Runs Saved/100", "Total Runs Saved"), digits = 3)
  })
  
  # Download handlers for pitch pairs
  output$download_pitch_pair_matrix <- downloadHandler(
    filename = function() {
      req(input$pitch_pair_pitcher)
      pitcher_info <- strsplit(input$pitch_pair_pitcher, " \\(")[[1]]
      pitcher_name <- gsub(" ", "_", pitcher_info[1])
      pitcher_year <- gsub("\\)", "", pitcher_info[2])
      paste0("pitch_pair_matrix_", pitcher_name, "_", pitcher_year, ".png")
    },
    content = function(file) {
      p <- pitch_pair_matrix_plot_func()
      if (!is.null(p)) {
        ggsave(file, plot = p, width = 12, height = 10, dpi = 300)
      }
    }
  )
  
  output$download_pitch_pair_data <- downloadHandler(
    filename = function() {
      req(input$pitch_pair_pitcher)
      pitcher_info <- strsplit(input$pitch_pair_pitcher, " \\(")[[1]]
      pitcher_name <- gsub(" ", "_", pitcher_info[1])
      pitcher_year <- gsub("\\)", "", pitcher_info[2])
      paste0("pitch_pair_data_", pitcher_name, "_", pitcher_year, ".csv")
    },
    content = function(file) {
      write.csv(selected_pitcher_pairs(), file, row.names = FALSE)
    }
  )
  
  # ========== PITCHER DETAIL TAB ==========
  
  output$pitcher_detail <- renderUI({
    req(input$selected_pitcher)
    
    # Extract pitcher name and year
    pitcher_info <- strsplit(input$selected_pitcher, " \\(")[[1]]
    pitcher_name <- pitcher_info[1]
    pitcher_year <- as.numeric(gsub("\\)", "", pitcher_info[2]))
    
    # Get pitcher data
    pitcher <- filtered_data() %>%
      filter(player_name == pitcher_name, game_year == pitcher_year)
    
    if (nrow(pitcher) == 0) {
      return(h4("No data available for selected pitcher"))
    }
    
    pitcher <- pitcher[1,]
    
    # Create detailed view
    tagList(
      h2(paste(pitcher$player_name, "-", pitcher$game_year)),
      hr(),
      
      fluidRow(
        column(4,
               h4("Overall Rating"),
               div(class = "metric-box",
                   h3(sprintf("%.1f", pitcher$tunneling_plus), style = "color: #3498DB;"),
                   p("Tunneling+")
               ),
               div(class = "metric-box",
                   h3(sprintf("%+.2f", pitcher$tunneling_WAA), style = "color: #27AE60;"),
                   p("tWAA (Cumulative)")
               ),
               div(class = "metric-box",
                   h3(sprintf("%+.2f", pitcher$tunneling_WAA_per162), style = "color: #E67E22;"),
                   p("tWAA/162 (Rate)")
               )
        ),
        
        column(8,
               h4("Tunneling Breakdown"),
               plotlyOutput("pitcher_breakdown_plot")
        )
      ),
      
      hr(),
      
      fluidRow(
        column(6,
               h4("Component Scores"),
               tags$table(class = "table",
                          tags$tr(tags$td("Tunnel Quality:"), 
                                  tags$td(sprintf("%.3f", pitcher$tunnel_quality_arsenal))),
                          tags$tr(tags$td("Release Similarity:"), 
                                  tags$td(sprintf("%.3f", pitcher$release_similarity_arsenal))),
                          tags$tr(tags$td("Approach Divergence:"), 
                                  tags$td(sprintf("%.3f", pitcher$approach_divergence_arsenal))),
                          tags$tr(tags$td("Runs Saved per 100:"), 
                                  tags$td(sprintf("%+.2f", pitcher$runs_saved_per_100)))
               )
        ),
        
        column(6,
               h4("Percentile Ranks"),
               plotlyOutput("pitcher_percentile_plot")
        )
      )
    )
  })
  
  output$pitcher_breakdown_plot <- renderPlotly({
    req(input$selected_pitcher)
    
    pitcher_info <- strsplit(input$selected_pitcher, " \\(")[[1]]
    pitcher_name <- pitcher_info[1]
    pitcher_year <- as.numeric(gsub("\\)", "", pitcher_info[2]))
    
    pitcher <- filtered_data() %>%
      filter(player_name == pitcher_name, game_year == pitcher_year)
    
    if (nrow(pitcher) == 0) return(NULL)
    
    pitcher <- pitcher[1,]
    
    breakdown_data <- data.frame(
      Metric = c("Release\nSimilarity", "Approach\nDivergence", "Tunnel\nQuality"),
      Value = c(pitcher$release_similarity_arsenal,
                pitcher$approach_divergence_arsenal,
                pitcher$tunnel_quality_arsenal)
    )
    
    p <- ggplot(breakdown_data, aes(x = Metric, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      scale_fill_manual(values = c("#3498DB", "#E74C3C", "#2ECC71")) +
      labs(title = "Tunneling Components", y = "Score (0-1 scale)") +
      theme_minimal() +
      theme(legend.position = "none") +
      ylim(0, 1)
    
    ggplotly(p)
  })
  
  output$pitcher_percentile_plot <- renderPlotly({
    req(input$selected_pitcher)
    
    pitcher_info <- strsplit(input$selected_pitcher, " \\(")[[1]]
    pitcher_name <- pitcher_info[1]
    pitcher_year <- as.numeric(gsub("\\)", "", pitcher_info[2]))
    
    pitcher <- filtered_data() %>%
      filter(player_name == pitcher_name, game_year == pitcher_year)
    
    if (nrow(pitcher) == 0) return(NULL)
    
    pitcher <- pitcher[1,]
    
    percentile_data <- data.frame(
      Metric = c("Tunneling+", "Tunnel\nQuality", "Release\nSim", "Approach\nDiv"),
      Percentile = c(pitcher$tunneling_plus_pct,
                     pitcher$tunnel_quality_pct,
                     pitcher$release_similarity_pct,
                     pitcher$approach_divergence_pct)
    )
    
    p <- ggplot(percentile_data, aes(x = Metric, y = Percentile, fill = Percentile)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      scale_fill_gradient(low = "#E74C3C", high = "#2ECC71") +
      labs(title = "Percentile Rankings", y = "Percentile") +
      theme_minimal() +
      ylim(0, 100)
    
    ggplotly(p)
  })
  
  # ========== COMPARE TAB ==========
  
  output$comparison_output <- renderUI({
    req(input$compare_pitcher1, input$compare_pitcher2)
    
    # Extract info for both pitchers
    p1_info <- strsplit(input$compare_pitcher1, " \\(")[[1]]
    p1_name <- p1_info[1]
    p1_year <- as.numeric(gsub("\\)", "", p1_info[2]))
    
    p2_info <- strsplit(input$compare_pitcher2, " \\(")[[1]]
    p2_name <- p2_info[1]
    p2_year <- as.numeric(gsub("\\)", "", p2_info[2]))
    
    # Get data
    p1 <- filtered_data() %>%
      filter(player_name == p1_name, game_year == p1_year)
    p2 <- filtered_data() %>%
      filter(player_name == p2_name, game_year == p2_year)
    
    if (nrow(p1) == 0 || nrow(p2) == 0) {
      return(h4("Data not available for comparison"))
    }
    
    p1 <- p1[1,]
    p2 <- p2[1,]
    
    tagList(
      h3("Head-to-Head Comparison"),
      hr(),
      plotlyOutput("comparison_plot", height = "500px")
    )
  })
  
  output$comparison_plot <- renderPlotly({
    req(input$compare_pitcher1, input$compare_pitcher2)
    
    # Get both pitchers
    p1_info <- strsplit(input$compare_pitcher1, " \\(")[[1]]
    p1_name <- p1_info[1]
    p1_year <- as.numeric(gsub("\\)", "", p1_info[2]))
    
    p2_info <- strsplit(input$compare_pitcher2, " \\(")[[1]]
    p2_name <- p2_info[1]
    p2_year <- as.numeric(gsub("\\)", "", p2_info[2]))
    
    p1 <- filtered_data() %>%
      filter(player_name == p1_name, game_year == p1_year) %>%
      slice(1)
    
    p2 <- filtered_data() %>%
      filter(player_name == p2_name, game_year == p2_year) %>%
      slice(1)
    
    compare_data <- data.frame(
      Metric = rep(c("Tunneling+", "tWAA/162", "Tunnel\nQuality", 
                     "Release\nSim", "Approach\nDiv"), 2),
      Value = c(
        p1$tunneling_plus / 100, p1$tunneling_WAA_per162 / 3, p1$tunnel_quality_arsenal,
        p1$release_similarity_arsenal, p1$approach_divergence_arsenal,
        p2$tunneling_plus / 100, p2$tunneling_WAA_per162 / 3, p2$tunnel_quality_arsenal,
        p2$release_similarity_arsenal, p2$approach_divergence_arsenal
      ),
      Pitcher = rep(c(input$compare_pitcher1, input$compare_pitcher2), each = 5)
    )
    
    p <- ggplot(compare_data, aes(x = Metric, y = Value, fill = Pitcher)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = c("#3498DB", "#E74C3C")) +
      labs(title = "Metric Comparison (Normalized)", y = "Score") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ========== PITCH TYPE ANALYSIS TAB (LEAGUE-WIDE) ==========
  
  output$league_pitch_type_table <- renderDT({
    # Get all pitch type data
    league_pitches <- pitch_type_data
    
    # Apply pitch type filter if not "all"
    if (!is.null(input$league_pitch_type_filter) && input$league_pitch_type_filter != "all") {
      league_pitches <- league_pitches %>%
        filter(pitch_type == input$league_pitch_type_filter)
    }
    
    # Apply year filter if set
    if (input$year != "All") {
      league_pitches <- league_pitches %>%
        filter(game_year == as.numeric(input$year))
    }
    
    # Sort by tunnel quality
    league_pitches <- league_pitches %>%
      arrange(desc(avg_tunnel_quality_for_pitch))
    
    if (nrow(league_pitches) == 0) {
      return(NULL)
    }
    
    # Create pitch name mapping
    pitch_names_display <- c(
      "FF" = "4-Seam FB", "SI" = "Sinker", "FC" = "Cutter", "SL" = "Slider",
      "CU" = "Curveball", "CH" = "Changeup", "FS" = "Splitter", 
      "KC" = "Knuckle Curve", "ST" = "Sweeper", "SV" = "Slurve"
    )
    
    league_pitches$pitch_name <- pitch_names_display[league_pitches$pitch_type]
    league_pitches$pitch_name <- ifelse(is.na(league_pitches$pitch_name), 
                                        league_pitches$pitch_type, 
                                        league_pitches$pitch_name)
    
    # Select and rename columns
    display_df <- league_pitches %>%
      select(player_name, game_year, pitch_name, n, 
             avg_tunnel_quality_for_pitch,
             avg_release_sim_for_pitch,
             avg_approach_div_for_pitch,
             avg_VRA_KDE, avg_HRA_KDE,
             avg_VAA_KDE, avg_HAA_KDE)
    
    colnames(display_df) <- c(
      "Player", "Year", "Pitch Type", "Count",
      "Tunnel Quality", "Release Sim", "Approach Div",
      "VRA Sim", "HRA Sim", "VAA Div", "HAA Div"
    )
    
    datatable(display_df,
              options = list(pageLength = 50, scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = 5:11, digits = 3)
  })
  
  # ========== DOWNLOAD ==========
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("tunneling_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)
