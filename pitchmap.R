library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(scales)

# ---------------------------------------------------------
# LOAD DATA
# ---------------------------------------------------------
data_dir  <- "datasets"
data_file <- "test_bbb-25.csv"
local_path <- file.path(data_dir, data_file)
remote_csv_url <- "https://www.dropbox.com/scl/fi/uxl3ofsbm82ko3kt6c7zb/test_bbb-25.csv?rlkey=5vmwnrfw74mgoa9yrq2ig59eq&st=o017dioz&dl=1"

# ensure datasets folder exists
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

# download only if file not present
if (!file.exists(local_path)) {
  message("Downloading large dataset (~500MB). This may take several minutes...")
  # increase timeout for large download
  old_to <- getOption("timeout")
  options(timeout = 1200)  # 20 minutes
  tryCatch({
    download.file(remote_csv_url, destfile = local_path, mode = "wb")
  }, error = function(e){
    stop("Failed to download dataset. Check the remote URL and network. Error: ", e$message)
  }, finally = {
    options(timeout = old_to)
  })
} else {
  message("Using local dataset at: ", local_path)
}

# Now read the CSV (use readr for speed)
test <- read_csv(local_path) %>%
  mutate(across(where(is.character), ~ na_if(., "")))

if ("date" %in% names(test)) test$date <- as.Date(test$date)
if ("inns" %in% names(test)) test$inns <- as.integer(test$inns)
if ("day" %in% names(test)) test$day <- as.integer(test$day)
if ("session" %in% names(test)) test$session <- as.integer(test$session)
if ("over" %in% names(test)) test$over <- as.integer(test$over)

# ---------------------------------------------------------
# COLORS / BACKGROUND helper
# ---------------------------------------------------------
get_bg_colors <- function(bg = c("dark", "white")) {
  bg <- match.arg(bg)
  if (bg == "white") {
    list(
      page_bg = "#FFFFFF",
      panel_bg = "#FFFFFF",
      plot_bg = "#FFFFFF",
      text_col = "#111111",
      tile_na = "#FFFFFF",
      tile_border = "#D9D9D9",
      accent = "#00E5FF"
    )
  } else {
    list(
      page_bg = "#4a4a4a",
      panel_bg = "#1F1F22",
      plot_bg = "#4a4a4a",
      text_col = "#F3F3F3",
      tile_na = "#1F1F22",
      tile_border = "#2C2C2E",
      accent = "#00E5FF"
    )
  }
}

# ---------------------------------------------------------
# EMPTY PLOT
# ---------------------------------------------------------
empty_pitch_plot <- function(bg = c("dark", "white")) {
  bgc <- get_bg_colors(bg)
  ggplot() +
    xlim(0, 5) +
    ylim(0, 6) +
    theme_minimal() +
    labs(title = "") +
    theme(
      plot.background = element_rect(fill = bgc$plot_bg, color = NA),
      panel.background = element_rect(fill = bgc$panel_bg, color = NA),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

# ---------------------------------------------------------
# GENERIC PITCHMAP (Control / Average / SR)
# expects df_results with columns: length, line, value
# ---------------------------------------------------------
pitchmap_generic <- function(df_results, handedness = "RHB", metric = c("control","average","sr"), bg = "white") {
  metric <- match.arg(metric)
  bgc <- get_bg_colors(bg)
  
  length_order <- c(
    "FULL_TOSS", "YORKER", "FULL",
    "GOOD_LENGTH", "SHORT_OF_A_GOOD_LENGTH", "SHORT"
  )
  length_labels_wrapped <- c(
    "Full toss", "Yorker", "Full",
    "Good", "Back of\nLength", "Short"
  )
  
  if (handedness == "LHB") {
    line_order <- c("WIDE_DOWN_LEG","DOWN_LEG","ON_THE_STUMPS","OUTSIDE_OFFSTUMP","WIDE_OUTSIDE_OFFSTUMP")
    line_labels <- c("Wide\nDown","Down\nLeg","On\nStumps","Outside\nOff","Wide\nOutside")
  } else {
    line_order <- c("WIDE_OUTSIDE_OFFSTUMP","OUTSIDE_OFFSTUMP","ON_THE_STUMPS","DOWN_LEG","WIDE_DOWN_LEG")
    line_labels <- c("Wide\nOutside","Outside\nOff","On\nStumps","Down\nLeg","Wide\nDown")
  }
  
  grid <- expand.grid(Row = 1:6, Col = 1:5) %>% arrange(Row, Col)
  
  df_results2 <- df_results %>%
    mutate(length = factor(length, levels = length_order),
           line   = factor(line,   levels = line_order))
  
  df_results2$Row <- match(df_results2$length, length_order)
  df_results2$Col <- match(df_results2$line,   line_order)
  
  grid <- grid %>% left_join(df_results2 %>% select(Row, Col, value), by = c("Row","Col"))
  
  row_labels_df <- data.frame(Row = 1:6, Label = length_labels_wrapped, stringsAsFactors = FALSE)
  col_labels_df <- data.frame(Col = 1:5, Label = line_labels, stringsAsFactors = FALSE)
  
  # Fill scales
  if (metric == "control") {
    fill_scale <- scale_fill_gradientn(
      colours = c("white", "#ff3300", "#ff9900", "#ffff66", "#99ff66", "#33cc33"),
      values  = scales::rescale(c(0, 60, 65, 75, 85, 100)),
      na.value = bgc$tile_na,
      limits = c(0, 100)
    )
    grid$fill_key <- grid$value
  } else if (metric == "average") {
    max_val <- suppressWarnings(max(grid$value, na.rm = TRUE))
    if (!is.finite(max_val) || is.na(max_val)) max_val <- 65
    max_limit <- max(ceiling(max_val), 65)
    stops <- c(0, 15, 30, 40, 65, max_limit)
    values_rescaled <- scales::rescale(stops, to = c(0, 1), from = c(0, max_limit))
    fill_scale <- scale_fill_gradientn(
      colours = c("white", "#ff3300", "#ff9900", "#ffff66", "#99ff66", "#33cc33"),
      values = values_rescaled,
      na.value = bgc$tile_na,
      limits = c(0, max_limit)
    )
    grid$fill_key <- grid$value
  } else { # sr
    grid$fill_key <- ifelse(is.na(grid$value), NA, "SR")
    fill_scale <- scale_fill_manual(values = c("SR" = "#f7c174"), na.value = "#FFFFFF")
  }
  
  title_text <- switch(metric,
                       control = "Control % Pitchmap",
                       average = "Average Pitchmap",
                       sr = "Strike Rate Pitchmap")
  
  ggplot(grid, aes(x = Col, y = Row)) +
    geom_tile(aes(fill = fill_key), color = bgc$tile_border) +
    geom_text(aes(label = ifelse(is.na(value) | value == 0, "", round(value, 1))),
              size = 4, color = bgc$text_col) +
    geom_text(data = row_labels_df, aes(x = 0.2, y = Row, label = Label),
              inherit.aes = FALSE, hjust = 1, size = 3.6, color = bgc$text_col, lineheight = 0.95) +
    geom_text(data = col_labels_df, aes(x = Col, y = 6.8, label = Label),
              inherit.aes = FALSE, vjust = 0, size = 2.8, color = bgc$text_col) +
    fill_scale +
    scale_y_reverse(breaks = 1:6, expand = c(0, 0)) +
    scale_x_continuous(breaks = 1:5, expand = c(0, 0)) +
    coord_fixed(ratio = 12/7.5, clip = "off") +
    theme_minimal() +
    labs(title = title_text) +
    theme(
      panel.background = element_rect(fill = bgc$panel_bg, color = NA),
      plot.background  = element_rect(fill = bgc$plot_bg,  color = NA),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.title = element_text(color = bgc$text_col, size = 15, face = "bold"),
      plot.margin = margin(t = 12, r = 8, b = 8, l = 64)
    )
}

# ---------------------------------------------------------
# UI choices (unchanged)
# ---------------------------------------------------------
batters <- sort(unique(test$bat[!is.na(test$bat)]))
bow_kind_choices <- c("All", sort(na.omit(unique(test$bowl_kind))))
team_choices     <- c("All", sort(na.omit(unique(test$team_bowl))))
country_choices  <- c("All", sort(na.omit(unique(test$country))))
bowler_choices   <- c("All", sort(na.omit(unique(test$bowl))))
ground_choices   <- c("All", sort(na.omit(unique(test$ground))))
min_date <- if ("date" %in% names(test)) min(test$date, na.rm = TRUE) else NULL
max_date <- if ("date" %in% names(test)) max(test$date, na.rm = TRUE) else NULL
bowlstyle_choices <- c("All", sort(na.omit(unique(test$bowl_style))))
inns_choices     <- c("All", as.character(intersect(1:4, unique(test$inns))))
day_choices      <- c("All", as.character(intersect(1:5, unique(test$day))))
session_choices  <- c("All", as.character(intersect(1:3, unique(test$session))))
min_over <- 1; max_over <- 336; default_over <- c(min_over, max_over)
min_bf <- 1; max_bf <- 540; default_bf <- c(min_bf, max_bf)

# ---------------------------------------------------------
# UI
# ---------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {background-color: #4a4a4a; color: #F3F3F3; font-family: Helvetica, Arial, sans-serif;}
      .sidebar {background-color: #232326; border: 1px solid #2C2C2E; border-radius: 8px; padding: 12px;}
      .control-label { color: #F8EFAF !important; }
      .shiny-input-container { color: #F8EFAF !important; }
      .irs-bar, .irs-bar-edge, .irs-single {background: #00E5FF !important; border-color: #00E5FF !important;}
      .irs-grid-text {color: #F3F3F3;}
      .btn-primary {background-color: #00E5FF; border-color: #00E5FF; color: #4a4a4a;}
      .shiny-output-error {color: red;}
      #main_panel {background-color: #1F1F22; padding: 15px; border-radius: 8px;}
      #main_panel.white-bg {background-color: #FFFFFF; color: #111111;}
      #main_panel.white-bg .shiny-plot-output { background-color: #FFFFFF !important; }
    "))
  ),
  
  titlePanel("Tests: Pitchmaps for batters"),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      selectInput("batter", "Select Batter:", choices = batters),
      selectizeInput("bowltype", "Bowler Type:", choices = bow_kind_choices, selected = "All", multiple = TRUE),
      selectizeInput("opposition", "Opposition:", choices = team_choices, selected = "All", multiple = TRUE),
      selectizeInput("host", "Host Country:", choices = country_choices, selected = "All", multiple = TRUE),
      selectizeInput("bowler", "Bowler:", choices = bowler_choices, selected = "All", multiple = TRUE),
      selectizeInput("ground", "Ground:", choices = ground_choices, selected = "All", multiple = TRUE),
      
      if (!is.null(min_date))
        dateRangeInput("daterange", "Select Date Range:", min_date, max_date),
      
      hr(),
      h4("Advanced filters"),
      
      selectizeInput("bowlstyle", "Bowling Style:", choices = bowlstyle_choices, selected = "All", multiple = TRUE),
      selectizeInput("inns", "Innings:", choices = inns_choices, selected = "All", multiple = TRUE),
      selectizeInput("day", "Day:", choices = day_choices, selected = "All", multiple = TRUE),
      selectizeInput("session", "Session:", choices = session_choices, selected = "All", multiple = TRUE),
      
      sliderInput("ballsfaced", "Balls faced:", min_bf, max_bf, value = default_bf),
      sliderInput("over_range", "Over range:", min_over, max_over, value = default_over),
      
      actionButton("run", "Generate Pitchmap", class = "btn-primary")
    ),
    
    mainPanel(
      id = "main_panel",
      fluidRow(
        column(4, plotOutput("control_plot", height = "520px")),
        column(4, plotOutput("avg_plot",     height = "520px")),
        column(4, plotOutput("sr_plot",      height = "520px"))
      ),
      br(),
      uiOutput("sr_note"),
      br(),
      div(
        id = "data_notes",
        style = "color:#e0e0e0; font-size:15px; line-height:1.4;",
        tags$b("About the data:"),
        br(),
        HTML("The original dataset used for the study, compiled by Himanish Ganjoo, consists of ball by ball data for <b>1,601,218 deliveries</b> bowled in men's Test cricket over a period of 20 years (Jan 2005 to Jan 2025), involving <b>971 batters</b> and <b>740 bowlers</b> in <b>827 matches</b>. Explicit data on lines and lengths are available for <b>786,593 deliveries</b> in the dataset, depending on which the pitchmaps have been plotted."),
        br(), br(),
        tags$i("Data last updated on: January 03, 2025"), br(),
        tags$i("Software last updated on: November 27, 2025")
      ),
      br(),
      uiOutput("nodata_msg")
    )
  )
)

# ---------------------------------------------------------
# SERVER
# ---------------------------------------------------------
server <- function(input, output, session) {
  
  # Centralized filtered dataframe that handles all filters correctly
  filtered_df <- reactive({
    req(input$batter)
    df <- test %>% filter(bat == input$batter)
    
    # character/string filters
    if (!is.null(input$bowltype) && !("All" %in% input$bowltype)) df <- df %>% filter(bowl_kind %in% input$bowltype)
    if (!is.null(input$opposition) && !("All" %in% input$opposition)) df <- df %>% filter(team_bowl %in% input$opposition)
    if (!is.null(input$host) && !("All" %in% input$host)) df <- df %>% filter(country %in% input$host)
    if (!is.null(input$bowler) && !("All" %in% input$bowler)) df <- df %>% filter(bowl %in% input$bowler)
    if (!is.null(input$ground) && !("All" %in% input$ground)) df <- df %>% filter(ground %in% input$ground)
    if (!is.null(input$bowlstyle) && !("All" %in% input$bowlstyle)) df <- df %>% filter(bowl_style %in% input$bowlstyle)
    
    # numeric filters: inns, day, session (inputs are strings)
    if (!is.null(input$inns) && !("All" %in% input$inns)) {
      sel_inns <- suppressWarnings(as.integer(input$inns))
      sel_inns <- sel_inns[!is.na(sel_inns)]
      if (length(sel_inns) > 0) df <- df %>% filter(inns %in% sel_inns)
    }
    if (!is.null(input$day) && !("All" %in% input$day)) {
      sel_day <- suppressWarnings(as.integer(input$day))
      sel_day <- sel_day[!is.na(sel_day)]
      if (length(sel_day) > 0) df <- df %>% filter(day %in% sel_day)
    }
    if (!is.null(input$session) && !("All" %in% input$session)) {
      sel_session <- suppressWarnings(as.integer(input$session))
      sel_session <- sel_session[!is.na(sel_session)]
      if (length(sel_session) > 0) df <- df %>% filter(session %in% sel_session)
    }
    
    # over range & balls faced
    df <- df %>% filter(between(over, input$over_range[1], input$over_range[2]),
                        between(cur_bat_bf, input$ballsfaced[1], input$ballsfaced[2]))
    
    # date range
    if ("date" %in% names(df) && !is.null(input$daterange)) {
      df <- df %>% filter(date >= input$daterange[1], date <= input$daterange[2])
    }
    
    df
  })
  
  # Toggle main panel white + switch data notes color
  observeEvent(input$run, {
    session$sendCustomMessage("changeMainPanelBg", "white")
  })
  
  # Control plot
  output$control_plot <- renderPlot({
    req(input$run)
    df <- filtered_df()
    
    # if no rows or no control column -> empty
    if (nrow(df) == 0 || !("control" %in% names(df))) {
      return(empty_pitch_plot(bg = "white"))
    }
    
    handedness <- df %>% filter(!is.na(bat_hand)) %>% distinct(bat_hand) %>% pull() %>% first()
    if (is.na(handedness)) handedness <- "RHB"
    
    results_control <- df %>%
      filter(!is.na(length), !is.na(line)) %>%
      group_by(length, line) %>%
      summarise(total = n(), control = sum(control == 1, na.rm = TRUE), value = 100 * control / total, .groups = "drop")
    
    if (nrow(results_control) == 0 || all(is.na(results_control$value))) return(empty_pitch_plot(bg = "white"))
    
    pitchmap_generic(results_control, handedness, metric = "control", bg = "white")
  })
  
  # Average plot (correct calculation from 'score' and 'out')
  output$avg_plot <- renderPlot({
    req(input$run)
    df <- filtered_df()
    
    handedness <- df %>% filter(!is.na(bat_hand)) %>% distinct(bat_hand) %>% pull() %>% first()
    if (is.na(handedness)) handedness <- "RHB"
    
    # Only rows with non-NA score AND non-NA out are considered
    results_avg <- df %>%
      filter(!is.na(length), !is.na(line), !is.na(score), !is.na(out)) %>%
      group_by(length, line) %>%
      summarise(
        total_runs = sum(score, na.rm = TRUE),
        outs = sum(out == TRUE, na.rm = TRUE),
        value = ifelse(outs > 0, total_runs / outs, NA_real_),
        .groups = "drop"
      )
    
    if (nrow(results_avg) == 0 || all(is.na(results_avg$value))) return(empty_pitch_plot(bg = "white"))
    
    pitchmap_generic(results_avg, handedness, metric = "average", bg = "white")
  })
  
  # Strike Rate plot (correct calculation from score only)
  output$sr_plot <- renderPlot({
    req(input$run)
    df <- filtered_df()
    
    handedness <- df %>% filter(!is.na(bat_hand)) %>% distinct(bat_hand) %>% pull() %>% first()
    if (is.na(handedness)) handedness <- "RHB"
    
    results_sr <- df %>%
      filter(!is.na(length), !is.na(line), !is.na(score)) %>%
      group_by(length, line) %>%
      summarise(
        total_runs = sum(score, na.rm = TRUE),
        balls = n(),
        value = ifelse(balls > 0, 100 * total_runs / balls, NA_real_),
        .groups = "drop"
      )
    
    # one-time note under plots
    output$sr_note <- renderUI({
      HTML("<span style='color:#111111; font-size:14px;'>
           <i>Note: Colour scalings have not been used for Strike Rate pitchmaps because strike rates often do not induce any primary inference in Test cricket.</i>
           </span>")
    })
    
    if (nrow(results_sr) == 0 || all(is.na(results_sr$value))) return(empty_pitch_plot(bg = "white"))
    
    pitchmap_generic(results_sr, handedness, metric = "sr", bg = "white")
  })
  
  # nodata message when ALL three metrics empty
  output$nodata_msg <- renderUI({
    req(input$run)
    df <- filtered_df()
    
    # control summary
    results_control <- df %>%
      filter(!is.na(length), !is.na(line)) %>%
      group_by(length, line) %>%
      summarise(total = n(), control = sum(control == 1, na.rm = TRUE), value = 100 * control / total, .groups = "drop")
    # average summary
    results_avg <- df %>%
      filter(!is.na(length), !is.na(line), !is.na(score), !is.na(out)) %>%
      group_by(length, line) %>%
      summarise(total_runs = sum(score, na.rm = TRUE), outs = sum(out == TRUE, na.rm = TRUE), value = ifelse(outs > 0, total_runs / outs, NA_real_), .groups = "drop")
    # sr summary
    results_sr <- df %>%
      filter(!is.na(length), !is.na(line), !is.na(score)) %>%
      group_by(length, line) %>%
      summarise(total_runs = sum(score, na.rm = TRUE), balls = n(), value = ifelse(balls > 0, 100 * total_runs / balls, NA_real_), .groups = "drop")
    
    empty_control <- (nrow(results_control) == 0) || all(is.na(results_control$value)) || all(results_control$value == 0, na.rm = TRUE)
    empty_avg     <- (nrow(results_avg) == 0) || all(is.na(results_avg$value))
    empty_sr      <- (nrow(results_sr) == 0) || all(is.na(results_sr$value))
    
    if (empty_control && empty_avg && empty_sr) {
      HTML("<b><span style='color:red; font-size:18px;'>Sorry, no ball by ball data is available for your query!</span></b>")
    } else {
      NULL
    }
  })
}

# ---------------------------------------------------------
# JS handler — toggles white main panel and makes data notes black
# ---------------------------------------------------------
js <- "
Shiny.addCustomMessageHandler('changeMainPanelBg', function(message) {
  var panel = document.getElementById('main_panel');
  if(panel) {
    panel.classList.add('white-bg');
  }
  var notes = document.getElementById('data_notes');
  if(notes) {
    notes.style.color = '#000000';
  }
});
"

shinyApp(ui = tagList(ui, tags$script(HTML(js))), server = server)
