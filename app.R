library(shiny)
library(plotly)
library(dplyr)

wp_results <- readRDS('./set_wp_results.rds')
data <- readRDS('./data.RDS')

ui <- fluidPage(
  titlePanel("Volleyball Win Probability Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("match_choice", "Select a Match", choices = names(wp_results)),
      uiOutput("set_selector"),
      checkboxInput("rice_perspective", "Rice Perspective", value = TRUE),
      helpText("Toggle to switch between Rice and opponent perspective"),
      hr(),
      strong(textOutput("home_team_display")),
      br(),
      strong(textOutput("away_team_display")),
      hr(),
      h4("Set Summary"),
      tableOutput("match_stats")
    ),
    
    mainPanel(
      h3(textOutput("match_title")),
      verbatimTextOutput("finalScore"),
      plotlyOutput("scorePlot", height = "400px"),
      hr()
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamically get available sets for the selected match
  available_sets <- reactive({
    req(input$match_choice)
    names(wp_results[[input$match_choice]])
  })
  
  # Set selector UI
  output$set_selector <- renderUI({
    selectInput("set_choice", "Select a Set", choices = available_sets())
  })
  
  # Reactive dataframe for selected set
  selected_set_data <- reactive({
    req(input$match_choice, input$set_choice)
    wp_results[[input$match_choice]][[input$set_choice]]
  })
  
  # Get raw match data for the selected set
  raw_match_data <- reactive({
    req(input$match_choice, input$set_choice)
    match_name <- gsub("Match: ", "", input$match_choice)
    set_num <- as.numeric(gsub("Set ", "", input$set_choice))
    
    data %>% 
      filter(match == match_name, 
             set_number == set_num) %>%  # Make sure we filter by set number
      arrange(point_id)  # Ensure data is in rally order
  })
  
  # Display home team
  output$home_team_display <- renderText({
    req(input$match_choice)
    match_data_single_row <- data %>%
      filter(match == gsub("Match: ", "", input$match_choice)) %>%
      head(1)
    if (nrow(match_data_single_row) > 0) {
      paste("Home Team:", unique(match_data_single_row$home_team))
    } else {
      "Home Team:"
    }
  })
  
  # Display away team
  output$away_team_display <- renderText({
    req(input$match_choice)
    match_data_single_row <- data %>%
      filter(match == gsub("Match: ", "", input$match_choice)) %>%
      head(1)
    if (nrow(match_data_single_row) > 0) {
      paste("Away Team:", unique(match_data_single_row$visiting_team))
    } else {
      "Away Team:"
    }
  })
  
  # Match title
  output$match_title <- renderText({
    req(input$match_choice, input$set_choice)
    paste(gsub("Match: ", "", input$match_choice), "-", input$set_choice)
  })
  
  # Final score display
  output$finalScore <- renderPrint({
  df <- raw_match_data()
  if (nrow(df) == 0) return("No data available")

  # Get last rally row
  last_row <- df[nrow(df), ]

  home_team <- unique(df$home_team)
  away_team <- unique(df$visiting_team)

  # Check who won the final point
  if (last_row$point_won_by == home_team) {
    home_final <- last_row$home_score_start_of_point + 1
    away_final <- last_row$visiting_score_start_of_point
  } else {
    home_final <- last_row$home_score_start_of_point
    away_final <- last_row$visiting_score_start_of_point + 1
  }

  paste0("Final Score: ", home_team, " ", home_final, " - ", 
         away_final, " ", away_team)
})

  
  # Function to calculate average passing grade
  calculate_passing_grade <- function(df, team_name) {
    team_passes <- df %>%
      filter(skill == "Reception", team == team_name) %>%
      select(evaluation_code)
    
    if (nrow(team_passes) == 0) {
      return(NA)
    }
    
    team_passes <- team_passes %>%
      mutate(
        pass_value = case_when(
          evaluation_code == "#" ~ 4,
          evaluation_code == "+" ~ 3,
          evaluation_code == "!" ~ 2,
          evaluation_code == "-" ~ 1,
          evaluation_code == "/" ~ 1,
          evaluation_code == "=" ~ 0,
          TRUE ~ NA_real_ # Handle other codes if necessary
        )
      ) %>%
      filter(!is.na(pass_value)) # Remove any unrated passes
    
    total_pass_points <- sum(team_passes$pass_value, na.rm = TRUE)
    total_passes <- nrow(team_passes)
    
    if (total_passes > 0) {
      return(total_pass_points / total_passes)
    } else {
      return(0) # Or NA, depending on how you want to handle no passes
    }
  }
  
  
  # Match statistics
  output$match_stats <- renderTable({
    df <- raw_match_data()
    if(nrow(df) == 0) return(data.frame())
    
    # Calculate basic stats
    home_team <- unique(df$home_team)
    away_team <- unique(df$visiting_team)
    
    home_won <- ifelse(max(df$home_score_start_of_point, na.rm = TRUE) > 
                         max(df$visiting_score_start_of_point, na.rm = TRUE), 
                       "Yes", "No")
    
    home_aces <- sum(df$skill == "Serve" & df$evaluation_code == "#" & df$team == home_team, na.rm = TRUE)
    away_aces <- sum(df$skill == "Serve" & df$evaluation_code == "#" & df$team == away_team, na.rm = TRUE)
    
    home_attacks <- sum(df$skill == "Attack" & df$team == home_team, na.rm = TRUE)
    away_attacks <- sum(df$skill == "Attack" & df$team == away_team, na.rm = TRUE)
    
    home_kills <- sum(df$skill == "Attack" & df$evaluation_code == "#" & df$team == home_team, na.rm = TRUE)
    away_kills <- sum(df$skill == "Attack" & df$evaluation_code == "#" & df$team == away_team, na.rm = TRUE)
    
    home_errors <- sum(df$skill == "Attack" & (df$evaluation_code == "/" | df$evaluation_code == "=") & df$team == home_team, na.rm = TRUE)
    away_errors <- sum(df$skill == "Attack" & (df$evaluation_code == "/" | df$evaluation_code == "=") & df$team == away_team, na.rm = TRUE)
    
    home_eff <- round((home_kills - home_errors)/home_attacks, 3)
    away_eff <- round((away_kills - away_errors)/away_attacks, 3)
    
    home_passing <- round(calculate_passing_grade(df, home_team), 2)
    away_passing <- round(calculate_passing_grade(df, away_team), 2)
    
    data.frame(
      Statistic = c("Set Winner", "Aces", "Passing Rate", "Attack Efficiency", "Kills", "Total Attacks"),
      Home = c(home_won, home_aces, home_passing, home_eff, home_kills, home_attacks),
      Away = c(ifelse(home_won == "Yes", "No", "Yes"), away_aces, away_passing, away_eff, away_kills, away_attacks),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE)
  

  # In the scorePlot renderPlotly function:
  output$scorePlot <- renderPlotly({
    df <- selected_set_data()
    raw_df <- raw_match_data()
    
    if(nrow(df) == 0) return(plotly_empty())
    
    # Adjust perspective
    if(input$rice_perspective) {
      y_values <- df$WinProbability_Rice
      y_title <- "Win Probability (Rice) at Rally Start"
      color_threshold <- 50
    } else {
      y_values <- 100 - df$WinProbability_Rice
      y_title <- "Win Probability (Opponent) at Rally Start"
      color_threshold <- 50
    }
    
    # Create color vector based on win probability
    colors <- ifelse(y_values >= color_threshold, "green", "red")
    
    # Get scores for each rally - match by rally order since point_id might not match
    rally_scores <- raw_df %>%
      group_by(point_id) %>%
      summarize(
        home_score = first(home_score_start_of_point),
        away_score = first(visiting_score_start_of_point),
        .groups = "drop"
      ) %>%
      arrange(point_id)  # Ensure scores are in order
    
    # Since we can't guarantee point_id matches Rally number, we'll match by position
    plot_data <- df %>%
      mutate(
        home_score = rally_scores$home_score[1:nrow(df)],
        away_score = rally_scores$away_score[1:nrow(df)],
        # Calculate the change caused by this rally (next rally's WP - current WP)
        WP_Change = lead(WinProbability_Rice, default = NA) - WinProbability_Rice
      )
    
    # Create hover text to show impact of the rally
    hover_text <- paste0(
      "Rally: ", plot_data$Rally, "<br>",
      "Start WP: ", round(y_values, 1), "%<br>",
      "Score: ", plot_data$home_score, "-", plot_data$away_score, "<br>",
      ifelse(is.na(plot_data$WP_Change), 
             "Final rally", 
             paste0("WP Impact: ", sprintf("%+.1f%%", plot_data$WP_Change)))
    )
    
    plot_ly(
      data = plot_data,
      x = ~Rally,
      y = ~y_values,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "blue"),
      marker = list(color = colors, size = 8),
      hoverinfo = "text",
      text = hover_text
    ) %>%
      layout(
        title = paste("Win Probability at Start of Each Rally -", input$set_choice),
        xaxis = list(title = "Rally Number"),
        yaxis = list(title = y_title, range = c(0, 100)),
        shapes = list(
          list(
            type = "line",
            x0 = 0,
            x1 = max(df$Rally, na.rm = TRUE),
            y0 = 50,
            y1 = 50,
            line = list(color = "gray", dash = "dot")
          )
        ),
        hovermode = "closest"
      )
  })
  
}

shinyApp(ui = ui, server = server)