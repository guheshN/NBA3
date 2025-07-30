# Load necessary libraries
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)

# Load data
shot_data <- read.csv("shot_data.csv")

# Filter for 3-point shots
three_point_data <- shot_data %>%
  filter(SHOT_TYPE == "3PT Field Goal") %>%
  group_by(SEASON_1) %>%
  summarize(
    total_attempts = n(),
    fg_percentage = mean(SHOT_MADE == "TRUE")
  )

shot_frequency <- shot_data %>%
  filter(SHOT_TYPE %in% c("2PT Field Goal", "3PT Field Goal")) %>%
  group_by(SEASON_1, SHOT_TYPE) %>%
  summarize(frequency = n()) %>%
  ungroup()

# Define court dimensions and themes
width <- 50
height <- 47
key_height <- 19
inner_key_width <- 12
outer_key_width <- 16
backboard_width <- 6
backboard_offset <- 4
neck_length <- 0.5
hoop_radius <- 0.75
hoop_center_y <- backboard_offset + neck_length + hoop_radius
three_point_radius <- 23.75
three_point_side_radius <- 22
three_point_side_height <- 14

# Define court themes
court_themes = list(
  light = list(
    court = 'floralwhite',
    lines = 'black',
    text = '#222222',
    hex_border_size = 1,
    hex_border_color = "#000000"
  )
)

# Function to create circular points
circle_points <- function(center = c(0, 0), radius = 1, npoints = 360) {
  angles <- seq(0, 2 * pi, length.out = npoints)
  return(data.frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

# Main court plotting function
plot_court <- function(court_theme = court_themes$light) {
  court_points <- data.frame(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )

  # Outer key rectangle
  court_points <- bind_rows(
    court_points,
    data.frame(x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
               y = c(0, key_height, key_height, 0),
               desc = "outer_key")
  )

  # Backboard
  court_points <- bind_rows(
    court_points,
    data.frame(x = c(-backboard_width / 2, backboard_width / 2),
               y = c(backboard_offset, backboard_offset),
               desc = "backboard")
  )

  # Hoop
  hoop <- circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>% mutate(desc = "hoop")

  # Restricted area
  restricted <- circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")

  # Foul circle
  foul_circle <- circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  foul_circle_top <- filter(foul_circle, y > key_height) %>%
    mutate(desc = "foul_circle_top")
  foul_circle_bottom <- filter(foul_circle, y < key_height) %>%
    mutate(desc = "foul_circle_bottom")

  # Three-point line
  three_point_circle <- circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  three_point_line <- data.frame(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )

  # Combine all elements
  court_points <- bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )

  # Return court_points for use in other functions
  list(court_points = court_points, hoop = hoop, restricted = restricted, foul_circle_top = foul_circle_top,
       foul_circle_bottom = foul_circle_bottom, three_point_line = three_point_line)
}


ui <- fluidPage(
  # Custom CSS for background color
  tags$head(tags$style(HTML("
    body { background-color: floralwhite; }
    .plot-container .plotly { background-color: floralwhite !important; }
  "))),

  titlePanel("NBA 3-Point Revolution"),

  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Shot Charts'", # Display sidebar only on Shot Locations tab
        selectInput("teamInput", "Select Team:", choices = unique(shot_data$TEAM_NAME)),
        selectInput("seasonInput", "Select Season:", choices = c("All Seasons", unique(shot_data$SEASON_1)))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Introduction",
                 fluidPage(
                   titlePanel("The Rise of the 3-Point Shot in the NBA"),
                   h3("Exploring the evolution of the 3-point shot from 2009-2019"),
                   p("The NBA has seen a significant shift in playing style over the years, with an increasing reliance on the 3-point shot. The following visualizations explore the growth in 3-point attempts and how shooting efficiency has changed from 2009 to 2019."),
                   verticalLayout(
                     plotlyOutput("threePointPlot", height = "400px"),
                     p("As you can see, teams started shooting significantly more 3 pointers but the efficiency never changed.When teams realised how good a 3 pointer was they naturally started shooting less 2 pointers as well."),
                     plotlyOutput("shotFrequencyPlot", height = "400px")
                   )
                 )
        ),
        tabPanel("Shot Charts",
                 fluidPage(
                   titlePanel("NBA team Shot Charts from 2009-2019"),
                   p("To get a better visual understanding of how teams shot throughout the decade, you can pick a team and view their individual seasons or select all seasons and watch them in a timelapse."),
                   plotlyOutput("shotLocationsPlot", height = "800px"),
                   p("As you can see, over the seasons teams generally started taking less long 2 pointers and started shooting more3 pointers especially in the corners. ")
                 )
        )
      )
    )
  )
)




server <- function(input, output) {
  court_points <- plot_court(court_themes$light)

  # Filtered Data based on team and season
  filtered_data <- reactive({
    data <- shot_data %>%
      filter(TEAM_NAME == input$teamInput,
             LOC_X >= -25, LOC_X <= 25,
             LOC_Y >= 0, LOC_Y <= 45)

    # Apply season filter if a specific season is selected otherwise, include all seasons
    if (input$seasonInput != "All Seasons") {
      data <- data %>% filter(SEASON_1 == input$seasonInput)
    }

    data %>%
      group_by(SEASON_1, LOC_X, LOC_Y, SHOT_TYPE) %>%
      summarize(attempts = n(), makes = sum(SHOT_MADE == "TRUE")) %>%
      ungroup() %>%  # Ensure proper frame handling
      mutate(SEASON_1 = as.factor(SEASON_1))  # Convert SEASON_1 to factor
  })
  # Plot for 3-point attempts and FG%
  output$threePointPlot <- renderPlotly({

    three_point_data <- three_point_data %>%
      mutate(
        fg_percentage_scaled = fg_percentage * max(total_attempts),
        hover_text = paste0("3-Point FG%: ", round(fg_percentage * 100, 1), "%")
      )

    # Create the base ggplot
    p <- ggplot(three_point_data, aes(x = SEASON_1)) +
      geom_bar(aes(y = total_attempts), stat = "identity", fill = "steelblue", alpha = 0.6) +
      geom_line(aes(y = fg_percentage_scaled, group = 1, text = hover_text), color = "orange", size = 1.2) +
      geom_point(aes(y = fg_percentage_scaled, text = hover_text), color = "orange", size = 2) +
      scale_y_continuous(
        name = "Total 3-Point Attempts",
        sec.axis = sec_axis(~ . / max(three_point_data$total_attempts), name = "3-Point FG%")
      ) +
      scale_x_continuous(
        breaks = seq(min(three_point_data$SEASON_1), max(three_point_data$SEASON_1), by = 1),
        name = "Season"
      ) +
      labs(title = "3-Point Attempts and Field Goal Percentage Over Time") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))


    ggplotly(p, tooltip = "text") %>%
      layout(
        yaxis2 = list(overlaying = "y", side = "right", title = "3-Point FG%"),
        title = list(text = "3-Point Attempts and Field Goal Percentage Over Time"),
        xaxis = list(title = "Season"),
        yaxis = list(title = "Total 3-Point Attempts"),
        plot_bgcolor = "floralwhite",
        paper_bgcolor = "floralwhite"
      )
  })

  # Plot frequency of 2PT and 3PT shots over the seasons
  output$shotFrequencyPlot <- renderPlotly({
    max_frequency <- max(shot_frequency$frequency, na.rm = TRUE)

    p <- ggplot(shot_frequency, aes(x = SEASON_1, y = frequency, color = SHOT_TYPE, group = SHOT_TYPE)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = c("2PT Field Goal" = "orange", "3PT Field Goal" = "steelblue")) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max_frequency * 1.05)) +
      scale_x_continuous(
        breaks = seq(min(shot_frequency$SEASON_1), max(shot_frequency$SEASON_1), by = 1),
        name = "Season"
      ) +
      labs(
        title = "Frequency of 2-Point and 3-Point Shots Throughout the Seasons",
        x = "Season",
        y = "Frequency of Shots",
        color = "Shot Type"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(
        plot_bgcolor = "floralwhite",
        paper_bgcolor = "floralwhite"
      )
  })


  # Render Shot Locations Plot with dynamic year range animation
  output$shotLocationsPlot <- renderPlotly({
    plot_data <- filtered_data()

    # Base court plot
    plot <- ggplot() +
      geom_path(data = court_points$court_points, aes(x = x, y = y, group = desc), color = court_themes$light$lines, size = 0.5) +
      geom_path(data = court_points$hoop, aes(x = x, y = y, group = desc), color = court_themes$light$lines, size = 0.5) +
      geom_path(data = court_points$restricted, aes(x = x, y = y, group = desc), color = court_themes$light$lines, size = 0.5) +
      geom_path(data = court_points$foul_circle_top, aes(x = x, y = y, group = desc), color = court_themes$light$lines, size = 0.5, linetype = "dashed") +
      geom_path(data = court_points$foul_circle_bottom, aes(x = x, y = y, group = desc), color = court_themes$light$lines, size = 0.5) +
      geom_path(data = court_points$three_point_line, aes(x = x, y = y, group = desc), color = court_themes$light$lines, size = 0.5) +
      coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = court_themes$light$court, color = court_themes$light$court),
        panel.background = element_rect(fill = court_themes$light$court, color = court_themes$light$court),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none"
      ) +
      labs(size = "Attempts", color = "Shot Type")

    # Animate shot locations within selected year range or specific season
    plot <- plot +
      geom_point(data = plot_data, aes(x = LOC_X, y = LOC_Y, color = SHOT_TYPE, size = attempts, frame = SEASON_1), alpha = 0.6) +
      scale_color_manual(values = c("3PT Field Goal" = "steelblue", "2PT Field Goal" = "orange")) +
      labs(title = ifelse(input$seasonInput != "All Seasons",
                          paste("Shot Chart for", input$seasonInput),
                          "Shot Chart for All Seasons"))

    # Convert to Plotly with smooth animation settings
    ggplotly(plot, tooltip = c("x", "y", "color", "size")) %>%
      animation_opts(
        frame = 1000,
        transition = 500,
        easing = "linear"
      ) %>%
      layout(
        legend = list(orientation = "h"),
        hoverlabel = list(bgcolor = "rgba(255,255,255,0.8)"),
        updatemenus = list(
          list(
            type = "buttons",
            showactive = FALSE,
            x = 0.1, y = -0.2, xanchor = "center", yanchor = "top",
            pad = list(r = 10, t = 10)
          )
        ),
        annotations = list(
          list(
            text = "Season:", x = 0.5, y = -0.1, showarrow = FALSE,
            xref = "paper", yref = "paper", font = list(size = 12)
          )
        )
      ) %>%
      style(hoverinfo = "text")
  })
}

shinyApp(ui = ui, server = server)



