
# -------------------------------------------------------------------------
# Shiny App for Assignment 3 (40%) ----------------------------------------
# -------------------------------------------------------------------------

# Import libraries --------------------------------------------------------

#Shiny
library(shiny)
library(shinythemes)

# Data visualisation
library(leaflet)
library(leaflet.extras)
library(sf)
library(plotly)
library(ggplot2)
library(RColorBrewer)

# data wrangling libraries
library(tidyverse)
library(dplyr)
library(stringr)

# import file libaries
library(readr)


# Read in data ------------------------------------------------------------

# Graph data
df_family_merged <- read_csv("df_family_incidents.csv")
df_harm_safest10 <- read_csv("df_harm_safest10.csv")
df_offence_recentyear <- read_csv("df_offence_recentyear.csv")
df_offence_top5_lga <- read_csv("df_offence_top5_lga.csv")

# Spatial Data
shapefile <- st_read("vic_shape_simple.shp")

# User Interface ----------------------------------------------------------

# Define UI
# Define UI
ui <- shinyUI(fluidPage(
  shinytheme("cerulean"),
  titlePanel(h3("Which LGA is the safest to live in Greater Melbourne?")),
  fluidRow(
    column(12, h6("Greater Melbourne Local Government Areas by crime offences per capital, harm caused and degree of family violence."))
  ),
  selectInput("year_select", "Select Year for Map", choices = unique(df_offence_top5_lga$year)),
  tags$style(HTML("
    .section-container {
      height: 40vh; /* 45% of the viewport height */
      margin: 1px;
      border: none;
      overflow: auto;
    }
    .plot-output {
      height: 100%; /* Adjust height to fit the content */
      margin: 1px; /* Reduce margin for tighter spacing */
      border: none;
      overflow: auto;
    }
  ")),
  fluidRow(
    column(width = 6,
           div(class = "section-container"
               , h5("Which areas have the lowest reported offences per capita?")
               , p("The below map shows the crime rate (per 100k population) for each LGA in Greater Melbourne.", style = "font-size: smaller;")
               , leafletOutput("section1_plot", height = "80%") 
               
           )
    ),
    column(width = 6,
           div(class = "section-container"
               , h5("Which areas have the highest proportion of crime that is low-harm?")
               , p("The below chart shows the 10 safest areas based on the proportion of crimes that are low-harm for year ending June 2023.",style = "font-size: smaller;")
               , plotOutput("section2_plot", height = "75%")
              
           )
    ),
    column(width = 3,
           div(class = "section-container"
               , h5("Offences by Region")
               , p("The number of offences by region in Greater Melbourne for year ending June 2023.", style = "font-size: smaller;")
               , plotOutput("section3a_plot", height = "70%") 
               
           )
    ),
    column(width = 3,
           div(class = "section-container"
               , h5("Domestic Violence Incidents")
               , p("Top 5 LGAs with the highest proportion of incidents involving family for the year ending June 2023.", style = "font-size: smaller;")
               , plotOutput("section3b_plot", height = "70%") 
               
           )
    ),
    column(width = 6,
           div(class = "section-container",
               h5("Read Me!"),
               style = "padding: 10px; border: 1px solid #DDD; background-color: #F9F9F9; height: 300px;",
               p("The data is sourced from the Victorian Government's Crime and Statistics Agency. This dashboard shows a snapshot of data for the year ending June 2023. The map in the top left corner is interactive. Select different years using the dropdown menu and hover or click on the map for more information."),  # Your explanatory text
               uiOutput("source_link_text") 
           )
    )
  )
))

# Server ------------------------------------------------------------------

# Define Server
server <- function(input, output, session) {
  
  # Leaflet plot for Section 1 ------------------------------------------------
  output$section1_plot <- renderLeaflet({
    
    # Set filter
    filtered_data <- df_offence_top5_lga %>% filter(year == input$year_select)
    # Join data to spatial data to show only the Greater Melbourne area
    spatial_data <- inner_join(shapefile, filtered_data, by = c("ABB_NAME" = "LGA"))
    # Build choropleth map in leaflet
    crime_map <- leaflet(spatial_data) %>%
      setView(lat = -37.9819, lng = 145.2145, zoom = 8) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric(
          palette = c("darkgreen", "orange", "darkred"),
          domain = spatial_data$rate_100k
        )(rate_100k),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        highlightOptions = highlightOptions(
          weight = 5, color = "#666", bringToFront = TRUE
        ),
        label = ~ABB_NAME,  # Show the LGA name as label
        layerId = ~ABB_NAME,
        popup = ~paste0(
          "Local Government Area: ", ABB_NAME, "<br>",
          "Crime Rate per 100k population: ", round(rate_100k, 0)
        )
      ) %>%
      addLegend(
        pal = colorNumeric(
          palette = c("darkgreen", "orange", "darkred"),
          domain = spatial_data$rate_100k,
          na.color = "grey"
        ),
        values = ~rate_100k,
        title = "Crime Rate",
        position = "bottomright"
      )
    
    crime_map
  })
  
  # TOP LEFT PLOT -----------------------------------------------------------
  output$section2_plot <- renderPlot({
    # GRAPH
    # Reorder the levels of 'harm_caused' for facetting
    df_harm_safest10$harm_caused <- factor(df_harm_safest10$harm_caused,
                                           levels = c('High Harm','Medium Harm','Low Harm'))
    
    # Reorder the levels of 'LGA' by the sum of 'victim_reports'
    df_harm_safest10$LGA <- factor(df_harm_safest10$LGA,
                                   levels = names(sort(tapply(df_harm_safest10$victim_reports, df_harm_safest10$LGA, sum))))
    
    # Create a stacked bar chart with LGAs on the x-axis and harm_caused facetted in the desired order
    plot_harm <- ggplot(df_harm_safest10, aes(x = reorder(LGA, victim_reports), y = victim_reports, fill = harm_caused)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = paste0(victim_reports, " (", prop_LGA * 100, "%)")), vjust = -0.5, size = 3, position = position_stack(vjust = 0.5)) +
      facet_grid(harm_caused ~ ., scales = "free_y", space = "free_y") +
      labs(x = "Local Government Area", y = "Victim Reports") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),  # Remove y-axis text
            axis.title.y = element_blank(),  # Remove y-axis label
            axis.ticks.y = element_blank(),  # Remove y-axis ticks
            axis.text.x = element_text(angle = 0),  # Adjust x-axis text for better readability
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none",
            legend.direction = "horizontal",
            legend.box = "horizontal") +
      scale_fill_manual(values = c("darkgrey", "lightgrey", "yellowgreen")) +
      guides(fill = guide_legend(title = NULL, order = 1, override.aes = list(color = NULL, size = NULL)))
    
    plot_harm
  })
  
  # BOTTOM LEFT PLOT --------------------------------------------------------
  output$section3a_plot <- renderPlot({
    # Create the ggplot object
    p_lga_vol <- ggplot(df_offence_recentyear, aes(x = reorder(police_region, -sum_offences), y = sum_offences)) +
      geom_bar(stat = "identity", fill = "grey") +  # Set the bar color to grey
      geom_text(aes(label = paste(sum_offences, "\n", scales::percent(sum_offences/sum(sum_offences)))), 
                vjust = 0.5, size = 4, position = position_stack(vjust = 0.5)) +  # Position labels in the center of bars
      labs(
        x = "Local Government Area",
        y = "Offence Count"
      ) +
      theme_minimal() +  # Apply a minimal theme to retain visibility of the chart
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
            axis.text.y = element_blank(),  # Hide y-axis text
            axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())  # Hide y-axis ticks
    
    p_lga_vol
    
  })
  
  output$section3b_plot <- renderPlot({
    
    # Create the ggplot object
    colors <- c("deepskyblue", "burlywood", "snow4", "dodgerblue4")  
    
    p_family <- ggplot(df_family_merged, aes(x = reorder(LGA, -prop_family), y = prop_family, fill = police_region)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(round(prop_family*100), "%")), 
                vjust = -0.5, size = 2.5, position = position_stack(vjust = 1.03)) +
      scale_fill_manual(values = colors) + 
      labs(x = "LGA",
           y = "% Total incidents involving family"
      ) +
      theme(legend.position = "top"
            , axis.text.x = element_text(angle = 0, hjust = 0.5)
            , axis.text.y = element_blank()  # Hide y-axis text
            , axis.ticks.y = element_blank()
            , panel.grid.major = element_blank()
            , panel.grid.minor = element_blank())  # Hide y-axis ticks)  
    
    p_family
    
  })
  
  # BOTTOM RIGHT PLOT -------------------------------------------------------
  output$section4_plot <- renderLeaflet({
    # text
  })
  
  output$source_link_text <- renderPrint({
    tags$a("Link to Data Source", href = "https://www.crimestatistics.vic.gov.au/crime-statistics/latest-victorian-crime-data/download-data")
  })
  
  
}

# Run Shiny App -----------------------------------------------------------

# Run the application
shinyApp(ui = ui, server = server)

# End ---------------------------------------------------------------------

