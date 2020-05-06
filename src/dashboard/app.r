library(shiny)
library(viridis)
library(scales)
library(dplyr)
library(sf)
library(shinythemes)
library(leaflet)
library(readr)
library(ggplot2)
library(htmlwidgets)
library(htmltools)
library(ggthemes)
library(sf)
library(RColorBrewer)

# Load data
data_med <- read_rds("~/Git/gates/rivanna_data/working/dashboard/data_med.Rds")
data_work <- read_rds("~/Git/gates/rivanna_data/working/dashboard/data_work.Rds")
data_edu <- read_rds("~/Git/gates/rivanna_data/working/dashboard/data_edu.Rds")

# User interface
ui <- fluidPage(theme = shinytheme("cosmo"),
                
                tags$style(type = "text/css",
                           ".recalculating {opacity: 1.0;}"
                ),
                
                tags$head(tags$style(HTML(" .sidebar { font-size: 40%; } "))
                ),
                
                headerPanel(img(src = "logo.png", class = "topimage", width = "50%", style = "display: block; margin-left: auto; margin-right: auto;")
                ),
                
                hr(),
                
                fluidRow(width = 12, 
                         column(12, align = "center", h2(strong("County Explorer")))
                ),
                
                hr(),
                
                fluidRow(width = 12, 
                         column(12, align = "left", h3(strong("Explore County Rankings")))
                ),
                
                fluidRow(
                  column(width = 3,
                         h4(strong("Selector"), align = "left"),
                         p(), 
                         selectInput("whichtopic", "Topic", 
                                     choices = list("Remote education",
                                                    "Remote work",
                                                    "Telehealth"), 
                                     selected = "Remote education"),
                         p(),
                         selectInput("whichstate", "State", 
                                     choices = list("Iowa",
                                                    "Oregon",
                                                    "Virginia"), 
                                     selected = "Iowa")
                  ),
                  column(width = 9, 
                         h4(strong("County Map"), align = "left"),
                         p(),
                         leafletOutput("mainplot", width = "800px")
                  )
                ),
                
                hr(),
                
                fluidRow(width = 12, 
                         column(12, align = "left", h3(strong("Explore Ranking Indicators")))
                ),
                
                fluidRow(width = 12, 
                         column(5, align = "left",
                                conditionalPanel("input.whichtopic == 'Remote education'",
                                                 "Quintile Cut-offs",
                                                 p(),
                                                 tableOutput("output$table_quint")
                                                 )
                         ),
                         column(7, align = "left",
                                conditionalPanel("input.whichtopic == 'Remote education'",
                                                 tabsetPanel(
                                                   tabPanel("No internet", leafletOutput("output$subplot_noint")),
                                                   tabPanel("No computer", leafletOutput("output$subplot_nocomp")),
                                                   tabPanel("Population in K-12", leafletOutput("output$subplot_ink12"))
                                                 ))
                         )
                )
                
)

# Server

server <- function(input, output, session) {
  
  # MAIN PLOTS
  output$mainplot <- renderLeaflet({
   
    # Main plot: remote education
    if (input$whichtopic == "Remote education") {
      data <- data_edu
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
    
      pal <- colorFactor("Oranges", domain = data$vulnerability)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Relative vulnerability:</strong>",
              data$vulnerability,
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$scoreTop, " times",
              "<br />",
              "<strong>% children (age <18) without internet access:</strong>",
              round(data$nointernet, 2), "(quintile", data$nointernetQuint, ")",
              "<br />",
              "<strong>% households without a computer:</strong>",
              round(data$nocomputer, 2), "(quintile", data$nocomputerQuint, ")",
              "<br />",
              "<strong>% population enrolled in K-12:</strong>",
              round(data$ink12, 2), "(quintile", data$ink12Quint, ")"),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$vulnerability), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                )))
      
    # Main plot: remote work
    } else if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorFactor("Oranges", domain = data$vulnerability)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Relative vulnerability:</strong>",
              data$vulnerability,
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$scoreTop, " times",
              "<br />",
              "<strong>% households without internet access:</strong>",
              round(data$nointernet, 2), "(quintile", data$nointernetQuint, ")",
              "<br />",
              "<strong>% population in labor force without a computer:</strong>",
              round(data$nocomputer, 2), "(quintile", data$nocomputerQuint, ")",
              "<br />",
              "<strong>% non-remote workers in non-remote friendly occupations:</strong>",
              round(data$occup, 2), "(quintile", data$occupQuint, ")",
              "<br />",
              "<strong>% non-remote workers in non-remote friendly industries:</strong>",
              round(data$industr, 2), "(quintile", data$industrQuint, ")"),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$vulnerability), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                )))
      
    # Main plot: telehealth
    } else if (input$whichtopic == "Telehealth") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorFactor("Oranges", domain = data$vulnerability)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Relative vulnerability:</strong>",
              data$vulnerability,
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$scoreTop, " times",
              "<br />",
              "<strong>% households without internet access:</strong>",
              round(data$nointernet, 2), "(quintile", data$nointernetQuint, ")",
              "<br />",
              "<strong>% households without a computer:</strong>",
              round(data$nocomputer, 2), "(quintile", data$nocomputerQuint, ")",
              "<br />",
              "<strong>% population without health insurance:</strong>",
              round(data$pct_unins, 2), "(quintile", data$uninsQuint, ")",
              "<br />",
              "<strong>Number of mental health providers per 100,000:</strong>",
              round(data$menthprov_per100k, 2), "(quintile", data$menthprovQuint, ")",
              "<br />",
              "<strong>Average number of poor mental health days:</strong>",
              round(data$avgnum_poormenth, 2), "(quintile", data$menthdaysQuint, ")"),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$vulnerability), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                )))

  }
  })

  # QUINTILE TABLES
  
  quintTopic <- reactive({ 
    switch(input$whichtopic,
                  "Remote education" = data_edu,
                  "Remote work" = data_work,
                  "Telehealth" = data_med)
    })
    
  quintData <- reactive({
    switch(input$whichstate,
           "Iowa" = quintTopic[quintTopic$STATEFP == "19", ],
           "Oregon" = quintTopic[quintTopic$STATEFP == "41", ],
           "Virginia" = quintTopic[quintTopic$STATEFP == "51", ])
  })
  
  output$table_quint <- renderTable({
      qnoint <- quantile(quintData$nointernet, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qnocomp <- quantile(quintData$nocomputer, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qink12 <- quantile(quintData$ink12, prob = seq(0, 1, 0.2), na.rm = TRUE)
      
      quintcuts <- bind_rows(qnoint, qnocomp, qink12)
      quintcuts$id <- c("% children without internet access", "% population with no computer", "% population enrolled in K-12")
      quintcuts
  })

}


# App
shinyApp(ui = ui, server = server)