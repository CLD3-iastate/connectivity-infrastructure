library(shiny)
library(viridis)
library(scales)
library(dplyr)
library(sf)
library(shinythemes)
library(leaflet)
library(readr)

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
                         column(6, align = "left",
                                conditionalPanel("input.whichtopic == 'Remote education'",
                                                 leafletOutput("plots_edu$plot1"))
                                ),
                         column(6,
                                p("Blah")
                         )
                )
                
)

# Server
server <- function(input, output) {
  
  # Main map
  output$mainplot <- renderLeaflet({
    
    data <- switch(input$whichtopic,
                     "Remote work" = data_work,
                     "Remote education" = data_edu,
                     "Telehealth" = data_med)
    
    data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
    
    pal <- colorFactor("Oranges", domain = data$vulnerability)
    
    labels <- lapply(
      paste("<strong>County: </strong>",
            data$name,
            "<br />",
            "<strong>Placed in 4th or 5th quintile:</strong>",
            data$scoreTop, " times",
            "<br />",
            "<strong>Relative vulnerability:</strong>",
            data$vulnerability),
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
  })
  
  # Subplots: Edu 1
  output$plots_edu <- renderLeaflet({
    
    data_edu1 <- switch(input$whichstate,
                   "Iowa" = data_edu[data_edu$STATEFP == "19", ],
                   "Oregon" = data_edu[data_edu$STATEFP == "41", ],
                   "Virginia" = data_edu[data_edu$STATEFP == "51", ])
    
    pal_edu1 <- colorNumeric("Greens", domain = data_edu1$nointernet)
    pal_edu2 <- colorNumeric("Purples", domain = data_edu1$nocomputer)
    pal_edu3 <- colorNumeric("Blues", domain = data_edu1$ink12)
    
    labels_edu1 <- lapply(
      paste("<strong>County:</strong>",
            data_edu1$name,
            "<br />",
            "<strong>% households without internet access:</strong>",
            round(data_edu1$nointernet, 2),
            "<br />",
            "<strong>Placed in 4th or 5th quintile:</strong>",
            data_edu1$nointernetTop),
      htmltools::HTML
    )
    
    labels_edu2 <- lapply(
      paste("<strong>County:</strong>",
            data_edu1$name,
            "<br />",
            "<strong>% households without computer:</strong>",
            round(data_edu1$nocomputer, 2),
            "<br />",
            "<strong>Placed in 4th or 5th quintile:</strong>",
            data_edu1$nocomputerTop),
      htmltools::HTML
    )
    
    labels_edu3 <- lapply(
      paste("<strong>County:</strong>",
            data_edu1$name,
            "<br />",
            "<strong>% population enroled in K-12:</strong>",
            round(data_edu1$nocomputer, 2),
            "<br />",
            "<strong>Placed in 4th or 5th quintile:</strong>",
            data_edu1$ink12Top),
      htmltools::HTML
    )
    
    plot1 <- leaflet(data_edu1) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal_edu1(data_edu1$nointernet), 
                  fillOpacity = 0.8,
                  stroke = TRUE,
                  weight = 0.9,
                  color = "gray",
                  smoothFactor = 0.7,
                  label = labels_edu1,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              )))
    
    plot2 <- leaflet(data_edu1) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal_edu2(data_edu1$nocomputer), 
                  fillOpacity = 0.8,
                  stroke = TRUE,
                  weight = 0.9,
                  color = "gray",
                  smoothFactor = 0.7,
                  label = labels_edu2,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              )))
  })
  
}


# App
shinyApp(ui = ui, server = server)