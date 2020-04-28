library(shiny)
library(ggplot2)
library(ggthemes)
library(viridis)
library(scales)
library(dplyr)
library(sf)
library(shinythemes)
library(leaflet)
library(RColorBrewer)

# Load data
appdata <- readRDS("~/Git/gates/rivanna_data/working/appdata.Rds")

# User interface
ui <- fluidPage(theme = shinytheme("cosmo"),
  tags$style(type  ="text/css",
             ".recalculating {opacity: 1.0;}"
  ),
  
  tags$head(tags$style(HTML(" .sidebar { font-size: 50%; } "))),
  
  titlePanel("Computing Devices and Internet Connectivity Explorer"),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(width = 3,
                 h3("Selector", align = "left"),
                 p(), 
                 selectInput("whichstate", "State", 
                             choices = list("Iowa",
                                            "Oregon",
                                            "Virginia"), 
                             selected = "Iowa"),
                 p(), 
                 selectInput("whichvariable", "Variable", 
                              choices = list("% households with no internet access",
                                             "% households with dial-up internet only",
                                             "% households with cellular internet only",
                                             "% households with satellite internet only",
                                             "% households with broadband internet",
                                             "% households making <10k/year and no internet subscription",
                                             "% households making 10-19.9k/year with no internet subscription",
                                             "% households making 20-34.9k/year with no internet subscription",
                                             "% households making 35-49.9k/year with no internet subscription",
                                             "% households making 50-74.9k/year with no internet subscription",
                                             "% households making >75k/year with no internet subscription",
                                             "% households with smartphone only",
                                             "% households with no computing device",
                                             "% children (age <18) without a computer",
                                             "% adults (age 18-64) without a computer",
                                             "% older adults (age 65+) without a computer",
                                             "% population in labor force without a computer",
                                             "% employed population without a computer",
                                             "% unemployed population without a computer"), 
                              selected = "% households with no internet access")
    ),
    mainPanel(width = 9,
      h3("County-Level Map", align = "left"),
      # plotOutput("map", width = "800px"), static map not currently using
      leafletOutput("leafplot", width = "800px")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Dynamic map
  output$leafplot <- renderLeaflet({
    
    selectedgeo <- switch(input$whichstate,
                          "Iowa" = appdata[appdata$STATEFP == "19", ],
                          "Oregon" = appdata[appdata$STATEFP == "41", ],
                          "Virginia" = appdata[appdata$STATEFP == "51", ])
    
    selectedgeo <- st_transform(selectedgeo, 4326)
    
    selectedvar <- switch(input$whichvariable,
                          "% households with no internet access" = selectedgeo$int_none,
                          "% households with dial-up internet only" = selectedgeo$int_dialup,
                          "% households with cellular internet only" = selectedgeo$int_cell,
                          "% households with satellite internet only" = selectedgeo$int_sat,
                          "% households with broadband internet" = selected_geo$int_bband,
                          "% households making <10k/year and no internet subscription" = selectedgeo$int_none_10kless,
                          "% households making 10-19.9k/year with no internet subscription" = selectedgeo$int_none_1019k,
                          "% households making 20-34.9k/year with no internet subscription" = selectedgeo$int_none_2034k,
                          "% households making 35-49.9k/year with no internet subscription" = selectedgeo$int_none_3549k,
                          "% households making 50-74.9k/year with no internet subscription" = selectedgeo$int_none_5075k,
                          "% households making >75k/year with no internet subscription" = selectedgeo$int_none_75kovr,
                          "% households with smartphone only" = selectedgeo$dev_phone,
                          "% households with no computing device" = selectedgeo$dev_none,
                          "% children (age <18) without a computer" = selectedgeo$nocomp_child,
                          "% adults (age 18-64) without a computer" = selectedgeo$nocomp_adult,
                          "% older adults (age 65+) without a computer" = selectedgeo$nocomp_old,
                          "% population in labor force without a computer" = selectedgeo$nocomp_labor,
                          "% employed population without a computer" = selectedgeo$nocomp_emp,
                          "% unemployed population without a computer" = selectedgeo$nocomp_unemp)
    
    pal <- colorBin("Blues", domain = selectedvar, bins = 5)
    
    labels <- lapply(
      paste("<strong>County: </strong>",
            selectedgeo$NAME,
            "<br />",
            "<strong>Value: </strong>",
            round(selectedvar, 2),"%"),
      htmltools::HTML
    )
    
    leaflet(selectedgeo) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(selectedvar), 
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
                                              ))) %>%
      addLegend(position = "bottomleft",
                values = ~(round(selectedvar, 0)),
                colors = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026"),
                labels = c("one", "two", "three", "four", "five"),
                title = "Percent",
                opacity = 0.8)
    
  })
  
  # Static map (not using)
  # output$map <- renderPlot({
  #   
  #   selectedgeo <- switch(input$whichstate,
  #                         "Iowa" = appdata[appdata$STATEFP == "19", ],
  #                         "Oregon" = appdata[appdata$STATEFP == "41", ],
  #                         "Virginia" = appdata[appdata$STATEFP == "51", ])
  #   
  #   selectedvar <- switch(input$whichvariable,
  #                         "% households with no internet access" = selectedgeo$int_none,
  #                         "% households with dial-up internet only" = selectedgeo$int_dialup,
  #                         "% households with cellular internet only" = selectedgeo$int_cell,
  #                         "% households with satellite internet only" = selectedgeo$int_sat,
  #                         "% households with broadband internet" = selected_geo$int_bband,
  #                         "% households making <10k/year and no internet subscription" = selectedgeo$int_none_10kless,
  #                         "% households making 10-19.9k/year with no internet subscription" = selectedgeo$int_none_1019k,
  #                         "% households making 20-34.9k/year with no internet subscription" = selectedgeo$int_none_2034k,
  #                         "% households making 35-49.9k/year with no internet subscription" = selectedgeo$int_none_3549k,
  #                         "% households making 50-74.9k/year with no internet subscription" = selectedgeo$int_none_5075k,
  #                         "% households making >75k/year with no internet subscription" = selectedgeo$int_none_75kovr,
  #                         "% households with smartphone only" = selectedgeo$dev_phone,
  #                         "% households with no computing device" = selectedgeo$dev_none,
  #                         "% children (age <18) without a computer" = selectedgeo$nocomp_child,
  #                         "% adults (age 18-64) without a computer" = selectedgeo$nocomp_adult,
  #                         "% older adults (age 65+) without a computer" = selectedgeo$nocomp_old,
  #                         "% population in labor force without a computer" = selectedgeo$nocomp_labor,
  #                         "% employed population without a computer" = selectedgeo$nocomp_emp,
  #                         "% unemployed population without a computer" = selectedgeo$nocomp_unemp)
  #   
  #   # Plot
  #   plot_var <- ggplot() +
  #     geom_sf(data = selectedgeo, size = 0.2, aes(fill = selectedvar)) +
  #     labs(fill = "Percent") +
  #     theme_map() +
  #     theme(plot.title = element_text(size = 13, face = "plain", hjust = 0.5),
  #           legend.title = element_text(size = 11, face = "bold"),
  #           legend.text = element_text(size = 10),
  #           legend.position = "bottom",
  #           legend.box.just = "center") +
  #     scale_fill_viridis_c()
  #   
  #   # Display plot
  #   plot_var
  # })
  
}


# App
shinyApp(ui = ui, server = server)