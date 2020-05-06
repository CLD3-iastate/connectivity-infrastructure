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
appdata <- readRDS("~/gates/rivanna_data/working/higher_ed/appdata.Rds")
appdata_nogeo <- appdata
appdata_nogeo$geometry = NULL


# User interface
ui <- fluidPage(theme = shinytheme("cosmo"),
  tags$style(type  ="text/css",
             ".recalculating {opacity: 1.0;}"
  ),
  
  tags$head(tags$style(HTML(" .sidebar { font-size: 50%; } "))),
  
  titlePanel("College and University Enrollment"),  
  
  sidebarLayout(
    position = "left",
    sidebarPanel(width = 3,
                 h3("Selector", align = "left"),
                 p(), 
                 selectInput("whichstate", "State",
                             choices = list("Iowa",
                                            "Oregon",
                                            "Virginia"),
                             selected = "Virginia"),
                 p(),
                 selectInput("whichtype", "Higher-Education Type",
                              choices = list("Grand Total, All Reporting Institutions",
                                             "Total Private, Non-Profit, Four-Year Institutions",
                                             "Total Public Four-Year Institutions",
                                             "Total Public Two-Year Institutions"
                                             ),
                                        selected = "Grand Total, All Reporting Institutions"),
                 p(),
                 selectInput("whichvar", "Demographic Variable", 
                             choices = list("% White",
                                             "% Black or African American",
                                             "% American Indian and Alaska Native",
                                             "% Asian",
                                             "% Native Hawaiian and Other Pacific Islander",
                                             "% Some Other Race",
                                             "% Two or More Races",
                                             "% Hispanic or Latino",
                                             "% households with no computing device",
                                             "% households with no internet access"
                                             ),
                              selected = "% White"),
    ),
    mainPanel(width = 9,
      h3("County-Level Map", align = "left"),
      # plotOutput("map", width = "800px"), static map not currently using
      leafletOutput("leafplot", width = "800px"),
      plotOutput("varPlot")
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
    
    selectedgeo <- st_transform(selectedgeo, 4326)  # ask Teja what this does - some sort of GEO
    
    selectedtype <- switch(input$whichtype,
                          "Grand Total, All Reporting Institutions" = selectedgeo$all,
                          "Total Private, Non-Profit, Four-Year Institutions" = selectedgeo$pr_4yr,
                          "Total Public Four-Year Institutions" = selectedgeo$pub_4yr,
                          "Total Public Two-Year Institutions" = selectedgeo$pub_2yr)
    
    selectedvar <- switch(input$whichvar,
                          #"total population" = selectedgeo$B02001_001E,
                          "% White" = selectedgeo$white,
                          "% Black or African American" = selectedgeo$black,
                          "% American Indian and Alaska Native" = selectedgeo$amind_alaska,
                          "% Asian" = selectedgeo$asian,
                          "% Native Hawaiian and Other Pacific Islander" = selectedgeo$hi_pac,
                          "% Some Other Race" = selectedgeo$other,
                          "% Two or More Races" = selectedgeo$two_plus,
                          "% Hispanic or Latino" = selectedgeo$his_lat,
                          "% households with no computing device" = selectedgeo$dev_none,
                          "% households with no internet access" = selectedgeo$int_none)
                          
    pal <- colorBin("Blues", domain = selectedtype, 
                    bins = c(0, 250, 500, 1000, 2500, 5000, 10000, 20000, 50000)) #bins = 8)
    
    labels <- lapply(
      paste("<strong>County: </strong>",
            selectedgeo$NAME.y,
            "<br />",
            "<strong>Value: </strong>",
            round(selectedtype, 2)),#,"%"),
      htmltools::HTML
    )
    
    leaflet(data = selectedgeo) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(selectedtype), 
                  fillOpacity = 0.8,
                  stroke = TRUE,
                  weight = 0.9,
                  color = "gray",
                  smoothFactor = 0.7,
                  layerId = ~COUNTYFP,
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      addLegend(position = "topleft",
                values = ~(round(selectedtype, 0)),
                colors = brewer.pal(8, "Blues"), #c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026"),
                labels = c("0-250", "251-500", "501-1000", "1001-2500", "2501-5000",
                           "5001-10000", "10001-20000", "20001+"),
                title = "Number of Students",
                opacity = 0.8)
    
  })
  
  observeEvent(input$leafplot_shape_click, { 
    p <- input$leafplot_shape_click
    print(p$id)
    print(class(p))
  })
  
  output$varPlot <- renderPlot({
    id <- input$leafplot_shape_click$id

    #temp <- appdata_nogeo
    #temp$geometry = NULL
    #print(temp[temp$COUNTYFP == "059"])
    x <- appdata_nogeo[appdata_nogeo$COUNTYFP == id, 12:18]
    x <- unlist(x, use.names=FALSE)
    #print(x)
    #dat$geometry = NULL
    
    barplot(x, xlab = "Race", main = "Race Distribution")
  })
    
  # 
  # observe({ 
  #   
  #   event <- input$map_shape_click
  #   
  #   message <- paste("widgets sold in North Carolina:", shape$widgets[shape$uid == event$id])
  #   
  #   output$widgets <- renderText(message)
  # })  
  
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