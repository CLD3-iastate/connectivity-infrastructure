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


#
# Load data ---------------------------------------------
#

data_med <- read_rds("~/Git/gates/rivanna_data/working/dashboard/data_med.Rds")
data_work <- read_rds("~/Git/gates/rivanna_data/working/dashboard/data_work.Rds")
data_edu <- read_rds("~/Git/gates/rivanna_data/working/dashboard/data_edu.Rds")


#
# User interface ---------------------------------------------
#

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
                         column(12, align = "center", h2(strong("Connectivity Infrastructure as Barrier to Remote Work, Education, and Mental Health Care: County Explorer")))
                ),
                
                hr(),
                
                fluidRow(width = 12,
                         column(1),
                         column(10, align = 'center',
                                em('This dashboard was created by at the Social and Decision Analytics Division of the Biocomplexity Institute and Initiative,
                      University of Virginia, in partnership with the Bill and Melinda Gates Foundation.')
                         ),
                         column(1)
                ),
                
                fluidRow(width = 12,
                         column(1),
                         column(10, 
                                p(),
                                br(tags$b('This dashboard visualizes three measures of connectivity infrastructure as barriers to remote work, education, and mental health care.'), 
                                  ('The dashboard displays county-level relative need and access, allowing extension professionals and policy-makers in Iowa, Oregon, and Virginia to 
                                  make informed decisions about interventions and resource allocation.')),
                                p(),
                                br('Using the map selector allows filtering by topic and state. Hovering over a county area on the resulting map displays information 
                                    about the calculated county relative vulnerability status, as well as relevant connectivity and need measures. 
                                    Methodology, data source, and measure descriptions are available below.')
                         ),
                         column(1)
                ),

                hr(),
                
                fluidRow(width = 12, style = "margin: 20px",
                         h2('Measures and Data Sources')
                ),
                
                fluidRow(style = "margin: 6px",
                         column(4, wellPanel(strong('Remote Work Need and Access'), 
                                             p(),
                                             em('Description.'),
                                             br('US Census Bureau’s ACS is an annual, nationally representative US household survey. It provides estimates on population sociodemographic characteristics and select topics, 
                                                including computer and internet access.'),
                                             p(),
                                             em('How We Measure Remote Work Need and Access.'),
                                             br(), tags$b('Our measure of broadband availability indicates the percent of the population in a given geographic unit (census block, census tract, or county) with access to at least one 
                                                          broadband provider offering at least a 25 advertised download speed.'), ('We use FCC provider and maximum advertised download speed information from December 2015 (at census block 
                                                                                                                                   group and census tract level for comparisons with ACS, and at county level for comparisons with Microsoft), and ACS 2013-17 (5-year) population estimates to calculate the percent 
                                                                                                                                   of the population in a given geography with broadband access.'),
                                             p(),
                                             em('Source and More Information.'),
                                             tags$li('Data description: ', a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target="_blank")),
                                             tags$li('Data source: ', a(href = 'https://www.fcc.gov/form-477-broadband-deployment-data-december-2015-version-4', 'FCC Form 477 December 2015', target="_blank")))),
                         
                         column(4, wellPanel(strong('Remote Education Need and Access'),
                                             p(),
                                             em('Description.'),
                                             br('US Census Bureau’s ACS is an annual, nationally representative US household survey. It provides estimates on population sociodemographic characteristics and select topics, 
                                                including computer and internet access.'),
                                             p(),
                                             em('How We Measure Remote Education Need and Access.'),
                                             br(), tags$b(' Our measure of broadband subscription indicates the percent of census block group or tract households that self-reports having access to any kind of broadband connection, 
                                                          excluding satellite and cellular.'), ('We obtain broadband subscription estimates from ACS 2013-17 (5-year) data table B28002, which contains information on the presence and 
                                                                                                types of internet subscriptions in households. We use estimates at census block group and census tract levels for comparisons with FCC. To calculate the metric, 
                                                                                                we divide the number of households reporting such access by the total population in a given geographic unit.'),
                                             p(),
                                             em('Source and More Information.'),
                                             tags$li('Data description: ', a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target="_blank")),
                                             tags$li('Data source: ', a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002 - Presence and types of internet subscriptions in household', target="_blank")))),
                         
                         column(4, wellPanel(strong('Telemental Health Need and Access'),
                                             p(),
                                             em('Description.'),
                                             br('US Census Bureau’s ACS is an annual, nationally representative US household survey. It provides estimates on population sociodemographic characteristics and select topics, 
                                                including computer and internet access.'),
                                             p(),
                                             em('How We Measure Telemental Health Need and Access.'),
                                             br(), tags$b('Our measure of broadband usage indicates the proportion of county population that uses Microsoft services (e.g., uses Bing.com, downloads Microsoft updates, or uses Xbox) at 
                                             25 mbps download speeds, according to Microsoft server logs.'), ('We use Microsoft data from 2018 at county level for comparisons with FCC; Microsoft performed the calculation of 
                                             population proportions and aggregation to county level.'),
                                             p(),
                                             em('Source and More Information.'),
                                             tags$li('Data description: ', a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target="_blank")),
                                             tags$li('Data description: ', a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources', 'County Health Rankings', target="_blank")),
                                             tags$li('Data request: ', a(href = 'https://forms.office.com/Pages/ResponsePage.aspx?id=v4j5cvGGr0GRqy180BHbRxi63BqtSQBHm4t78vwTzcZUNkFKV04xMEVDN0RMNkxTWkVERVMyNlZHViQlQCN0PWcu', 'Request Microsoft Airband data', target="_blank"))))
                         ),
                hr(),
                
                fluidRow(style = "margin: 6px",
                         width = 12, 
                         column(12, align = "left", h3(strong("Explore County Rankings")))
                ),
                
                fluidRow(style = "margin: 6px",
                  column(width = 3,
                         h4(strong("Selector"), align = "left"),
                         p(), 
                         selectInput("whichtopic", "Topic", 
                                     choices = list("Remote education",
                                                    "Remote work",
                                                    "Telemental health"), 
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
                
                fluidRow(style = "margin: 6px",
                         width = 12, 
                         column(12, align = "left", h3(strong("Explore Ranking Indicators")))
                ),
                
                conditionalPanel("input.whichtopic == 'Remote education'",
                                 
                    fluidRow(style = "margin: 6px",
                             width = 12, 
                             column(6, align = "left",
                                                     strong("Quintile Cut-Offs"),
                                                     p(),
                                                     tableOutput("table_quint")
                             ),
                             column(6, align = "left",
                                                     tabsetPanel(
                                                       tabPanel(title = "Households without internet", 
                                                                p(),
                                                                strong("County-Level Percent Households Without Internet Acccess"),
                                                                p(),
                                                                leafletOutput("subplot_noint", width = "500px")),
                                                       tabPanel(title = "Children without computer", 
                                                                p(),
                                                                strong("County-Level Percent Children (Age <18) Without a Computer"),
                                                                p(),
                                                                leafletOutput("subplot_nocomp", width = "500px")),
                                                       tabPanel(title = "Population in K-12", 
                                                                p(),
                                                                strong("County-Level Percent Population Enrolled in K-12"),
                                                                p(),
                                                                leafletOutput("subplot_ink12", width = "500px"))
                                                     ))
                             )
                ),
                
                hr(),
                
                fluidRow(style = "margin: 20px",
                         width = 12, 
                         column(12, align = 'center',
                                h2('Contact'),
                                br(a(href = 'https://biocomplexity.virginia.edu/teja-pristavec', 'Teja Pristavec')),
                                br(a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics', 'Social and Decision Analytics Division', target = '_blank'),
                                br('Biocomplexity Institute and Initiative, University of Virginia')
                                )
                         )
                ),
                
                hr(),
                
                fluidRow(style = "margin: 20px",
                         width = 12, 
                         column(12, align = 'center',
                                em('Last updated: May 2020'))
                )
                
)


#
# Server ---------------------------------------------
#

server <- function(input, output) {
  
  #
  # Main plots ---------------------------------------------
  #
  
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
    } else if (input$whichtopic == "Telemental health") {
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

  #
  # Quintile tables ---------------------------------------------
  #
  
  output$table_quint <- renderTable({
    
    # Remote education
    if (input$whichtopic == "Remote education") {
      data <- data_edu
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      qnoint <- quantile(data$nointernet, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qnocomp <- quantile(data$nocomputer, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qink12 <- quantile(data$ink12, prob = seq(0, 1, 0.2), na.rm = TRUE)
      
      quintcuts <- bind_rows(qnoint, qnocomp, qink12)
      quintcuts$Indicator <- c("% Children without internet access", "% Population with no computer", "% Population enrolled in K-12")
      quintcuts <- quintcuts %>% select(Indicator, 2:7)
      colnames(quintcuts) <- c("Indicator", "Q1", "Q2", "Q3", "Q4", "Q5")
      quintcuts
      
      # Remote work
    } else if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      qnoint <- quantile(data$nointernet, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qnocomp <- quantile(data$nocomputer, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qoccup <- quantile(data$occup, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qind <- quantile(data$industr, prob = seq(0, 1, 0.2), na.rm = TRUE)
      
      quintcuts <- bind_rows(qnoint, qnocomp, qoccup, qind)
      quintcuts$Indicator <- c("% Households without broadband internet", "% Population in labor force without a computer", "% Population in non-remote labor force in non-remote-friendly occupations", "% Population in non-remote labor force in non-remote-friendly industries")
      quintcuts <- quintcuts %>% select(Indicator, 2:7)
      colnames(quintcuts) <- c("Indicator", "Q1", "Q2", "Q3", "Q4", "Q5")
      quintcuts
      
      # Telehealth
    } else if (input$whichtopic == "Telemental health") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      qnoint <- quantile(data$nointernet, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qnocomp <- quantile(data$nocomputer, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qpoorment <- quantile(data$avgnum_poormenth, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qnumprov <- quantile(desc(data$dmenthprov_per100k), prob = seq(0, 1, 0.2), na.rm = TRUE)
      qunins <- quantile(data$pct_unins, prob = seq(0, 1, 0.2), na.rm = TRUE)
      
      quintcuts <- bind_rows(orqnoint, orqnocomp, orqpoorment, orqnumprov, orqunins)
      quintcuts$Indicator <- c("% households without internet access", "% households without a computer", "Average number of poor mental health days", "Number of mental health providers per 100,000 population (reverse-coded)", "% population without health insurance")
      quintcuts <- quintcuts %>% select(Indicator, 2:7)
      colnames(quintcuts) <- c("Indicator", "Q1", "Q2", "Q3", "Q4", "Q5")
      quintcuts
      
    }
  })
  
  #
  # Subplots: No internet ------------------------------------------------
  #
  
  output$subplot_noint <- renderLeaflet({
    
    # Education
    if (input$whichtopic == "Remote education") {
      data <- data_edu
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorNumeric("Blues", domain = data$nointernet)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Children (age <18) without internet access:</strong>",
              round(data$nointernet, 2), "(quintile", data$nointernetQuint, ")",
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$nointernetTop),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$nointernet), 
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
      
      # Remote work
    } else if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorNumeric("Blues", domain = data$nointernet)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% population without broadband internet:</strong>",
              round(data$nointernet, 2), "(quintile", data$nointernetQuint, ")",
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$nointernetTop),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$nointernet), 
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
      
      # Telehealth
    } else if (input$whichtopic == "Telemental health") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorNumeric("Blues", domain = data$nointernet)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% population without internet access:</strong>",
              round(data$nointernet, 2), "(quintile", data$nointernetQuint, ")",
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$nointernetTop),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$nointernet), 
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
  
  
  #
  # Subplots: No computer -----------------------------------------------
  #
  
  output$subplot_nocomp <- renderLeaflet({
    
    # Education
    if (input$whichtopic == "Remote education") {
      data <- data_edu
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorNumeric("Blues", domain = data$nocomputer)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% households without a computer:</strong>",
              round(data$nocomputer, 2), "(quintile", data$nocomputerQuint, ")",
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$nocomputerTop),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$nocomputer), 
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
      
      # Work
    } else if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorNumeric("Blues", domain = data$nointernet)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% households without a computer:</strong>",
              round(data$nocomputer, 2), "(quintile", data$nocomputerQuint, ")",
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$nocomputerTop),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$nocomputer), 
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
      
      # Telehealth
    } else if (input$whichtopic == "Telemental health") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorNumeric("Blues", domain = data$nocomputer)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% households without a computer:</strong>",
              round(data$nocomputer, 2), "(quintile", data$nocomputerQuint, ")",
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$nocomputerTop),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$nocomputer), 
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
  
  #
  # Subplots: various -----------------------------------------------
  #
  
  output$subplot_ink12 <- renderLeaflet({
    
    # In K12
    if (input$whichtopic == "Remote education") {
      data <- data_edu
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorNumeric("Blues", domain = data$ink12)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% population enrolled in K-12:</strong>",
              round(data$ink12, 2), "(quintile", data$ink12Quint, ")",
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$ink12Top),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$ink12), 
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
  
  output$subplot_occup <- renderLeaflet({
    
    # Occupation
    if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorNumeric("Blues", domain = data$occup)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Non-remote workers in non-remote-friendly occupations:</strong>",
              round(data$occup, 2), "(quintile", data$occupQuint, ")",
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$occupTop),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$occup), 
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
  
  output$subplot_industr <- renderLeaflet({
    
    # Industry
    if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorNumeric("Blues", domain = data$industr)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Non-remote workers in non-remote-friendly industries:</strong>",
              round(data$industr, 2), "(quintile", data$industrQuint, ")",
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$industrTop),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$occup), 
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

}


#
# App ---------------------------------------------
#

shinyApp(ui = ui, server = server)