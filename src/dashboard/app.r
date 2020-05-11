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
                
                headerPanel(img(src = "logo.png", class = "topimage", width = "25%", style = "display: block; margin-left: auto; margin-right: auto;")
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
                         column(4, wellPanel(strong('Remote Work Vulnerability'), 
                                             p(),
                                             em('Description.'),
                                             br(), tags$b('The remote work relative vulnerability measure highlights counties where residents may have difficulty working remotely if instructed to do so.'),
                                                ('It conceptualizes four telecommunication infastructure and employment characteristics as potential barriers, providing
                                                 a relative ranking of county telework preparedness.'),
                                             p(),
                                             em('How We Measure Remote Work Vulnerability.'),
                                             br('We calculate remote work relative vulnerability using information on percent households with no broadband internet subscription,
                                                percent persons in labor force with no computer available, percent persons who are not currently working remotely and are employed in telework unfriendly occupations,
                                                and percent persons who are not currently working remotely and are employed in telework unfriendly industries. We compute quintile cut-offs for each indicator.
                                                Counties are considered very high vulnerability if they placed in 4th or 5th quintile on all 4 indicators considered, high if on 3 indicators, medium if on 2, 
                                                low if on 1, and no vulnerability if they did not place in the 4th or 5th quintile on any indicators.'),
                                             p(),
                                             em('More Information.'),
                                             tags$li('Data source: ', a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target="_blank")),
                                             tags$li('Related read: ', a(href = 'https://www.popsci.com/story/technology/work-from-home-broadband-connection-internet-fcc/', 'Internet bandwith and remote work', target="_blank")))),
                         
                         column(4, wellPanel(strong('Remote Education Vulnerability'),
                                             p(),
                                             em('Description.'),
                                             br(), tags$b('The remote education relative vulnerability measure highlights counties where K-12 students may have difficulty participating in online education.'), 
                                             ('It considers telecommunication infastructure and K-12 enrollment in providing a relative ranking of county K-12 remote education preparedness.'),
                                             p(),
                                             em('How We Measure Remote Education Vulnerability.'),
                                             br('We calculate remote education relative vulnerability using information on percent households with no internet access subscription,
                                                 percent population under age 18 without a computer, and percent population enrolled in K-12. We compute quintile cut-offs for each indicator.
                                                 Counties are considered high vulnerability if they placed in 4th or 5th quintile on all 3 indicators
                                                 considered, medium if on 2 indicators, low if on 1, and no vulnerability if they did not
                                                 place in the 4th or 5th quintile on any indicators.'),
                                             p(),
                                             em('Source and More Information.'),
                                             tags$li('Data source: ', a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target = "_blank")),
                                             tags$li('Data source: ', a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002 - Presence and types of internet subscriptions in household', target = "_blank")))),
                         
                         column(4, wellPanel(strong('Telemental Health Vulnerability'),
                                             p(),
                                             em('Description.'),
                                             br(), tags$b('The telemental health relative vulnerability measure highlights counties where high need for mental health services is coupled with
                                             barriers to access.'), ('It considers telecommunication infastructure, health insurance, in-person provider availability, and mental health status
                                             in providing a relative ranking of county K-12 telemental health vulnerability.'),
                                             p(),
                                             em('How We Measure Telemental Vulnerability.'),
                                             br('We calculate telemental health relative vulnerability using information on percent households without internet access, 
                                                percent households with no computer, average number of poor mental health days in past month, number of mental health providers per 100,000
                                                population (reverse-coded), and percent population under age 65 without health insurance. We compute quintile cut-offs for each indicator.
                                                Counties are considered very high vulnerability if they placed in 4th or 5th quintile on all 5 indicators
                                                considered, high if on 4 indicators, medium if on 3, very low if on 2, low if on 1, and no vulnerability
                                                if they did not place in the 4th or 5th quintile on any indicators.'),
                                             p(),
                                             em('Source and More Information.'),
                                             tags$li('Data source: ', a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target = "_blank")),
                                             tags$li('Data source: ', a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources', 'County Health Rankings', target = "_blank")),
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
                
                conditionalPanel("input.whichtopic == 'Remote work'",
                                 
                                 fluidRow(style = "margin: 6px",
                                          width = 12, 
                                          column(6, align = "left",
                                                 strong("Quintile Cut-Offs"),
                                                 p(),
                                                 tableOutput("table_quint_work")
                                          ),
                                          column(6, align = "left",
                                                 tabsetPanel(
                                                   tabPanel(title = "Households without broadband", 
                                                            p(),
                                                            strong("County-Level Percent Households Without Broadband Internet"),
                                                            p(),
                                                            leafletOutput("subplot_noint_work", width = "600px"),
                                                            p(),
                                                            tags$em('Data source: American Community Survey 2014/18 (5-year) estimates, ', a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002, Presence and types of internet subscriptions in household', target = "_blank"))),
                                                   tabPanel(title = "Workers without computer", 
                                                            p(),
                                                            strong("County-Level Percent Population in Labor Force Without a Computer"),
                                                            p(),
                                                            leafletOutput("subplot_nocomp_work", width = "600px"),
                                                            p(),
                                                            tags$em('Data source: American Community Survey 2014/18 (5-year) estimates, ', a(href = 'https://data.census.gov/cedsci/table?q=computer&tid=ACSDT5Y2018.B28007&t=Telephone,%20Computer,%20and%20Internet%20Access&vintage=2018&d=ACS%205-Year%20Estimates%20Detailed%20Tables', 'Table B28007, Labor force status by presence of a computer and types of internet subscription in household', target = "_blank"))),
                                                   tabPanel(title = "Non-remote workers in remote unfriendly occupations", 
                                                            p(),
                                                            strong("County-Level Percent Population in Labor Force not Working Remotely Employed in Remote Unfriendly Occupations"),
                                                            p(),
                                                            leafletOutput("subplot_occup", width = "600px"),
                                                            p(),
                                                            tags$em('Data source: American Community Survey 2014/18 (5-year) estimates, ', a(href = 'https://data.census.gov/cedsci/table?q=B08124%3A%20MEANS%20OF%20TRANSPORTATION%20TO%20WORK%20BY%20OCCUPATION&hidePreview=true&tid=ACSDT5Y2018.B08124', 'Table B08124, Means of transportation to work by occupation', target = "_blank"))),
                                                   tabPanel(title = "Non-remote workers in remote unfriendly industries", 
                                                            p(),
                                                            strong("County-Level Percent Population in Labor Force not Working Remotely Employed in Remote Unfriendly Industries"),
                                                            p(),
                                                            leafletOutput("subplot_industr", width = "600px"),
                                                            p(),
                                                            tags$em('Data source: American Community Survey 2014/18 (5-year) estimates, ', a(href = 'https://data.census.gov/cedsci/table?q=B08126%3A%20MEANS%20OF%20TRANSPORTATION%20TO%20WORK%20BY%20INDUSTRY&hidePreview=true&tid=ACSDT5Y2018.B08126&vintage=2018', 'Table B08126, Means of transportation to work by industry', target = "_blank")))
                                                 ))
                                 )
                ),
                
                conditionalPanel("input.whichtopic == 'Remote education'",
                                 fluidRow(style = "margin: 6px",
                                          width = 12, 
                                          column(6, align = "left",
                                                 strong("Quintile Cut-Offs"),
                                                 p(),
                                                 tableOutput("table_quint_edu")
                                          ),
                                          column(6, align = "left",
                                                 tabsetPanel(
                                                   tabPanel(title = "Households without internet", 
                                                            p(),
                                                            strong("County-Level Percent Households Without Internet Access"),
                                                            p(),
                                                            leafletOutput("subplot_noint_edu", width = "600px"),
                                                            p(),
                                                            tags$em('Data source: American Community Survey 2014/18 (5-year) estimates, ', a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002, Presence and types of internet subscriptions in household', target = "_blank"))),
                                                   tabPanel(title = "Children without computer", 
                                                            p(),
                                                            strong("County-Level Percent Children (Age <18) Without a Computer"),
                                                            p(),
                                                            leafletOutput("subplot_nocomp_edu", width = "600px"),
                                                            p(),
                                                            tags$em('Data source: American Community Survey 2014/18 (5-year) estimates, ', a(href = 'https://data.census.gov/cedsci/table?q=computer&tid=ACSDT5Y2018.B28005&t=Telephone,%20Computer,%20and%20Internet%20Access&vintage=2018&d=ACS%205-Year%20Estimates%20Detailed%20Tables', 'Table B28005, Age by presence of a computer and types of internet subscription in household', target = "_blank"))),
                                                   tabPanel(title = "Population in K-12", 
                                                            p(),
                                                            strong("County-Level Percent Population Enrolled in K-12"),
                                                            p(),
                                                            leafletOutput("subplot_ink12", width = "600px"),
                                                            p(),
                                                            tags$em('Data source: American Community Survey 2014/18 (5-year) estimates, ', a(href = 'https://data.census.gov/cedsci/table?q=B14001%3A%20SCHOOL%20ENROLLMENT%20BY%20LEVEL%20OF%20SCHOOL%20FOR%20THE%20POPULATION%203%20YEARS%20AND%20OVER&hidePreview=true&tid=ACSDT5Y2018.B14001&vintage=2018', 'Table B14001,	School enrollment by level of school for the population 3 years and over', target = "_blank")))
                                                 ))
                                 )
                ),
                
                conditionalPanel("input.whichtopic == 'Telemental health'",
                                 fluidRow(style = "margin: 6px",
                                          width = 12, 
                                          column(6, align = "left",
                                                 strong("Quintile Cut-Offs"),
                                                 p(),
                                                 tableOutput("table_quint_med")
                                          ),
                                          column(6, align = "left",
                                                 tabsetPanel(
                                                   tabPanel(title = "Households without internet", 
                                                            p(),
                                                            strong("County-Level Percent Households Without Internet Access"),
                                                            p(),
                                                            leafletOutput("subplot_noint_med", width = "600px"),
                                                            p(),
                                                            tags$em('Data source: American Community Survey 2014/18 (5-year) estimates, ', a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002, Presence and types of internet subscriptions in household', target = "_blank"))),
                                                   tabPanel(title = "Households without computer", 
                                                            p(),
                                                            strong("County-Level Percent Households Without a Computer"),
                                                            p(),
                                                            leafletOutput("subplot_nocomp_med", width = "600px"),
                                                            p(),
                                                            tags$em('Data source: American Community Survey 2014/18 (5-year) estimates, ', a(href = 'https://data.census.gov/cedsci/table?q=computer&tid=ACSDT5Y2018.B28003&t=Telephone,%20Computer,%20and%20Internet%20Access&vintage=2018&d=ACS%205-Year%20Estimates%20Detailed%20Tables', 'Table B28003, Presence of a computer and type of internet subscription in household', target = "_blank"))),
                                                   tabPanel(title = "Uninsured", 
                                                            p(),
                                                            strong("County-Level Percent Population Under Age 65 Without Health Insurance"),
                                                            p(),
                                                            leafletOutput("subplot_unins", width = "600px"),
                                                            p(),
                                                            tags$em('Data source: Small Area Health Insurance Estimates 2017 via CountyHealthRankings, ', a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-factors/clinical-care/access-to-care/uninsured', 'Percentage of population under age 65 without health insurance', target = "_blank"))),
                                                   tabPanel(title = "Poor mental health days", 
                                                            p(),
                                                            strong("County-Level Average Number of Poor Mental Health Days in Past Month"),
                                                            p(),
                                                            leafletOutput("subplot_poorment", width = "600px"),
                                                            p(),
                                                            tags$em('Data source: Behavioral Risk Factor Surveillance System 2017 via CountyHealthRankings, ', a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-outcomes/quality-of-life/poor-mental-health-days', 'Average number of mentally unhealthy days reported in past 30 days (age-adjusted)', target = "_blank"))),
                                                   tabPanel(title = "Mental health providers", 
                                                            p(),
                                                            strong("County-Level Number of Mental Health Providers per 100,000 Population"),
                                                            p(),
                                                            leafletOutput("subplot_healthprov", width = "600px"),
                                                            p(),
                                                            tags$em('Data source: CMS National Provider Identification 2019 via CountyHealthRankings, ', a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-factors/clinical-care/access-to-care/mental-health-providers', 'Number of mental health providers per 100,000 population', target = "_blank")))
                                                 ))
                                 )
                ),
                                 
                hr(),
                
                fluidRow(style = "margin: 20px",
                         width = 12, 
                         column(6, align = "center",
                                h2("Acknowledgments"),
                                br("We used the quintile method described by the", a(href = "https://pcrd.purdue.edu/blog/remote-work-and-the-coronavirus.php", "Purdue Center for Regional Development"), 
                                   "to construct our relative vulnerability measures. One of our indicators, remote work vulnerability, is also an adaptation of Purdue's remote work metric. 
                                   We depart from their operationalization in using American Community Survey instead of Federal Communications Commission (FCC) data given", a(href = "https://bband.policy-analytics.net", "known issues with FCC Form 477"),
                                   ", as well as in tailoring indicators used to the population in the labor force and to the labor force population that reports not already working from home.")
                         ),
                         column(6, align = "center",
                                h2("Contact"),
                                br(a(href = "https://biocomplexity.virginia.edu/teja-pristavec", "Teja Pristavec")),
                                br(a(href = "https://biocomplexity.virginia.edu/social-decision-analytics", "Social and Decision Analytics Division", target = "_blank"),
                                br("Biocomplexity Institute and Initiative, University of Virginia")
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
  
  # Remote education
  output$table_quint_edu <- renderTable({
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
    }
  })
      
  # Remote work
  output$table_quint_work <- renderTable({
      if (input$whichtopic == "Remote work") {
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
      }
  })
     
  output$table_quint_med <- renderTable({ 
      # Telehealth
      if (input$whichtopic == "Telemental health") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      qnoint <- quantile(data$nointernet, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qnocomp <- quantile(data$nocomputer, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qpoorment <- quantile(data$avgnum_poormenth, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qnumprov <- quantile(desc(data$menthprov_per100k), prob = seq(0, 1, 0.2), na.rm = TRUE)
      qnumprov <- abs(qnumprov)
      qunins <- quantile(data$pct_unins, prob = seq(0, 1, 0.2), na.rm = TRUE)
      
      quintcuts <- bind_rows(qnoint, qnocomp, qpoorment, qnumprov, qunins)
      quintcuts$Indicator <- c("% households without internet access", "% households without a computer", "Average number of poor mental health days", "Number of mental health providers per 100,000 population", "% population without health insurance")
      quintcuts <- quintcuts %>% select(Indicator, 2:7)
      colnames(quintcuts) <- c("Indicator", "Q1", "Q2", "Q3", "Q4", "Q5")
      quintcuts
    }
  })
  
  #
  # Subplots: No internet ------------------------------------------------
  #
 
  # Education 
  output$subplot_noint_edu <- renderLeaflet({
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
    }
  })
      
  # Work
  output$subplot_noint_work <- renderLeaflet({
      if (input$whichtopic == "Remote work") {
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
    }
  })
      
  # Telehealth
  output$subplot_noint_med <- renderLeaflet({
      if (input$whichtopic == "Telemental health") {
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
  
  # Education
  output$subplot_nocomp_edu <- renderLeaflet({
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
    }
  })
      
  # Work
  output$subplot_nocomp_work <- renderLeaflet({
    if (input$whichtopic == "Remote work") {
      data <- data_work
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
      
  # Telehealth
  output$subplot_nocomp_med <- renderLeaflet({
    if (input$whichtopic == "Telemental health") {
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
  
  # In K12
  output$subplot_ink12 <- renderLeaflet({
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
  
  # Occupation
  output$subplot_occup <- renderLeaflet({
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
  
  # Industry
  output$subplot_industr <- renderLeaflet({
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
        addPolygons(fillColor = ~pal(data$industr), 
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
  
  # Poor mental health days
  output$subplot_poorment <- renderLeaflet({
    if (input$whichtopic == "Telemental health") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorNumeric("Blues", domain = data$avgnum_poormenth)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Average number of poor mental health days in past month:</strong>",
              round(data$avgnum_poormenth, 2), "(quintile", data$menthdaysQuint, ")",
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$menthdaysTop),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$avgnum_poormenth), 
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
  
  # Percent uninsured
  output$subplot_unins <- renderLeaflet({
    if (input$whichtopic == "Telemental health") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorNumeric("Blues", domain = data$pct_unins)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% population under age 65 without health insurance:</strong>",
              data$pct_unins, "(quintile", data$uninsQuint, ")",
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$uninsTop),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$pct_unins), 
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
  
  # Mental health providers
  output$subplot_healthprov <- renderLeaflet({
    if (input$whichtopic == "Telemental health") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorNumeric("Blues", domain = data$menthprov_per100k)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Number of mental health providers per 100,000 population:</strong>",
              round(data$menthprov_per100k, 2), "(quintile", data$menthprovQuint, ")",
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$menthprovTop),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$menthprov_per100k), 
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