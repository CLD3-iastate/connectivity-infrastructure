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
library(plotly)


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
                                em('This dashboard was created at the Social and Decision Analytics Division of the Biocomplexity Institute and Initiative,
                                   University of Virginia.')
                                ),
                         column(1)
                         ),
                
                fluidRow(width = 12,
                         column(1),
                         column(10, 
                                p(),
                                br(tags$b('This dashboard visualizes three measures of connectivity infrastructure as barriers to remote work, education, and mental health care.'), 
                                   ('It facilitates relative comparisons between counties, highlighting areas where workers may have difficulty working remotely, where youth would
                                     face barriers to participating in online education, and where county residents may have high need but low access to telemental health services. 
                                     The dashboard allows extension professionals and policy-makers in Iowa, Oregon, and Virginia to make informed decisions about interventions and resource allocation
                                    based on conditions in their counties.')),
                                p(),
                                br('Using the map selector allows filtering by topic and state. Hovering over a county area on the resulting map displays information 
                                   about the calculated county relative vulnerability status, as well as relevant connectivity and need measures. Supplemental maps
                                   allow users to explore individual indicators used to create the three relative vulnerabilty measures.
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
                                             em('Data source.'),
                                             p(a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target = "_blank"), "2014/18 (5-year) estimates.")
                                             )),
                         
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
                                             em('Data source.'),
                                             p(a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target = "_blank"), "2014/18 (5-year) estimates.")
                                             )),
                         
                         column(4, wellPanel(strong('Telemental Health Vulnerability'),
                                             p(),
                                             em('Description.'),
                                             br(), tags$b('The telemental health relative vulnerability measure highlights counties where high need for mental health services is coupled with
                                                          barriers to access.'), ('It considers telecommunication infastructure, health insurance, in-person provider availability, and mental health status
                                                                                  in providing a relative ranking of county K-12 telemental health vulnerability.'),
                                             p(),
                                             em('How We Measure Telemental Health Vulnerability.'),
                                             br('We calculate telemental health relative vulnerability using information on percent households without internet access, 
                                                percent households with no computer, average number of poor mental health days in past month, number of mental health providers per 100,000
                                                population (reverse-coded), and percent population under age 65 without health insurance. We compute quintile cut-offs for each indicator.
                                                Counties are considered very high vulnerability if they placed in 4th or 5th quintile on all 5 indicators
                                                considered, high if on 4 indicators, medium if on 3, very low if on 2, low if on 1, and no vulnerability
                                                if they did not place in the 4th or 5th quintile on any indicators.'),
                                             p(),
                                             em('Data source.'),
                                             p(a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target = "_blank"), "2014/18 (5-year) estimates and",
                                               a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources', 'County Health Rankings', target = "_blank"), "2019.")
                                             ))
                         ),
                hr(),
                
                fluidRow(style = "margin: 6px",
                         width = 12, 
                         column(12, align = "left", h3(strong("Explore County Relative Vulnerability")))
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
                                            selected = "Iowa"),
                                p(),
                                p("Use the tools above to select your topic and state of interest.
                                  The selected map will display on the right, and individual indicators associated
                                  with the selected topic will be come available for further exploration below."),
                                p(),
                                p("Please be patient as the map loads.")
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
                         column(12, align = "left", 
                                h3(strong("Explore Ranking Indicators")),
                                p(),
                                p(strong("Click on the tabs to explore the individual indicators we used to 
                                  construct the selected relative vulnerability measure."), "Each tab displays a 
                                  box plot with descriptive statistics and a state map at county level for an individual indicator. 
                                  The selection of indicators will update depending on the topic selected."))
                ),
                
                conditionalPanel("input.whichtopic == 'Remote work'",
                                 fluidRow(style = "margin: 6px",
                                          tabsetPanel(
                                            tabPanel(title = "Households without broadband", 
                                                     br(),
                                                     h4(strong("Indicator: County-Level Percent Households Without Broadband Internet")),
                                                     p(),
                                                     p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                       a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002, Presence and types of internet subscriptions in household.', target = "_blank")),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Box Plot"),
                                                            p(),
                                                            plotlyOutput("plotly_noint_work"),
                                                            p()),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Map"),
                                                            p(),
                                                            leafletOutput("subplot_noint_work"))
                                            ),
                                            tabPanel(title = "Workers without computer", 
                                                     br(),
                                                     h4(strong("Indicator: County-Level Percent Population in Labor Force Without a Computer")),
                                                     p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                       a(href = 'https://data.census.gov/cedsci/table?q=computer&tid=ACSDT5Y2018.B28007&t=Telephone,%20Computer,%20and%20Internet%20Access&vintage=2018&d=ACS%205-Year%20Estimates%20Detailed%20Tables', 'Table B28007, Labor force status by presence of a computer and types of internet subscription in household.', target = "_blank")),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Box Plot"),
                                                            p(),
                                                            plotlyOutput("plotly_nocomp_work"),
                                                            p()),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Map"),
                                                            p(),
                                                            leafletOutput("subplot_nocomp_work"))
                                            ),
                                            tabPanel(title = "Non-remote workers in remote unfriendly occupations", 
                                                     br(),
                                                     h4(strong("Indicator: County-Level Percent Population in Labor Force not Working Remotely Employed in Remote Unfriendly Occupations")),
                                                     p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                       a(href = 'https://data.census.gov/cedsci/table?q=B08124%3A%20MEANS%20OF%20TRANSPORTATION%20TO%20WORK%20BY%20OCCUPATION&hidePreview=true&tid=ACSDT5Y2018.B08124', 'Table B08124, Means of transportation to work by occupation.', target = "_blank")),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Box Plot"),
                                                            p(),
                                                            plotlyOutput("plotly_occup"),
                                                            p()),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Map"),
                                                            p(),
                                                            leafletOutput("subplot_occup"))
                                            ),
                                            tabPanel(title = "Non-remote workers in remote unfriendly industries", 
                                                     br(),
                                                     h4(strong("Indicator: County-Level Percent Population in Labor Force not Working Remotely Employed in Remote Unfriendly Industries")),
                                                     p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                       a(href = 'https://data.census.gov/cedsci/table?q=B08126%3A%20MEANS%20OF%20TRANSPORTATION%20TO%20WORK%20BY%20INDUSTRY&hidePreview=true&tid=ACSDT5Y2018.B08126&vintage=2018', 'Table B08126, Means of transportation to work by industry.', target = "_blank")),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Box Plot"),
                                                            p(),
                                                            plotlyOutput("plotly_industr"),
                                                            p()),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Map"),
                                                            p(),
                                                            leafletOutput("subplot_industr"))
                                            )
                                            )
                                          )
                ),
                
                conditionalPanel("input.whichtopic == 'Remote education'",
                                 fluidRow(style = "margin: 6px",
                                          tabsetPanel(
                                            tabPanel(title = "Households without internet", 
                                                     br(),
                                                     h4(strong("Indicator: County-Level Percent Households Without Internet Access")),
                                                     p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                       a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002, Presence and types of internet subscriptions in household.', target = "_blank")),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Box Plot"),
                                                            p(),
                                                            plotlyOutput("plotly_noint_edu"),
                                                            p()),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Map"),
                                                            p(),
                                                            leafletOutput("subplot_noint_edu"))
                                            ),
                                            tabPanel(title = "Youth without computer", 
                                                     br(),
                                                     h4(strong("Indicator: County-Level Percent Youth (Age <18) Without a Computer")),
                                                     p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                       a(href = 'https://data.census.gov/cedsci/table?q=computer&tid=ACSDT5Y2018.B28005&t=Telephone,%20Computer,%20and%20Internet%20Access&vintage=2018&d=ACS%205-Year%20Estimates%20Detailed%20Tables', 'Table B28005, Age by presence of a computer and types of internet subscription in household.', target = "_blank")),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Box Plot"),
                                                            p(),
                                                            plotlyOutput("plotly_nocomp_edu"),
                                                            p()),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Map"),
                                                            p(),
                                                            leafletOutput("subplot_nocomp_edu"))
                                            ),
                                            tabPanel(title = "Population in K-12", 
                                                     br(),
                                                     h4(strong("Indicator: County-Level Percent Population Enrolled in K-12")),
                                                     p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                       a(href = 'https://data.census.gov/cedsci/table?q=B14001%3A%20SCHOOL%20ENROLLMENT%20BY%20LEVEL%20OF%20SCHOOL%20FOR%20THE%20POPULATION%203%20YEARS%20AND%20OVER&hidePreview=true&tid=ACSDT5Y2018.B14001&vintage=2018', 'Table B14001,	School enrollment by level of school for the population 3 years and over.', target = "_blank")),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Box Plot"),
                                                            p(),
                                                            plotlyOutput("plotly_ink12"),
                                                            p()),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Map"),
                                                            p(),
                                                            leafletOutput("subplot_ink12"))
                                            )
                                          )
                                 )
                ),
                
                conditionalPanel("input.whichtopic == 'Telemental health'",
                                 fluidRow(style = "margin: 6px",
                                          tabsetPanel(
                                            tabPanel(title = "Households without internet", 
                                                     br(),
                                                     h4(strong("Indicator: County-Level Percent Households Without Internet Access")),
                                                     p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                       a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002, Presence and types of internet subscriptions in household.', target = "_blank")),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Box Plot"),
                                                            p(),
                                                            plotlyOutput("plotly_noint_med"),
                                                            p()),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Map"),
                                                            p(),
                                                            leafletOutput("subplot_noint_med"))
                                            ),
                                            tabPanel(title = "Households without computer", 
                                                     br(),
                                                     h4(strong("Indicator: County-Level Percent Households Without a Computer")),
                                                     p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                       a(href = 'https://data.census.gov/cedsci/table?q=computer&tid=ACSDT5Y2018.B28003&t=Telephone,%20Computer,%20and%20Internet%20Access&vintage=2018&d=ACS%205-Year%20Estimates%20Detailed%20Tables', 'Table B28003, Presence of a computer and type of internet subscription in household.', target = "_blank")),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Box Plot"),
                                                            p(),
                                                            plotlyOutput("plotly_nocomp_med"),
                                                            p()),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Map"),
                                                            p(),
                                                            leafletOutput("subplot_nocomp_med"))
                                            ),
                                            tabPanel(title = "Population uninsured", 
                                                     br(),
                                                     h4(strong("Indicator: County-Level Percent Population Under Age 65 Without Health Insurance")),
                                                     p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       Data come from the Small Area Health Insurance Estimates 2017 via CountyHealthRankings,", 
                                                       a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-factors/clinical-care/access-to-care/uninsured', 'Percentage of population under age 65 without health insurance.', target = "_blank")),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Box Plot"),
                                                            p(),
                                                            plotlyOutput("plotly_unins"),
                                                            p()),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Map"),
                                                            p(),
                                                            leafletOutput("subplot_unins"))
                                            ),
                                            tabPanel(title = "Poor mental health days", 
                                                     br(),
                                                     h4(strong("Indicator: County-Level Average Number of Poor Mental Health Days in Past Month")),
                                                     p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       Data come from the Behavioral Risk Factor Surveillance System 2017 via CountyHealthRankings,", 
                                                       a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-outcomes/quality-of-life/poor-mental-health-days', 'Average number of mentally unhealthy days reported in past 30 days (age-adjusted).', target = "_blank")),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Box Plot"),
                                                            p(),
                                                            plotlyOutput("plotly_menthdays"),
                                                            p()),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Map"),
                                                            p(),
                                                            leafletOutput("subplot_poorment"))
                                            ),
                                            tabPanel(title = "Mental health providers", 
                                                     br(),
                                                     h4(strong("Indicator: County-Level Number of Mental Health Providers per 100,000 Population")),
                                                     p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       Data come from the CMS National Provider Identification 2019 via CountyHealthRankings,", 
                                                       a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-factors/clinical-care/access-to-care/mental-health-providers', 'Number of mental health providers per 100,000 population.', target = "_blank")),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Box Plot"),
                                                            p(),
                                                            plotlyOutput("plotly_menthprov"),
                                                            p()),
                                                     column(6, align = "left",
                                                            p(),
                                                            strong("County-Level Map"),
                                                            p(),
                                                            leafletOutput("subplot_healthprov"))
                                            )
                                          )
                                 )
                ), 
                
                fluidRow(style = "margin: 6px",
                         width = 12,
                         column(6, align = "left",
                                tags$em("How to interpret this box plot."),
                                p("The box plot visualizes data distribution for the selected indicator. The bottom and top horizontal lines extending from the box represent the smallest and largest non-outlier values on the selected indicator.
                                  The bottom of the box represents the lower quartile; 25% of counties have indicator values below this threshold. Similarly, the top of the box represents the upper quartile, with 25% of counties having 
                                  indicator values above this threshold. The body of the box represents the interquartile range, capturing indicator values for 50% of all counties. The horizontal line drawn through the box is the median 
                                  -- the middle quartile or the midpoint value in the distribution. Half of all counties have indicator values below this threshold, and half have indicator values above the threshold.
                                  Dots represent outlier values. Hover over the boxplot to view exact summary values.")),
                         column(6, align = "left",
                                tags$em("How to interpret this map."),
                                p("The map visualizes values on the selected indicator at county level. Counties with darker map colors have a higher value on the selected indicator. Conversely, counties with lighter colors have lower
                                  values on the selected indicator. To facilitate comparisons, map colors represent grouped values. To view the exact indicator value for a particular county, hover over the county on the map. The information
                                  box will display the county name, exact indicator value, and its corresponding quintile."))
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
  # Plotly boxplots ------------------------------------------
  # 
  
  # Work: No computer
  output$plotly_nocomp_work <- renderPlotly({
    if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$nocomputer, 
              type = "box",
              name = "Percent population in labor force without a computer") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE))
    }
  })
  
  # Work: No internet
  output$plotly_noint_work <- renderPlotly({
    if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$nointernet, 
              type = "box",
              name = "Percent households without broadband internet") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE))
    }
  })
  
  # Work: Occupations
  output$plotly_occup <- renderPlotly({
    if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$occup, 
              type = "box",
              name = "Percent non-remote workers in non-remote friendly occupations") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE))
    }
  })
  
  # Work: Industries
  output$plotly_industr <- renderPlotly({
    if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$industr, 
              type = "box",
              name = "Percent non-remote workers in non-remote friendly industries") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE))
    }
  })
  
  # Med: No internet
  output$plotly_noint_med <- renderPlotly({
    if (input$whichtopic == "Telemental health") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$nointernet, 
              type = "box",
              name = "Percent households without internet access") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE))
    }
  })
  
  # Med: No computer
  output$plotly_nocomp_med <- renderPlotly({
    if (input$whichtopic == "Telemental health") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$nocomputer, 
              type = "box",
              name = "Percent households without a computer") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE))
    }
  })
  
  # Med: Uninsured
  output$plotly_unins <- renderPlotly({
    if (input$whichtopic == "Telemental health") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$pct_unins, 
              type = "box",
              name = "Percent population under age 65 without health insurance") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE))
    }
  })
  
  # Med: Mental health days
  output$plotly_menthdays <- renderPlotly({
    if (input$whichtopic == "Telemental health") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$avgnum_poormenth, 
              type = "box",
              name = "Average number of poor mental health days in past month") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE))
    }
  })
  
  # Med: Providers
  output$plotly_menthprov <- renderPlotly({
    if (input$whichtopic == "Telemental health") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$menthprov_per100k, 
              type = "box",
              name = "Number of mental health providers per 100,000 population") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE))
    }
  })
  
  # Edu: No internet
  output$plotly_noint_edu <- renderPlotly({
    if (input$whichtopic == "Remote education") {
      data <- data_edu
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$nointernet, 
              type = "box",
              name = "Percent households without internet access") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE))
    }
  })
  
  # Edu: No computer
  output$plotly_nocomp_edu <- renderPlotly({
    if (input$whichtopic == "Remote education") {
      data <- data_edu
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$nocomputer, 
              type = "box",
              name = "Percent youth (age <18) without a computer") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE))
    }
  })
  
  # In K-12
  output$plotly_ink12 <- renderPlotly({
    if (input$whichtopic == "Remote education") {
      data <- data_edu
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$ink12, 
              type = "box",
              name = "Percent population enrolled in K-12") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE))
    }
  })
  
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
              "<strong>% Youth (age <18) without internet access:</strong>",
              round(data$nointernet, 2), "(quintile", data$nointernetQuint, ")",
              "<br />",
              "<strong>% Households without a computer:</strong>",
              round(data$nocomputer, 2), "(quintile", data$nocomputerQuint, ")",
              "<br />",
              "<strong>% Population enrolled in K-12:</strong>",
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
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$vulnerability,
                  title = "Relative Vulnerability", opacity = 1)
      
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
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$vulnerability,
                  title = "Relative Vulnerability", opacity = 1)
      
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
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$vulnerability,
                  title = "Relative Vulnerability", opacity = 1)
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
      quintcuts$Indicator <- c("% Households without internet access", "% Youth (age <18) with no computer", "% Population enrolled in K-12")
      quintcuts <- quintcuts %>% select(Indicator, 2:7)
      colnames(quintcuts) <- c("Indicator", "<Q1", "<Q2", "<Q3", "<Q4", "<Q5")
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
      colnames(quintcuts) <- c("Indicator", "<Q1", "<Q2", "<Q3", "<Q4", "<Q5")
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
      quintcuts$Indicator <- c("% Households without internet access", "% Households without a computer", "Average number of poor mental health days", "Number of mental health providers per 100,000 population", "% Population without health insurance")
      quintcuts <- quintcuts %>% select(Indicator, 2:7)
      colnames(quintcuts) <- c("Indicator", "<Q1", "<Q2", "<Q3", "<Q4", "<Q5")
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
      
      pal <- colorBin("Blues", domain = data$nointernet, bins = 5)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Households without internet access:</strong>",
              round(data$nointernet, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$nointernetQuint),
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
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$nointernet,
                  title = "Percent", opacity = 1,
                  na.label = "Not Available")
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
      
      pal <- colorBin("Blues", domain = data$nointernet, bins = 5)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Households without broadband internet:</strong>",
              round(data$nointernet, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$nointernetQuint),
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
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$nointernet,
                  title = "Percent", opacity = 1,
                  na.label = "Not Available")
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
      
      pal <- colorBin("Blues", domain = data$nointernet, bins = 5)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Households without internet access:</strong>",
              round(data$nointernet, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$nointernetQuint),
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
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$nointernet,
                  title = "Percent", opacity = 1,
                  na.label = "Not Available")
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
      
      pal <- colorBin("Blues", domain = data$nocomputer, bins = 5)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Households without a computer:</strong>",
              round(data$nocomputer, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$nocomputerQuint),
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
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$nocomputer,
                  title = "Percent", opacity = 1,
                  na.label = "Not Available")
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
      
      pal <- colorBin("Blues", domain = data$nocomputer, bins = 5)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Households without a computer:</strong>",
              round(data$nocomputer, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$nocomputerQuint),
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
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$nocomputer,
                  title = "Percent", opacity = 1,
                  na.label = "Not Available")
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
      
      pal <- colorBin("Blues", domain = data$nocomputer, bins = 5)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Households without a computer:</strong>",
              round(data$nocomputer, 2),
              "<br />",
              "<strong>Quintile:</strong>",
              data$nocomputerQuint),
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
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$nocomputer,
                  title = "Percent", opacity = 1,
                  na.label = "Not Available")
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
      
      pal <- colorBin("Blues", domain = data$ink12, bins = 5)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Population enrolled in K-12:</strong>",
              round(data$ink12, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$ink12Quint),
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
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$ink12,
                  title = "Percent", opacity = 1,
                  na.label = "Not Available")
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
      
      pal <- colorBin("Blues", domain = data$occup, bins = 5)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Non-remote workers in non-remote-friendly occupations:</strong>",
              round(data$occup, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$occupQuint),
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
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$occup,
                  title = "Percent", opacity = 1,
                  na.label = "Not Available")
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
      
      pal <- colorBin("Blues", domain = data$industr, bins = 5)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Non-remote workers in non-remote-friendly industries:</strong>",
              round(data$industr, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$industrQuint),
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
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$industr,
                  title = "Percent", opacity = 1,
                  na.label = "Not Available")
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
      
      pal <- colorBin("Blues", domain = data$avgnum_poormenth, bins = 5)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Average number of poor mental health days in past month:</strong>",
              round(data$avgnum_poormenth, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$menthdaysQuint),
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
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$avgnum_poormenth,
                  title = "Number", opacity = 1,
                  na.label = "Not Available")
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
      
      pal <- colorBin("Blues", domain = data$pct_unins, bins = 5)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Population under age 65 without health insurance:</strong>",
              data$pct_unins, "(quintile", data$uninsQuint, ")",
              "<br />",
              "<strong>Quintile:</strong>",
              data$uninsQuint),
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
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$pct_unins,
                  title = "Percent", opacity = 1,
                  na.label = "Not Available")
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
      
      pal <- colorBin("Blues", domain = data$menthprov_per100k, bins = 5)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Number of mental health providers per 100,000 population:</strong>",
              round(data$menthprov_per100k, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$menthprovQuint),
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
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$menthprov_per100k,
                  title = "Number", opacity = 1,
                  na.label = "Not Available")
    }
  })
  
}


#
# App ---------------------------------------------
#

shinyApp(ui = ui, server = server)