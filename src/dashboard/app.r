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
                
                fluidRow(width = 12,
                         align = "center",
                         img(src = "logo.jpg", class = "topimage", width = "40%", style = "display: block; margin-left: auto; margin-right: auto;")
                ),
                
                fluidRow(width = 12, 
                         column(1),
                         column(10, align = "center", h2(strong("Connectivity Infrastructure as Barrier to Remote Work, Education, and Mental Health Care: County Explorer"))),
                         column(1)
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
                                   ('It facilitates relative comparisons between counties within states, highlighting areas where workers may have difficulty working remotely, where youth would
                                     face barriers to participating in online education, and where county residents may have high need but low access to telemental health services. 
                                     The dashboard allows extension professionals and policy-makers in Iowa, Oregon, and Virginia to make informed decisions about interventions and resource allocation
                                    based on conditions in their counties.')),
                                p(),
                                br('Using the map selector allows filtering by topic and state. Hovering over a county area on the resulting map displays information 
                                   about the calculated county relative accessibility status, as well as relevant connectivity and need measures. Supplemental maps
                                   allow users to explore individual indicators used to create the three relative accessibility measures.
                                   Methodology, data source, and measure descriptions are available below.'),
                                p(),
                                br('If you would like to explore broadband internet coverage further, please visit our', a(href = 'https://bband.policy-analytics.net', 'broadband data source comparison', target = "_blank"), 'dashboard.')
                                ),
                         column(1)
                                ),
                
                hr(),
                
                fluidRow(width = 12, style = "margin: 20px",
                         h2('Measures and Data Sources'),
                         tabsetPanel(
                           tabPanel(title = "Remote Work Accessibility", 
                                    wellPanel(strong('Remote Work Accessibility'), 
                                              p(),
                                              em('Description.'),
                                              br(), tags$b('The remote work relative accessibility measure highlights counties where residents may have difficulty working remotely if instructed to do so.'),
                                              ('It conceptualizes four telecommunication infrastructure and employment characteristics as potential barriers, providing
                                              a relative ranking of county telework preparedness.'),
                                              p(),
                                              em('How We Measure Remote Work Accessibility.'),
                                              br('We calculate remote work relative accessibility using information on percent:'),
                                              tags$li('Households with no broadband internet subscription.'),
                                              tags$li('Persons in labor force with no computer available.'),
                                              tags$li('Persons who are not currently working remotely and are employed in telework unfriendly occupations
                                                     (service, natural, construction, maintenance, production, transportation, material moving, and military specific occupations).'),
                                              tags$li('Persons who are not currently working remotely and are employed in telework unfriendly industries 
                                                (construction, manufacturing, wholesale, retail, transportation and warehousing, utilities, and government, including armed forces).'),
                                              br('We compute quintile cut-offs for each indicator. County placement in a higher quintile indicates lower relative accessibility.
                                                 We assign descriptive labels for county relative accessibility based on whether they placed in 4th or 5th quintile a certain number of times:'),
                                              tags$li('Very high: 0 indicators.'),
                                              tags$li('High: 1 indicator.'),
                                              tags$li('Medium: 2 indicators.'), 
                                              tags$li('Low: 3 indicators.'),
                                              tags$li('Very low: all 4 indicators.'),
                                              p(),
                                              em('Data source.'),
                                              p(a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target = "_blank"), "2014/18 (5-year) estimates.")
                                    )),
                           tabPanel(title = "Remote Education Accessibility", 
                                    wellPanel(strong('Remote Education Accessibility'), 
                                              p(),
                                              em('Description.'),
                                              br(), tags$b('The remote education relative accessibility measure highlights counties where K-12 students may have difficulty participating in online education.'), 
                                              ('It considers telecommunication infastructure and K-12 enrollment in providing a relative ranking of county K-12 remote education preparedness.'),
                                              p(),
                                              em('How We Measure Remote Education Accessibility.'),
                                              br('We calculate remote education relative accessibility using information on percent:'),
                                              tags$li('Households with no internet access subscription.'),
                                              tags$li('Population under age 18 without a computer.'),
                                              tags$li('Population enrolled in K-12.'),
                                              br('We compute quintile cut-offs for each indicator. County placement in a higher quintile indicates lower relative accessibility.
                                                We assign descriptive labels for county relative accessibility based on whether they placed in 4th or 5th quintile a certain number of times:'),
                                              tags$li('High: 0 indicators.'),
                                              tags$li('Medium: 1 indicator.'),
                                              tags$li('Low: 2 indicators.'),
                                              tags$li('Very low: all 3 indicators.'),
                                              p(),
                                              em('Data source.'),
                                              p(a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target = "_blank"), "2014/18 (5-year) estimates.")
                                    )),
                           tabPanel(title = "Telemental Health Accessibility", 
                                    wellPanel(strong('Telemental Health Accessibility'), 
                                              p(),
                                              em('Description.'),
                                              br(), tags$b('The telemental health relative accessibility measure highlights counties where high need for mental health services is coupled with
                                                  barriers to access.'), ('It considers telecommunication infastructure, health insurance, in-person provider availability, and mental health status
                                                                          in providing a relative ranking of county K-12 telemental health accessibility.'),
                                              p(),
                                              em('How We Measure Telemental Health Accessibility.'),
                                              br('We calculate telemental health relative accessibility using information on:'),
                                              tags$li('Percent households without internet access.'),
                                              tags$li('Percent households with no computer.'),
                                              tags$li('Average number of poor mental health days in past month.'),
                                              tags$li('Number of mental health providers per 100,000 population (reverse-coded).'),
                                              tags$li('Percent population under age 65 without health insurance.'),
                                              br('We compute quintile cut-offs for each indicator. County placement in a higher quintile indicates lower relative accessibility.
                                                We assign descriptive labels for county relative accessibility based on whether they placed in 4th or 5th quintile a certain number of times:'),
                                              tags$li('Very high: 0 indicators.'),
                                              tags$li('High: 1 indicator.'),
                                              tags$li('Medium: 2 or 3 indicators.'),
                                              tags$li('Low: 4 indicators.'),
                                              tags$li('Very low: all 5 indicators'),
                                              p(),
                                              em('Data source.'),
                                              p(a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target = "_blank"), "2014/18 (5-year) estimates and",
                                                a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources', 'County Health Rankings', target = "_blank"), "2019.")
                                    ))
                         )
                ),
                
                hr(),
                
                fluidRow(style = "margin: 6px",
                         width = 12, 
                         column(12, align = "left", h3(strong("Explore County Relative Accessibility")))
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
                                p("The map may take a moment to load.")
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
                                  construct the selected relative accessibility measure."), "Each tab displays a 
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
                                                     h4(strong("Indicator: County-Level Percent Population Without Health Insurance")),
                                                     p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                       a(href = 'https://data.census.gov/cedsci/table?q=coverage&tid=ACSDT5Y2018.B27020&d=ACS%205-Year%20Estimates%20Detailed%20Tables&vintage=2018', 'Table B27020, Health insurance coverage status and type by citizenship status.', target = "_blank")),
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
                                  values on the selected indicator. To facilitate comparisons and display quintile cut-off points, map colors represent quintile-grouped values. To view the exact indicator value for a particular county, hover over the county on the map. The information
                                  box will display the county name, exact indicator value, and its corresponding quintile."))
                         ),
                
                hr(),
                
                fluidRow(style = "margin: 20px",
                         width = 12, 
                         column(6, align = "center",
                                h2("Acknowledgments"),
                                br("We used the quintile method described by the", a(href = "https://pcrd.purdue.edu/blog/remote-work-and-the-coronavirus.php", "Purdue Center for Regional Development"), 
                                   "to construct our relative accessibility measures. One of our indicators, remote work accessibility, is also an adaptation of Purdue's remote work metric. 
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
              x = "Percent population in labor force without a computer",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
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
              x = "Percent households without broadband internet",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
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
              x = "Percent non-remote workers in non-remote friendly occupations",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
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
              x = "Percent non-remote workers in non-remote friendly industries",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
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
              x = "Percent households without internet access",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
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
              x = "Percent households without a computer",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
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
              x = "Percent population without health insurance",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
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
              x = "Average number of poor mental health days in past month",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
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
              x = "Number of mental health providers per 100,000 population",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
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
              x = "Percent households without internet access",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
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
              x = "Percent youth (age <18) without a computer",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
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
              x = "Percent population enrolled in K-12",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
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
      
      pal <- colorFactor("Oranges", domain = data$accessibility)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Relative accessibility:</strong>",
              data$accessibility,
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
        addPolygons(fillColor = ~pal(data$accessibility), 
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
        addLegend("bottomleft", pal = pal, values = ~data$accessibility,
                  title = "Relative Accessibility", opacity = 1,
                  na.label = "Not Available")
      
      # Main plot: remote work
    } else if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorFactor("Oranges", domain = data$accessibility)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Relative accessibility:</strong>",
              data$accessibility,
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
        addPolygons(fillColor = ~pal(data$accessibility), 
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
        addLegend("bottomleft", pal = pal, values = ~data$accessibility,
                  title = "Relative Accessibility", opacity = 1,
                  na.label = "Not Available")
      
      # Main plot: telehealth
    } else if (input$whichtopic == "Telemental health") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorFactor("Oranges", domain = data$accessibility)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Relative accessibility:</strong>",
              data$accessibility,
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
        addPolygons(fillColor = ~pal(data$accessibility), 
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
        addLegend("bottomleft", pal = pal, values = ~data$accessibility,
                  title = "Relative Accessibility", opacity = 1,
                  na.label = "Not Available")
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
      
      pal <- colorQuantile("Blues", domain = data$nointernet, probs = seq(0, 1, length = 6), right = TRUE)
      
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
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
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
      
      pal <- colorQuantile("Blues", domain = data$nointernet, probs = seq(0, 1, length = 6), right = TRUE)
      
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
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
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
      
      pal <- colorQuantile("Blues", domain = data$nointernet, probs = seq(0, 1, length = 6), right = TRUE)
      
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
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
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
      
      pal <- colorQuantile("Blues", domain = data$nocomputer, probs = seq(0, 1, length = 6), right = TRUE)
      
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
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
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
      
      pal <- colorQuantile("Blues", domain = data$nocomputer, probs = seq(0, 1, length = 6), right = TRUE)
      
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
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
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
      
      pal <- colorQuantile("Blues", domain = data$nocomputer, probs = seq(0, 1, length = 6), right = TRUE)
      
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
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
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
      
      pal <- colorQuantile("Blues", domain = data$ink12, probs = seq(0, 1, length = 6), right = TRUE)
      
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
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
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
      
      pal <- colorQuantile("Blues", domain = data$occup, probs = seq(0, 1, length = 6), right = TRUE)
      
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
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
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
      
      pal <- colorQuantile("Blues", domain = data$industr, probs = seq(0, 1, length = 6), right = TRUE)
      
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
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
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
      
      pal <- colorQuantile("Blues", domain = data$avgnum_poormenth, probs = seq(0, 1, length = 6), right = TRUE)
      
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
                  title = "Number<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
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
      
      pal <- colorQuantile("Blues", domain = data$pct_unins, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Population without health insurance:</strong>",
              round(data$pct_unins, 2),
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
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
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
      
      pal <- colorQuantile("Blues", domain = data$menthprov_per100k, probs = seq(0, 1, length = 6), right = TRUE, reverse = TRUE)
      
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
                  title = "Number<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
}


#
# App ---------------------------------------------
#

shinyApp(ui = ui, server = server)