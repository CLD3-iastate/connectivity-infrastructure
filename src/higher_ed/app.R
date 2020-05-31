# VA In-State Undergraduate Higher Education Enrollment, R-Shiny app

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
library(readxl)

# Load data
df <- readRDS("./Data/hi_ed_acs.rds")  
hi_ed <- df[c(1,2,9,14:54)]
demo_df <- read_excel("./Data/Demo.xlsx", col_names = TRUE)
pell <- readRDS("./Data/pell_acs.rds")


# User interface

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
                         column(12, align = "center", h2(strong("In-State Higher-Education Undergraduate Enrollment in Virginia")))
                ),
                
                hr(),
                
                fluidRow(width = 12, 
                         column(1),
                         column(10, 
                                p(),
                                br(tags$b('This dashboard visualizes in-state higher-education undergraduate enrollment in Virginia.'), 
                                   ('...')),
                                p(),
                                br('Use the selectors to choose academic year and higher-education type. Hover over a county to show the 
                                    county name, and number of students enrolled and percentage of students receiving PELL for the 
                                    year and higher-education type selected.'
                                   ),
                                br('PELL data for 2019-2020 is not available yet.  All of the 2019-2020 PELL data is listed as NA.')
                         ),
                         column(1)
                ),
                
                hr(),
                
                fluidRow(align="center",
                  h3(strong("In-State Undergraduate Enrollment by Locality")),
                  br()
                ),
                
                fluidRow(#width = 12, 

                  column(width = 4, align = "left",
                         selectInput("whichyear", "Academic Year",
                                     choices = list(
                                                    "2019-2020",
                                                    "2018-2019",
                                                    "2017-2018",
                                                    "2016-2017",
                                                    "2015-2016"
                                                    ),
                                     selected = "2019-2020")
                       ),

                  column(width=4, align="left",
                         selectInput("whichtype", "Higher-Education Type",
                                     choices = list("Grand Total, All Reporting Institutions",
                                                    "Total Private, Non-Profit, Four-Year Institutions",
                                                    "Total Public Four-Year Institutions",
                                                    "Total Public Two-Year Institutions"
                                     ),
                                     selected = "Grand Total, All Reporting Institutions")
                        ),
                  
                  column(width=4, align="center",
                    h4(strong("Virginia Summary")),
                    textOutput("year")
                         
                         )
                  
                  ),
                
                
                fluidRow(
                  
                  column(width = 8, 
                         leafletOutput("leafplot", width = "800px")
                  ),

                  column(width = 4, align = "center",
                         plotOutput("VA_enr_by_type", height = "250px", width = "250px"),
                         tableOutput("enroll_table"),
                         plotOutput("FT_PT", height="100px")
                  )
                                    
                ),
                
                fluidRow(align="center",
                  print("Source: State Council of Higher Education for Virginia, 
                               Tables LD03, E02, and FA31C")
                ),
                
                hr(),
                
                fluidRow(align="center",
                         h3(strong("In-State Undergraduate Demographic Trends")),
                         br()
                ),
                
                fluidRow(align="center",
                         selectInput("hi_ed_type", "Higher-Education Type",
                                     choices = list("Grand Total, All Reporting Institutions",
                                                    "Total Private, Non-Profit, Four-Year Institutions",
                                                    "Total Public Four-Year Institutions",
                                                    "Total Public Two-Year Institutions"
                                     ),
                                     selected = "Grand Total, All Reporting Institutions")
                  
                ),
                
                fluidRow(
                  
                  column(width = 6,
                         plotOutput("race_eth", height="350px")
                  ),

                  column(width = 6,
                         plotOutput("gender", height="350px")
                  )

                ),
                
                fluidRow(align="center",
                         print("Source: State Council of Higher Education for Virginia, Table E02")
                )

)


server <- function(input, output) {
  

  # Dynamic map
  output$leafplot <- renderLeaflet({
    
    selected_year <- switch(input$whichyear,
                            "2019-2020" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2019_20", "PR_4YR_2019_20", 
                                                  "PUB_4YR_2019_20", "PUB_2YR_2019_20", 
                                                  "geometry")],
                            "2018-2019" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2018_19", "PR_4YR_2018_19", 
                                                  "PUB_4YR_2018_19", "PUB_2YR_2018_19",
                                                   "geometry")],
                            "2017-2018" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2017_18", "PR_4YR_2017_18", 
                                                  "PUB_4YR_2017_18", "PUB_2YR_2017_18", 
                                                  "geometry")],
                            "2016-2017" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2016_17", "PR_4YR_2016_17", 
                                                  "PUB_4YR_2016_17", "PUB_2YR_2016_17", 
                                                  "geometry")],
                            "2015-2016" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2015_16", "PR_4YR_2015_16", 
                                                  "PUB_4YR_2015_16", "PUB_2YR_2015_16",
                                                  "geometry")])
    
    selected_year <- st_transform(selected_year, 4326)  # ask Teja what this does - some sort of GEO
    
    # rename columns so each year has the same column names
    
    old_names <- colnames(selected_year)
    new_names <- c("STATEFP", "COUNTYFP", "NAME", "ALL", "PR_4YR", "PUB_4YR", "PUB_2YR", "geometry")
    selected_year <- selected_year %>% rename_at(vars(all_of(old_names)), ~ new_names)

    
    selected_type <- switch(input$whichtype,
                          "Grand Total, All Reporting Institutions" = selected_year$ALL,
                          "Total Private, Non-Profit, Four-Year Institutions" = selected_year$PR_4YR,
                          "Total Public Four-Year Institutions" = selected_year$PUB_4YR,
                          "Total Public Two-Year Institutions" = selected_year$PUB_2YR )
     

    selected_year_pell <- switch(input$whichyear,
                            "2019-2020" = pell[c("STATEFP", "COUNTYFP", "NAME",
                              "19_20 ALL Perc PELL",  "19_20 PR4 Perc PELL",
                                                  "19_20 PUB4 Perc PELL", "19_20 PUB2 Perc PELL")],
                            "2018-2019" = pell[c("STATEFP", "COUNTYFP", "NAME",
                                "18_19 ALL Perc PELL",  "18_19 PR4 Perc PELL",
                                                  "18_19 PUB4 Perc PELL", "18_19 PUB2 Perc PELL")],
                            "2017-2018" = pell[c("STATEFP", "COUNTYFP", "NAME",
                              "17_18 ALL Perc PELL",  "17_18 PR4 Perc PELL",
                                                  "17_18 PUB4 Perc PELL", "17_18 PUB2 Perc PELL")],
                            "2016-2017" = pell[c("STATEFP", "COUNTYFP", "NAME",
                              "16_17 ALL Perc PELL",  "16_17 PR4 Perc PELL",
                                                  "16_17 PUB4 Perc PELL", "16_17 PUB2 Perc PELL")],
                            "2015-2016" = pell[c("STATEFP", "COUNTYFP", "NAME",
                              "15_16 ALL Perc PELL",  "15_16 PR4 Perc PELL",
                                                  "15_16 PUB4 Perc PELL", "15_16 PUB2 Perc PELL")])

    old_names <- colnames(selected_year_pell)
    new_names <- c("STATEFP", "COUNTYFP", "NAME", "ALL_pell", "PR4YR_pell", "PUB4YR_pell", "PUB2YR_pell", "geometry")
    selected_year_pell <- selected_year_pell %>% rename_at(vars(all_of(old_names)), ~ new_names)


    selected_pell <- switch(input$whichtype,
                            "Grand Total, All Reporting Institutions" = selected_year_pell$ALL_pell,
                            "Total Private, Non-Profit, Four-Year Institutions" = selected_year_pell$PR4YR_pell,
                            "Total Public Four-Year Institutions" = selected_year_pell$PUB4YR_pell,
                            "Total Public Two-Year Institutions" = selected_year_pell$PUB2YR_pell )


    pal <- colorBin("Blues", domain = selected_type, 
                    bins = c(0, 250, 500, 1000, 2500, 5000, 10000, 20000, 50000)) #bins = 8)
    
    labels <- lapply(
      paste("<strong>County: </strong>",
            selected_year$NAME,
            "<br />",
            "<strong># of Students: </strong>",
            comma(selected_type, format="f"),
            "<br />",
            "<strong>% who Received PELL: </strong>",
            substr(selected_pell, start=1, stop=2),
            "%"),
      htmltools::HTML
    )
    
    leaflet(data = selected_year) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(selected_type), 
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
                values = ~(round(selected_type, 0)),
                colors = brewer.pal(8, "Blues"), #c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026"),
                labels = c("0-250", "251-500", "501-1000", "1001-2500", "2501-5000",
                           "5001-10000", "10001-20000", "20001+"),
                title = "Number of Students",
                opacity = 0.8)
    
  })
  

  output$enroll_table <- renderTable({
    
    # get data for the input year
    
    selected_year <- switch(input$whichyear,
                            "2019-2020" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2019_20", "PR_4YR_2019_20", 
                                                  "PUB_4YR_2019_20", "PUB_2YR_2019_20", "geometry")],
                            "2018-2019" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2018_19", "PR_4YR_2018_19", 
                                                  "PUB_4YR_2018_19", "PUB_2YR_2018_19", "geometry")],
                            "2017-2018" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2017_18", "PR_4YR_2017_18", 
                                                  "PUB_4YR_2017_18", "PUB_2YR_2017_18", "geometry")],
                            "2016-2017" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2016_17", "PR_4YR_2016_17", 
                                                  "PUB_4YR_2016_17", "PUB_2YR_2016_17", "geometry")],
                            "2015-2016" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2015_16", "PR_4YR_2015_16", 
                                                  "PUB_4YR_2015_16", "PUB_2YR_2015_16", "geometry")])
    
    selected_year <- st_transform(selected_year, 4326)  # ask Teja what this does - some sort of GEO
    
    # rename columns so each year has the same column names
    
    old_names <- colnames(selected_year)
    new_names <- c("STATEFP", "COUNTYFP", "NAME", "ALL", "PR_4YR", "PUB_4YR", "PUB_2YR", "geometry")
    selected_year <- selected_year %>% rename_at(vars(all_of(old_names)), ~ new_names)
    
    # build table

    total_all <- sum(selected_year$ALL)
    total_pub4yr <- sum(selected_year$PUB_4YR)
    total_pr4yr <- sum(selected_year$PR_4YR)
    total_pub2yr <- sum(selected_year$PUB_2YR)

    Enrollment <- c(total_pub4yr, total_pr4yr, total_pub2yr, total_all)
    Type <- c("Four-Year Public", "Four-Year Private", "Two-Year Public", "Total")
    data <- data.frame(Type, Enrollment)
    data$Enrollment <- comma(as.integer(data$Enrollment), format='d')
    
    
    data
  })
  
  
  output$VA_enr_by_type <- renderPlot({
    
    # get data for the input year
    
    selected_year <- switch(input$whichyear,
                            "2019-2020" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2019_20", "PR_4YR_2019_20", 
                                                  "PUB_4YR_2019_20", "PUB_2YR_2019_20", "geometry")],
                            "2018-2019" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2018_19", "PR_4YR_2018_19", 
                                                  "PUB_4YR_2018_19", "PUB_2YR_2018_19", "geometry")],
                            "2017-2018" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2017_18", "PR_4YR_2017_18", 
                                                  "PUB_4YR_2017_18", "PUB_2YR_2017_18", "geometry")],
                            "2016-2017" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2016_17", "PR_4YR_2016_17", 
                                                  "PUB_4YR_2016_17", "PUB_2YR_2016_17", "geometry")],
                            "2015-2016" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2015_16", "PR_4YR_2015_16", 
                                                  "PUB_4YR_2015_16", "PUB_2YR_2015_16", "geometry")])
    
    selected_year <- st_transform(selected_year, 4326)  # ask Teja what this does - some sort of GEO
    
    # rename columns so each year has the same column names
    
    old_names <- colnames(selected_year)
    new_names <- c("STATEFP", "COUNTYFP", "NAME", "ALL", "PR_4YR", "PUB_4YR", "PUB_2YR", "geometry")
    selected_year <- selected_year %>% rename_at(vars(all_of(old_names)), ~ new_names)
    
    # build plot
    
    total_all <- sum(selected_year$ALL)
    total_pub4yr <- sum(selected_year$PUB_4YR)
    total_pr4yr <- sum(selected_year$PR_4YR)
    total_pub2yr <- sum(selected_year$PUB_2YR)

    enroll <- c(100*total_pub4yr/total_all, 100*total_pr4yr/total_all, 100*total_pub2yr/total_all)
    type <- c("Four-Year Public", "Four-Year Private", "Two-Year Public")
  
    data <- data.frame(type, enroll)
    data <- data %>% arrange(desc(type)) %>%
                     mutate(lab.ypos = cumsum(enroll) - 0.5*enroll) %>%
                     mutate(lab = paste0(type, "\n" ,round(enroll,2), "%"))
                    

    ggplot(data, aes(x="", y=enroll, fill=type)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      
      theme_void() + 
      theme(legend.position="none") +
      #labs(title=input$whichyear) +
      geom_text(aes(y = lab.ypos, label = lab), color = "black") + 
      scale_fill_brewer(palette="GnBu")
      
  
  })

  output$year <- renderText({
    
    s <- input$whichyear
    s
    
  })
  

  output$race_eth <- renderPlot({
    dat <- demo_df[demo_df$Description == "Foreign/International" | 
                demo_df$Description == "African American or Black (NH)" |
                demo_df$Description == "American Indian/Native American (NH)" |
                demo_df$Description == "Asian/Pacific Islander (NH)" |
                demo_df$Description == "Hispanic" |
                demo_df$Description == "White, Caucasian American (NH)" |
                demo_df$Description == "Multi-Race (NH)" |
                demo_df$Description == "Unknown/Unreported (NH)", ]
    
    selected_type <- switch(input$hi_ed_type,
                            "Grand Total, All Reporting Institutions" = 
                              dat[dat$Institution == "Grand Total, All Reporting Institutions", ],
                            "Total Private, Non-Profit, Four-Year Institutions" = 
                              dat[dat$Institution == "Total Private, Nonprofit, Four-Year Institutions", ],
                            "Total Public Four-Year Institutions" = 
                              dat[dat$Institution == "Total Public Four-Year Institutions", ],
                            "Total Public Two-Year Institutions" = 
                              dat[dat$Institution == "Total Public Two-Year Institutions", ]
    )
    
    
    
    ggplot(selected_type, aes(x = `Fall Term`, y = `In-State Percentage`, color = Description)) +
      geom_line() +
      geom_point() + 
      labs(title = "Race/Ethicity") +
      xlab("Year") +
      ylab("Percentage of In-State Undergraduates") +
      theme(legend.position="bottom", legend.title = element_blank(), panel.background = element_blank()) +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))  
  })

  output$gender <- renderPlot({
    dat <- demo_df[demo_df$Description == "Men" | 
                     demo_df$Description == "Women" |
                     demo_df$Description == "Gender Unreported", ]

    selected_type <- switch(input$hi_ed_type,
                            "Grand Total, All Reporting Institutions" = 
                              dat[dat$Institution == "Grand Total, All Reporting Institutions", ],
                            "Total Private, Non-Profit, Four-Year Institutions" = 
                              dat[dat$Institution == "Total Private, Nonprofit, Four-Year Institutions", ],
                            "Total Public Four-Year Institutions" = 
                              dat[dat$Institution == "Total Public Four-Year Institutions", ],
                            "Total Public Two-Year Institutions" = 
                              dat[dat$Institution == "Total Public Two-Year Institutions", ]
    )
    
    
    
    ggplot(selected_type, aes(x = `Fall Term`, y = `In-State Percentage`, color = Description)) +
      geom_line() +
      geom_point() +
      labs(title ="Gender") +
      xlab("Year") +
      ylab("Percentage of In-State Undergraduates") +
      theme(legend.position="bottom", legend.title = element_blank(), panel.background = element_blank()) + 
      scale_color_manual(values=c("#F0E442", "#0072B2", "#CC79A7"))
    

  })

  
  output$FT_PT <- renderPlot({

    selected_year <- switch(input$whichyear,
                            "2019-2020" = demo_df[demo_df$`Fall Term` == 2019 &
                                                     (demo_df$Description == "Full-Time" |
                                                        demo_df$Description=="Part-Time"), ],
                             "2018-2019" = demo_df[demo_df$`Fall Term` == 2018 &
                                                     (demo_df$Description == "Full-Time" |
                                                        demo_df$Description=="Part-Time"), ],
                             "2017-2018" = demo_df[demo_df$`Fall Term` == 2017 &
                                                     (demo_df$Description == "Full-Time" |
                                                        demo_df$Description=="Part-Time"), ],
                             "2016-2017" = demo_df[demo_df$`Fall Term` == 2016 &
                                                     (demo_df$Description == "Full-Time" |
                                                        demo_df$Description=="Part-Time"), ],
                             "2015-2016" = demo_df[demo_df$`Fall Term` == 2015 &
                                                     (demo_df$Description == "Full-Time" |
                                                        demo_df$Description=="Part-Time"), ])
    
    
    selected_type <- switch(input$whichtype,
                            "Grand Total, All Reporting Institutions" = 
                              selected_year[selected_year$Institution == "Grand Total, All Reporting Institutions", ],
                            "Total Private, Non-Profit, Four-Year Institutions" = 
                              selected_year[selected_year$Institution == "Total Private, Nonprofit, Four-Year Institutions", ],
                            "Total Public Four-Year Institutions" = 
                              selected_year[selected_year$Institution == "Total Public Four-Year Institutions", ],
                            "Total Public Two-Year Institutions" = 
                              selected_year[selected_year$Institution == "Total Public Two-Year Institutions", ]
                            )
    
    
    ggplot(selected_type, aes(x = Description, y = `In-State Percentage`, fill = Description)) +
      geom_bar(stat="identity", position='dodge') +
      labs(title ="Enrollment Status") +
      ylab("Percentage of In-State Undergraduates") +
      theme(legend.position="none", axis.title.y=element_blank(), panel.background = element_blank()) +
      scale_fill_manual(values=c("#56B4E9", "#009E73")) +
      coord_flip()

  })
  
  
}


# App
shinyApp(ui = ui, server = server)