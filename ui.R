navbarPage('Suicide Rate', theme = shinytheme("flatly"),
           
           #===================== Intro tab =====================
           tabPanel('Introduction',
                    div(
                      class='coverDiv',
                      tags$head(includeCSS('styles.css')), # custom styles
                      # customize introduction panel size
                      absolutePanel(fixed = TRUE, draggable = FALSE, 
                                    top = 600, left = 750, right = 750, bottom = 'auto',
                                    width = 1200, height = 1700, 
                                    
                                    div( class = 'coverTextDiv',
                                         
                                         br(),
                                         h3("Overview"),
                                         p("This application provides visualisations on suicide data among OECD countries, ",
                                           "it contains trend and factors ", "."),
                                        
                                         br(),
                                         h3("Panels"),
                                         p("The project has the following panels:"),
                                         tags$ul(
                                           tags$li(strong("Trend"), "- shows how the suicide rate change for different period and countries"),
                                           tags$li(strong("Size"), "- shows the total suicide rate for different year and countries"),
                                           tags$li(strong("Age and gender"), "- shows the proportion of suicide rate on different age group and gender"),
                                           tags$li(strong("Model"), "- shows details about a participating country, including medals won and in which sports")
                                         )
                                    )
                                    
                      )
                      
                      
                    )
           ),
           
           #===================== Trend tab =====================
           tabPanel('Trend', inputId = 'trendTab',
                    
                    sidebarLayout(
                      sidebarPanel(
                        
                        
                        selectInput(inputId = 'plotCountries',
                                    label = h4('Country'),
                                    choices = all_country,
                                    selected = 'OECD'),
                        
                        sliderInput(inputId = 'plotSliderYear', label = h4('Year range'),
                                    min = 1995, max = 2015, value = c(1995, 2015), step = 1), 
                      
                      checkboxGroupInput(inputId = 'ageGroup', label = h4('Choose Age Group'), 
                                         choices = age_group, selected = "5-14 years"),
                      
                      checkboxGroupInput(inputId = 'gender', label = h4('Choose Gender'), 
                                         choices = gender, selected = "female")
                      
                    ),
                    
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Year trend",highchartOutput("trendPlot", width = '70%', height = '130%') ),
                        tabPanel("Suicide Rate by Age", highchartOutput("lineTrendAge", width = '70%', height = '130%')),
                        tabPanel("Suicide Rate by Gender", highchartOutput("genderTrendLine", width = '70%', height = '130%'))
                      )
                    )
           ),),
           
         #################################### size tab ####################################
         tabPanel("Size", inputId = 'sizeTab',
                  fluidRow(
                    column(2,
                           sliderInput(inputId = 'sizePlotSliderYear', label = h4('Year range'),
                                       min = 1995, max = 2015, value = c(1995, 2015), step = 1, 
                                       animate = animationOptions(interval = 1000, loop = T))),
                    column(4,
                           highchartOutput("sizeBar", width = '100%', height = '100%')),
                    column(6,
                           highchartOutput("mapView", width = '100%', height = '100%'))
                  )
         ),
         
         #################################### age and gender tab ####################################
         tabPanel("Gender and Age", inputId = 'genderTab',
                  fluidPage(
                    fluidRow(
                      column(2,
                             wellPanel(
                               selectInput(inputId = 'plotGACountries',
                                           label = h4('Country'),
                                           choices = all_country,
                                           selected = 'OECD')
                             )
                             ),
                      
                      column(4,
                              (sankeyNetworkOutput("sankeyGender", width = 700, height=800)
                             
                      ),
                    ),
                    column(8,
                           highchartOutput("pieAge", width =700, height=800)
                    )
                  )
)
))