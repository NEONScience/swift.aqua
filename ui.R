# Aquatics UI
require(fst)
require(shiny)
require(dplyr)
require(plotly)
require(ggplot2)
require(DT)
require(tidyr)
require(data.table)
require(shinycssloaders)
require(shinydashboard)
require(viridis)
require(stringr)
require(data.table)
library(tidytable)
# Define UI for application that draws a histogram
shiny::shinyUI(
  shinydashboard::dashboardPage(skin = "black",
    # Header
    shinydashboard::dashboardHeader(title = 'Swift',
                                    titleWidth = 170
                                    ),
    
    # Menu bar
    shinydashboard::dashboardSidebar(
      width = 150,
      shinydashboard::sidebarMenu( id = "menu",
      shinydashboard::menuItem("Aquatics",tabName = "AquaTab", icon = shiny::icon("fish", lib = "font-awesome"))
      )
    ),
    
    # Body
    shinydashboard::dashboardBody(
      tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "AquaTab",
          shinydashboard::box(width = 12,
            shiny::column(width=12,
                          shiny::h2("Aquatics LC Data"),
                          shiny::p("Data is collected nightly by Mike Purlsey"),
                          shiny::p("All data are 1 minute nightly averages."),
              shiny::selectizeInput(inputId = 'swiftAISSiteID',label = 'Select SiteID',
                                    multiple=TRUE,c("HOPB",'LEWI','POSE','FLNT','BARC','SUGG','CUPE','GUIL','LIRO',
                                    'CRAM','MCDI','KING','LECO','WALK','BLWA','TOMB','MAYF','PRPO',
                                    'PRLA','ARIK','PRIN','BLUE','WLOU','COMO','BLDE','SYCA','REDB','MCRA',
                                    'MART','BIGC','TECR','OKSR','TOOK','CARI'),	                                                                                                                                                       
                                    selected = base::sample(c("HOPB",'LEWI','POSE','FLNT','BARC','SUGG','CUPE','GUIL',
                                    'LIRO','CRAM','MCDI','KING','LECO','WALK','BLWA','TOMB',
                                    'MAYF','PRPO','PRLA','ARIK','PRIN','BLUE','WLOU','COMO','BLDE',
                                    'SYCA','REDB','MCRA','MART','BIGC','TECR','OKSR','TOOK',
                                    'CARI'), 
                                    size = 2),
                                    options = list(maxItems = 3)
                                    ), 
              # Filter by Date	                                                                     
              shiny::dateRangeInput(inputId = 'swiftAISDateRange', label="Select Date Range", 	                                                  
                                    start = Sys.Date()-30,	                              
                                    end = Sys.Date() + 1, 
                                    min = Sys.Date()-150, 
                                    max = Sys.Date() +1
                                    )	
            ),	
            shiny::column(width = 12,
              shinydashboard::tabBox(width = 12,
                shiny::tabPanel("Water Health Indicators",width=12,	
                  shiny::br(), 
                  shinydashboard::tabBox(width = 12,
                    shiny::tabPanel('S1/S2: Conductivity', width = 11,	
                      shiny::fluidRow(
                        plotly::plotlyOutput('swiftAISCondPlot')# %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                      ), # End fluidRow	
                      shiny::tags$i(' ')	
                    ),	
                    shiny::tabPanel('S1/S2: Dissolved Oxygen', width = 11,	
                      shiny::fluidRow(
                        plotly::plotlyOutput("swiftAISDOPlot")# %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                      ), # End fluidRow	
                      shiny::tags$i(' ')	
                      ),	
                    shiny::tabPanel('S1/S2: pH', width = 11,	
                      shiny::fluidRow(	
                        plotly::plotlyOutput('swiftAISpHPlot') #%>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                      ), # End fluidRow	
                      shiny::tags$i(' ')	
                    ),	
                    shiny::tabPanel('S1/S2: Algae', width = 11,	
                      shiny::fluidRow(	
                        plotly::plotlyOutput("swiftAISAlgaePlot")# %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                      ), # End fluidRow	
                      shiny::tags$i(' ')	
                    ),	
                    shiny::tabPanel('S1/S2: Turbidity', width = 11,	
                      shiny::fluidRow(	
                        plotly::plotlyOutput('swiftAISTurbPlot') #%>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                      ), # End fluidRow	
                      shiny::tags$i(' ')	
                    ),	
                    shiny::tabPanel('S1/S2: Sonde Temp & PRT', width = 11,	
                      shiny::fluidRow(	
                        plotly::plotlyOutput("swiftAISTempPlot") #%>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                      ), # End fluidRow	
                      shiny::tags$i(' ')	
                    ), # End Sonde Temp Panel	
                    shiny::tabPanel('S1/S2: PAR', width = 11,	
                      shiny::fluidRow(	
                        plotly::plotlyOutput("swiftAISTrollPARPlot")# %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                        ), # End fluidRow	
                      shiny::tags$i(' ')	
                    ),	
                    shiny::tabPanel('S2: FDOM', width = 11,	
                      shiny::fluidRow(	
                        plotly::plotlyOutput("swiftAISFDOMPlot") #%>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                      ), # End fluidRow	
                      shiny::tags$i(' ')	
                    ),	
                    shiny::tabPanel('S2: SUNA Nitrate', width = 11,	
                      shiny::fluidRow(	
                        plotly::plotlyOutput('swiftAISSUNAPlot') #%>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                      ), # End fluidRow	
                      shiny::tags$i(' ')	
                    )	
                  )
                ),# End Water Health Indicators Tab Panel	
                shiny::tabPanel("Water Troll",width=12,	
                  shiny::br(),         
                  shinydashboard::tabBox(width = 12,
                    shiny::tabPanel('Troll Level', width = 11,	
                      shiny::fluidRow(	
                        plotly::plotlyOutput("swiftAISLevelPlot") #%>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                      ), # End fluidRow	
                      shiny::tags$i(' ')	
                    ),	
                    shiny::tabPanel('Troll Temperature', width = 11,	
                      shiny::fluidRow(	
                        plotly::plotlyOutput("swiftAISTrollTempPlot")# %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                      ), # End fluidRow	
                      shiny::tags$i(' ')	
                    )	
                  )
                ),	
                shiny::tabPanel("Wells",width=12,	
                  shiny::br(),      
                  shinydashboard::tabBox(width = 12,
                    shiny::tabPanel('Well Temperature', width = 11,	
                      shiny::fluidRow(	
                        plotly::plotlyOutput("swiftAISWellTempPlot") #%>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                      ), # End fluidRow	
                      shiny::tags$i(' ')	
                    ),	
                    shiny::tabPanel('Well Conductivity', width = 11,	
                      shiny::fluidRow(	
                        plotly::plotlyOutput("swiftAISWellCondPlot") #%>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                      ), # End fluidRow	
                    shiny::tags$i(' ')	
                    ),	
                    shiny::tabPanel('Well Pressure', width = 11,	
                      shiny::fluidRow(	
                        plotly::plotlyOutput("swiftAISWellPressurePlot")# %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                      ), # End fluidRow	
                      shiny::tags$i(' ')	
                    ) # End tabPanel	
                  ) # End tabBox
                ), # End tabPanel	
                shiny::tabPanel("Meteorological Station",width=12,	
                  shiny::br(),      
                    shinydashboard::tabBox(width = 12,
                      shiny::tabPanel('ATS Fan Speed', width = 11,	
                        shiny::fluidRow(	
                          plotly::plotlyOutput("swiftAISMetSpeedPlot")# %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                        ), # End fluidRow	
                        shiny::tags$i(' ')	
                      ),	
                      shiny::tabPanel('Temperature Sensors', width = 11,	
                        shiny::fluidRow(	
                          plotly::plotlyOutput("swiftAISMetTempPlot") # %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                        ), # End fluidRow	
                        shiny::tags$i(' ')	
                      ),	
                      shiny::tabPanel('Relative Humidity', width = 11,	
                        shiny::fluidRow(	
                          plotly::plotlyOutput("swiftMetRHPlot") # %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                        ), # End fluidRow	
                        shiny::tags$i(' ')	
                      ),	
                      shiny::tabPanel('Wind Variables', width = 11,	
                        shiny::fluidRow(	
                          plotly::plotlyOutput("swiftMetWindPlot") # %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                        ), # End fluidRow	
                        shiny::tags$i(' ')	
                      ),	
                      shiny::tabPanel('Speed of Sound', width = 11,	
                        shiny::fluidRow(	
                          plotly::plotlyOutput("swiftMetWindSSPlot") # %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                        ), # End fluidRow	
                        shiny::tags$i(' ')	
                      ),	
                      shiny::tabPanel('2D Wind Health', width = 11,	
                        shiny::fluidRow(	
                          plotly::plotlyOutput("swiftMetWindSHPlot") # %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                        ), # End fluidRow	
                        shiny::tags$i(' ')	
                      ),	
                      shiny::tabPanel('NR01', width = 11,	
                        shiny::fluidRow(	
                          plotly::plotlyOutput("swiftMetNR01Plot") # %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                        ), # End fluidRow	
                        shiny::tags$i(' ')	
                      ),	
                      shiny::tabPanel('PAR', width = 11,	
                        shiny::fluidRow(	
                          plotly::plotlyOutput("swiftMetPARPlot") # %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                        ), # End fluidRow	
                        shiny::tags$i(' ')	
                      ),	
                      shiny::tabPanel('Barometer', width = 11,	
                        shiny::fluidRow(	
                          plotly::plotlyOutput("swiftMetBaroPressurePlot") # %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                        ), # End fluidRow	
                        shiny::tags$i(' ')	
                      ),	
                      shiny::tabPanel('Barometer Health Code', width = 11,	
                        shiny::fluidRow(	
                          plotly::plotlyOutput("swiftMetBaroStatusPlot")# %>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                        ), # End fluidRow	
                        shiny::tags$i(' ')	
                      ),	
                      shiny::tabPanel('DFIR - Belfort Levels', width = 11,	
                        shiny::fluidRow(	
                          plotly::plotlyOutput("swiftAISBelPlot") #%>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                        ), # End fluidRow	
                        shiny::tags$i(' ')	
                      ),
                      shiny::tabPanel('Wet Dep Temp Checl', width = 11,	
                        shiny::fluidRow(	
                          plotly::plotlyOutput("swiftWetDepPlot") #%>% shinycssloaders::withSpinner(color="#012D74",type="3",color.background = "white")	
                        ), # End fluidRow	
                        shiny::tags$i(' ')	
                      ) # End tabPanel
                    ) # End tabBox
                  )	# End Met tabPAnel
                ) # End Water Health Indicators tabBox
              ) # End column
            ) # End AIS tabBox
          ) # End AIS tabPanel
        ) # End Tab Items
      )
    )
  )