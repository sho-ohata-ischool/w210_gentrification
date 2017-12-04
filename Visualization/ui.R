library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)
library(sp)
library(ggplot2)


ui <- fluidPage(
  
  tags$head(tags$style(
    HTML('
         #sidebar {
         background-color: white;
         border-width: 0px;
         }
         ')
  )),
  
  # App title ----
  titlePanel(tags$img(src = "PRIMARY_logo_transparent_background.png",  height = 50, width = 120)),
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(id="sidebar",width = 6,
      sliderInput("pickyear", "Select Gentrification Year:", min = 2000, max = 2025, 
                  value = 2010, sep = "", width = "50%"),
      br(), 
      br(),
      leafletOutput("mymap", height=500)
    ),
    
    
    mainPanel(width = 6,
      textOutput("yearvaluetext"),
      tags$head(tags$style("#yearvaluetext{color: purple;
                                           font-size: 25px;font-weight: bold;}")),
      br(),
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Table", 
                           tags$h5("Select a row in the table for zip code level analysis in GentriFacts and GentriStats"),
                           tags$head(
                             tags$style(HTML("
                                             h5 {
                                             font-size: 15px;
                                             font-style: italic;
                                             color: #396a93;
                                             }
                                             
                                             "))
                             ),
                           
                           textOutput("selectzip3"),
                           tags$head(tags$style("#selectzip3{color: #396a93;
                                                font-size: 15px;
                                                font-weight: bold;
                                                }"
                                                )
                           ),
                           br(),
                           dataTableOutput("mytable")),
                  
                  tabPanel("GentriStats", 
                           #textOutput("selectzip"),
                           #tags$head(tags$style("#selectzip{color: #396a93;
                          #                      font-size: 20px;
                          #                      font-weight: bold;
                           #                     }"
                          #                      )
                          # ),
                           br(),
                           
                           tabsetPanel(type = "tabs",
                                       
                                       tabPanel("House Value",
                                                
                                                textOutput("selectzipa"),
                                                tags$head(tags$style("#selectzipa{color: #396a93;
                                                                     font-size: 20px;
                                                                     font-weight: bold;
                                                                     }")),
                                                
                                                tags$h6("House Value and Income over Time from 2005 for Selected Zip Code"),
                                                tags$head(
                                                  tags$style(HTML("
                                                                  h6 {
                                                                  font-size: 15px;
                                                                  font-style: bold;
                                                                  color: #396a93;
                                                                  }
                                                                  
                                                                  "))
                                                  ),
                                                fluidRow(
                                                  column(6, plotOutput("houseplot")),
                                                  column(6, plotOutput("incomeplot"))
                                                  )
                                                ),
                                       tabPanel("Crimes",
                                                
                                                textOutput("selectzipb"),
                                                tags$head(tags$style("#selectzipb{color: #396a93;
                                                                     font-size: 20px;
                                                                     font-weight: bold;
                                                                     }")),
                                                
                                                tags$h6("Number of violent crimes in zip code area"),
                                                tags$head(
                                                  tags$style(HTML("
                                                                  h6 {
                                                                  font-size: 15px;
                                                                  font-style: bold;
                                                                  color: #396a93;
                                                                  }
                                                                  
                                                                  "))
                                                  ),
                                                fluidRow(
                                                  column(6,plotOutput("crimeplot2006")),
                                                  column(6,plotOutput("crimeplot2015"))
                                                ),
                                                fluidRow(
                                                  column(12, align="center",
                                                         tags$img(src = "crimelegend.png",  height = 30, width = 320))
                                                )
                                                ),
                                       
                                       tabPanel("Building Permits",
                                                textOutput("selectzipc"),
                                                tags$head(tags$style("#selectzipc{color: #396a93;
                                                                     font-size: 20px;
                                                                     font-weight: bold;
                                                                     }"))
                                                )
                           )
                           #fluidRow(
                           #  column(6, plotOutput("houseplot")),
                           #  column(6, plotOutput("incomeplot"))
                           #  )
                           ),

                  tabPanel("Gentrifacts", 
                           textOutput("selectzip2"),
                           tags$head(tags$style("#selectzip2{color: #396a93;
                                                        font-size: 20px;
                                                        font-weight: bold;
                                                        }"
                                                )
                                       ),
                           br()
                           )
                  )
    )
  )
)