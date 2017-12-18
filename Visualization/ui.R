library(shiny)
library(leaflet)
library(leaflet.extras)
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
  # Removed since will be embedded in GentrifAI webpage with a logo in nav bar already
  #titlePanel(tags$img(src = "PRIMARY_logo_transparent_background.png",  height = 50, width = 120)),
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(id="sidebar",width = 6,
    ##sliderInput("pickyear", "Select Gentrification Year:", min = 2005, max = 2025, 
    ##              value = 2017, sep = "", width = "50%"),
    textOutput("yearvaluetext"),
    tags$head(tags$style("#yearvaluetext{color: #396a93;
                         font-size: 25px;font-weight: bold;}")),
      br(), 
      br(),
      leafletOutput("mymap", height=500)
    ),
    
    
    mainPanel(width = 6,
      ##textOutput("yearvaluetext"),
      ##tags$head(tags$style("#yearvaluetext{color: purple;
      ##                                     font-size: 25px;font-weight: bold;}")),
      ##br(),
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Table", 
                           br(),
                           sliderInput("pickyear", "1. Select Gentrification Year:", min = 2005, max = 2025, 
                                         value = 2017, sep = ""),
                           br(),
                           tags$h5("2. Select a row in the table for zip code level analysis in GentriFacts and GentriStats"),
                           tags$head(
                             tags$style(HTML("
                                             h5 {
                                             font-size: 15px;
                                             font-weight: bold;

                                             }
                                             
                                             "))
                             ),
                           #font-style: italic;
                           #color: #396a93;
                           textOutput("selectzip3"),
                           tags$head(tags$style("#selectzip3{color: #396a93;
                                                font-size: 15px;
                                                font-weight: bold;
                                                }"
                                                )
                           ),
                           textOutput("text_gent"),
                           tags$head(tags$style("#text_gent{color: red;
                                                font-size: 15px;
                                                font-weight: bold;
                                                }")),
                           br(),
                           dataTableOutput("mytable")
                           ),
                  
                  tabPanel("GentriStats", 
                           br(),
                           
                           tabsetPanel(type = "tabs",
                                       
                                       tabPanel("House Value",
                                                
                                                textOutput("selectzipa"),
                                                tags$head(tags$style("#selectzipa{color: #396a93;
                                                                     font-size: 20px;
                                                                     font-weight: bold;
                                                                     }")),
                                                
                                                #tags$h6("House Value and Income over Time from 2005 for Selected Zip Code"),
                                                #tags$head(
                                                #  tags$style(HTML("
                                                #                  h6 {
                                                #                  font-size: 15px;
                                                #                  font-style: bold;
                                                #                  color: #396a93;
                                                #                  }
                                                #                  
                                                #                  "))
                                                #  ),
                                                br(),
                                                
                                                fluidRow(
                                                  column(6,plotOutput("houseplot", height=250)),
                                                  column(6,plotOutput("houseincreaseplot", height=250))
                                                  ),
                                                fluidRow(
                                                  column(8, 
                                                         textOutput("text1"),
                                                         tags$head(tags$style("#text1{color: #396a93;
                                                                              font-size: 20px;
                                                                              font-weight: bold;
                                                                              }")),
                                                         plotOutput("probplot", height=250)),
                                                  #column(1,br(),br(),br(),br(),tags$img(src = "arrow.png", height=50, width = 20)),
                                                  column(4, 
                                                         br(), br(),
                                                         textOutput("gentprob"),
                                                         tags$head(tags$style("#gentprob{color: #396a93;
                                                                              font-size: 30px;
                                                                              font-weight: bold;
                                                                              }")),
                                                         textOutput("gentprob_text"),
                                                         tags$head(tags$style("#gentprob_text{color: #396a93;
                                                                              font-size: 15px;
                                                                              font-weight: normal;
                                                                                text-align:left;
                                                                              }"))
                                                         
                                                         #tags$h5("Probability that zip code will gentrify over 2005 to 2015"),
                                                         #tags$head(
                                                          # tags$style(HTML("h5 {
                                                          #                   text-align:left;
                                                          #                   font-size: 15px;
                                                          #                   font-style: normal;
                                                          #                   color: #396a93;} ")))
                                                         ))
                                                ),
                                       
                                       tabPanel("Crimes",
                                                
                                                textOutput("selectzipb"),
                                                tags$head(tags$style("#selectzipb{color: #396a93;
                                                                     font-size: 20px;
                                                                     font-weight: bold;
                                                                     }")),
                                                
                                                tags$h6("Historical number of violent crimes in zip code area"),
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
                                                                     }")),
                                                tags$h6("Historical heatmap of building permits in zip code area"),
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
                                                  column(6,tags$h6("In 2006")),
                                                  column(6,tags$h6("In 2016"))
                                                ),
                                                fluidRow(
                                                  column(6,leafletOutput("permitplot2006")),
                                                  column(6,leafletOutput("permitplot2016"))
                                                ),
                                                
                                                fluidRow(
                                                  column(6,
                                                         textOutput("permit2006count"),
                                                         tags$head(tags$style("#permit2006count{color: #396a93;
                                                                              font-size: 11px;
                                                                              font-weight: bold;
                                                                              }"))
                                                         ),
                                                  column(6,
                                                         textOutput("permit2016count"),
                                                         tags$head(tags$style("#permit2016count{color: #396a93;
                                                                              font-size: 11px;
                                                                              font-weight: bold;
                                                                              }"))
                                                         )
                                                  )
                                                )
                           )
                           ),

                  tabPanel("GentriFacts", 
                           textOutput("selectzip2"),
                           tags$head(tags$style("#selectzip2{color: #396a93;
                                                        font-size: 20px;
                                                        font-weight: bold;
                                                        }")),
                           textOutput("selectname"),
                           tags$head(tags$style("#selectname{color: #396a93;
                                                font-size: 20px;
                                                font-weight: bold;
                                                }")),
                           textOutput("selectboro"),
                           tags$head(tags$style("#selectboro{color: #396a93;
                                                font-size: 15px;
                                                font-weight: normal;
                                                }")),
                           
                           br(),
                           textOutput("description"),
                           textOutput("selectlink"),
                           br(),
                           tags$h6("Additional Features"),
                           tags$head(tags$style(HTML("
                                                      h6{
                                                      color: #396a93;
                                                      font-size: 15px;
                                                      font-weight: normal;
                                                      }"))),
                           textOutput("featwater"),
                           textOutput("featlines"),
                           textOutput("featparks"),
                           textOutput("featplays")
                           )
                  )
    )
  )
)