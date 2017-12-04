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
         background-color: #dec4de;
         }
         ')
  )),
  
  # App title ----
  titlePanel(tags$img(src = "PRIMARY_logo_transparent_background.png",  height = 50, width = 120)),
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 6,
      sliderInput("pickyear", "Select Gentrification Year:", min = 2000, max = 2025, 
                  value = 2010, sep = "", width = "50%"),
      #textOutput("yearvaluetext"),
      #tags$head(tags$style("#yearvaluetext{color: purple;
      #                     font-size: 30px;
      #                     font-weight: bold;}")),
      br(), 
      br(),
      leafletOutput("mymap")
    ),
    
    
    mainPanel(width = 6,
      #sliderInput("pickyear", "Select Gentrification Year:", min = 2000, max = 2030, 
      #            value = 2010, sep = ""),
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
                           textOutput("selectzip"),
                           tags$head(tags$style("#selectzip{color: #396a93;
                                                font-size: 20px;
                                                font-weight: bold;
                                                }"
                                                )
                           ),
                           br(),
                           fluidRow(
                             column(6, plotOutput("houseplot")),
                             column(6, plotOutput("incomeplot"))
                             )
                           ),
                           #plotOutput("houseplot"), plotOutput("incomeplot")),
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