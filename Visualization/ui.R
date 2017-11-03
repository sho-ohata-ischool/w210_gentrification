library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)


header <- dashboardHeader(
  title = "gentrifAI")

sidebar <- dashboardSidebar()


body <- dashboardBody(
  fixedRow(
    # column(width = 3,
    #        box(width = NULL, solidHeader = TRUE, status = "primary",
    #            sliderInput("Year", "Gentrification Prediction by Year:", min = as.Date("2000","%Y"), max = as.Date("2030","%Y"), 
    #                        value = as.Date("2020", "%Y"), timeFormat = "%Y"),
  
    br(), br(),
    column(width = 6,
           box(width = NULL, solidHeader = TRUE, status = "primary",
               sliderInput("Year", "Gentrification Prediction by Year:", min = as.Date("2000","%Y"), max = as.Date("2030","%Y"), 
                           value = as.Date("2020", "%Y"), timeFormat = "%Y"), br(),
               leafletOutput("mymap")
               )),
    
  column(width = 5, offset = "1",
           box(width = NULL, solidHeader = TRUE,
               dataTableOutput("mytable"))
  )
  ))


ui <- dashboardPage(
  header,
  sidebar,
  body
)


# shinyUI(fluidPage(
#   shinyUI(navbarPage("gentrifAI",
#                      tabPanel("Map View",
#                        sliderInput("Year", "Gentrification Prediction by Year:", min = as.Date("2000","%Y"), max = as.Date("2030","%Y"), 
#                                   value = as.Date("2020", "%Y"), timeFormat = "%Y"),
#                        mainPanel(
#                          leafletOutput("mymap",width = "100%", height = "100%")
#                        )), 
#                      tabPanel("ZIP Code Data"),
#                      tabPanel("About the gentrifAI team"),
#   position = "static-top",
#   #sliderInput("Year", "Gentrification Prediction by Year:", min = as.Date("2000","%Y"), max = as.Date("2030","%Y"), 
#   #            value = as.Date("2020", "%Y"), timeFormat = "%Y"),                     
#   #Likely won't use the pull down menu option here, but leaving the code in and commented out
#   #for now, in case we change our minds
#   #selectInput("Forecast", "Forecast Year",
#   #            list("Years" = c("2020", "2025", "2030"))),
#   leafletOutput("mymap"))
#           
#   ))
# )
