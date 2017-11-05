library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)


#header <- dashboardHeader(tags$li(class = "dropdown", tags$a(target="_blank", tags$img(height = "20px", alt="SNAP Logo", 
#                                                                                                         src="white_logo.png", height = '300', width = '50'))))

header <- dashboardHeader(title = "")

sidebar <- dashboardSidebar(
  sidebarMenu(width = 1, tags$img(src = "white_logo.png", height = 100, width = 225),
    menuItem("Map View", tabname = "mapview", selected = TRUE, icon = icon("map")),
    menuItem("Charts", tabname = "charts", selected = TRUE, icon = icon("line-chart")),
    menuItem("About gentrifAI", tabname = "aboutus", selected = TRUE, icon = icon("user-circle")),
  textOutput("res")
))


body <- dashboardBody(
  fluidRow(
    # column(width = 3,
    #        box(width = NULL, solidHeader = TRUE, status = "primary",
    #            sliderInput("Year", "Gentrification Prediction by Year:", min = as.Date("2000","%Y"), max = as.Date("2030","%Y"), 
    #                        value = as.Date("2020", "%Y"), timeFormat = "%Y"),
  
    br(), br(),
    column(width = 6,
           box(title = "Gentrification Map of New York City", width = NULL, solidHeader = TRUE, status = "primary",
               sliderInput("pickyear", "Select a Year:", min = as.Date("2000","%Y"), max = as.Date("2030","%Y"), 
                           value = as.Date("2010", "%Y"), timeFormat = "%Y"), tags$br(),
               leafletOutput("mymap")
               )),


    column(width = 6,
         box(title = "Top Gentrified ZIP Codes", width = NULL, solidHeader = TRUE, status = "primary",
             dataTableOutput("mytable"))
  )
  ))


ui <- dashboardPage(
  header,
  sidebar,
  body
)

