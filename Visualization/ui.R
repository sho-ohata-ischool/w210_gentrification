library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)
library(sp)
library(ggplot2)

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
    br(), br(),
    column(width = 6,
           box(title = "Gentrification Map of New York City", width = NULL, solidHeader = FALSE, status = "primary",
               sliderInput("pickyear", "Select a Year:", min = 2000, max = 2025, 
                           value = 2010, sep = "", width = '50%'), 
               tags$br(),
               leafletOutput("mymap")
               #, 
               #tags$br(),
               #tags$img(src = "legend.png",  height = 150, width = 240)
               )),


    column(width = 6,
         box(title = "Top Gentrified ZIP Codes", width = NULL, solidHeader = FALSE, status = "primary",
             dataTableOutput("mytable")),
         #uiOutput("top5zips"),
         plotOutput("houseplot"),
         plotOutput("incomeplot")
  )
  ))


ui <- dashboardPage(
  header,
  sidebar,
  body
)

