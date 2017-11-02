library(shiny)
library(leaflet)


shinyUI(fluidPage(
  shinyUI(navbarPage("gentrifAI",
                     tabPanel("Map View"),
                     tabPanel("ZIP Code Data"),
                     tabPanel("About the gentrifAI team"),
  sliderInput("Year", "Gentrification Prediction by Year:", min = as.Date("2000","%Y"), max = as.Date("2030","%Y"), 
              value = as.Date("2020", "%Y"), timeFormat = "%Y"),                     
  #Likely won't use the pull down menu option here, but leaving the code in and commented out
  #for now, in case we change our minds
  #selectInput("Forecast", "Forecast Year",
  #            list("Years" = c("2020", "2025", "2030"))),
  leafletOutput("mymap"))
          
  ))
)
