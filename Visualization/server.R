library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)
library(sp)

gdata <- read.csv("Income_Home_Prices_ZIP.csv")

gdata$long <- as.numeric(gdata$Longitude)

gdata$lat <- as.numeric(gdata$Latitude)

gdatatable <- datatable(gdata[as.numeric(c(1, 2, 29, 8, 27)), drop = FALSE])

#For now, I have this feeding the table and it's fixed at "2010", but I want it to pull in the reactive
#year from my slider.
mydatatable <- gdata[gdata$Year == "2010", c("ZIP", "Borough", "Year", "Probability", "AGI", "Price_Index")]

#For now, I have this feeding the map and it's also fixed at "2010", but I want it to pull in the reactive
#year from my slider.
gdata2010 <- gdata[gdata$Year == "2010", ]

data.SP <- SpatialPointsDataFrame(gdata[,c(23, 24)], gdata[,-c(23,24)])

getColor <- function(gdata) {
  sapply(gdata$Color, function(Color) {
    if(Color == "green") {
      "green"
    } else if(Color == "blue") {
      "darkblue"
    } else if(Color == "purple") {
      "purple"      
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'home',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(gdata)
)

shinyServer(function(input, output, session) {
  
  yeartest <- reactive(as.numeric(input$pickyear))  
  #output$yearvalue <- renderText(as.numeric(input$pickyear))
  output$yearvalue <- renderText(as.numeric(input$pickyear))
  
  output$mymap <- renderLeaflet({
      leaflet() %>% 
      #addTiles() %>%
        addProviderTiles("Esri.WorldTopoMap") %>%
        setView(lng=-73.935, lat=40.690, zoom=10) %>% 
      # addAwesomeMarkers(data = gdata, clusterOptions = markerClusterOptions(), lng = ~Longitude, lat = ~Latitude, icon = icons,
      #           popup = ~paste("<b>ZIP Code:</b>", ZIP, "<br>", "<b>Neighborhood:</b>", Neighborhood,"<br>",
      #                         "<b>Year</b>", Year,"<br>","<b>Average Income</b>", AGI, sep = " "))
      addAwesomeMarkers(data = gdata2010, clusterOptions = markerClusterOptions(), lng = ~Longitude, lat = ~Latitude, icon = icons,
                        popup = ~paste("<b>ZIP Code:</b>", ZIP, "<br>", "<b>Neighborhood:</b>", Neighborhood,"<br>",
                                       "<b>Year</b>", Year,"<br>","<b>Average Income</b>", AGI, sep = " "))   
  })

  
  output$mytable <- renderDataTable(mydatatable)
  
})  

shinyApp(ui, server)

