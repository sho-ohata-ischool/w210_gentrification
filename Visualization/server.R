library(shiny)
library(leaflet)
library(shinydashboard)

gdata <- read.csv("Income_Home_Prices_ZIP.csv")

gdata$long <- as.numeric(gdata$Longitude)
gdata$lat <- as.numeric(gdata$Latitude)

data.SP <- SpatialPointsDataFrame(gdata[,c(23, 24)], gdata[,-c(23,24)])


shinyServer(function(input, output) {
  output$mymap <- renderLeaflet({
    # define the leaflet map object
      leaflet() %>% 
      #addTiles() %>%
        addProviderTiles("OpenMapSurfer.Roads") %>%
      setView(lng=-73.935, lat=40.690, zoom=10) %>% 
      #addMarkers(data = gdata, lng = ~Longitude, lat = ~Latitude, popup = ~Neighborhood)
      #addMarkers(data = gdata, lng = ~Longitude, lat = ~Latitude, popup = ~paste(ZIP, Neighborhood, AGI, sep = ", "))
      addMarkers(data = gdata, clusterOptions = markerClusterOptions(), lng = ~Longitude, lat = ~Latitude,
                popup = ~paste("<b>ZIP Code:</b>", ZIP, "<br>", "<b>Neighborhood:</b>", Neighborhood,"<br>",
                              "<b>Year</b>", Year,"<br>","<b>Average Income</b>", AGI, sep = " "))
  })
  
})
