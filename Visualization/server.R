library(shiny)
library(leaflet)
library(shinydashboard)


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
