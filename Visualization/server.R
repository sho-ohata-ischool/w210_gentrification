library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)

gdata <- read.csv("Income_Home_Prices_ZIP.csv")

gdata$long <- as.numeric(gdata$Longitude)
gdata$lat <- as.numeric(gdata$Latitude)

gdatatable <- datatable(gdata[as.numeric(c(1, 2, 4, 8, 27)), drop = FALSE])

data.SP <- SpatialPointsDataFrame(gdata[,c(23, 24)], gdata[,-c(23,24)])


shinyServer(function(input, output) {
  output$mymap <- renderLeaflet({
    # define the leaflet map object
      sliderInput("Year", "Gentrification Prediction by Year:", min = as.Date("2000","%Y"), max = as.Date("2030","%Y"), 
                value = as.Date("2020", "%Y"), timeFormat = "%Y")                     
      leaflet() %>% 
      #addTiles() %>%
        addProviderTiles("Esri.WorldTopoMap") %>%
      setView(lng=-73.935, lat=40.690, zoom=10) %>% 
      #addMarkers(data = gdata, lng = ~Longitude, lat = ~Latitude, popup = ~Neighborhood)
      #addMarkers(data = gdata, lng = ~Longitude, lat = ~Latitude, popup = ~paste(ZIP, Neighborhood, AGI, sep = ", "))
      addMarkers(data = gdata, clusterOptions = markerClusterOptions(), lng = ~Longitude, lat = ~Latitude,
                popup = ~paste("<b>ZIP Code:</b>", ZIP, "<br>", "<b>Neighborhood:</b>", Neighborhood,"<br>",
                              "<b>Year</b>", Year,"<br>","<b>Average Income</b>", AGI, sep = " "))
   
  })

  output$mytable <- renderDataTable(gdatatable, scrollX = TRUE, target = column, cols = c(1,3))
  })

