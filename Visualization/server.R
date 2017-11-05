library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)


gdata <- read.csv("Income_Home_Prices_ZIP.csv")

gdata$long <- as.numeric(gdata$Longitude)

gdata$lat <- as.numeric(gdata$Latitude)

gdatatable <- datatable(gdata[as.numeric(c(1, 2, 29, 8, 27)), drop = FALSE])

# sliderValue <- reactive({
#   Value = as.character(c(input$pickyear))})

mydatatable <- gdata[gdata$Year == "2012", c("ZIP", "Borough", "Probability", "AGI", "Price_Index")]

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
    
  output$mymap <- renderLeaflet({
      # sliderInput("Year", "Gentrification Prediction by Year:", min = as.Date("2000","%Y"), max = as.Date("2030","%Y"), 
      #           value = as.Date("2020", "%Y"), timeFormat = "%Y")                     
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

  #output$mytable <- renderDataTable(gdatatable, scrollX = TRUE, target = column, cols = c(1,3))
  #output$mytable <- renderDataTable(gdatatable, scrollX = TRUE)
  
  output$mytable <- renderDataTable(mydatatable)
  
})  

shinyApp(ui, server)

