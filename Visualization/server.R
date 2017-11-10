library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)
library(sp)

gdata <- read.csv("Income_Home_Prices_ZIP.csv")

library(ggplot2)

# Main data frame
gdata <- read.csv("Income_Home_Prices_ZIP_v2.csv")
 
# Longitude and Latitude vectors
gdata$long <- as.numeric(gdata$Longitude)
gdata$lat <- as.numeric(gdata$Latitude)

# Data frame sorted by gentrification probability (largest to lowest)
gdata_prob <- gdata[rev(order(gdata$Probability)),]

# Smaller data frame to only keep columns for table in UI
mydatatable <- gdata_prob[, c("ZIP", "Borough", "Year", "Probability", "AGI", "Price_Index")]

# For line charts
gdataplot <- gdata[gdata$Year >= 2005, c("ZIP", "Year", "AGI_num", "Price_Index",
                                         "IncomeLow", "IncomeHigh", "HouseLow", "HouseHigh")]

# For Spatial Data
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
  
  filtered_data <- reactive({
    data <- mydatatable[mydatatable$Year == input$pickyear,]
    n <- length(data$Year)
    rownames(data) <- 1:n
    data
  })
  
  output$yearvalue <- renderText(as.numeric(input$pickyear))
  
  output$mymap <- renderLeaflet({
      leaflet() %>% 
      #addTiles() %>%
        addProviderTiles("Esri.WorldTopoMap") %>%
        setView(lng=-73.935, lat=40.690, zoom=10) %>% 
      # addAwesomeMarkers(data = gdata, clusterOptions = markerClusterOptions(), lng = ~Longitude, lat = ~Latitude, icon = icons,
      #           popup = ~paste("<b>ZIP Code:</b>", ZIP, "<br>", "<b>Neighborhood:</b>", Neighborhood,"<br>",
      #                         "<b>Year</b>", Year,"<br>","<b>Average Income</b>", AGI, sep = " "))
      addAwesomeMarkers(data = gdata[gdata$Year == input$pickyear,], clusterOptions = markerClusterOptions(), lng = ~Longitude, lat = ~Latitude, icon = icons,
                        popup = ~paste("<b>ZIP Code:</b>", ZIP, "<br>", "<b>Neighborhood:</b>", Neighborhood,"<br>",
                                       "<b>Year</b>", Year,"<br>","<b>Average Income</b>", AGI, sep = " "))   
      
    })

  output$mytable <- renderDataTable(filtered_data())
  
  # Dynamic UI - based on input year, new input selections of top 5 zip codes through radio buttons
  output$top5zips <- renderUI({
    top5 <- head(filtered_data())$ZIP
    radioButtons("dynamic", 
                 paste("Pick one of the top 5 potential gentrification zip codes in year", 
                                  input$pickyear, sep =" "),
                 choices = top5,
                 selected = top5[1],
                 inline = TRUE)
  })
  
  zip_data <- reactive({
    data <- gdataplot[gdataplot$ZIP == input$dynamic, 
                      c("Year", "AGI_num", "Price_Index",
                        "IncomeLow", "IncomeHigh", "HouseLow", "HouseHigh")]
    data
  })
  
  # Plot for Income
  output$incomeplot <- renderPlot({
    ggplot(data=zip_data(), aes(x=Year)) +
             geom_line(aes(y = AGI_num), colour = "blue", linetype = "solid", size = 2) + 
             geom_line(aes(y = IncomeLow), colour = "yellowgreen", linetype = "dashed", size = 1) + 
             geom_line(aes(y = IncomeHigh), colour = "orchid2", linetype = "dashed", size = 1) + 
      xlab("Year") + ylab("Income in 2015 $") + # Set axis labels
      ggtitle(paste("Annual Mean Income for Zip Code", input$dynamic, 
                    sep = " "))
  })
  
  # Plot for House Index
  output$houseplot <- renderPlot({
    ggplot(data=zip_data(), aes(x=Year)) +
      geom_line(aes(y = Price_Index), colour = "blue", linetype = "solid", size = 2) + 
      geom_line(aes(y = HouseLow), colour = "yellowgreen", linetype = "dashed", size = 1) + 
      geom_line(aes(y = HouseHigh), colour = "orchid2", linetype = "dashed", size = 1) + 
      xlab("Year") + ylab("House Index - inflation adjusted") + # Set axis labels
      ggtitle(paste("Annual Mean House Index for Zip Code", input$dynamic, 
                    sep = " "))
  })
  
})  

#shinyApp(ui, server=shinyServer)

