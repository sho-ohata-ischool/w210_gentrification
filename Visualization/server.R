library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)
library(sp)
library(rgdal)
library(ggplot2)
library(geojsonio)


################
# Data frames  #
################

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


# Zip code boundaries polygons - GEOJSON file
nyzipcode <- geojsonio::geojson_read("nyc_zipcode_polygons.geojson",
                                     what = "sp")


# Color list and functions
cgent = "#000080"
clow = "#FFE5CC"
cmid = "#FFB266"
chigh = "#FF8000"

getColor <- function(Color) {
  
  if(Color == "blue") { #GENTRIFYING
    cgent
  } else if(Color == "green") {  #Not gentrifying - low income
    clow
  } else if(Color == "red") { #Not gentrifying - mid income
    cmid   
  } else if(Color == "purple") { #Not gentrifying - high income
    chigh
  } else {  
    "gray"
  } 
  
}

# Simpler color for the icons since the "awesomeIcons" function only recognize simple colors
getColor2 <- function(Color) {
  
  if(Color == "blue") { #GENTRIFYING
    "blue"            
  } else if(Color == "green") {  #Not gentrifying - low income
    "orange"           
  } else if(Color == "red") { #Not gentrifying - mid income
    "orange"                 
  } else if(Color == "purple") { #Not gentrifying - high income
    "orange"             
  } else {  
    "gray"
  } 
  
}



#icons <- awesomeIcons(
#  icon = 'home',
#  iconColor = 'black',
#  library = 'ion',
#  markerColor = getColor(gdata)
#)
#####

################
# SHINY SERVER #
################

shinyServer(function(input, output, session) {
  
  # Filtered data for selected year
  filtered_data <- reactive({
    data <- mydatatable[mydatatable$Year == input$pickyear,]
    n <- length(data$Year)
    rownames(data) <- 1:n
    data
  })
  
  # Zip code area colors
  zipcolor <- reactive({
    datacolor = gdata[gdata$Year == input$pickyear,c("ZIP","Color")]
    colnames(datacolor)[colnames(datacolor) == 'ZIP'] <- 'postalCode'
    test <- merge(nyzipcode, datacolor, by="postalCode", all.x=TRUE)
    zipc <- test$Color
    levels <- levels(zipc)
    levels[length(levels) + 1] <- "gray"
    zipc <- factor(zipc, levels = levels)
    zipc[which(is.na(zipc))] <-"gray"  # For zip codes in GEOJSON but not in our data of 175 zip codes
    zipc <- sapply(zipc, getColor, USE.NAMES = FALSE)
  })
  
  # Icons for markers/popups and colors based on zip code color
  icons <- reactive({
    markerc <- gdata[gdata$Year == input$pickyear,"Color"]
    icons <- awesomeIcons(
      icon = 'home',
      iconColor = 'black',
      library = 'ion',
      #markerColor = markerc
      markerColor = sapply(markerc, getColor2, USE.NAMES = FALSE)
    )
    icons
  })
  
  output$yearvalue <- renderText(as.numeric(input$pickyear))
  
  # NYC zip code boundaries map
  output$mymap <- renderLeaflet({
      leaflet(nyzipcode) %>% 
      addProviderTiles("Esri.WorldTopoMap") %>%
      setView(lng=-73.935, lat=40.690, zoom=10) %>%
      addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.5, weight=1,
                  fillColor=zipcolor(), color="white") %>%
      addAwesomeMarkers(data = gdata[gdata$Year == input$pickyear,], clusterOptions = markerClusterOptions(), lng = ~Longitude, lat = ~Latitude, icon = icons(),
                        popup = ~paste("<b>ZIP Code:</b>", ZIP, "<br>", "<b>Neighborhood:</b>", Neighborhood,"<br>",
                                       "<b>Year</b>", Year,"<br>","<b>Average Income</b>", AGI, sep = " ")) %>%
      addLegend("topleft",   colors =c(cgent, clow, cmid, chigh),
                labels= c("Gentrifying", "Non-gentrifying (low income)", "Non-gentrifying (mid income)",
                          "Non-gentrifying (high income)"),
                title = "Gentrification",
                opacity = 0.7
      )
      
    })

  # Table of zip codes and gentri proba
  output$mytable <- renderDataTable(filtered_data()[, c("ZIP", "Borough", 
                                                        "Probability", "AGI", "Price_Index")], 
                                    selection = 'single')  # Single row selection and drop "Year" column
  
  # Dynamic UI not needed for now since selection of row from table above directly used as input
  # Dynamic UI - based on input year, new input selections of top 5 zip codes through radio buttons
  #output$top5zips <- renderUI({
  #  top5 <- head(filtered_data())$ZIP
  #  radioButtons("dynamic", 
  #               paste("Pick one of the top 5 potential gentrification zip codes in year", 
  #                                input$pickyear, sep =" "),
  #               choices = top5,
  #               selected = top5[1],
  #               inline = TRUE)
  #})
  
  #zip_data <- reactive({
  #  data <- gdataplot[gdataplot$ZIP == input$dynamic, 
  #                    c("Year", "AGI_num", "Price_Index",
  #                      "IncomeLow", "IncomeHigh", "HouseLow", "HouseHigh")]
  #  data
  #})
  
  # Plot for Income
  output$incomeplot <- renderPlot({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    zip_data <- gdataplot[gdataplot$ZIP == zz, 
                          c("Year", "AGI_num", "Price_Index",
                            "IncomeLow", "IncomeHigh", "HouseLow", "HouseHigh")]
    
    ggplot(data=zip_data, aes(x=Year)) +
             geom_line(aes(y = AGI_num), colour = "blue", linetype = "solid", size = 2) + 
             geom_line(aes(y = IncomeLow), colour = "yellowgreen", linetype = "dashed", size = 1) + 
             geom_line(aes(y = IncomeHigh), colour = "orchid2", linetype = "dashed", size = 1) + 
      xlab("Year") + ylab("Income in 2015 $") + # Set axis labels
      ggtitle(paste("Annual Mean Income for Zip Code", zz, 
                    sep = " "))
  })
  
  # Plot for House Index
  output$houseplot <- renderPlot({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    zip_data <- gdataplot[gdataplot$ZIP == zz, 
                          c("Year", "AGI_num", "Price_Index",
                            "IncomeLow", "IncomeHigh", "HouseLow", "HouseHigh")]
    
    ggplot(data=zip_data, aes(x=Year)) +
      geom_line(aes(y = Price_Index), colour = "blue", linetype = "solid", size = 2) + 
      geom_line(aes(y = HouseLow), colour = "yellowgreen", linetype = "dashed", size = 1) + 
      geom_line(aes(y = HouseHigh), colour = "orchid2", linetype = "dashed", size = 1) + 
      xlab("Year") + ylab("House Index - inflation adjusted") + # Set axis labels
      ggtitle(paste("Annual Mean House Index for Zip Code", zz, 
                    sep = " "))
  })
  
})  

#shinyApp(ui, server=shinyServer)

