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


## Crime data
c2006 <- read.csv("crimes_2006.csv")
#c2007 <- read.csv("crimes_2007.csv")
#c2008 <- read.csv("crimes_2008.csv")
#c2009 <- read.csv("crimes_2009.csv")
#c2010 <- read.csv("crimes_2010.csv")
#c2011 <- read.csv("crimes_2011.csv")
#c2012 <- read.csv("crimes_2012.csv")
#c2013 <- read.csv("crimes_2013.csv")
#c2014 <- read.csv("crimes_2014.csv")
c2015 <- read.csv("crimes_2015.csv")
#c2016 <- read.csv("crimes_2016.csv")

c2006$color <- ifelse(c2006$crime_count>=10, "red", "grey")
#c2011$color <- ifelse(c2011$crime_count>=10, "red", "grey")
c2015$color <- ifelse(c2015$crime_count>=10, "red", "grey")
#c2016$color <- ifelse(c2016$crime_count>=10, "red", "grey")

minmax <- read.csv("minmaxlatlon.csv")


# Zip code area description
desc <- read.csv("zipdescription2.csv")
desc['NAME'] <- as.character(desc$NAME)
desc['description'] <- as.character(desc$description)

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
  output$yearvaluetext <- renderText(paste("New York City - Year",input$pickyear, sep = " "))
  
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
      #ggtitle(paste("Annual Mean Income for Zip Code", zz, 
      #              sep = " "))
      ggtitle("Annual Mean Income")
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
      #ggtitle(paste("Annual Mean House Index for Zip Code", zz, 
      #              sep = " "))
      ggtitle("Annual Mean House Index")
  })
  
  output$selectzip <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    paste("  Zip Code:",  zz  , sep = " ")
  })
  
  
  output$selectzipa <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    paste("  Zip Code:",  zz  , sep = " ")
  })
  
  output$selectzipb <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    paste("  Zip Code:",  zz  , sep = " ")
  })
  
  output$selectzipc <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    paste("  Zip Code:",  zz  , sep = " ")
  })
  
  output$selectzip2 <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    paste("  Zip Code:",  zz  , sep = " ")
  })
  
  output$selectzip3 <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    paste(" Zip code selected:",  zz  , sep = " ")
  })
  
  
  output$crimeplot2006 <- renderPlot({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    
    c2006zc <- c2006[c2006$Zip.Code == zz,]
    total_crime <- sum(c2006zc$crime_count)
    ymin = minmax[minmax$Zip.Code == zz, "minlat"]
    ymax = minmax[minmax$Zip.Code == zz, "maxlat"]
    xmin = minmax[minmax$Zip.Code == zz, "minlon"]
    xmax = minmax[minmax$Zip.Code == zz, "maxlon"]
    
    ggplot(c2006zc, aes(LON_TRIM, LAT_TRIM)) +
      geom_point(size = c2006zc$crime_count/10, 
                 color=c2006zc$color,
                 alpha=0.8) +
      coord_cartesian(xlim=c(xmin, xmax),ylim=c(ymin, ymax)) +
      ggtitle(paste("2006 - Number of Violent Crimes: ", total_crime,sep = " ")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Longitude") +
      ylab("Latitude")
    
  })
  
  output$crimeplot2015 <- renderPlot({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    
    c2015zc <- c2015[c2015$Zip.Code == zz,]
    total_crime <- sum(c2015zc$crime_count)
    ymin = minmax[minmax$Zip.Code == zz, "minlat"]
    ymax = minmax[minmax$Zip.Code == zz, "maxlat"]
    xmin = minmax[minmax$Zip.Code == zz, "minlon"]
    xmax = minmax[minmax$Zip.Code == zz, "maxlon"]
    
    ggplot(c2015zc, aes(LON_TRIM, LAT_TRIM)) +
      geom_point(size = c2015zc$crime_count/10, 
                 color=c2015zc$color,
                 alpha=0.8) +
      #geom_point(aes(color=factor(c2016zc$color), size = c2016zc$crime_count/10)) +
      #theme_minimal() +
      #scale_color_manual(name ="Number of Crimes",values = c("grey", "red"), 
      #                   labels=c("Less than 10","Greater or equal to 10"),
      #                   guide = guide_legend()) +
      #theme(legend.position = "bottom")  +
      #scale_size_continuous(guide=FALSE) +
      coord_cartesian(xlim=c(xmin, xmax),ylim=c(ymin, ymax)) +
      ggtitle(paste("2015 - Number of Violent Crimes: ", total_crime,sep = " ")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Longitude") +
      ylab("Latitude")
    
  })
  
  
  output$description <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    
    zztext <- desc[desc$ZIP == zz, 'description']
    })
  
  
  output$selectname <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    desc['NAME'] <- as.character(desc$NAME)
    zzname <- desc[desc$ZIP == zz, 'NAME']
    
  })
  
  output$selectlink <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    zzlink <- desc[desc$ZIP == zz, 'fulllink']
    paste("link:", zzlink, sep=" ")
  })
  
  output$selectboro <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    
    zzboro <- desc[desc$ZIP == zz, 'BORO']
    zzneigh <- desc[desc$ZIP == zz, 'NEIGHBORHOOD']
    paste(zzneigh,",", zzboro, sep = " ")
  })
  
})  

#shinyApp(ui, server=shinyServer)

