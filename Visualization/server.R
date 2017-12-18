library(shiny)
library(leaflet)
library(leaflet.extras)
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
gdata <- read.csv("Income_Home_Prices_ZIP_viz.csv")

# Additional Features for Zip Code description
myfeaturetable <- gdata[gdata$Year == 2015, c("ZIP", "Bordering.Water","Number.of.Subway.Lines.Serving.ZIP", 
                                              "Number.of.Parks", "Number.of.Playgrounds")]
myprobtable <- gdata[gdata$Year >= 2005, c("ZIP", "Year", "alpha", "beta", "HouseHigh", "threshold", "NotGentReason")]

 
# Longitude and Latitude vectors
gdata$long <- as.numeric(gdata$Longitude)
gdata$lat <- as.numeric(gdata$Latitude)
gdatacoord <- gdata[gdata$Year == 2015, c("ZIP", "Longitude", "Latitude")]

# Data frame sorted by gentrification probability (largest to lowest)
gdata_prob <- gdata[rev(order(gdata$Probability)),]

# Smaller data frame to only keep columns for table in UI
mydatatable <- gdata_prob[, c("ZIP", "Borough", "Neighborhood", "Year", "Probability", "AGI", "Price_Index")]
mydatatable$Price_Index <- round(mydatatable$Price_Index,4)


# For line charts
gdataplot <- gdata[gdata$Year >= 2005, c("ZIP", "Year", "AGI_num", "Price_Index",
                                         "IncomeLow", "IncomeHigh", "HouseLow", "HouseHigh", "topincreasepercent")]

# For Spatial Data
#data.SP <- SpatialPointsDataFrame(gdata[,c(23, 24)], gdata[,-c(23,24)])


# Zip code boundaries polygons - GEOJSON file
nyzipcode <- geojsonio::geojson_read("nyc_zipcode_polygons.geojson",
                                     what = "sp")


# Color list and functions
cgent = "#1565C0"
clow = "#4CAF50"
cmid = "#FBC02D"
chigh = "#424242"

#getColor <- function(Color) {
  
#  if(Color == "blue") { #GENTRIFYING
#    cgent
#  } else if(Color == "green") {  #Not gentrifying - low income
#    clow
#  } else if(Color == "red") { #Not gentrifying - mid income
#    cmid   
#  } else if(Color == "purple") { #Not gentrifying - high income
#    chigh
#  } else {  
#    "gray"
#  } 
#  
#}

getColor <- function(Color) {
  
  if(Color == "gent") { #GENTRIFYING
    cgent
  } else if(Color == "no_low") {  #Not gentrifying - low income
    clow
  } else if(Color == "no_mid") { #Not gentrifying - mid income
    cmid   
  } else if(Color == "no_high") { #Not gentrifying - high income
    chigh
  } else {  
    "gray"
  } 
  
}

# Simpler color for the icons since the "awesomeIcons" function only recognize simple colors
getColor2 <- function(Color) {
  
  if(Color == "gent") { #GENTRIFYING
    "blue"            
  } else if(Color == "no_low") {  #Not gentrifying - low income
    "green"           
  } else if(Color == "no_mid") { #Not gentrifying - mid income
    "orange"           
  } else if(Color == "no_high") { #Not gentrifying - high income
    "lightgray"             
  } else {  
    "black"
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
c2015$color <- ifelse(c2015$crime_count>=10, "red", "grey")
minmax <- read.csv("minmaxlatlon.csv")

# Zip code area description
desc <- read.csv("zipdescription2.csv")
desc['NAME'] <- as.character(desc$NAME)
desc['description'] <- as.character(desc$description)


## Building permits data
permits2006 <- read.csv("permits2006.csv")
#permits2014 <- read.csv("permits2014.csv")
#permits2015 <- read.csv("permits2015.csv")
permits2016 <- read.csv("permits2016.csv")

permits2006$num_permitsTRIM <- ifelse(permits2006$num_permits >=10, 10, permits2006$num_permits)
permits2016$num_permitsTRIM <- ifelse(permits2016$num_permits >=10, 10, permits2016$num_permits)

pal <- colorNumeric(
  palette = "RdYlBu",
  domain = c(1,2,3,4,5,6,7,8,9,10)
  #domain = num_permitsTRIM
)

# custom label format function
myLabelFormat = function(..., reverse_order = FALSE){ 
  if(reverse_order){ 
    function(type = "numeric", cuts){ 
      cuts <- sort(cuts, decreasing = T)
    } 
  }else{
    labelFormat(...)
  }
}


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
  
  # Table of zip codes and gentri proba
  output$mytable <- renderDataTable(filtered_data()[, c("ZIP", "Borough", "Neighborhood",
                                                        "Probability", "Price_Index")], # income dropped since no projected data after 2015
                                    colnames = c('Zip Code', 'Borough', 'Neighborhood', 'Gentrification Probability (%)', 'House Index'),
                                    selection = 'single')  # Single row selection and drop "Year" column
  
  
  zoomlng <- reactive({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){
      zlong <- -73.935
    } else {
      zlong <- gdatacoord[gdatacoord == zz, 'Longitude']
    }
  })
  
  zoomlat <- reactive({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){
      zlong <- 40.690
    } else {
      zlong <- gdatacoord[gdatacoord == zz, 'Latitude']
    }
  })
  
  zoomscale <- reactive({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){
      zscale <- 10
    } else {
      zscale <- 14
    }
  })
  
  
  # NYC zip code boundaries map
  output$mymap <- renderLeaflet({
      leaflet(nyzipcode) %>% 
      addProviderTiles("Esri.WorldTopoMap") %>%
      ##setView(lng=-73.935, lat=40.690, zoom=10) %>%
      setView(lng=zoomlng(), lat=zoomlat(), zoom=zoomscale()) %>%
      addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.5, weight=1,
                  fillColor=zipcolor(), color="white") %>%
      addAwesomeMarkers(data = gdata[gdata$Year == input$pickyear,], clusterOptions = markerClusterOptions(), lng = ~Longitude, lat = ~Latitude, icon = icons(),
                        popup = ~paste("<b>ZIP Code:</b>", ZIP, "<br>", "<b>Neighborhood:</b>", Neighborhood,"<br>",
                                       "<b>Year</b>", Year,"<br>","<b>Average Income</b>", AGI, sep = " ")) %>%
      addLegend("topleft",   colors =c(cgent, clow, cmid, chigh),
                labels= c("Gentrifying", "Non-gentrifying (low home value)", "Non-gentrifying (mid home value)",
                          "Non-gentrifying (high home value)"),
                title = "Gentrification",
                opacity = 0.7
      )
      
    })


  
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
  
  # Eligible for Gentrification Flag for different output in UI
  gentflag <- reactive({     
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    
    zip_data <- myprobtable[myprobtable$ZIP == zz,]
    zip_data <- zip_data[zip_data$Year == input$pickyear,]
    zip_data['NotGentReason'] <- as.character(zip_data$NotGentReason)
    gent <- zip_data$NotGentReason
    ifelse(gent == "N/A",1,0)
  }) 
  
  # If not eligible for gentrification
  output$text1 <- renderText({
    if(gentflag()== 0){
      zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
      if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
      
      zip_data <- myprobtable[myprobtable$ZIP == zz,]
      zip_data <- zip_data[zip_data$Year == input$pickyear,]
      zip_data['NotGentReason'] <- as.character(zip_data$NotGentReason)
      gent <- zip_data$NotGentReason
      
      if(gent=="Past"){
        paste("Gentrification prediction only applicable for years after 2015")
      } else if(gent=="High Income and/or Housing"){
        paste("Not eligible for gentrification: Above NYC average house value and/or income")
      } else if(gent=="Low Increase Rate"){
        paste("Not eligible for gentrification: House Value did not increase enough")
      }
    }
    })
  
  output$text_gent <- renderText({
    if(gentflag()== 0){
      zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
      if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
      
      zip_data <- myprobtable[myprobtable$ZIP == zz,]
      zip_data <- zip_data[zip_data$Year == input$pickyear,]
      zip_data['NotGentReason'] <- as.character(zip_data$NotGentReason)
      gent <- zip_data$NotGentReason
      
      if(gent=="Past"){
        paste("Gentrification prediction only applicable for years after 2015. All probabilities set to 0.")
      }
    }})
  
  # Plot for House Probability
  output$probplot <- renderPlot({
    if(gentflag()== 1){
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    zip_data <- myprobtable[myprobtable$ZIP == zz,]
    zip_data <- zip_data[zip_data$Year == input$pickyear,]
    
    alpha <- zip_data$alpha	
    beta <- zip_data$beta
    #threshold_highvalue <- zip_data$HouseHigh  #if criteria is top third percentile of house value
    threshold_highvalue <- zip_data$threshold  # if criteria is top third percentile of house value INCREASE
    
    dfgamma <- data.frame(x=c(1:10000),HouseIndex = rgamma(100000, shape=alpha, scale=1/beta))
    dens <- density(dfgamma$HouseIndex)
    df <- data.frame(x=dens$x, y=dens$y)
    probs <- c(0, pgamma(threshold_highvalue, shape=alpha, scale=1/beta))
    quantiles <- quantile(dfgamma$HouseIndex, prob=probs)
    quantiles_labels <- 1 - quantiles
    df$quant <- factor(findInterval(df$x,quantiles))
    
    ggplot(df, aes(x,y)) + geom_line() + 
      geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
      #scale_x_continuous(breaks=quantiles) + 
      #scale_x_continuous(breaks=quantiles, labels=c("", paste("Minimum House Value Required*:",round(threshold_highvalue,4), sep=" " ))) + 
      scale_x_continuous(breaks=quantiles, labels=c("", "")) + 
      scale_fill_brewer(guide="none") +
      geom_vline(xintercept=threshold_highvalue, color='blue', linetype = "longdash") +
      ggtitle(paste("Probability of House Value in Zip Code", zz, "in", input$pickyear, sep =" ")) +
      xlab("House Value Index") +
      ylab("Probability") +
      theme_minimal() +
      theme(#axis.title.x=element_blank(), 
            axis.title.y=element_blank(), axis.text.y=element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    ### old codes
    #prob <- round((1 - pgamma(threshold_highvalue, shape=alpha, scale=1/beta))*100,0)
    #title1 <- paste("Probability greater than NYC top 1/3 indices:", prob, "%", sep=" ")
    
    #dfgamma <- data.frame(HouseIndex = rgamma(100000, shape=alpha, scale=1/beta))
    
    #ggplot(dfgamma, aes(HouseIndex)) + 
    #  geom_density(alpha = 0.2, color='grey', fill="blue") + 
    #  geom_vline(xintercept=threshold_highvalue, color='purple', linetype = "longdash") +
    #  ggtitle(title1) +
    #  theme_minimal() +
    #  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }
  })
  
  output$gentprob <- renderText({
    if(gentflag()== 1){
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    
    zip_data <- myprobtable[myprobtable$ZIP == zz,]
    zip_data <- zip_data[zip_data$Year == input$pickyear,]
    
    alpha <- zip_data$alpha	
    beta <- zip_data$beta
    #threshold_highvalue <- zip_data$HouseHigh  #if criteria is top third percentile of house value
    threshold_highvalue <- zip_data$threshold  # if criteria is top third percentile of house value INCREASE
    
    prob <- 1.0 - pgamma(threshold_highvalue,shape=alpha, scale=1/beta)
    prob <- round(prob*100,0)
    paste(prob, "%", sep = " ")
    }
  })
  
  output$gentprob_text <- renderText({
    if(gentflag()== 1){
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    t0 <- input$pickyear-10
    t1 <- input$pickyear
    paste("Probability that zip code", zz, "will gentrify over", t0, "to", t1, sep = " ")
    }
  })
  

  
  # Plot for Income
  output$incomeplot <- renderPlot({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    zip_data <- gdataplot[gdataplot$ZIP == zz, 
                          c("Year", "AGI_num", "Price_Index",
                            "IncomeLow", "IncomeHigh", "HouseLow", "HouseHigh")]
    zip_data <- zip_data[zip_data$Year <= 2015,]
    
    ggplot(data=zip_data, aes(x=Year)) +
             geom_line(aes(y = AGI_num), colour = "blue", linetype = "solid", size = 2) + 
             geom_line(aes(y = IncomeLow), colour = "yellowgreen", linetype = "dashed", size = 1) + 
             geom_line(aes(y = IncomeHigh), colour = "orchid2", linetype = "dashed", size = 1) + 
      xlab("Year") + ylab("Income in 2015 $") + # Set axis labels
      #ggtitle(paste("Annual Mean Income for Zip Code", zz, 
      #              sep = " "))
      ggtitle("Historical Mean Income from 2005 to 2015") +
      theme_minimal()
  })
  
  # Plot for House Index
  output$houseplot <- renderPlot({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    zip_data <- gdataplot[gdataplot$ZIP == zz, 
                          c("Year", "AGI_num", "Price_Index",
                            "IncomeLow", "IncomeHigh", "HouseLow", "HouseHigh")]
    zip_data <- zip_data[zip_data$Year <= input$pickyear,]
    
    ggplot(data=zip_data, aes(x=Year)) +
      geom_line(aes(y = Price_Index), colour = "blue", linetype = "solid", size = 2) + 
      geom_line(aes(y = HouseLow), colour = "yellowgreen", linetype = "dashed", size = 1) + 
      geom_line(aes(y = HouseHigh), colour = "orchid2", linetype = "dashed", size = 1) + 
      xlab("Year") + ylab("House Index - inflation adjusted") + # Set axis labels
      #ggtitle(paste("Annual Mean House Index for Zip Code", zz, 
      #              sep = " "))
      ggtitle(paste("Mean House Index from 2005 to", input$pickyear, sep=" ")) +
      theme_minimal()
  })
  
  # Plot for House Index Increase
  output$houseincreaseplot <- renderPlot({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    zip_data <- gdataplot[gdataplot$ZIP == zz, 
                          c("Year", "Price_Index")]
    d <- zip_data[(zip_data$Year <= input$pickyear) & (zip_data$Year >= (input$pickyear - 10)),]
    d_list <- as.vector(d[(d$Year <= input$pickyear) & (d$Year >= (input$pickyear -10)),"Price_Index"])
    
    initprice <- d_list[1]
    initprice
    
    increase=c()
    increase[1] = 0
    
    for (i in 2:length(d_list)) { 
      tmp <- ((d_list[i]/initprice) - 1) * 100
      increase[i] = tmp 
    }
    
    l <- length(increase) - 1
    years <- seq(input$pickyear -l, input$pickyear, 1)
    df <- data.frame("Year" = years, "Increase" = increase)
    threshold_percent = gdataplot[(gdataplot$ZIP == zz) & (gdataplot$Year == input$pickyear), 
                                  "topincreasepercent"]
    
    ggplot(df, aes(x=Year, y=Increase)) +
      geom_line(colour = "blue", linetype = "solid", size = 2) +
      geom_hline(yintercept=threshold_percent*100, color='yellowgreen', linetype = "longdash", size=1) +
      xlab("Year") + ylab("Accumulated House Value Increase (%)") +
      ggtitle(paste("Accumulated Increase in House Value from", input$pickyear -10, sep=" ")) +
      theme_minimal()
  })
  
  
  # Headers for multiple tabs
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
  
  
  # Crime plots - 2006 and 2015
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
  
  output$permitplot2006 <- renderLeaflet({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}

    permits_plot <- permits2006[permits2006$zipcode == zz,]
    num_permitsTRIM <- permits_plot$num_permitsTRIM
    rev_num_permitsTRIM <- 11 - num_permitsTRIM
    ziplon <- gdata[gdata$ZIP == zz, 'Longitude'][1]
    ziplat <- gdata[gdata$ZIP == zz, 'Latitude'][1]
    
    leaflet(nyzipcode) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      setView(lng=ziplon, lat=ziplat, zoom=14) %>% 
      addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.5, weight=3, 
                  color = "black", fillColor = "white") %>%
      addHeatmap(data=permits_plot, lat = ~LATITUDE, lng = ~LONGITUDE, intensity = ~num_permitsTRIM, #problem with this line!
                 radius = 7) 
      #%>%
      #addLegend("bottomright", pal=pal,
      #          values = ~c(1,2,3,4,5,6,7,8,9,10),
                #values = ~num_permitsTRIM,
      #          labFormat = myLabelFormat(reverse_order = T),
      #          title = "Count",
      #          opacity = 0.75)
    
  })
  
  output$permitplot2016 <- renderLeaflet({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    
    permits_plot <- permits2016[permits2016$zipcode == zz,]
    num_permitsTRIM <- permits_plot$num_permitsTRIM
    rev_num_permitsTRIM <- 11 - num_permitsTRIM
    ziplon <- gdata[gdata$ZIP == zz, 'Longitude'][1]
    ziplat <- gdata[gdata$ZIP == zz, 'Latitude'][1]
    
    leaflet(nyzipcode) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      setView(lng=ziplon, lat=ziplat, zoom=14) %>% 
      addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.5, weight=3, 
                  color = "black", fillColor = "white") %>%
      addHeatmap(data=permits_plot, lat = ~LATITUDE, lng = ~LONGITUDE, intensity = ~num_permitsTRIM, #problem with this line!
                 radius = 7) 
      #%>%
      #addLegend("bottomright", pal=pal,
      #          values = ~c(1,2,3,4,5,6,7,8,9,10),
                #values = ~num_permitsTRIM,
      #          labFormat = myLabelFormat(reverse_order = T),  # problem: stops at 7 with red since max =7 in 2016
      #          title = "Count",
      #          opacity = 0.75)
    
  })
  
  output$permit2006count <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    
    a1 <- sum(permits2006[(permits2006$zipcode == zz) & (permits2006$job_type == 'A1'), 'num_permits'])
    a2 <- sum(permits2006[(permits2006$zipcode == zz) & (permits2006$job_type == 'A2'), 'num_permits'])
    nb <- sum(permits2006[(permits2006$zipcode == zz) & (permits2006$job_type == 'NB'), 'num_permits'])
    dm <- sum(permits2006[(permits2006$zipcode == zz) & (permits2006$job_type == 'DM'), 'num_permits'])
    paste("Count by permit type: A1 =", a1, ", A2 =", a2, ", NB =", nb, ", DM =", dm, sep = " ")
  })
  
  output$permit2016count <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    
    a1 <- sum(permits2016[(permits2016$zipcode == zz) & (permits2016$job_type == 'A1'), 'num_permits'])
    a2 <- sum(permits2016[(permits2016$zipcode == zz) & (permits2016$job_type == 'A2'), 'num_permits'])
    nb <- sum(permits2016[(permits2016$zipcode == zz) & (permits2016$job_type == 'NB'), 'num_permits'])
    dm <- sum(permits2016[(permits2016$zipcode == zz) & (permits2016$job_type == 'DM'), 'num_permits'])
    paste("Count by permit type: A1 =", a1, ", A2 =", a2, ", NB =", nb, ", DM =", dm, sep = " ")
  })
  
  # Wikipedia description
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
  
  output$featwater <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    
    zzfeat <- myfeaturetable[myfeaturetable$ZIP == zz, 'Bordering.Water']
    zzfeat <- ifelse(zzfeat=="N","No", "Yes")
    paste("Bordering Water:", zzfeat, sep = " ")
  })
  
  output$featlines <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    
    zzfeat <- myfeaturetable[myfeaturetable$ZIP == zz, 'Number.of.Subway.Lines.Serving.ZIP']
    zzfeat <- ifelse(is.na(zzfeat),0, zzfeat)
    paste("Number of subway lines:", zzfeat, sep = " ")
  })
  
  output$featparks <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    
    zzfeat <- myfeaturetable[myfeaturetable$ZIP == zz, 'Number.of.Parks']
    zzfeat <- ifelse(is.na(zzfeat),0, zzfeat)
    paste("Number of parks:", zzfeat, sep = " ")
  })
  
  output$featplays <- renderText({
    zz <- filtered_data()[input$mytable_rows_selected, "ZIP"]
    if(length(zz)==0){zz<- filtered_data()$ZIP[1]}
    
    zzfeat <- myfeaturetable[myfeaturetable$ZIP == zz, 'Number.of.Playgrounds']
    zzfeat <- ifelse(is.na(zzfeat),0, zzfeat)
    paste("Number of playgrounds:", zzfeat, sep = " ")
  })
})  

#shinyApp(ui, server=shinyServer)

