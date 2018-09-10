library(dplyr)
library(ggmap)
library(ggplot2)
library(tmap)
library(rgdal)
library(shiny)
library(leaflet)
library(shinydashboard)
calls <- read.csv("311.csv")

shelters <- read.csv("shelters.csv")

crime <- read.csv("crime.csv")

tract <- read.csv("tracts.csv")

homeless <- read.csv("TOT1.csv")

zipcodes <- read.csv("zipcode.csv")

census <- read.csv("2010_Census_Populations_by_Zip_Code.csv")


#sum categories and join tables


Shelters <- shelters %>% group_by(ZIPCODE) %>% summarize(Shelters=n())%>%
  mutate(Shelters)

Calls <- calls %>% group_by(ZIPCODE) %>% summarize(Calls311=n())%>%
  mutate(Calls311)

homeless <- inner_join(homeless, tract, by ="tract")

homeless <- homeless %>% group_by(ZIPCODE) %>% summarize(TotalHomeless=sum(totPeople),
                                                         TotalSheltered=sum(totSheltPeople),
                                                         TotalUnsheltered=sum(totUnsheltPeople),
                                                         YouthHomeless=sum(totSheltPeople))%>%
  mutate(TotalHomeless, TotalSheltered, TotalUnsheltered, YouthHomeless)


df <- full_join(Calls, Shelters, by = "ZIPCODE")

df <- full_join(df, homeless, by="ZIPCODE")

df <- full_join(df, zipcodes, by ="ZIPCODE")

df <- inner_join(df, census, by ="ZIPCODE")

zipcode = readOGR(dsn = ".", 
                  layer = "CAMS_ZIPCODE_PARCEL_SPECIFIC")

bus = readOGR(dsn=".", layer = "MTA_PARK_AND_RIDE")

busStops = bus@data

plot(bus)

zip <- zipcode@data

plot(zipcode)

?readOGR

zip$ZIPCODE <- as.numeric(zip$ZIPCODE)


communities = inner_join(zip, df, by = "ZIPCODE")
View(zipcode)

?colorQuantile

homeless_pal = colorQuantile("Reds", df$TotalHomeless, n = 4)
sheltHomeless_pal = colorQuantile("Reds", communities$TotalSheltered, n = 4)
unsheltHomeless_pal = colorQuantile("Reds", communities$TotalUnsheltered, n = 4)
youthHomeless_pal = colorQuantile("Reds", communities$YouthHomeless, n = 4)
###########################

library(shiny)
library(leaflet)

homeless_popup <- paste0("<strong>Homeless Count: </strong>", 
                         df$TotalHomeless, 
                         "<br><strong>Zipcode: </strong>",
                         df$ZIPCODE)%>% lapply(htmltools::HTML)
popup_311 <- paste0("<strong>Number of 311 Calls: </strong>", 
                    df$Calls311, 
                    "<br><strong>Zipcode: </strong>",
                    df$ZIPCODE)%>% lapply(htmltools::HTML)

popup_youth <- paste0("<strong>Youth Homeless Count: </strong>", 
                      df$YouthHomeless, 
                      "<br><strong>Zipcode: </strong>",
                      df$ZIPCODE)%>% lapply(htmltools::HTML)




popup_unsheltered <- paste0("<strong>Unsheltered Count: </strong>", 
                            df$TotalUnsheltered, 
                            "<br><strong>Zipcode: </strong>",
                            df$ZIPCODE)%>% lapply(htmltools::HTML)

popup_shelters <- paste0("<strong>Count of Shelters: </strong>", 
                         df$Shelters, 
                         "<br><strong>Zipcode: </strong>",
                         df$ZIPCODE)%>% lapply(htmltools::HTML)

popup_sheltered <- paste0("<strong>Count of Sheltered: </strong>", 
                          df$TotalSheltered, 
                          "<br><strong>Zipcode: </strong>",
                          df$ZIPCODE)%>% lapply(htmltools::HTML)

popup_population <- paste0("<strong>Population Count: </strong>", 
                           df$Total.Population, 
                           "<br><strong>Zipcode: </strong>",
                           df$ZIPCODE)%>% lapply(htmltools::HTML)

popup_males <- paste0("<strong>Male Population Count: </strong>", 
                      df$Total.Males, 
                      "<br><strong>Zipcode: </strong>",
                      df$ZIPCODE)%>% lapply(htmltools::HTML)

popup_females <- paste0("<strong>Female Population Count: </strong>", 
                        df$Total.Females, 
                        "<br><strong>Zipcode: </strong>",
                        df$ZIPCODE)%>% lapply(htmltools::HTML)

popup_households <- paste0("<strong>Household Count: </strong>", 
                           df$Total.Households, 
                           "<br><strong>Zipcode: </strong>",
                           df$ZIPCODE)%>% lapply(htmltools::HTML)




############################


zipcode <- spTransform(zipcode, CRS("+proj=longlat +ellps=GRS80"))

?colorQuantile

pal_homeless <- colorQuantile("Reds", df$TotalHomeless,
            na.color = "#808080", alpha = FALSE, reverse = FALSE, n=4)

  
library(shinythemes)

# User interface ----
ui <- fluidPage(
  
  theme=shinytheme(cyborg),
  
  titlePanel("Overlay of Variables Related to Homelessness in lA"),
  
  sidebarLayout(
    sidebarPanel(
      
      
      selectInput("var", 
                  label = NULL,
                  choices = c("No Overlay Selected", "311 Calls", "Population", "Male Population", "Female Population", "Households", "Youth Homeless", "Total Unsheltered", "Total Sheltered", "Shelters"),
                  selected = "No Overlay Selected"),
      plotOutput("correlation")
      
    ),
    
    
    mainPanel(leafletOutput("map"))
  )
)

#####
ui <- dashboardPage(
  dashboardHeader(title="Comparing Independent Variables to Homelessness in LA"), dashboardSidebar(),
  
  dashboardBody( fluidRow( box(selectInput("var", 
                                           label = NULL,
                                           choices = c("No Overlay Selected", "311 Calls", "Population", 
                                                       "Male Population", "Female Population", "Households", 
                                                       "Youth Homeless", "Total Unsheltered", "Total Sheltered", 
                                                       "Shelters"),
                                           selected = "No Overlay Selected")
                               
                               ),
                           box(plotOutput("correlation", height = 250)),
                           
                           box(leafletOutput("map"))
                           
                           
                           )
                 
                 
                 
                 )
  
)
  
  
# Server logic ----

server <- function(input, output, session) {
  
  observe ({
  
    
  if (input$var == "No Overlay Selected") {
    
   
  
  output$map = renderLeaflet({
 
    map = leaflet() %>% 
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addPolygons(data=zipcode,weight=1, color="gray", 
                  fillColor = ~colorQuantile("YlOrRd", df$TotalHomeless)(df$TotalHomeless),
                  smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5, popup = popup_311, 
                  highlight = highlightOptions(
                    weight = 5,
                    color = "orange",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = popup_311)  })
  
  
  }

    
      else if (input$var == "311 Calls") {
        
        
        output$correlation<- renderPlot({
          x<-as.numeric(df$Calls311)
          y<-as.numeric(df$TotalHomeless)
          plot(x, y, log = "xy", xlab="311 Calls", ylab="Total Homeless")
        })
        
        output$map = renderLeaflet({
          
          map = leaflet() %>% 
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
            addPolygons(data=zipcode,weight=1, color="gray", 
                        fillColor = ~colorQuantile("YlOrRd", df$TotalHomeless)(df$TotalHomeless),
                        smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, popup = popup_311, 
                        highlight = highlightOptions(
                          weight = 5,
                          color = "orange",
                          dashArray = "",
                          fillOpacity = 0.7,
                          bringToFront = TRUE),
                        label = popup_311)  %>%
            addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 2,
                       radius = ~Calls311, popup = popup_311, color="blue", 
                       highlight = highlightOptions(
                         weight = 5,
                         color = "orange",
                         dashArray = "",
                         fillOpacity = 0.7,
                         bringToFront = TRUE),
                       label = popup_311,
                       data=df)  %>%
            addLegend(title = "311 Calls", colors = NULL, labels = NULL)})}
      
      
      else if (input$var == "Youth Homeless") {
        
        output$correlation<- renderPlot({
          x<-as.numeric(df$YouthHomeless)
          y<-as.numeric(df$TotalHomeless)
          plot(x, y, log = "xy", xlab="Youth Homeless", ylab="Total Homeless")
        })
        
        
        output$map = renderLeaflet({
          
          map = leaflet() %>% 
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
            addPolygons(data=zipcode,weight=1, color="gray", 
                        fillColor = ~colorQuantile("YlOrRd", df$TotalHomeless)(df$TotalHomeless),
                        smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, popup = popup_311, 
                        highlight = highlightOptions(
                          weight = 5,
                          color = "orange",
                          dashArray = "",
                          fillOpacity = 0.7,
                          bringToFront = TRUE),
                        label = popup_311) %>%
            addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 2,
                       radius = ~YouthHomeless, popup = popup_youth, color="green", 
                       highlight = highlightOptions(
                         weight = 5,
                         color = "orange",
                         dashArray = "",
                         fillOpacity = 0.7,
                         bringToFront = TRUE),
                       label = popup_youth,
                       data=df) %>%
            addLegend(title = "Youth Homeless Estimate", colors = NULL, labels = NULL)})
      }
      else if (input$var == "Total Unsheltered") {
        
        output$correlation<- renderPlot({
          x<-as.numeric(df$TotalUnsheltered)
          y<-as.numeric(df$TotalHomeless)
          plot(x, y, log = "xy", xlab="Total Unsheltered", ylab="Total Homeless")
        })
        
        
        output$map = renderLeaflet({
          
          map = leaflet() %>% 
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
            addPolygons(data=zipcode,weight=1, color="gray", 
                        fillColor = ~colorQuantile("YlOrRd", df$TotalHomeless)(df$TotalHomeless),
                        smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, popup = popup_311, 
                        highlight = highlightOptions(
                          weight = 5,
                          color = "orange",
                          dashArray = "",
                          fillOpacity = 0.7,
                          bringToFront = TRUE),
                        label = popup_311)  %>%
            addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 2,
                       radius = ~TotalUnsheltered, popup = popup_unsheltered, color="gray", 
                       highlight = highlightOptions(
                         weight = 5,
                         color = "orange",
                         dashArray = "",
                         fillOpacity = 0.7,
                         bringToFront = TRUE),
                       label = popup_unsheltered,
                       data=df) %>%
            addLegend(title = "Total Unsheltered Homeless", colors = NULL, labels = NULL)})
      }
      
      else if (input$var == "Shelters") {
        
        output$correlation<- renderPlot({
          x<-as.numeric(df$Shelters)
          y<-as.numeric(df$TotalHomeless)
          plot(x, y, log = "xy", xlab="Shelters", ylab="Total Homeless")
        })
        
        
        output$map = renderLeaflet({
          
          map = leaflet() %>% 
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
            addPolygons(data=zipcode,weight=1, color="gray", 
                        fillColor = ~colorQuantile("YlOrRd", df$TotalHomeless)(df$TotalHomeless),
                        smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, popup = popup_311, 
                        highlight = highlightOptions(
                          weight = 5,
                          color = "orange",
                          dashArray = "",
                          fillOpacity = 0.7,
                          bringToFront = TRUE),
                        label = popup_311)  %>%
            addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 2,
                       radius = ~Shelters*100, popup = popup_shelters, color="blue", 
                       highlight = highlightOptions(
                         weight = 5,
                         color = "orange",
                         dashArray = "",
                         fillOpacity = 0.7,
                         bringToFront = TRUE),
                       label = popup_shelters,
                       data=df) %>%
            addLegend(title = "Shelters", colors = NULL, labels = NULL)})
      }
      
      
      else if (input$var == "Total Sheltered") {
        
        output$correlation<- renderPlot({
          x<-as.numeric(df$TotalSheltered)
          y<-as.numeric(df$TotalHomeless)
          plot(x, y, log = "xy", xlab="Total Sheltered", ylab="Total Homeless")
        })
        
        
        output$map = renderLeaflet({
          
          map = leaflet() %>% 
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
            addPolygons(data=zipcode,weight=1, color="gray", 
                        fillColor = ~colorQuantile("YlOrRd", df$TotalHomeless)(df$TotalHomeless),
                        smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, popup = popup_311, 
                        highlight = highlightOptions(
                          weight = 5,
                          color = "orange",
                          dashArray = "",
                          fillOpacity = 0.7,
                          bringToFront = TRUE),
                        label = popup_311)  %>%
            addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 2,
                       radius = ~Shelters*100, popup = popup_sheltered, color="orange", 
                       highlight = highlightOptions(
                         weight = 5,
                         color = "red",
                         dashArray = "",
                         fillOpacity = 0.7,
                         bringToFront = TRUE),
                       label = popup_sheltered,
                       data=df) %>%
            addLegend(title = "Total Sheltered", colors = NULL, labels = NULL)})
      }
      
      else if (input$var == "Population") {
        
        output$correlation<- renderPlot({
          x<-as.numeric(df$Total.Population)
          y<-as.numeric(df$TotalHomeless)
          plot(x, y, log = "xy", xlab="Total Population", ylab="Total Homeless")
        })
        
        
        output$map = renderLeaflet({
          
          map = leaflet() %>% 
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
            addPolygons(data=zipcode,weight=1, color="gray", 
                        fillColor = ~colorQuantile("YlOrRd", df$TotalHomeless)(df$TotalHomeless),
                        smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, popup = popup_311, 
                        highlight = highlightOptions(
                          weight = 5,
                          color = "orange",
                          dashArray = "",
                          fillOpacity = 0.7,
                          bringToFront = TRUE),
                        label = popup_311)  %>%
            addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 2,
                       radius = ~Total.Population/30, popup = popup_population, color="orange", 
                       highlight = highlightOptions(
                         weight = 5,
                         color = "red",
                         dashArray = "",
                         fillOpacity = 0.7,
                         bringToFront = TRUE),
                       label = popup_population,
                       data=df) %>%
            addLegend(title = "Total Population", colors = NULL, labels = NULL)})
      }
      
      else if (input$var == "Male Population") {
        
        output$correlation<- renderPlot({
          x<-as.numeric(df$Total.Males)
          y<-as.numeric(df$TotalHomeless)
          plot(x, y, log = "xy", xlab="Male Population", ylab="Total Homeless")
        })
        
        
        output$map = renderLeaflet({
          
          map = leaflet() %>% 
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
            addPolygons(data=zipcode,weight=1, color="gray", 
                        fillColor = ~colorQuantile("YlOrRd", df$TotalHomeless)(df$TotalHomeless),
                        smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, popup = popup_311, 
                        highlight = highlightOptions(
                          weight = 5,
                          color = "orange",
                          dashArray = "",
                          fillOpacity = 0.7,
                          bringToFront = TRUE),
                        label = popup_311)  %>%
            addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 1,
                       radius = ~Total.Males/30, popup = popup_males, color="violet", 
                       highlight = highlightOptions(
                         weight = 5,
                         color = "red",
                         dashArray = "",
                         fillOpacity = 0.7,
                         bringToFront = TRUE),
                       label = popup_males,
                       data=df) %>%
            addLegend(title = "Total Population", colors = NULL, labels = NULL)})
      } 
      
      else if (input$var == "Female Population") {
        
        output$correlation<- renderPlot({
          x<-as.numeric(df$Total.Females)
          y<-as.numeric(df$TotalHomeless)
          plot(x, y, log = "xy", xlab="Females Population", ylab="Total Homeless")
        })
        
        
        output$map = renderLeaflet({
          
          map = leaflet() %>% 
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
            addPolygons(data=zipcode,weight=1, color="gray", 
                        fillColor = ~colorQuantile("YlOrRd", df$TotalHomeless)(df$TotalHomeless),
                        smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, popup = popup_311, 
                        highlight = highlightOptions(
                          weight = 5,
                          color = "orange",
                          dashArray = "",
                          fillOpacity = 0.7,
                          bringToFront = TRUE),
                        label = popup_311)  %>%
            addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 1,
                       radius = ~Total.Females/30, popup = popup_females, color="orange", 
                       highlight = highlightOptions(
                         weight = 5,
                         color = "red",
                         dashArray = "",
                         fillOpacity = 0.7,
                         bringToFront = TRUE),
                       label = popup_females,
                       data=df) %>%
            addLegend(title = "Female Population", colors = NULL, labels = NULL)})
      } 
      
      else if (input$var == "Households") {
        
        output$correlation<- renderPlot({
          x<-as.numeric(df$Total.Households)
          y<-as.numeric(df$TotalHomeless)
          plot(x, y, log = "xy", xlab="Households", ylab="Total Homeless")
        })
        
        
        output$map = renderLeaflet({
          
          map = leaflet() %>% 
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
            addPolygons(data=zipcode,weight=1, color="gray", 
                        fillColor = ~colorQuantile("YlOrRd", df$TotalHomeless)(df$TotalHomeless),
                        smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5, popup = popup_311, 
                        highlight = highlightOptions(
                          weight = 5,
                          color = "orange",
                          dashArray = "",
                          fillOpacity = 0.7,
                          bringToFront = TRUE),
                        label = popup_311)  %>%
            addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 1,
                       radius = ~Total.Households/10, popup = popup_households, color="black", 
                       highlight = highlightOptions(
                         weight = 5,
                         color = "orange",
                         dashArray = "",
                         fillOpacity = 0.7,
                         bringToFront = TRUE),
                       label = popup_households,
                       data=df) %>%
            addLegend(title = "Households", colors = NULL, labels = NULL)})
     
  
        
  
  
}})}
  

shinyApp(ui, server)







##############3
