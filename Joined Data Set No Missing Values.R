calls <- read.csv("311.csv")

shelters <- read.csv("shelters.csv")

crime <- read.csv("crime.csv")

tract <- read.csv("tracts.csv")

homeless <- read.csv("TOT1.csv")

migration2017 <-read.csv("Homeless2017.csv")

homeless2016 <- read.csv("HC2016_Total_Counts_by_Census_Tract_LA_CoC_07132016.csv")

migration2016 <- read.csv("HC2016_Total_Counts_by_Census_Tract_LA_CoC_07132016.csv")

homeless2015 <- read.csv("Homeless2015.csv")

migration2015 <- read.csv("Homeless2015.csv")

zipcodes <- read.csv("zipcode.csv")

census <- read.csv("2010_Census_Populations_by_Zip_Code.csv")

zipcode = readOGR(dsn = ".", 
                  layer = "CAMS_ZIPCODE_PARCEL_SPECIFIC")

zipcode <- spTransform(zipcode, CRS("+proj=longlat +ellps=GRS80"))

bus = readOGR(dsn=".", layer = "MTA_PARK_AND_RIDE")


bus <- spTransform(bus, CRS("+proj=longlat +ellps=GRS80"))



library(dplyr)
library(ggmap)
library(ggplot2)
library(tmap)
library(rgdal)
library(shiny)
library(leaflet)
library(shinydashboard)

#sum categories and join tables


Shelters <- shelters %>% group_by(ZIPCODE) %>% summarize(Shelters=n())%>%
  mutate(Shelters)

Calls <- calls %>% group_by(ZIPCODE) %>% summarize(Calls311=n())%>%
  mutate(Calls311)

homeless <- inner_join(homeless, tract, by ="tract")

migration2015 <- inner_join(migration2015, tract, by ="tract")


homeless2016 <- inner_join(homeless2016, tract, by="tract")

homeless2016 <- homeless2016 %>% group_by(ZIPCODE) %>% summarize(TotalHomeless=sum(totPeople),
                                                         TotalSheltered=sum(totSheltPeople),
                                                         TotalUnsheltered=sum(totUnsheltPeople),
                                                         YouthHomeless=sum(totSheltPeople))%>%
  mutate(TotalHomeless, TotalSheltered, TotalUnsheltered, YouthHomeless) %>% filter(TotalSheltered != 0)




homeless2015 <- inner_join(homeless2015, tract, by="tract")

homeless2015 <- homeless2015 %>% group_by(ZIPCODE) %>% summarize(TotalHomeless=sum(TotalHomeless), TotalSheltered = sum(Sheltered))%>% 
  mutate(TotalHomeless, TotalSheltered)%>% filter(TotalSheltered != 0)

homeless2017 <- homeless %>% group_by(ZIPCODE) %>% summarize(TotalHomeless=sum(totPeople),
                                                  TotalSheltered=sum(totSheltPeople),
                                                  TotalUnsheltered=sum(totUnsheltPeople),
                                                  YouthHomeless=sum(totSheltPeople))%>%
  mutate(TotalHomeless, TotalSheltered, TotalUnsheltered, YouthHomeless) %>% filter(TotalSheltered !=0)

homeless <- homeless %>% group_by(ZIPCODE) %>% summarize(TotalHomeless=sum(totPeople),
                                                             TotalSheltered=sum(totSheltPeople),
                                                             TotalUnsheltered=sum(totUnsheltPeople),
                                                             YouthHomeless=sum(totYouthIndiv
))%>%
  mutate(TotalHomeless, TotalSheltered, TotalUnsheltered, YouthHomeless)


df <- full_join(Calls, Shelters, by = "ZIPCODE")

df <- full_join(df, homeless, by="ZIPCODE")

df <- full_join(df, zipcodes, by ="ZIPCODE")

df <- inner_join(df, census, by ="ZIPCODE")

gmap <- get_googlemap("los angeles", zoom = 10,
                     size = c(640, 640), scale = 2, format = "png8", maptype = c("satellite"), language = "en-EN", sensor = FALSE, messaging = FALSE,
                     urlonly = FALSE, filename = "ggmapTemp", color = c("color", "bw"),
                     force = TRUE)

colnames(crime)

target <- c("ROBBERY", "CRIMINAL HOMICIDE", "RAPE, FORCIBLE")

violentcrimes <- crime %>% filter(CRIME.CODE.DESCRIPTION %in% target)


## Plot Variables
library(qmap)

map <- qmap("los angeles", zoom=10)

mymap <- map + geom_point(aes(x=LONGITUDE, y=LATITUDE, size=TotalHomeless),
                                              data = df) +
  stat_bin_2d(aes(x=LONGITUDE, y=LATITUDE, colour= CRIME.CODE.DESCRIPTION, fill = CRIME.CODE.DESCRIPTION),
       size=.1, bins=30, alpha=.3, data=violentcrimes)

#shiny app



##############

library(shiny)
library(leaflet)

homeless_popup <- paste0("<strong> Sheltered Homeless Count: </strong>", 
                        df$TotalHomeless, 
                        "<br><strong>Zipcode: </strong>",
                        df$ZIPCODE)%>% lapply(htmltools::HTML)

popup_2016 <- paste0("<strong>Sheltered Homeless Count: </strong>", 
                     homeless2016$TotalSheltered, 
                     "<br><strong>Zipcode: </strong>",
                     homeless2016$ZIPCODE)%>% lapply(htmltools::HTML)


popup_2015 <- paste0("<strong>SHeltered Homeless Count: </strong>", 
                     homeless2015$TotalSheltered, 
                     "<br><strong>Zipcode: </strong>",
                     homeless2015$ZIPCODE)%>% lapply(htmltools::HTML)

popup_2017 <- paste0("<strong>Homeless Count: </strong>", 
                     homeless2017$TotalSheltered, 
                     "<br><strong>Zipcode: </strong>",
                     homeless2017$ZIPCODE)%>% lapply(htmltools::HTML)


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
  


##################################################

install.packages(shinythemes)
library(maps)
library(mapproj)
library(shinythemes)
library(shinydashboard)
?shinytheme

?dashboardHeader

?dashboardPage

# User interface ----
ui <- fluidPage(
  
  
  
  titlePanel("Homeless Characteristics by Zipcode in Los Angeles"),
  
  sidebarLayout(
   sidebarPanel(
      
      
      selectInput("var", 
                  label = NULL,
                  choices = c("311 Calls", "Population", "Male Population", "Female Population", "Households", "Youth Homeless", "Total Unsheltered", "Total Sheltered", "Shelters"),
                  selected = "311 Calls"),
      plotOutput("correlation")
      
    ),
  
    
    mainPanel(leafletOutput("map"), leafletOutput("mymap"))
  )
)

##################

ui <- dashboardPage(
  dashboardHeader(title="2017 LA Homeless Analysis", titleWidth = 300), dashboardSidebar(
    
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = NULL),
      menuItem("Demographics", tabName = "Demographics", icon = NULL),
      menuItem("Transportation", tabName = "Transportation", icon = NULL),
      menuItem("Migration", tabName = "Migration", icon = NULL),
      menuItem("Evaluation", tabName = "Evaluation", icon = NULL),
      menuItem("Recommendations", tabName = "Recommendations", icon = NULL)
      
  )), 
  
  dashboardBody(tabItems(
    tabItem(tabName = "Introduction"),
    
    tabItem(tabName="Demographics",
    
    
    fluidRow( 
      
      box(leafletOutput("mymap")),
      box(leafletOutput("map")),
      box(selectInput("var", 
                                           label = NULL,
                                           choices = c("No Overlay Selected", "311 Calls", "Population", 
                                                       "Male Population", "Female Population", "Households", 
                                                       "Youth Homeless", "Total Unsheltered", "Total Sheltered", 
                                                       "Shelters"),
                                           selected = "311 Calls")
                               
  ),
  box(plotOutput("correlation", height = 250))
  
  
  
  )),
  tabItem(tabName="Transportation"
            
                
            
          
          
          
          ),
  
  tabItem(tabName="Migration", fluidRow( 
    
    box(leafletOutput("migration")),
    box(selectInput("migration", 
                label = NULL,
                choices = c("2017", "2016", "2015"),
                selected = "2015"),
        box(plotOutput("plot1"), width = 250)
    
  )),
    
    
    
  tabItem(tabName="Evaluation"),
  tabItem(tabName="Recommendations")
  
  
  
  
  ) ) ) )


  
  
  
  
  
  



# Server logic ----

  server <- function(input, output, session) {
    
    observe({
      if (input$migration == "2016") {

      output$plot1 = renderPlot ({  
      x<-as.numeric(migration2016$totPeople)
      y<-as.numeric(migration2016$tract)
      plot(x, y, log = "xy", xlab="Total Homeless Count", ylab="Tract", title = "Total Homeless Distribution by Tract") })
      
      
    
    output$migration = renderLeaflet({
      
      map = leaflet(homeless2016) %>% setView(-118.24, 33.97, zoom = 9) %>%
        addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
        addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 5,
                   radius = ~homeless2016$TotalSheltered, popup = popup_2016, color="red",  
                   highlight = highlightOptions(
                     weight = 5,
                     color = "orange",
                     dashArray = "",
                     fillOpacity = 0.7,
                     bringToFront = TRUE),
                   label = popup_2016,
                   data=df) %>%
        addLegend("bottomright", title = "Sheltered Homeless 2016", colors = NULL, labels = NULL)})
    
      }
      
      else if (input$migration == "2017") {
        
        output$plot1 = renderPlot ({  
          x<-as.numeric(migration2017$totPeople)
          y<-as.numeric(migration2017$tract)
          plot(x, y, log = "xy", xlab="Total Homeless Count", ylab="Tract", title = "Total Homeless Distribution by Tract") })
        
        
    
    output$migration = renderLeaflet({
      
      map = leaflet(homeless2017) %>% setView(-118.24, 33.97, zoom = 9) %>%
        addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
        addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 5, 
                   radius = ~homeless2017$TotalSheltered, popup = popup_2017, color="red", 
                   highlight = highlightOptions(
                     weight = 5,
                     color = "orange",
                     dashArray = "",
                     fillOpacity = 0.7,
                     bringToFront = TRUE),
                   label = popup_2017,
                   data=df) %>%
        addLegend("bottomright", title = "Sheltered Homeless 2017", colors = NULL, labels = NULL)}) 
    
    
      }
      
      else if (input$migration == "2015") {
        
        output$plot1 = renderPlot ({  
          x<-as.numeric(migration2015$TotalHomeless)
          y<-as.numeric(migration2015$tract)
          plot(x, y, log = "xy", xlab="Total Homeless Count", ylab="Tract", title = "Total Homeless Distribution by Tract") })
        
        
        
        
        output$migration = renderLeaflet({
          
          map = leaflet(homeless2015) %>% setView(-118.24, 33.97, zoom = 9) %>%
            addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
            addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 5,
                       radius = ~homeless2015$TotalSheltered, popup = popup_2015, color="red", 
                       highlight = highlightOptions(
                         weight = 5,
                         color = "orange",
                         dashArray = "",
                         fillOpacity = 0.7,
                         bringToFront = TRUE),
                       label = popup_2015,
                       data=df) %>%
            addLegend("bottomright", title = "Sheltered Homeless 2015", colors = NULL, labels = NULL)}) 
        
        
      }
      
      
      
      
    })
    
    
    output$mymap = renderLeaflet({
      
      map = leaflet(df %>% filter(TotalHomeless != 0)) %>% setView(-118.24, 33.97, zoom = 9) %>%
        addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
        addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 2,
                   radius = ~TotalHomeless, popup = homeless_popup, color="red", 
                   highlight = highlightOptions(
                     weight = 5,
                     color = "orange",
                     dashArray = "",
                     fillOpacity = 0.7,
                     bringToFront = TRUE),
                   label = homeless_popup,
                   data=df) %>%
      addLegend(title = "Homeless Estimate", colors = NULL, labels = NULL)})
      

     observe({
       if (input$var == "311 Calls") {
         
         
         output$correlation<- renderPlot({
           x<-as.numeric(df$Calls311)
           y<-as.numeric(df$TotalHomeless)
           plot(x, y, log = "xy", xlab="311 Calls", ylab="Total Homeless")
         })
         
         output$map = renderLeaflet({

 map = leaflet(df %>% filter(Calls311 != 0)) %>% setView(-118.24, 33.97, zoom = 10) %>%
        addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
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
          
          map = leaflet(df %>% filter(YouthHomeless != 0)) %>% setView(-118.24, 33.97, zoom = 10) %>%
            addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
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
            
            map = leaflet(df %>% filter(TotalUnsheltered != 0)) %>% setView(-118.24, 33.97, zoom = 10) %>%
              addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
              addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 2,
                         radius = ~TotalUnsheltered, popup = popup_unsheltered, color="pink", 
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
           
           map = leaflet(df) %>% setView(-118.24, 33.97, zoom = 10) %>%
             addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
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
           
           map = leaflet(df %>% filter(Shelters != 0)) %>% setView(-118.24, 33.97, zoom = 10) %>%
             addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
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
           
           map = leaflet(df) %>% setView(-118.24, 33.97, zoom = 10) %>%
             addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
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
           
           map = leaflet(df) %>% setView(-118.24, 33.97, zoom = 10) %>%
             addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
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
           
           map = leaflet(df) %>% setView(-118.24, 33.97, zoom = 10) %>%
             addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
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
           
           map = leaflet(df) %>% setView(-118.24, 33.97, zoom = 10) %>%
             addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
             addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 1,
                        radius = ~Total.Households/10, popup = popup_households, color="yellow", 
                        highlight = highlightOptions(
                          weight = 5,
                          color = "orange",
                          dashArray = "",
                          fillOpacity = 0.7,
                          bringToFront = TRUE),
                        label = popup_households,
                        data=df) %>%
             addLegend(title = "Households", colors = NULL, labels = NULL)})
       } 
       
       
         
     })
  }
    
       
       
     
       
      
 




shinyApp(ui, server)



colnames(df)



