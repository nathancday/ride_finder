
library(cpdcrimedata)
library(shiny)
# library(gmapsdistance)
library(googleway)
library(leaflet)
library(sf)
library(magrittr)
library(tidyverse)

set_key(Sys.getenv("GOOGLE_API"))


# respond on ENTER
# https://stackoverflow.com/questions/31415301/shiny-responds-to-enter
js <- '
$(document).on("keyup", function(e) {
if(e.keyCode == 13){
Shiny.onInputChange("keyPressed", Math.random());
}
});
'

# data in CID repo on GitHub
cat_sf <- read.table(file="https://raw.githubusercontent.com/Smart-Cville/CID-2018-Regional-Transit-Challenge/master/data/CAT_2017_08_GTFS/stops.txt", 
                     sep=",", header = T, stringsAsFactors = F) %>%
    select(stop_name, stop_lon, stop_lat) %>%
    st_as_sf(coords = c("stop_lon", "stop_lat")) %>%
    slice(-82)

jaunt_sf <- st_read("https://raw.githubusercontent.com/Smart-Cville/CID-2018-Regional-Transit-Challenge/master/data/doc.kml",
                    stringsAsFactors = F) %>%
    select(name = Name, geometry) %>%
    mutate(shape_id = 1:26) %>% # has CRS already
    st_zm() # drop Z dim (https://gis.stackexchange.com/questions/253898/adding-a-linestring-by-st-read-in-shiny-leaflet)

cat_sf %<>% st_set_crs(st_crs(jaunt_sf))


# Define UI for application that draws a histogram
ui <- fluidPage(
                 
    tags$script(js),
    
    # https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/ui.R
    sidebarLayout(
        sidebarPanel(
            h4("Welcome to Ride-Finder, where are you?"),
            textInput("address", NULL, "Green County"),
            h6("^^ Press ENTER ^^"),
            h5("This is where the computer thinks you are:"),
            textOutput("location"),
            wellPanel( style = "margin-top: 20px",
                radioButtons("services", "What option would you like to see?",
                            c("JAUNT", "CAT"),
                            "JAUNT"),
                h5("The closest access to you is:"),
                textOutput("closest_access")
            )
        ),
        mainPanel(
            leafletOutput("map", height = 600)
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
   values <- reactiveValues()
   
   observeEvent(input[["keyPressed"]], {
       
       values$address <- input$address
       
       values$location <- values$address %>%
           paste("VA")
       
       values$geocode <- tibble(address = values$location) %>%
           re_geocode() %>%
           filter(geocode_good) %>%
           mutate(extracted = map(geocode, extract_geocode)) %>%
           select(-matches("geocode")) %>%
           unnest(extracted)
       
       values$sf <- values$geocode %>%
           mutate_at(vars(lon, lat), as.numeric) %>%
           st_as_sf(coords = c("lon", "lat"), crs = st_crs(jaunt_sf))
   })
   
   output$map <- renderLeaflet({
       
       req(values$sf)
       req(values$location)
       
       leaflet() %>%
           addProviderTiles(provider = "OpenStreetMap.HOT") %>%
           addCircleMarkers(data = values$sf, radius = 20)
   })
   
   observe({
       x <- input$services
       
       if ("CAT" == x) {
           req(values$sf)
           
           ### Closest options ---------------------------------------------------
           print(st_distance(values$sf, cat_sf))
           
           cat_min_idx <- which.min(st_distance(values$sf, cat_sf))
           print(cat_min_idx)
           values$closest_cat <- cat_sf[cat_min_idx,]
           
           # print(values$closest_cat)
           
           values$remain_cat <- cat_sf[-cat_min_idx,]
           
           output$closest_access <- renderText({
               values$closest_cat$stop_name
           })
           
           values$view <- st_union(values$sf, values$closest_cat) %>%
               st_bbox() %>%
               unclass() %>%
               unname()
           
           leafletProxy("map") %>%
               clearShapes() %>%
               addCircleMarkers(data = values$closest_cat, radius = 20, color = "green") %>%
               addCircleMarkers(data = values$remain_cat, radius = 5, color = "green") %>%
               fitBounds(values$view[1], values$view[2], values$view[3], values$view[4])
       }
       
       if ("JAUNT" == input$services) {
           req(values$sf)
           
           ### Closest options ---------------------------------------------------
           jaunt_min_idx <- which.min(st_distance(values$sf, jaunt_sf))
           
           values$closest_jaunt <- jaunt_sf[jaunt_min_idx,]
           
           output$closest_access <- renderText({
               values$closest_jaunt$name
           })
           
           values$view <- values$closest_jaunt %>%
               st_bbox() %>%
               unclass() %>%
               unname()
           
           leafletProxy("map") %>%
               clearMarkers() %>%
               addPolygons(data = values$closest_jaunt, color = "red") %>%
               addCircleMarkers(data = values$sf, radius = 20) %>%
               fitBounds(values$view[1], values$view[2], values$view[3], values$view[4])
       }
       
   })
   
   output$location <- renderText({
       req(values$geocode)
       values$geocode$formatted_address
       })
   
   output$cat_stop <- renderText({
       req(values$closest_cat)
       values$closest_cat$stop_name
   })
   output$jaunt_area <- renderText({
       req(values$closest_jaunt)
       values$closest_jaunt$name
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

