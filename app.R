
library(cpdcrimedata)
library(shiny)
library(DT)
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
    slice(-82) # bad row???

jaunt_sf <- readRDS("app_data.RDS")

cat_sf %<>% st_set_crs(st_crs(jaunt_sf))


# Define UI for application that draws a histogram
ui <- fluidPage(
                 
    tags$script(js),
    
    sidebarLayout(
        sidebarPanel(
            h4("Welcome to JAUNT Ride-Finder"),
            h5("Where are you?"),
            textInput("address", NULL, "Louisa Airport"),
            p("^^ Press ENTER ^^"),
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
            DTOutput("routes"),
            leafletOutput("map", height = 600)
        )
    )
    
)

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
       req(values$sf)

      # Closest options ---------------------------------------------------
       if ("CAT" == x) {
           min_idx <- which.min(st_distance(values$sf, cat_sf))
           
           values$closest <- cat_sf[min_idx,]
           values$remain <- cat_sf[min_idx,]
           
           output$closest_access <- renderText({
               values$closest_cat$stop_name
           })
           
           # calculate a bounding box; pretty map view default
           values$view <- st_union(values$sf, values$closest) %>%
               st_bbox() %>%
               unclass() %>%
               unname()
           
           # build via leafletProxy() for speed; tiles are always down :)
           leafletProxy("map") %>%
               clearShapes() %>%
               addCircleMarkers(data = values$closest_cat, radius = 20, color = "green") %>%
               addCircleMarkers(data = values$remain_cat, radius = 5, color = "green") %>%
               fitBounds(values$view[1], values$view[2], values$view[3], values$view[4])
       }
       # in-ellegant but effective RY; perhaps re-factor to DRY?
       if ("JAUNT" == input$services) {
           req(values$sf)
           
           jaunt_min_idx <- which.min(st_distance(values$sf, jaunt_sf))
           
           values$closest <- jaunt_sf[jaunt_min_idx,]
           
           output$closest_access <- renderText({
               values$closest$name
           })
           
           values$view <- values$closest %>%
               st_bbox() %>%
               unclass() %>%
               unname()
           
           leafletProxy("map") %>%
               clearMarkers() %>%
               addPolygons(data = values$closest, color = "red") %>%
               addCircleMarkers(data = values$sf, radius = 20) %>%
               fitBounds(values$view[1], values$view[2], values$view[3], values$view[4])
       }
       
   })
   
   # * geo-coded location -------
   output$location <- renderText({
       req(values$geocode)
       values$geocode$formatted_address
       })
   
   # * available routes table -----
   output$routes <- renderDT({
       req(values$closest)
       values$closest %>%
           unnest() %>%
           select(route_name, destination = to_name, contains("hours")) %>%
           st_set_geometry(NULL) %>%
           DT::datatable(options = list(dom = "t"))
           
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

