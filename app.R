
library(cpdcrimedata)
library(shiny)
library(DT)
# library(gmapsdistance) # not being utilized yet
library(googleway)
library(leaflet)
library(lwgeom) # required for sf::st_distance()
library(sf)
library(magrittr)
library(tidyverse)

set_key(Sys.getenv(("your_key")))


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

# make CRS match for distance calculations
cat_sf %<>% st_set_crs(st_crs(jaunt_sf))


# Define UI for application that draws a histogram
ui <- fluidPage(
                 
    tags$script(js),
    
    sidebarLayout(
        sidebarPanel(
            h4("Welcome to Ride-Finder 🚌"),
            h5("Where are you? 📍 (Press ENTER to search)"),
            textInput("address", NULL, "Louisa Airport"),
            h5("This is where the computer thinks you are:"),
            textOutput("location"),
            h5("So the closest service area to you is:"),
            textOutput("closest_access"),
            h5("Selected route:"),
            textOutput("chosen_route"),
            textOutput("tst")
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
   })
   
   # * geo-coded location -------
   output$location <- renderText({
       req(values$geocode)
       values$geocode$formatted_address
       })
   
   # * available routes table -----
   output$routes <- renderDT({
       req(values$closest)
       
       values$routes <- values$closest %>%
           unnest() %>%
           select(route_name, destination = to_name, contains("hours"))
       
       st_set_geometry(values$routes, NULL) %>%
           DT::datatable(caption = "Available routes:",
                         selection = list(mode = "single", selected = 1),
                         options = list(dom = "t"))
   })
   
   observe({
       x <- input$routes_rows_selected
       req(input$routes_rows_selected)
       
       dest <- jaunt_sf[jaunt_sf$name == values$routes$destination[x],]
       
       view <- st_union(isolate(values$closest), dest) %>%
           st_bbox() %>%
           unclass() %>%
           unname()
       
       print(st_union(values$closest, dest))
       
       leafletProxy("map") %>%
           clearMarkers() %>%
           clearShapes() %>%
           addPolygons(data = values$closest, color = "red") %>%
           addPolygons(data = dest, color = "blue") %>%
           addCircleMarkers(data = values$sf, radius = 20) %>%
           fitBounds(view[1], view[2], view[3], view[4])
   })
   
   output$chosen_route <- renderText({
       values$routes$route_name[input$routes_rows_selected]
       })
}

# Run the application 
shinyApp(ui = ui, server = server)

