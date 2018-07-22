
library(cpdcrimedata)
library(shiny)
# library(gmapsdistance)
library(googleway)
library(leaflet)
library(sf)
library(magrittr)
library(tidyverse)

set_key("AIzaSyDlERX-_Ii1n9rZdYgAhxOY9l7cr8SX2jE")


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
    st_as_sf(coords = c("stop_lon", "stop_lat"))

jaunt_sf <- st_read("https://raw.githubusercontent.com/Smart-Cville/CID-2018-Regional-Transit-Challenge/master/data/doc.kml",
                    stringsAsFactors = F) %>%
    select(name = Name, geometry) %>%
    mutate(shape_id = 1:26) # has CRS already

cat_sf %<>% st_set_crs(st_crs(jaunt_sf))


# Define UI for application that draws a histogram
ui <- fluidPage(
                 
    tags$script(js),
    
    # https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/ui.R
    sidebarLayout(
        sidebarPanel(
            h4("Welcome to Ride-Finder, where are you?"),
            textInput("address", NULL, "600 E Market St"),
            h4("This is where I think you are,"),
            textOutput("location"),
            h4("And this is the closest CAT stop,"),
            textOutput("cat_stop"),
            h4("And this is the closes JAUNT service area,"),
            textOutput("jaunt_area")
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
           paste("Charlottesville VA")
       
       values$geocode <- tibble(address = values$location) %>%
           re_geocode() %>%
           filter(geocode_good) %>%
           mutate(extracted = map(geocode, extract_geocode)) %>%
           select(-matches("geocode")) %>%
           unnest(extracted)
       
       values$sf <- values$geocode %>%
           mutate_at(vars(lon, lat), as.numeric) %>%
           st_as_sf(coords = c("lon", "lat"), crs = st_crs(jaunt_sf))
       
       ### Closest options ---------------------------------------------------
       cat_min_idx <- which.min(st_distance(values$sf, cat_sf))
       values$closest_cat <- cat_sf[cat_min_idx,]
       values$remain_cat <- cat_sf[-cat_min_idx,]
       
       jaunt_min_idx <- which.min(st_distance(values$sf, jaunt_sf))
       values$closest_jaunt <- jaunt_sf[jaunt_min_idx,]
       
       values$view <- st_union(values$sf, values$closest_cat) %>%
           st_bbox() %>%
           unclass() %>%
           unname()
   })
   
   output$location <- renderText({
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
   
   ### Leaflet -------------------------------------------------------------
   output$map <- renderLeaflet({
       
       req(values$sf)
       req(values$location)
       
       mapview(values$closest_jaunt, color = "red", col.regions = "red", alpha.regions = .2)@map %>%
           addCircleMarkers(data = values$remain_cat, radius = 5, color = "green") %>%
           addCircleMarkers(data = values$sf, radius = 20) %>%
           addCircleMarkers(data = values$closest_cat, radius = 20, color = "green") %>%
           fitBounds(values$view[1], values$view[2], values$view[3], values$view[4])
       })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

