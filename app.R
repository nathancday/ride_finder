
library(cpdcrimedata)
library(shiny)
library(ggmap)
library(leaflet)
library(sf)
library(magrittr)
library(tidyverse)


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
            h4("And this is the closest CAT stop to you,"),
            textOutput("cat_stop")
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
           re_geocode(retry = 10) %>%
           mutate(extracted = map(geocode, extract_geocode)) %>%
           unnest(extracted) %>%
           select(-matches("geocode"))
       
       values$sf <- values$geocode %>%
           mutate_at(vars(lon, lat), as.numeric) %>%
           st_as_sf(coords = c("lon", "lat"))
       
       values$closest <- cat_sf[which.min(st_distance(values$sf, cat_sf)),]
   })
   
   output$location <- renderText(values$geocode$formatted_address)
   output$cat_stop <- renderText({
       req(values$closest)
       values$closest$stop_name
   })
   
   output$map <- renderLeaflet({
       
       req(values$sf)
       req(values$location)
       
       values$sf %>%
           leaflet() %>%
           addTiles() %>%
           addCircleMarkers(radius = 20) %>%
           addCircleMarkers(data = values$closest, radius = 20, color = "green")
       })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

