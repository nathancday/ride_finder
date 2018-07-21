
library(cpdcrimedata)
library(shiny)
library(ggmap)
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
                     sep=",", header = T) %>%
    select(stop_name, stop_lon, stop_lat) %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"))


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tags$script(js),
    
    textInput("address", "Where are you?", "600 E Market St"),
    h5("This is where I think you are,"),
    textOutput("location"),
    h2("Ignore the rest of this for now"),
    dataTableOutput("geocodes")
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
           mutate(extracted = map(geocode, extract_geocode)) %>%
           unnest(extracted)
   })
   
   output$location <- renderText(values$location)
   
   output$geocodes <- renderDataTable(values$geocode)
   
}

# Run the application 
shinyApp(ui = ui, server = server)

