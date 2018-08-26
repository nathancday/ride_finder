# This app needs library(shiny) from "nathancday/shiny" fork (exposes datepicker params)
# devtools::install_github("nathancday/shiny", dep = F) # 'stable'
# The PR #2147 has been merged to master, but current build there is failing
# so until it recovers run off my fork with new functionality

# mobile friendly miniUI
# devtools::install_github("rstudio/miniui")


library(cpdcrimedata)
library(shiny)
library(shinyjs)
library(miniUI)
library(DT)
# library(gmapsdistance) # not being utilized yet
library(googleway)
library(googlesheets)
library(leaflet)
library(lwgeom) # required for sf::st_distance()
library(sf)
library(magrittr)
library(tidyverse)

set_key(
    "AIzaSyDzdGdfW6JDKTBgQRLzhSp1IIseVJVCWy8"
    )

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

#### UI ------------------------------------------------------------------

ui <- miniPage(
    useShinyjs(),
    tags$script(js),
    
    miniTitleBar("Welcome to Ride-Finder 🚌"),
    miniContentPanel(
        fillRow(
            textInput("address", NULL, NULL, placeholder = "Where are you?", width = "90%"),
            actionButton("address_btn", "📍"),
            height = "50px",
            flex = c(6,1)
        ),
        shinyjs::hidden(
            div(id = "request",
                style = "margin-top:5px;",
                actionButton("request_btn", "Request a pick up", class = "btn-danger btn-block")
            )
        ),
        # * request form --------
        shinyjs::hidden(
            wellPanel(
                id = "form",
                style = "margin-top:5px;",
                # google-ish form inputs
                # https://deanattali.com/2015/06/14/mimicking-google-form-shiny/#build-inputs
                textInput("email", "Email"),
                uiOutput("available_dates"),
                selectizeInput(
                    "prefered_contact",
                    "Prefered contact method:",
                    c("Email", "Phone", "Text")
                    ),
                splitLayout(
                    checkboxInput("terms", "I agree to terms"),
                    actionButton("submit", "Submit", class = "btn-primary")
                )
            )
        ),
        shinyjs::hidden(
            div(id = "thankyou_msg",
                h4("Thanks! Our team will be in touch to confirm your trip."),
                actionLink("submit_another", "Submit another request")
            )
        ),
        div(id = "table_map",
            style = "margin:auto;
                     width:95%;",
            DTOutput("routes", width = "90%"),
            leafletOutput("map", height = "200px", width = "90%")
            )
    )
)

## SERVER -------------------------------------------------------------------

server <- function(input, output, session) {
    
   values <- reactiveValues()
   
   # * geocode user input ---------------
   observeEvent(c(input[["keyPressed"]], input[["address_btn"]]), {
       
       print("Button pressed.")
       
       values$location <- input$address %>%
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
       
       shinyjs::show("request")
       
       # show("search_results")
   }, ignoreInit = T) 
   
   # start the map with tiles; not sure if this is smart
   output$map <- renderLeaflet({
       req(values$sf)
       req(values$location)
       
       leaflet() %>%
           addProviderTiles(provider = "OpenStreetMap.HOT") %>%
           addCircleMarkers(data = values$sf, radius = 20, popup = "you")
   })
   
   observe({
       x <- input$services
       req(values$sf)

      # Closest options ---------------------------------------------------
       jaunt_min_idx <- which.min(st_distance(values$sf, jaunt_sf))
       
       values$closest <- jaunt_sf[jaunt_min_idx,]
       
       # output$closest_access <- renderText({
       #     values$closest$name
       # })
       
       values$view <- values$closest %>%
           st_bbox() %>%
           unclass() %>%
           unname()
       
       leaflet() %>%
           addProviderTiles(provider = "OpenStreetMap.HOT") %>%
           addPolygons(data = values$closest, color = "red") %>%
           addCircleMarkers(data = values$sf, radius = 20, popup = "you") %>%
           fitBounds(values$view[1], values$view[2], values$view[3], values$view[4])
   })
   
   # * guessed location -------
   # output$location <- renderText({
   #     req(values$geocode)
   #     values$geocode$formatted_address
   #     })
   
   # * available routes table -----
   output$routes <- renderDT({
       req(values$closest)
       
       values$routes <- values$closest %>%
           unnest() %>%
           select(route_name, destination = to_name, inservice, disabled, contains("hours"))
       
       st_set_geometry(values$routes, NULL) %>%
           select(-disabled) %>%
           DT::datatable(selection = list(mode = "single", selected = 1),
                         options = list(dom = "t"),
                         rownames = F,
                         fillContainer = T
                         )
   })
   # watch DT for special id_rows_selected attribute
   observe({
       x <- input$routes_rows_selected
       req(input$routes_rows_selected)
       
       dest <- jaunt_sf[jaunt_sf$name == values$routes$destination[x],]
       days_disabled <- unlist(values$routes$disabled[x])
       
       output$available_dates <- renderUI({
           tagList(
               dateInput("ride_date", "Pick up date",
                         datesdisabled = "2018-12-25",
                         daysofweekdisabled = days_disabled
                         )
           )
       })
       
       view <- st_union(isolate(values$closest), dest) %>%
           st_bbox() %>%
           unclass() %>%
           unname()
       
       leafletProxy("map") %>%
           clearMarkers() %>%
           clearShapes() %>%
           addPolygons(data = values$closest, color = "red", label = ~name) %>%
           addPolygons(data = dest, color = "blue", label = ~name) %>%
           addCircleMarkers(data = values$sf, radius = 20, label = "you") %>%
           fitBounds(view[1], view[2], view[3], view[4])
       
       shinyjs::show("request")
       shinyjs::hide("thankyou_msg")
       shinyjs::hide("form")
   })
   
   observeEvent(input[["request_btn"]], {
       show("form")
       hide("request")
   })
   
   output$chosen_route <- renderText({
       values$routes$route_name[input$routes_rows_selected]
       })
   
   # form server logic
   fieldsAll <- c("email", "ride_date", "terms")
   responsesDir <- file.path("requests")
   epochTime <- function() {
       as.integer(Sys.time())
   }
   
   formData <- reactive({
       data <- map_chr(fieldsAll, ~ input[[.]]) %>%
           set_names(fieldsAll)
       data <- c(data,
                 # add in current app info
                 location = values$geocode$formatted_address,
                 route = values$routes$route_name[input$routes_rows_selected],
                 timestamp = epochTime())
       data
   })
   
   humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
   
   saveData <- function(data) {
       # Google Sheet seciont
       # https://shiny.rstudio.com/articles/persistent-data-storage.html#gsheets
       sheet <- gs_title("Ride_requests")
       gs_add_row(sheet, input = data)   
   }
   
   # action to take when submit button is pressed
   observeEvent(input$submit, {
       saveData(formData())
       shinyjs::reset("form")
       shinyjs::hide("form")
       shinyjs::show("thankyou_msg")
       hide("request")
   })
   observeEvent(input$submit_another, {
       shinyjs::show("form")
       shinyjs::hide("thankyou_msg")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
