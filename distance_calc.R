cat <- read.table(file="https://raw.githubusercontent.com/Smart-Cville/CID-2018-Regional-Transit-Challenge/master/data/CAT_2017_08_GTFS/stops.txt", 
                     sep=",", header = T, stringsAsFactors = F) %>%
    select(stop_name, stop_lon, stop_lat) %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"))

sf <- readRDS("tst_loc.RDS")

closest <- cat[which.min(st_distance(sf, cat)),]


jaunt_sf <- st_read("https://raw.githubusercontent.com/Smart-Cville/CID-2018-Regional-Transit-Challenge/master/data/doc.kml") %>%
    select(name = Name, geometry) %>%
    mutate(shape_id = 1:26) %>%
    slice(1:26)

sf %<>%
    st_set_crs(st_crs(jaunt_sf))

jsf <- jaunt_sf[which.min(st_distance(sf, jaunt_sf)),]


leaflet() %>%
    addPolygons(data = jsf, options = NULL)

mapview(jsf)

library(gmapdistance) # https://cran.r-project.org/web/packages/gmapsdistance/README.html
# collapse to points format
as_gmap <- . %>%
    st_geometry() %>%
    unlist() %>%
    rev %>%
    paste(collapse = "+")
as_gmap(sf)
as_gmap(closest)

results <- gmapsdistance(origin = as_gmap(sf),
                         destination = as_gmap(closest),
                         mode = "walking")
# reports Time(s), Distance(m), Status(bool)
