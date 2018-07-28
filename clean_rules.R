#' ----
#' title: Clean JAUNT_ParaServiceAreaRules.xlsx
#' description: Go from an XLS mess to the tidy niceness.
#' author: nathancday@@gmail.com
#' date: 2018-07-28
#' -----

# JAUNT polygon data already in app
sf <- st_read("https://raw.githubusercontent.com/Smart-Cville/CID-2018-Regional-Transit-Challenge/master/data/doc.kml",
                    stringsAsFactors = F) %>%
    select(name = Name, geometry) %>%
    mutate(shape_id = 1:26) %>% # has CRS already
    st_zm() # drop Z dim (https://gis.stackexchange.com/questions/253898/adding-a-linestring-by-st-read-in-shiny-leaflet)

# JAUNT service rules (times, connections, etc); never parsed before :(
library(openxlsx)
rules <- read.xlsx("https://raw.githubusercontent.com/Smart-Cville/CID-2018-Regional-Transit-Challenge/master/data/JAUNT_ParaServiceAreaRules.xlsx",
                   detectDates = T)

### Joinability ---------------------------------------------------------

# all polygons in rules ( these should match 'sf$name')
tmp <- rules %>%
    select(FromPolyName, ToPolyName) %>%
    unlist() %>%
    unique()

tmp[!tmp %in% sf$name] # 3 are missing?
sf$name  # its typos, its always typos

# two in 'rules'
rules %<>%
    mutate_at(vars(FromPolyName, ToPolyName),
              ~ gsub("NELLYS FORD", "NELLYSFORD", .) %>%
                  gsub("29NORTH", "29 NORTH", .) )
# and one in 'sf'
sf %<>% mutate(name = gsub("LOUSIA COUNTY", "LOUISA COUNTY", name))

# recheck
tmp <- rules %>%
    select(FromPolyName, ToPolyName) %>%
    unlist() %>%
    unique()

tmp[!tmp %in% sf$name] # all better

### Time Cleanup ------------------------------------------------------------

# a messy anonymous function to handle time mopping; close your eyes
time_mop <- . %>%
    `*`(., 24) %>%
    str_split("\\.") %>%
    map(~ replace_na(c(.[1], as.numeric(paste0(".", .[2])) * 60), "00")) %>%
    map_chr(~ paste(.[1], .[2], sep = ":") %>%
                gsub("\\..*", "", .))
    
rules %<>%
    mutate_at(vars(FromTime:ReverseToTime),
              funs(time_mop))

head(rules)

### Weekly Schedule ---------------------------------------------------------

rules$WeekTemplate %>%
    nchar()
# confusing, asked Stephen 2018-07-28

### Finalize ------------------------------------------------------------

# keep only "key" columns; this may change
head(rules)
rules %<>%
    select(route_name = ParaServiceDescription,
           name = FromPolyName,
           to_name = ToPolyName,
           contains("Time")) %>%
    unite(morning_hours, c("FromTime", "ToTime"), sep = "-") %>%
    unite(evening_hours, c("ReverseFromTime", "ReverseToTime"), sep = "-")

# keep only first polygon-to-polygon route for now (and simplicity in prototyping)
rules %<>%
    group_by(route_name, name, to_name) %>%
    slice(1) %>%
    group_by()

# nest rules by name
# keep route options relative to starting polygon (does that make sense?)
rules %<>%
    nest(-name, .key = "routes")
map(rules$routes, class)

rules$routes

sf %>%
    inner_join(rules) %>%
    saveRDS("app_data.RDS")

