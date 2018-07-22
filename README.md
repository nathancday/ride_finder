## Ride Finder
#### A tool to locate the closest public transit options to you.

This is a Shiny app, designed as a prototype for a unified public transit front end covering the Charlottesville Virginia metro area. A user can enter a address, press ENTER, and get a map back with their location and the closest transit options highlighted.

This is part of the [CID 2018 Regional Transit Challenge](https://github.com/Smart-Cville/CID-2018-Regional-Transit-Challenge).

[Live Version](https://github.com/Smart-Cville/CID-2018-Regional-Transit-Challenge)
Current status: The app will find the closest (as the crow flies) CAT stop only. JAUNT polygons coming soon

This project is under active development, if you want to get involved here are some things it could use:

* JAUNT service region calculations
    + Data is available as polygons that can be added to the current map
    + See [this R file](https://github.com/Smart-Cville/CID-2018-Regional-Transit-Challenge/blob/master/jaunt_leaflet.R)
* UI Enhancements
    + Make the app look cooler and act cooler
    + Shiny can use [custom CSS](https://shiny.rstudio.com/articles/css.html)
    + And [custom Javascript](https://shiny.rstudio.com/articles/packaging-javascript.html)
* Alternate distance calculations
    + Get actual walking/biking distances to nearest transit access points
    + The [Google Directions API](https://developers.google.com/maps/documentation/directions/start) has this
    + Currently the app uses `library(googleway)` to interact with Google APIs
    
Notes:
For security purposes this app does not include a Google API key, you can make your own [here](https://developers.google.com/maps/documentation/directions/get-api-key). If you are trying implement additional features like Google Directions, you will need a valid key.


