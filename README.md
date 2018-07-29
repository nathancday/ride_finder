## Ride Finder
#### A tool to locate the closest public transit options to you.
[Live Demo](https://nathanday.shinyapps.io/ride_finder/)

This is a [Shiny](https://shiny.rstudio.com/) app, designed to help riders utilize [JAUNT], an on demand tranist service for area surrounding Charlottesville, Virginia. A user can enter their location see a map with the route options before requesting a pick up.

This is part of the [CID 2018 Regional Transit Challenge](https://github.com/Smart-Cville/CID-2018-Regional-Transit-Challenge).

This project is under active development, if you want to get involved here are some things it could use:

* UI Enhancements
    + Front-end wizard to make the app sparkle for users
    + Shiny can incorporate [custom CSS](https://shiny.rstudio.com/articles/css.html)
    + And also [custom Javascript](https://shiny.rstudio.com/articles/packaging-javascript.html)
* Alternate distance calculations
    + Actual walking/biking distances to nearest transit access points (not a problem for JAUNT, but others maybe)
    + Currently the app uses `library(googleway)` to interact with [Google APIs](https://developers.google.com/maps/documentation/directions/start)
    + If you are trying implement new features like Google Directions, you will need a valid key of your own, which you can make [here](https://developers.google.com/maps/documentation/directions/get-api-key).
* Backend data management
    + "Ride requests" are being save to a [Google Sheet](https://docs.google.com/spreadsheets/d/1MNdUOi-l6A863MUwp-LwxxBEG71d4zf_NRJ-pTF5eh4/edit#gid=0) right now. Long term we will need a solution to pipe into JAUNT's existing systems.


