# WDI_map_app

### World Development Indicators on a universal choropleth map

The [World Development Indicators](http://datatopics.worldbank.org/world-development-indicators/) are a compilation of relevant, high-quality, and internationally comparable statistics compiled by the World Bank from officially recognized international sources. The database contains 1,600 time series indicators for 217 economies and more than 40 country groups, with data for many indicators going back more than 50 years.

#### While the [WDI web site](https://data.worldbank.org/indicator) provides some basic map visualization, the WDI_map_app extends the visual appeal of the maps by offering a simplified procedure to extract the indicator and representing the data on a choropleth map with a lot of additional options. Therefore, the main purpose of the app is to serve as a one-stop shop for visualizing the vast array of the World Development Indicators using a *choropleth map*. 

### The app features:
*	One universal tool for all indicators regardless their type
*	Interdependent filtering by topic, up to 2 subtopics and year
*	Automatic filtering out the years without any data
*	Extensive palette color selection between sequential and diverging color sets
*	Palette type selection between continuous, improved bins (with a proprietary algorithm) and quantile ones
*	Map type selection between regular and scaled (normalized cartogram-like)

Big thanks to all relevant sources of code, such as [Stack Overflow](https://stackoverflow.com/), [Leaflet for R](https://rstudio.github.io/leaflet/),  [Geocomputation with R](https://geocompr.robinlovelace.net/adv-map.html) and [Garrett & Hadley](https://r4ds.had.co.nz/).

Please note that some code glitches might be placeholders for future app development.

Here is the direct link to the WDI map app on the shinyapp.io:

https://ksimi.shinyapps.io/WDI_map_app/
