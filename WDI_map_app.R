library(tidyverse)
library(leaflet)
library(dplyr)
library(sf)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)
library(viridis)
library(shiny)
library(shinyWidgets)
library(shinybusy)

# Reading the world polygon file using the "sf" library
World <- read_sf(dsn = "data-raw/ne_50m_admin_0_countries.shp") %>%
  select("ISO_A3", "ISO_A2", "SOVEREIGNT", "geometry") %>%
  rename("Country" = "SOVEREIGNT")

# Extracting the ISO A3 codes from the World file - for simplyfing the future merges of the master file
World_ISO <- World %>% 
  select("ISO_A3") %>%
  st_drop_geometry()

# Downloading the full WDI dataset and unzipping the WDIData.csv and WDIseries.csv files
# Put the .csv files into "data-raw" directory
download.file("http://databank.worldbank.org/data/download/WDI_csv.zip", "WDI_csv.zip")
  
unzip("WDI_csv.zip", files = c("WDIData.csv", "WDISeries.csv"), exdir = "data-raw")

# Reading the WDIData file
WDIData <- read_csv("data-raw/WDIData.csv")

# Reading the WDISeries file 
WDISeries <- read_csv("data-raw/WDISeries.csv")

# Converting a wide data set into a long one, renaming columns, removing empty column
WDI_data <- WDIData %>%
  pivot_longer(cols = "1960":"2019",
               names_to = "Year",
               values_to = "Value"
               ) %>%
  rename("ISO_A3" = "Country Code",
         "Indicator_code" = "Indicator Code",
         "Indicator_name" = "Indicator Name"
         ) %>%
  select("ISO_A3",
         "Indicator_code",
         "Indicator_name",
         "Year",
         "Value"
         )

# Subsetting the indicator names
WDI_defs <- WDI_data %>%
  select("Indicator_code",
         "Indicator_name") %>%
  unique() %>%
  arrange_all()

# Tidying up WDI_series file and creating/separating Topic from Subtopic by a first ":" character
WDI_series <- WDISeries %>%
  rename("Indicator_code" = "Series Code",
         "Definition" = "Long definition"
  ) %>%
  select("Indicator_code",
         "Topic",
         "Definition"
  ) %>%
  separate(Topic,
           into = c("Topic", "Subtopic"),
           sep =": ",
           extra = "merge"
           )

# Converting years into integers
WDI_data$Year <- as.integer(WDI_data$Year)

# Creating master text file with names, topics, subtopics and definitions
WDI_text <- merge(WDI_defs, WDI_series, by = "Indicator_code") %>%
  select("Indicator_name",
         "Indicator_code",
         "Topic",
         "Subtopic",
         "Definition")

# Extracting a topic list for the first selector
Topic_list <- WDI_text %>%
  select(Topic) %>%
  unique() %>%
  arrange_all()

# Creating a structured list of the colors for palette selection
Palette_tibble <- tibble(
  Pal_name = c("Sequential: Yellow-Green-Blue",
               "Sequential: Blue-Purple",
               "Sequential: Blues",
               "Sequential: Greens",
               "Sequential: Greys",
               "Sequential: Inferno",
               "Sequential: Magma",
               "Sequential: Orange-Red",
               "Sequential: Oranges",
               "Sequential: Plasma",
               "Sequential: Purple-Blue",
               "Sequential: Purple-Blue-Green",
               "Sequential: Purples",
               "Sequential: Reds",
               "Sequential: Viridis",
               "Sequential: Yellow-Green",
               "Sequential: Yellow-Orange-Red",
               "Diverging: Pink-Yellow-Green",
               "Diverging: Purple-Green",
               "Diverging: Purple-Orange",
               "Diverging: Red-Yellow-Green"
  ),
  Pal_value =c("YlGnBu",
               "BuPu",
               "Blues",
               "Greens",
               "Greys",
               "inferno",
               "magma",
               "OrRd",
               "Oranges",
               "plasma",
               "PuBu",
               "PuBuGn",
               "Purples",
               "Reds",
               "viridis",
               "YlGn",
               "YlOrRd",
               "PiYG",
               "PRGn",
               "PuOr",
               "RdYlGn"
  )
)

# Extracting palette names for using in a palette selector
Palette_list <- Palette_tibble$Pal_name

# Designing the UI with the separate CSS file
ui <- bootstrapPage(
  tags$head(
    includeCSS("styles.css")
    ),
  absolutePanel(top = 10, 
                left = 10, 
                draggable = T,
                style =
                "background-color: #E8E8E8;
                width: 310px;
                z-index: 500;
                opacity: 0.9;
                padding: 10px;
                box-shadow: 0 0 10px rgba(0,0,0,0.2);
                border-radius: 4px",
                h2("World Development Indicators"),
                selectizeInput("Sel_top", "Select a topic", 
                            choices = sort(unique(WDI_text$Topic))),
                selectizeInput("Sel_subtop", "Select a subtopic", 
                            choices = NULL),
                selectizeInput("Sel_indic", "Select an indicator", 
                            choices = NULL),
                selectizeInput("years",
                            label = "Select a year",
                            choices = NULL),
                selectizeInput("Choose_palette",
                               label = "Select color palette",
                               choices = Palette_list),
                radioButtons("Sel_pal_type", 
                             label = "Select a palette type",
                             choices = c("Continuous", 
                                         "Improved bins", 
                                         "Quantile"),
                             selected = "Continuous"),
                radioButtons("Sel_geo", 
                             label = "Select a map type",
                             choices = c("Regular",
                                         "Scaled (normalized)"),
                             selected = "Regular"
                             )
                ),
  leafletOutput("WDI_map",
                width = "100%",
                height = "100%"
                ),
  add_busy_spinner(spin = "fading-circle",
                   position = "full-page",
                   height = "75px", 
                   width = "75px",
                   color = "grey"
                   )
  )

server <- function (input, output, session) {

  # Topc selector  
  Top <- reactive({
    req(Topic_list)
    if (input$Sel_top == "NA") {
      filter(WDI_text, is.na(Topic))
    } else {
      filter(WDI_text, Topic == input$Sel_top) %>%
        arrange(Subtopic)
    }
    })
  
  # Subtopic selector
  Subtop <- reactive({
    req(input$Sel_subtop)
    if(input$Sel_subtop == "NA") {
      filter(WDI_text, is.na(Subtopic))
    } else {
      filter(WDI_text, Subtopic == input$Sel_subtop) %>%
        arrange(Indicator_name)
    }
    })
  
  # Indicator selector
  Indic <- reactive({
    req(input$Sel_indic)
    filter(Subtop(), Indicator_name == input$Sel_indic) %>%
      arrange_all()
  })

  # Filtering out the years without any data
  Year_check <- reactive({
    req(input$Sel_indic)
    Indic() %>%
    merge(WDI_data, by = "Indicator_name") %>% 
      filter(!is.na(Value)) %>%
      select(Year) %>% 
      unique() %>%
      arrange()
    })
  
  # Creating the master data file to plot the choropleth maps
  Indic_data <- reactive({
    req(Year_check())
    
    # Filtering the year as per input selector
    WDI_data_year <- filter(WDI_data, Year == input$years)
    
    # Merging all necessary data into one master file
    Master_data <- Indic() %>%
      merge(WDI_data_year, by = "Indicator_name") %>%
      merge(World_ISO, by = "ISO_A3") %>%
      merge(World, by = "ISO_A3") %>%
      drop_na() %>%
      st_as_sf()
    
    # Extracting the values for plotting
    Master_to_show <- Master_data$Value
    
    # Selecting a map type between regular an scaled (normalized)
    What_to_show <- if(input$Sel_geo == "Regular") {
      Master_data
      } else {
          # Extracting geometry and calculating the scale factor
          Geometry_what_to_show <- Master_data %>%
            st_geometry()
          Point_what_to_show <- st_point_on_surface(Geometry_what_to_show)
          Scale_factor <- Master_data$Value/max(Master_data$Value)
          
          # Changing the geometry as per scale factor
          Scaled_geo <- (Geometry_what_to_show - Point_what_to_show)*Scale_factor +
            Point_what_to_show
          
          # Rewriting the geometry column in a copy of What_to_show dataset with a scaled one
          What_to_show_scaled <- Master_data
          What_to_show_scaled$geometry <- Scaled_geo
          What_to_show_scaled$Value <- Scale_factor
          What_to_show_scaled
          }
    
    # Extracting a year number for showing on the legend, popups etc.
    Year_filter <- input$years
    
    # Selecting an indicator to draw a map
    Select_data <- input$Sel_indic
    
    # Extracting the indicator values
    Sel_data_show <- What_to_show$Value
    
    # Extracting coordinates for displaying the labels with numbers over the countries
    label_coord <- What_to_show %>%
      st_geometry() %>%
      st_point_on_surface() %>%
      st_coordinates() %>%
      as_tibble()
    
    #### Creating the improved bins palette for all possible value combinations ####
    # Extracting breakponts for a value range
    Breakpoints <- ggplot2:::breaks(Master_to_show, "number", 10)
    
    # Finding the max/min breakpoints
    Max_point <- max(Master_to_show, na.rm = T)
    Min_point <- min(Master_to_show, na.rm = T)
    
    # Finding the second highest value in breakpoints
    Second_to_max <- sort(Breakpoints, partial = length(Breakpoints) - 1)[length(Breakpoints) - 1]
    
    # Defining the data ranges
    Data_range_1 <- Second_to_max - Min_point
    Data_range_2 <- Max_point - Second_to_max
    
    Data_range_negative <- 0 - Min_point
    Data_range_positive <- Max_point - 0
    
    # Finding the scale ratio
    Scale_ratio_1 <- Second_to_max/Min_point
    Scale_ratio_2 <- Max_point/Second_to_max
    Sc_1_to_Sc_2 <- Scale_ratio_1/Scale_ratio_2
    
    # Getting the range steps for positive palettes
    Range_1_step_less <- Data_range_1/6
    Range_2_step_less <- Data_range_2/3
    Range_1_step_more <- Data_range_1/3
    Range_2_step_more <- Data_range_2/6
    
    # Getting the range steps for diverging palettes
    Range_neg_step = Data_range_negative/5
    Range_pos_step = Data_range_positive/5
    
    # Adjusting the breaks for positive palettes
    Breaks_adjusted_less <- c(Min_point,
                              Min_point + Range_1_step_less,
                              Min_point + Range_1_step_less*2,
                              Min_point + Range_1_step_less*3,
                              Min_point + Range_1_step_less*4,
                              Min_point + Range_1_step_less*5,
                              Min_point + Range_1_step_less*6,
                              Second_to_max + Range_2_step_less,
                              Second_to_max + Range_2_step_less*2,
                              Max_point)
    
    Breaks_adjusted_more <- c(Min_point,
                              Min_point + Range_1_step_more,
                              Min_point + Range_1_step_more*2,
                              Min_point + Range_1_step_more*3,
                              Second_to_max + Range_2_step_more,
                              Second_to_max + Range_2_step_more*2,
                              Second_to_max + Range_2_step_more*3,
                              Second_to_max + Range_2_step_more*4,
                              Second_to_max + Range_2_step_more*5,
                              Max_point)
    
    # Adjusting the breaks for diverging palettes
    Breaks_diverging <- c(Min_point,
                          Min_point + Range_neg_step,
                          Min_point + Range_neg_step*2,
                          Min_point + Range_neg_step*3,
                          Min_point + Range_neg_step*4,
                          Min_point + Range_neg_step*5,
                          0 + Range_pos_step,
                          0 + Range_pos_step*2,
                          0 + Range_pos_step*3,
                          0 + Range_pos_step*4,
                          0 + Range_pos_step*5)
    
    # Switching between the breaks depending on the scale ratio
    Breaks_adjusted <- if(Min_point < 0) {
      Breaks_diverging
    } else {
      if(Scale_ratio_2 < 20) {
        Breaks_adjusted_less
      } else {
        Breaks_adjusted_more
      }
    }
    
    # Selecting a color palette to draw a map
    # Advising on palette selection when data are diverging (ranging from negative to positive)
    Select_palette <- if(Min_point < 0 & !(input$Choose_palette %in% c("Diverging: Pink-Yellow-Green",
                                                                       "Diverging: Purple-Green",
                                                                       "Diverging: Red-Yellow-Green"
                                                                      )
                                           )
                         ) 
      {
      showNotification(paste0("For a better visualization of the '",
                              Select_data,
                              "', please select a diverging palette"
                              ),
                       duration = 5,
                       type = "message")
      filter(Palette_tibble,
             Pal_name == input$Choose_palette
      ) %>% 
        select(Pal_value) %>%
        as.character()
    } else {
      filter(Palette_tibble,
             Pal_name == input$Choose_palette
             ) %>% 
        select(Pal_value) %>% 
        as.character()
    }

    # Choosing a palette type between continuous/improved bins/quantile
    Palette_map <- if(input$Sel_pal_type == "Continuous") {
      colorNumeric(
        Select_palette, 
        Master_to_show)
      } else {
        if(input$Sel_pal_type == "Improved bins") {
        colorBin(
          Select_palette, 
          Master_to_show, 
          bins = Breaks_adjusted, 
          reverse = F
        ) } else {
    colorQuantile(
      Select_palette, 
      Master_to_show, 
      10
    )
          }
      }

    # Advising on switching to continuous palette when breaks in a data range are not unique
    ifelse(Max_point == Second_to_max & input$Sel_pal_type != "Continuous", 
           showNotification("Palette type is not compatible with selected data set;
                            Please select continuous palette",
                            duration = NULL,
                            type = "error"),
           "")
    
    # Advising on selecting a regular map type when displaying a diverging data
    ifelse(Min_point < 0 & input$Sel_geo == "Scaled",
      showNotification("Please select a regular map, otherwise countries representing negative values will appear mirrored & inverted",
                       duration = NULL,
                       type = "error"),
      "")
    
    # Creating labels for pop-ups with HTML
    Popups_map <- sprintf("<strong> %s, </strong> <br/>
                          %s <br/>
                          Year: %s, <br/>
                          Value: %s",
                          What_to_show$Country,
                          Select_data,
                          What_to_show$Year,
                          prettyNum(round(Master_data$Value, 1),
                                    scientific = F,
                                    big.mark = ",")
                          ) %>%
      lapply(htmltools::HTML)
    
    # Creating the legend with HTML
    Legend_title <- sprintf(
      "%s <br/>
      Year: %s <br/>",
      Select_data,
      Year_filter
      ) %>%
      lapply(htmltools::HTML)
    
    #### Defining the leaflet components as functions ####
    # Plotting the coloured countries with the values as per master data
    Polygons_map <- function(map){
      addPolygons(map, options = leafletOptions(pane = "Countries"),
                  group = "Countries",
                  smoothFactor = 0.2, fillOpacity = 1,
                  dashArray = "", color = "lightgray",
                  weight = 0.7, opacity = 0.7,
                  fillColor = ~Palette_map(Master_to_show),
                  highlight = highlightOptions(weight = 1.5,
                                               color = "darkgray",
                                               dashArray = "",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = Popups_map,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           "color" = "black",
                                                           "background-color" = "#E8E8E8",
                                                           "line-height" = "18px",
                                                           padding = "2px 2px"),
                                              textsize = "14px",
                                              opacity = 0.9,
                                              direction = "auto")
                  )
      }
    
    # Countries pane as a background (zIndex = 420)
    Countries_pane <- function(map, zIndex){
      addMapPane(map, "Countries", zIndex = 420)
    }
    
    # Names pane on top of countries (zIndex = 430)
    Names_pane <- function(map, zIndex){
      addMapPane(map, "Names", zIndex = 430)
    }
    
    # Tiles with no names as a background
    Provider_tiles_Nonames <- function(map){
      addProviderTiles(map, "Hydda.Base",
                       group = "Map tiles")
      }
    
    # Name tiles
    Provider_tiles_names <- function(map){
      addProviderTiles(map, "CartoDB.PositronOnlyLabels",
                       options = leafletOptions(pane = "Names"),
                       group = "Names on the map")
    }
    
    # Extracting the definitions/links and creating a text block for showing an indicator definition
    Definition_WDI <- WDI_text %>%
      filter(Indicator_name == input$Sel_indic) %>%
      select(Definition)
    
    Ind_code_WDI <- WDI_text %>%
      filter(Indicator_name == input$Sel_indic) %>%
      select(Indicator_code)
    
    Indic_link <- paste0("https://data.worldbank.org/indicator/", Ind_code_WDI)
   
    # Text block for a definition
    Def_control <- function(map){
      addControl(map,
      sprintf("<b> %s </b> <br/>
             <a href = %s> <b> <u> %s </a> </b> </u> </br>
            <b> %s </b> <br/>
            %s",
            "World Development Indicator:",
            Indic_link, Select_data,
            "Indicator definition: ",
            Definition_WDI
            ),
      position = "bottomleft",
      className = "definition",
      layerId = "Def")
      }
   
    # Creating the legend
    Legend_map <- function(map){
      addLegend(map, group = "Legend",
                pal = Palette_map,
                values = ~Master_to_show,
                labFormat = labelFormat(digits = 2),
                opacity = 1,
                title = Legend_title,
                position = "topright"
      )
    }
    
    # Creating the layers control
    Layer_control <- function(map){
      addLayersControl(map,
                       overlayGroups = c(
                         "Map tiles",
                         "Countries",
                         "Legend",
                         "Names on the map"
                       ),
                       options = layersControlOptions(collapsed = F),
                       position = "bottomright"
      )
    }
    
    #### Assembling the map from the code & functions blocks ####
    Map_viz <- leaflet(What_to_show,
                       options = leafletOptions(zoomControl = FALSE)) %>%
      setView(0, 33, 3) %>%
      Countries_pane %>% 
      Names_pane %>%
      Provider_tiles_Nonames %>%
      Provider_tiles_names %>%
      Polygons_map %>%
      Legend_map %>%
      Def_control %>%
      Layer_control %>%
      hideGroup("Names on the map")
      
    Map_viz
    
  })
  
  # Observing the palette selection
  updateSelectInput(
    session,
    "Choose_palette",
    choices = Palette_list
  )
  
  # Sending the map into ShinyApp
  output$WDI_map <- renderLeaflet(Indic_data())

  # Observing the topic selection
  observeEvent(Top(), {
    updateSelectizeInput(session, "Sel_subtop", choices = unique(Top()$Subtopic), selected = character())
    updateSelectizeInput(session, "Sel_indic", choices = unique(Subtop()$Indicator_name), selected = character())
  })
  
  # Observing the Subtopic selection
  observeEvent(Subtop(), {
    updateSelectizeInput(session, "Sel_indic", choices = unique(Subtop()$Indicator_name), selected = character())
  })
  
  # Observing the year selection
  observeEvent(Year_check(), {
    updateSelectizeInput(session, "years", choices = sort(Year_check()$Year))
  })
  
}

shinyApp(ui, server)