
# Setting -----------------------------------------------------------------

# Libraries 
library(shiny)
library(tidyverse)
library(shiny)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(fastmap) # this sometimes creates conflict  with the versions
library(sf)
library(leaflet)
library(leaflet.extras)
library(raster)
library (rsconnect)

# Working directory 
setwd("~/Desktop/Birds_Canada/1_JV_science_coordinator/Projects/e-bird/Project_Joint_Ventures_Canada_ebird/BirdsConservationTool_JV")

select<-dplyr::select


getwd()
raw_dataset<-read.csv("1_species_sorting_attributes_updated_long_format_2025.csv")


# Define UI for application that draws a histogram
ui <- navbarPage("Joint Ventures Conservation Tool.v5", theme = shinytheme("superhero"),
                 
                 ## UI: Birds Hotspots Panel
                 tabPanel("Birds hotspots",
                          sidebarLayout(
                            sidebarPanel(width=3,
                                         selectInput('JV_name',
                                                     label="Select a Joint Venture",
                                                     choices=sort(unique(raw_dataset$JV_name)),
                                                     selected="CIJV"),
                                         selectInput('bird_group',
                                                     label = "Bird group",
                                                     choices = sort(unique(raw_dataset$bird_group)),
                                                     selected ="all-bird-groups"), 
                                         radioButtons("priority", "JV priority", 
                                                      choices = list("all-species"="all-species-priority",
                                                                     "BC-JV-Priority"="BC-JV-Priority"), 
                                                      selected ="all-species-priority"),
                                         radioButtons("SAR", "Conservation status", 
                                                      choices = list("all-species"="all-species-conservation",
                                                                     "SpeciesAtRisk"="SAR"),
                                                      selected ="all-species-conservation"),
                                         radioButtons("seasonal", "Seasonal", 
                                                      choices = list("Seasonal"="seasonal",
                                                                     "Annual"="annual"), 
                                                      selected ="seasonal"),
                                         
                                         # 🔹 Dynamic UI output for season selection
                                         uiOutput("season_ui"),
                                         
                                         # 🔹 Dynamic UI output for estimate selection
                                         uiOutput("estimate_type_ui"),
                                         
                                         # 🔹 Opacity Slider
                                         sliderInput("opacity", "Opacity", min = 0, max = 1, value = 0.5, step = 0.2),
                                         
                                         tags$head(tags$style(".butt{background-color:#99CCCC;} .butt{color: black;font-style: italic;font-size:12px;}")),
                                         (HTML('</p>_____________________________________</p>')),
                                         (HTML('</p>This map shows the birds estimates in a 3km*3km area.</p>')),
                                         downloadLink('downloadData1', 'Download map (.jpg)', class = "butt"),
                                         downloadLink('downloadData2', 'Download raster', class = "butt"),
                                         downloadButton("downloadSpeciesList", "Download Species List"),
                                         (HTML('</p>_____________________________________</p>')),
                                         fluidRow(column(3, imageOutput("hotspotImage2")), 
                                                  column(3, imageOutput("hotspotImage3")),
                                                  column(3, imageOutput("hotspotImage1")))
                            ),
                            mainPanel(
                              h1(),
                              leafletOutput("leafletMap", width="100%", height="900px")
                            ) 
                          )  
                 ),
                 
                 ## UI: About Panel
                 tabPanel("About",
                          h3("The JV Conservation Tool is a web-based application designed to visualize eBird data within Joint Venture areas."),
                          br("This tool was developed to support conservation practitioners in decision-making..."),
                          br(),
                          uiOutput("tab"),
                          mainPanel(
                            fluidRow(
                              column(4, imageOutput("introImage1")),
                              column(4, imageOutput("introImage2")),
                              column(4, imageOutput("introImage3")),
                              h5("The JV Conservation tool was collaboratively coded by @JM"),
                              br()
                            )
                          )  
                 )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # 🔹 Dynamic UI for Season selection
  output$season_ui <- renderUI({
    if (input$seasonal == "seasonal") {
      radioButtons("season", "Season", 
                   choices = list("Breeding" = "breeding",
                                  "Nonbreeding" = "nonbreeding", 
                                  "Prebreeding" = "prebreeding",
                                  "Postbreeding" = "postbreeding"),
                   selected = "breeding")
    } else {
      return(NULL)  # Hide the Season selection when "Annual" is selected
    }
  })
  
  # 🔹 Dynamic UI for Estimate selection based on Seasonal input
  output$estimate_type_ui <- renderUI({
    if (input$seasonal == "annual") {
      radioButtons("estimate_type",
                   "Estimate:",
                   choices = list("relative_abundance (annual max)" = "relative abundance",
                                  "richness (annual max)" = "richness"),
                   selected = "max_relative_abundance")
    } else {
      radioButtons("estimate_type",
                   "Estimate:",
                   choices = list("Richness" = "richness",
                                  "Mean Relative abundance (Normalized) " = "normalized_mean_relative_abundance", 
                                  "Percentage population" = "percentage_population"),
                   selected = "richness")
    }
  })
  
  # 🔹 Function to construct the correct filename
  pickimage <- function(JV_name, bird_group, priority, SAR, season, estimate_type, seasonal) {
    if (seasonal == "annual") {
      season <- "annual-max"  # Replace season with "annual-max"
    }
    paste(JV_name, bird_group, priority, SAR, season, estimate_type, sep = "_")
  }
  
  # Reactive expression to filter dataset
  filtered_data <- reactive({
    raw_dataset %>%
      filter(
        JV_name == input$JV_name,
        bird_group == input$bird_group,
        priority == input$priority,
        SAR == input$SAR,
        (input$seasonal == "annual" | season == input$season)
      )
  })
  
  # Leaflet Map Rendering
  output$leafletMap <- renderLeaflet({
    # Pick the raster file based on input selections
    raster_name <- pickimage(input$JV_name, input$bird_group, input$priority, input$SAR, input$season, input$estimate_type, input$seasonal)
    raster_path <- file.path("output_stacked_all_rasters", paste0(raster_name, ".tif"))
    
    # Load the raster and downsample it 
    raster_data <- raster(raster_path)
    raster_data_downsampled <- aggregate(raster_data, fact = 1, fun = "max")
    
    # Load the geometry shapefile
    geometry_path <- file.path("geometry", paste0(input$JV_name, ".shp"))
    geometry_shape <- st_read(geometry_path)
    
    # Ensure the shapefile is transformed to WGS 84 (EPSG:4326) for Leaflet
    if (st_crs(geometry_shape) != st_crs(4326)) {
      geometry_shape <- st_transform(geometry_shape, crs = 4326)
    }
    
    # Define custom palette function
    custom_pal <- function(x) {
      ifelse(x == 0, "darkseagreen1", colorNumeric(c("#FFD700", "#c21807", "#110788"), x, na.color = "transparent")(x))
    }
    
    # Create the Leaflet map
    leaflet() %>%
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}", 
               options = tileOptions(opacity = 0.7)) %>%
      addRasterImage(raster_data_downsampled, colors = custom_pal, opacity = input$opacity) %>%  # 🔹 Opacity is now dynamic
      addPolygons(data = geometry_shape, color = "black", weight = 2, fill = FALSE, opacity = 1) %>%
      addLegend(pal = colorNumeric(c("darkseagreen1", "#FFD700", "#c21807", "#110788"), 
                                   values(raster_data_downsampled), na.color = "transparent"), 
                values = values(raster_data_downsampled), title = paste0(input$estimate_type)) %>%
      setView(lng = -122.1302, lat = 52.184, zoom = 5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
