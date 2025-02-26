# Load necessary libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(raster)
library(sf)
library(dplyr)
library(viridis)  # For plasma color scale

# Define UI for application
# Load necessary libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(raster)
library(sf)
library(dplyr)
library(viridis)  # For plasma color scale

# Load necessary libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(raster)
library(sf)
library(dplyr)
library(viridis)  # For plasma color scale

# Define UI for application
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
                                         sliderInput("opacity", "Raster Opacity", min = 0, max = 1, value = 0.4, step = 0.1),
                                         
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
                 )
)

# Define server logic
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
      season <- "annual-max"  
    }
    paste(JV_name, bird_group, priority, SAR, season, estimate_type, sep = "_")
  }
  
  # Leaflet Map Rendering with Updated Color Palette
  output$leafletMap <- renderLeaflet({
    raster_name <- pickimage(input$JV_name, input$bird_group, input$priority, input$SAR, input$season, input$estimate_type, input$seasonal)
    raster_path <- file.path("output_stacked_all_rasters", paste0(raster_name, ".tif"))
    
    raster_data <- raster(raster_path)
    raster_data_downsampled <- aggregate(raster_data, fact = 1, fun = "max")
    
    raster_min <- minValue(raster_data_downsampled)
    raster_max <- maxValue(raster_data_downsampled)
    
    if (is.na(raster_min) | is.na(raster_max)) {
      raster_min <- 0
      raster_max <- 1
    }
    
    # Generate bins dynamically
    num_bins <- 10  # Adjust as needed
    bins <- seq(0, raster_max, length.out = num_bins + 1)
    bins1 <- bins[!duplicated(bins)]  # Ensure unique bins
    bins2 <- unique(c(0, 1, bins1))
    
    # Define qcol palette
    qcol <- c("#6E776B",  # Color for zero values
              "#cacf29",           # Color for value = 1
              rev(plasma(length(bins2) - 2, end = 0.9)))  # Plasma for remaining bins
    
    # Create color mapping
    color_pal <- colorBin(palette = qcol, domain = c(raster_min, raster_max), bins = bins2, na.color = "transparent")
    
    # Load the geometry shapefile
    geometry_path <- file.path("geometry", paste0(input$JV_name, ".shp"))
    geometry_shape <- st_read(geometry_path)
    
    if (st_crs(geometry_shape) != st_crs(4326)) {
      geometry_shape <- st_transform(geometry_shape, crs = 4326)
    }
    
    # Create the Leaflet map
    leaflet() %>%
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}", 
               options = tileOptions(opacity = 0.7)) %>%
      addRasterImage(raster_data_downsampled, colors = color_pal, opacity = input$opacity) %>%  
      addPolygons(data = geometry_shape, color = "black", weight = 2, fill = FALSE, opacity = 1) %>%
      addLegend(pal = color_pal, values = bins2, title = paste0(input$estimate_type)) %>%
      setView(lng = -122.1302, lat = 52.184, zoom = 5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

