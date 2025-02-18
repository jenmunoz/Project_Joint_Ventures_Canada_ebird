library(tidyverse)
library(shiny)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(fastmap) # this sometimes creates conflict  with the versions
library(leaflet)
library(sf)
library(leaflet.extras)
library(raster)
library (rsconnect)
# 




raster_path<-file.path("/Users/jennymunoz/Desktop/2_Birds_Canada_Contract_2024/Projects/e-bird/new_Testing_shiny_app_birds/output_stacked_all_rasters/PacificBirds_landbirds_BC-JV-Priority_all-species-conservation_breeding_richness.tif")
raster_path <- file.path("/Users/jennymunoz/Desktop/2_Birds_Canada_Contract_2024/Projects/e-bird/new_Testing_shiny_app_birds/output_stacked_all_rasters/CIJV_waterfowl_no-priority_no-SAR_postbreeding_richness.tif") # # sometimes this can create issues so alternatively use the whole raster path if running into problems

# Load the raster file
raster_data <- raster(raster_path)

plot(raster_data)

# Create a custom color function
custom_pal <- function(x) {
  ifelse(x == 0, "darkseagreen1", colorNumeric(c("#FFD700", "#c21807", "#110788"), x, na.color = "transparent")(x))
}

leaflet() %>%
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png", options = tileOptions(opacity = 0.7)) %>%
  addRasterImage(raster_data, colors = custom_pal, opacity = 0.6, rotate(raster_data)) %>%
  addLegend(pal = colorNumeric(c("darkseagreen1", "#FFD700", "#c21807", "#110788"), values(raster_data), na.color = "transparent"), 
            values = values(raster_data), title ="null") %>%
  setView(lng = -122.1302, lat =  52.184, zoom = 4)  # Set the initial view to a specific location

####

raster_data_downsampled <- aggregate(raster_data, fact = 2)  # Adjust 'fact' as needed


# Now plot using leaflet
leaflet() %>%
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png", options = tileOptions(opacity = 0.7)) %>%
  addRasterImage(raster_data_downsampled, colors = custom_pal, opacity = 0.6) %>%
  addLegend(pal = colorNumeric(c("darkseagreen1", "#FFD700", "#c21807", "#110788"), values(raster_data_projected), na.color = "transparent"), 
            values = values(raster_data_projected), title = "null") %>%
  setView(lng = -122.1302, lat = 52.184, zoom = 4)
