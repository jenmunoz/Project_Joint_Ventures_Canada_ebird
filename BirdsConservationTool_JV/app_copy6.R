# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# The link for the app https://conservationtools.shinyapps.io/Testing_shiny_app_birds/

# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("shinydashboard")
# install.packages("fastmap")
# install.packages("leaflet")
# install.packages("sf")
# install.packages("leaflet.extras")
# install.packages("raster")
# install.packages("tidyverse")

library(tidyverse)
library(shiny)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(fastmap) # this sometimes creates conflict  with the versions
library(leaflet)
library(sf)
library(leaflet)
library(leaflet.extras)
library(raster)
library (rsconnect)
# 
# Data-----------
setwd("~/Desktop/2_Birds_Canada_Contract_2024/Projects/e-bird/new_Testing_shiny_app_birds")

raw_dataset<-read.csv("data_structure_dashboardv2.csv")
raw_dataset2<-read.csv("data_structure_dashboardv2_panel2.csv")
raw_dataset<-read.csv("/Users/jennymunoz/Desktop/new_dashboard/data/general_data/1_species_sorting_attributes_updated_long_format_2025.csv") 

unique(raw_dataset$bird_group) 
unique(raw_dataset$SAR)
unique(raw_dataset$season)
unique(raw_dataset$estimate_type)
unique(raw_dataset$priority)

library(shiny)
library(shinythemes)
library(dplyr)
library(leaflet)
library(raster)
library(RColorBrewer)


library(shiny)
library(shinythemes)
library(dplyr)
library(leaflet)
library(raster)
library(RColorBrewer)

ui = navbarPage("Joint Ventures Conservation Tool.v4", theme = shinytheme("superhero"),
                
                ##  ui Richness ----------------------------
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
                                                     choices = list("all-species-priority"="all-species-priority",
                                                                    "BC-JV-Priority"="BC-JV-Priority"), 
                                                     selected ="all-species-priority"),
                                        radioButtons("SAR", "Conservation status", 
                                                     choices = sort(unique(raw_dataset$SAR)),
                                                     selected ="all-species-conservation"),
                                        radioButtons("seasonal", "Seasonal", 
                                                     choices = list("seasonal"="seasonal",
                                                                    "annual"="annual"), 
                                                     selected ="seasonal"),
                                        
                                        # 🔹 Dynamic UI output for season selection
                                        uiOutput("season_ui"),
                                        
                                        radioButtons("estimate_type",
                                                     "Estimate:",
                                                     choices = list("richness"="richness",
                                                                    "normalized_mean_relative_abundance"="normalized mean relative abundance", 
                                                                    "max_relative_abundance"="max relative abundance",
                                                                    "percentage_population"="percentage_population"),
                                                     selected ="richness"),
                                        tags$head(tags$style(".butt{background-color:#99CCCC;} .butt{color: black;font-style: italic;font-size:12px;}")),
                                        (HTML('</p>_____________________________________</p>')),
                                        (HTML('</p>This map shows the birds estimates in a 3km*3km area.</p>')),
                                        downloadLink('downloadData1', 'Download map (.jpg)', class = "butt"),
                                        downloadLink('downloadData2', 'Download raster', class = "butt"),
                                        downloadButton("downloadSpeciesList", "Download Species List"),
                                        (HTML('</p>_____________________________________</p>')),
                                        fluidRow(column(3, imageOutput("hotspotImage2")), 
                                                 column(3, imageOutput("hotspotImage3")),
                                                 column(3, imageOutput("hotspotImage1")))),
                           mainPanel(h1(),
                                     leafletOutput("leafletMap", width="100%", height="900px")  ) )  ),
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
                             br()   ))   ))

###_###_###_####_###_###
# Server -----
####_####_###_###_##_#_#

server <- function(input, output, session) {
  
  # 🔹 Dynamic UI for Season based on Seasonal selection
  output$season_ui <- renderUI({
    if (input$seasonal == "seasonal") {
      radioButtons("season", "Season", 
                   choices = list("Breeding" = "breeding",
                                  "Nonbreeding" = "nonbreeding", 
                                  "Prebreeding" = "prebreeding",
                                  "Postbreeding" = "postbreeding"),
                   selected = "breeding")
    } else {
      return(NULL)  # Hides the Season selection when "Annual" is selected
    }
  })
  
  # 🔹 Function to construct the correct filename
  pickimage <- function(JV_name, bird_group, priority, SAR, season, estimate_type, seasonal) {
    if (seasonal == "annual") {
      season <- "annual-max"  # 🔹 Use "annual-max" instead of a season name
    }
    
    # Construct the filename
    filename <- paste(JV_name, bird_group, priority, SAR, season, estimate_type, sep = "_")
    return(filename)
  }
  
  # Reactive expression to filter dataset
  filtered_data <- reactive({
    raw_dataset %>%
      filter(
        JV_name == input$JV_name,
        bird_group == input$bird_group,
        priority == input$priority,
        SAR == input$SAR,
        (input$seasonal == "annual" | season == input$season) ) 
  }) #If input$seasonal == "annual", this condition always returns TRUE, meaning the season column is effectively ignored Otherwise, it ensures that only rows where season == input$season are kept.
  
  # Download handler for the species list
  output$downloadSpeciesList <- downloadHandler(
    filename = function() {
      paste("Species_list_", input$JV_name, input$bird_group, input$priority,input$SAR, input$season, ".csv", sep = "_")
      },
    content = function(file) {
      # Ensure that filtered_data() has valid data before proceeding
      req(nrow(filtered_data()) > 0)
      
      species_list <- filtered_data() %>%
        dplyr::select(common_name,species_code, scientific_name,bird_group,priority,SAR,season) %>%
        dplyr::distinct(common_name,scientific_name,bird_group,priority,SAR)
      write.csv(species_list, file, row.names = FALSE) } )
  
  #  Leaflet Map Rendering
  output$leafletMap <- renderLeaflet({
    raster_name <- pickimage(input$JV_name, input$bird_group, input$priority, input$SAR, input$season, input$estimate_type, input$seasonal)
    raster_path <- file.path("output_stacked_all_rasters", paste0(raster_name, ".tif"))
    
    raster_data <- raster(raster_path)
    raster_data_downsampled <- aggregate(raster_data, fact = 2)
    
    custom_pal <- function(x) {
      ifelse(x == 0, "darkseagreen1", colorNumeric(c("#FFD700", "#c21807", "#110788"), x, na.color = "transparent")(x))
    }
    
    leaflet() %>%
      addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png", options = tileOptions(opacity = 0.7)) %>%
      addRasterImage(raster_data_downsampled, colors = custom_pal, opacity = 0.6) %>%
      addLegend(pal = colorNumeric(c("darkseagreen1", "#FFD700", "#c21807", "#110788"), values(raster_data), na.color = "transparent"), 
                values = values(raster_data_downsampled), title = input$estimate_type) %>%
      setView(lng = -122.1302, lat =  52.184, zoom = 3)
  })
  # Download handler for the plot (png)
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste(pickimage(input$JV_name, input$bird_group, input$priority, input$SAR, input$season, input$estimate_type, input$seasonal), ".png", sep = "")
    },
    content = function(file) {
      file.copy(
        normalizePath(file.path('./maps', paste(pickimage(input$JV_name, input$bird_group, input$priority, input$SAR, input$season, input$estimate_type, input$seasonal), '.png', sep = ''))), 
        file
      ) })
  
  # Download handler for the raster file
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste(pickimage(input$JV_name, input$bird_group, input$priority, input$SAR, input$season, input$estimate_type, input$seasonal), ".tif", sep = "")
    },
    content = function(file) {
      file.copy(
        normalizePath(file.path('./output_stacked_all_rasters', paste(pickimage(input$JV_name, input$bird_group, input$priority, input$SAR, input$season, input$estimate_type, input$seasonal), '.tif', sep = ''))), 
        file
      ) })
}

shinyApp(ui, server)

###_###_###_####_###_###
# To publish the app online -----------------------------------------------
###_###_###_####_###_###
file.exists("/Users/jennymunoz/Desktop/2_Birds_Canada_Contract_2024/Projects/e-bird/new_Testing_shiny_app_birds/output_stacked/CIJV_allbirdgroups_allspeciespriority_allspeciesconservation_breeding_richness.tif")

# the instruction are in this page 
#https://www.shinyapps.io/admin/#/dashboard
# My account is associated with my gmail email jen.munnoz@gmail.com
# The password in Solecito27
# tokens are saved there too

#There are two option run the app and pibliches form teh R studio console or running the following
#runApp()
#rsconnect::deployApp()



### somme notes if i need to do multiple selection of thin