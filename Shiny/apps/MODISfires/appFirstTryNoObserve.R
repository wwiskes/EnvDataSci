#
library(shiny)
library(sf)
library(leaflet)
library(tidyverse)

# From:  https://fsapps.nwcg.gov/afm/gisdata.php

MODISfile <- "modis_fire_2020_283_conus.shp"   # cumulative 2020 (through day ___)
yrst <- str_sub(MODISfile,12,15)
endday <- as.numeric(str_sub(MODISfile,17,19))# dat <- as.Date(as.numeric(str_sub(MODISfile,17,19)), origin=str_c(yrst,"-01-01"))  
fires <- st_read(MODISfile)


pal <- colorNumeric(c("orange", "firebrick4"),domain=300:550)


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("MODIS fire detections"), 
    leafletOutput("view"),
    sliderInput(inputId = "numdays",
                label = "number of days",
                value = 7, min = 1, max = 14),
    sliderInput(inputId = "end_jday",
                label = "ending Julian day",
                value = endday, min = 1, max = endday)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$view <- renderLeaflet({
        dat <- as.Date(input$end_jday, origin=str_c(yrst,"-01-01"))
        leaflet(fires %>% filter(between(JULIAN, input$end_jday - input$numdays, input$end_jday))) %>%
            addProviderTiles(providers$Esri.WorldTopoMap) %>%
            fitBounds(-122.6,38.3,-122.3,38.7) %>%
            addCircleMarkers(
                radius = ~(TEMP-100)/100,
                color = ~pal(TEMP),
                stroke = FALSE, fillOpacity = 0.8) %>%
            addLegend("topright", pal=pal, values=~TEMP, opacity=0.6,
                      title=str_c("MODIS temps</br>",input$numdays, " days: ", dat))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


