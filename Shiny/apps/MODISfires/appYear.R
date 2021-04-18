#
library(shiny)
library(sf)
library(leaflet)
library(tidyverse)

#txt <- read_file("https://fsapps.nwcg.gov/afm/data/fireptdata/modisfire_2020_conus.htm")
#shpPath <- str_extract(txt, "https://fsapps.nwcg.gov/afm/data/fireptdata/modis_fire_.........conus_shapefile.zip")
#shpZip <- str_extract(shpPath, "modis_fire_.........conus_shapefile.zip")
#download.file(shpPath, str_c(getwd(),"/",shpZip))
#unzip(shpZip)
 
#MODISfile <- str_c(str_extract(shpZip, "modis_fire_.........conus"),".shp")
#yrst <- str_sub(MODISfile,12,15)    # Something like "modis_fire_2020_284_conus.shp
#fires <- st_read(MODISfile)       
endday <- 180 #max(fires$JULIAN)
pal <- colorNumeric(c("orange", "firebrick4"),domain=300:550)

ui <- fluidPage(
    titlePanel("MODIS fire detections"), 
    leafletOutput("view"),
    sliderInput(inputId = "numdays",
                label = "number of days",
                value = 3, min = 1, max = 14),
    sliderInput(inputId = "end_jday",
                label = "ending Julian day",
                value = 180, min = 1, max = 365, step = 1),
    sliderInput(inputId = "year",
                label = "year",
                value = 2020, min = 2009, max = 2020, step = 1, sep = "")
)

server <- function(input, output, session) {
    output$view <- renderLeaflet({ # Nothing reactive in here, so only runs initially
        yrst <<- as.character(input$year)
        txt <- read_file(str_c("https://fsapps.nwcg.gov/afm/data/fireptdata/modisfire_",yrst,"_conus.htm"))
        shpPath <- str_extract(txt, "https://fsapps.nwcg.gov/afm/data/fireptdata/modis_fire_.........conus_shapefile.zip")
        shpZip <- str_extract(shpPath, "modis_fire_.........conus_shapefile.zip")
        download.file(shpPath, str_c(getwd(),"/",shpZip))
        unzip(shpZip)
        MODISfile <- str_c(str_extract(shpZip, "modis_fire_.........conus"),".shp")
        #yrst <- str_sub(MODISfile,12,15)    # Something like "modis_fire_2020_284_conus.shp
        fires <<- st_read(MODISfile)       
        endday <<- max(fires$JULIAN)
        leaflet() %>%
            addProviderTiles(providers$Esri.WorldTopoMap) %>%
            fitBounds(-123,37,-120,39)
    })
    observe({                   # So this allows the map to retain its location and zoom
        numdays <- input$numdays; end_jday <- input$end_jday #; endday <<- end_jday
        # updateSliderInput(session, inputId = "end_jday", min = (if(endday > 60) {endday-60} else {0}), max = endday + 60)
        fireFilt <- filter(fires,between(JULIAN, end_jday - numdays, end_jday))
        yrst <- as.character(input$year)
        dat <- as.Date(end_jday, origin=str_c(yrst,"-01-01"))
        leafletProxy("view", data = fireFilt) %>%
            clearMarkers() %>%
            addCircleMarkers(
                radius = ~(TEMP-250)/50,  # scales 300-500 from 1:5 
                color = ~pal(TEMP),
                stroke = FALSE, fillOpacity = 0.8) %>%
            clearControls() %>%   # clears the legend
            addLegend("topright", pal=pal, values=~TEMP, opacity=0.6,
                title=str_c("MODIS temps</br>",numdays, " days: ", dat))
    })
}

shinyApp(ui = ui, server = server)


