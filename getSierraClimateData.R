# Sierra Climate Exercise

# 1. Read in sierraStations.csv, downloaded CA normals data, and counties,
#    select the county NAME to be COUNTY
#    spatial join to add counties to the stations data frame,
#    and write out as a new csv
library(sf)
library(tidyverse)
sierraStations <- read_csv("data/sierraStations.csv")
normals <- read_csv("data/2289543.csv")
#Add counties
co <- st_read("data/CA_counties.shp") %>%
  select(COUNTY=NAME)

sierraData <- right_join(sierraStations,normals,by="STATION") %>%
  filter(!is.na(STATION_NA)) %>% dplyr::select(-STATION_NA) %>%
  mutate(PRECIPITATION = `MLY-PRCP-NORMAL`,
         TEMPERATURE = `MLY-TAVG-NORMAL`) %>%
  dplyr::select(NAME, ELEVATION, LATITUDE, LONGITUDE, PRECIPITATION, TEMPERATURE)
write_csv(sierraData, "data/sierraData.csv")

sierraFeb <- right_join(sierraStations,normals,by="STATION") %>%
  filter(!is.na(STATION_NA)) %>% dplyr::select(-STATION_NA) %>%
  filter(DATE == "02") %>%
  mutate(PRECIPITATION = `MLY-PRCP-NORMAL`,
         TEMPERATURE = `MLY-TAVG-NORMAL`) %>%
  dplyr::select(NAME, ELEVATION, LATITUDE, LONGITUDE, PRECIPITATION, TEMPERATURE)

sierraFeb <- st_as_sf(sierraFeb, coords=c("LONGITUDE","LATITUDE"), remove=FALSE, crs=4326) %>%
  rename(STATION_NAME = NAME) %>%
  st_join(co) %>%
  st_drop_geometry() %>%
  dplyr::select(STATION_NAME, COUNTY, ELEVATION, LATITUDE, LONGITUDE, PRECIPITATION, TEMPERATURE)

write_csv(sierraFeb, "data/sierraFeb.csv")
