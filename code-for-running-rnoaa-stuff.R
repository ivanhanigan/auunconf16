library(ggplot2)
library(ggmap)
library(devtools)
library(htmlwidgets)
library(leaflet)
library(tidyr)
library(dplyr)
library(knitr)
library(curl)

# library(gmap)

devtools::install_github("hrbrmstr/rnoaa")
install.packages("curl")

stations <- ghcnd_stations()

list_oz_stations <- grep('^ASN', stations$data$id)

oz_stations <- stations$data[list_oz_stations,]

oz_tmax_stations <- oz_stations[oz_stations$element == "TMAX", ]
oz_prcp_stations <- oz_stations[oz_stations$element == "PRCP", ]

oz_tmax_long = oz_tmax_stations$longitude
oz_tmax_lat = oz_tmax_stations$latitude

oz_prcp_long = oz_prcp_stations$longitude
oz_prcp_lat = oz_prcp_stations$latitude

saveRDS(oz_stations, "oz_stations.RData")

oz_stations %>% head %>% kable
head(oz_stations, 10)

locn.long = 135.947
locn.lat = -18.639
stn_meta_data <-
  meteo_distance(data = oz_stations,
                 lat = locn.lat, #ggmap::geocode("brisbane")[[2]],
                 long = locn.long, #ggmap::geocode("brisbane")[[1]],
                 limit = 1)

db = "GHCND"
api_key = "jfzPFjTWgTCHXHZMrJCEAWUDFjbpWpSc"
out1 <- ncdc(datasetid='GHCND',
             stationid= paste('GHCND:', stn_meta_data$id, sep = ""),
             # datatypeid='TMAX',
             startdate = '2010-03-01', enddate = '2010-05-31',
             token = 	api_key,
             limit=500)

bris_df <- meteo_pull_monitors(unique(bris_meta_data$id))

bris_meta_data

library(lubridate)

bris_super_df <- bris_df %>% left_join(bris_meta_data, by = "id")

