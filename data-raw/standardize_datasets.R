library(tidyverse)
library(sf)
library(tidygeocoder)

fix_encoding <- function(dataset) {
  dataset_names <- names(dataset)[which(names(dataset) != "geometry")]

  for(col in dataset_names) {
    Encoding(dataset[[col]]) <- "UTF-8"
  }
  dataset
}

standardize <- function(dataset) {
  dataset %>%
    select(name, category, street_address, city, state, zip_code)
}

# # reverse geocoding
# convenience_stores_osm <- readRDS("data/convenience_stores_osm.rds")
#
# # need to remove excess variables, do some reverse geocoding, and add category
# temp_arcgis <- convenience_stores_osm %>%
#   mutate(long = st_coordinates(.)[,1],
#          lat = st_coordinates(.)[,2]) %>%
#   reverse_geocode(lat = lat, long = long,
#                   method = "arcgis")
#

