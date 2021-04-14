library(tidyverse)
library(osmdata) # OSM overpass API
library(leaflet)
library(tigris) # census geography boundaries
library(janitor)
library(sf) # spatial data wrangling

# get the boundary of San Francisco City/County from cropping data

sf_boundary <- counties(state = "California", cb = TRUE) %>%
  clean_names() %>%
  filter(name == "San Francisco")

# Supermarket query

supermarket_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("shop", "supermarket") %>%
  osmdata_sf()

supermarkets <- supermarket_q$osm_points %>%
  st_intersection(sf_boundary) # the supermarket query includes some from outside of the city, so this limits the data to only supermarkets in SF

# visually check data

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = sf_boundary)
  addCircleMarkers(data = supermarkets, fillColor = "#840651")
