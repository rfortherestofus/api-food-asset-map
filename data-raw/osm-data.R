# TODO: clean the data that exists here: name, address, and supporting info

# what to do about places where all fields are NA? Drop?


library(tidyverse)
library(osmdata) # OSM overpass API
library(leaflet)
library(htmltools)
library(tigris) # census geography boundaries
library(janitor)
library(sf) # spatial data wrangling

# get the boundary of San Francisco City/County for cropping data

sf_boundary <- counties(state = "California", cb = TRUE) %>%
  clean_names() %>%
  filter(name == "San Francisco") %>%
  st_transform(4326)

# Supermarket query

supermarket_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("shop", "supermarket") %>%
  osmdata_sf()

supermarkets <- supermarket_q$osm_polygons %>%
  st_intersection(sf_boundary) %>% # the supermarket query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()

supermarkets_clean <- supermarkets %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "supermarket")

# visually check data

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
  addPolygons(data = supermarkets_clean, fillColor = "#840651", color = "#840651", opacity = 1, fillOpacity = 0.7, weight = 1, label = ~htmlEscape(name))

write_rds(supermarkets_clean, "data/supermarkets.rds")

# Convenience store query

convenience_store_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("shop", "convenience") %>%
  osmdata_sf()

convenience_stores <- convenience_store_q$osm_polygons %>%
  st_intersection(sf_boundary) %>% # the convenience store query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()

convenience_stores_clean <- convenience_stores %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "convenience")

# visually check data

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
  addCircleMarkers(data = convenience_stores_clean, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~htmlEscape(name))

write_rds(convenience_stores_clean, "data/convenience_stores_osm.rds")

# Restaurants query

restaurant_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("amenity", "restaurant") %>%
  osmdata_sf()

restaurants <- restaurant_q$osm_points %>%
  st_intersection(sf_boundary) %>%  # the convenience store query includes some from outside of the city, so this limits the data to only places in SF
  st_centroid()

restaurants_clean <- restaurants %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "restaurant")

# visually check data

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
  addCircleMarkers(data = restaurants_clean, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~htmlEscape(name))

write_rds(restaurants_clean, "data/restaurants_osm.rds")

# Fast food query

fastfood_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("amenity", "fast_food") %>%
  osmdata_sf()

fast_food <- fastfood_q$osm_points %>%
  st_intersection(sf_boundary) %>%  # the convenience store query includes some from outside of the city, so this limits the data to only places in SF
  st_centroid()

fast_food_clean <- fast_food %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "fast food")

# visually check data

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
  addCircleMarkers(data = fast_food_clean, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~htmlEscape(name))

write_rds(fast_food_clean, "data/fast_food_osm.rds")
