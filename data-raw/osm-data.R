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


supermarkets_pts <- supermarket_q$osm_points %>%
  st_intersection(sf_boundary) %>% # the supermarket query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()



supermarkets_clean <- supermarkets %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "supermarket") %>%
  rbind(supermarkets_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "supermarket")) %>%
  drop_na(name)

# visually check data

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
  addCircleMarkers(data = supermarkets_clean, fillColor = "#840651", color = "#840651", opacity = 1, fillOpacity = 0.7, weight = 1, label = ~htmlEscape(name), radius = 2)

write_rds(supermarkets_clean, "data/supermarkets.rds")

# Convenience store query

convenience_store_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("shop", "convenience") %>%
  osmdata_sf()

convenience_stores <- convenience_store_q$osm_polygons %>%
  st_intersection(sf_boundary) %>% # the convenience store query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()

convenience_stores_pts <- convenience_store_q$osm_points %>%
  st_intersection(sf_boundary) %>% # the convenience store query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()

convenience_stores_clean <- convenience_stores %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "convenience") %>%
  rbind(convenience_stores_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "convenience")) %>%
  drop_na(name)

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

restaurants <- restaurant_q$osm_polygons %>%
  st_intersection(sf_boundary) %>%  # the convenience store query includes some from outside of the city, so this limits the data to only places in SF
  st_centroid()

restaurants_pts <- restaurant_q$osm_points %>%
  st_intersection(sf_boundary) %>%  # the convenience store query includes some from outside of the city, so this limits the data to only places in SF
  st_centroid()

restaurants_clean <- restaurants %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "restaurant") %>%
  rbind(restaurants_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "restaurant")) %>%
  drop_na(name)

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

fast_food <- fastfood_q$osm_polygons %>%
  st_intersection(sf_boundary) %>%  # the convenience store query includes some from outside of the city, so this limits the data to only places in SF
  st_centroid()

fast_food_pts <- fastfood_q$osm_points %>%
  st_intersection(sf_boundary) %>%  # the convenience store query includes some from outside of the city, so this limits the data to only places in SF
  st_centroid()

fast_food_clean <- fast_food %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "fast food") %>%
  rbind(fast_food_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "fast food")) %>%
  drop_na(name)

# visually check data

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
  addCircleMarkers(data = fast_food_clean, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~htmlEscape(name))

write_rds(fast_food_clean, "data/fast_food_osm.rds")


# Farmers market query

market_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("amenity", "marketplace") %>%
  osmdata_sf()

markets <- market_q$osm_polygons %>%
  st_intersection(sf_boundary) %>%  # the convenience store query includes some from outside of the city, so this limits the data to only places in SF
  st_centroid()

markets_pts <- market_q$osm_points %>%
  st_intersection(sf_boundary) %>%  # the convenience store query includes some from outside of the city, so this limits the data to only places in SF
  st_centroid()

markets_clean <- markets %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, city = "San Francisco", state = "CA", business_type = "market") %>%
  rbind(markets_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, city = "San Francisco", state = "CA", business_type = "market")) %>%
  drop_na(name)

# visually check data

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
  addCircleMarkers(data = markets_clean, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~htmlEscape(name))

write_rds(markets_clean, "data/markets_osm.rds")

# Drugstore query

drugstore_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("amenity", "pharmacy") %>% # drugstore is deprecated, chemist returns subset of these results
  osmdata_sf()

drugstores <- drugstore_q$osm_polygons %>%
  st_intersection(sf_boundary) %>%  # the convenience store query includes some from outside of the city, so this limits the data to only places in SF
  st_centroid()

drugstores_pts <- drugstore_q$osm_points %>%
  st_intersection(sf_boundary) %>%  # the convenience store query includes some from outside of the city, so this limits the data to only places in SF
  st_centroid()

drugstores_clean <- drugstores %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, city = "San Francisco", state = "CA", business_type = "drugstore") %>%
  rbind(drugstores_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, city = "San Francisco", state = "CA", business_type = "drugstore")) %>%
  drop_na(name)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
  addCircleMarkers(data = drugstores_clean, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~htmlEscape(name))

write_rds(drugstores_clean, "data/drugstores_clean.rds")
