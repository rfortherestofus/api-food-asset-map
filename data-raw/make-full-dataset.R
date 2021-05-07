library(tidyverse)
library(sf)
library(glue)


standardize <- function(dataset) {
  if ("housenumber" %in% names(dataset)) {
    dataset <- dataset %>%
      mutate(street_address = glue("{housenumber} {street}", .na = "")) %>%
      rename(zip_code = zip)
  }

  # extract lat and long as sf doesn't play nice with binding
  dataset %>%
    select(name, street_address, city, state, zip_code) %>%
    mutate(long = st_coordinates(.)[,1],
           lat = st_coordinates(.)[,2]) %>%
    st_drop_geometry()


}

data_file_names <- tibble(name = list.files("data/"))

# ignoring markets_osm because I can't tell what it is really

file_category_map <- tribble(~name, ~category,
        "bay_area_211", "Offers Free, Prepared Food or Hot Meals",
        "convenience_stores_osm", "Corner Store",
        "drugstores_osm", "Drug Store",
        "farmers_markets", "Farmers Market",
        "fast_food_osm", "Fast Food Restaurant",
        "food_banks", "Food Bank",
        "food_pantries", "Food Pantry",
        "food_pharmacies", "Food Pharmacy",
        "pop_up_pantries", "Food Pantry",
        "prepared_food", "Offers Free, Prepared Food or Hot Meals",
        "restaurants_osm", "Restaurant",
        "snap_stores", "Accepts SNAP",
        "supermarkets", "Supermarket",
        "wic_stores", "Accepts WIC") %>%
  mutate(full_path = glue("data/{name}.rds"))

data_files <- map(file_category_map$full_path, readRDS) %>%
  setNames(file_category_map$name)

data_files_fixed <- data_files %>%
  map(standardize) %>%
  bind_rows(.id = "file_name") %>%
  left_join(file_category_map, c("file_name" = "name")) %>%
  select(-file_name, -full_path) %>%
  relocate(category, .after = name) %>%
  st_as_sf(coords = c("long", "lat"),
           crs = 4326)

Encoding(data_files_fixed$name) <- "UTF-8"
data_enc <- data_files_fixed %>%
  mutate(name = iconv(data_files_fixed$name, "UTF-8", "UTF-8",sub='')) %>%
  distinct() # remove easy duplicates



# visual check
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = data_enc, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~htmlEscape(name))

write_rds(data_enc, "data/full_dataset.rds")

write_rds(data_enc)

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

