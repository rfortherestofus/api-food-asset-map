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
        "bay_area_211", "Free, Prepared Food or Hot Meals",
        "convenience_stores_osm", "Corner Stores",
        "drugstores_osm", "Drug Stores",
        "farmers_markets", "Farmers Markets",
        "fast_food_osm", "Restaurants (Fast Food)",
        "food_banks", "Food Banks",
        "food_pantries", "Food Pantries",
        "food_pharmacies", "Food Pharmacies",
        "pop_up_pantries", "Food Pantries",
        "prepared_food", "Free, Prepared Food or Hot Meals",
        "restaurants_osm", "Restaurants",
        "snap_stores", "Stores that Accept SNAP/WIC",
        "supermarkets", "Supermarkets",
        "wic_stores", "Stores that Accept SNAP/WIC",
        "liquor_stores_osm", "Liquor Stores",
        "ethnic_markets", "Ethnic Markets") %>%
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
  mutate(name = iconv(data_files_fixed$name, "UTF-8", "UTF-8",sub='')) %>% # remove easy duplicates
  arrange(category)

# visual check
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = data_enc, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~htmlEscape(name))


client_annot_intl <- read_csv("data-raw/client_annotated_intl_groc.csv")

full_data <- client_annot_intl %>%
  rename(international_grocery_store = "ethnic food market") %>%
  select(international_grocery_store) %>%
  bind_cols(data_enc) %>%
  filter(international_grocery_store != "CLOSED" | is.na(international_grocery_store)) %>%
  mutate(international_grocery_store = !is.na(international_grocery_store)) %>%
  relocate(international_grocery_store, .after = zip_code)

write_rds(full_data, "data/full_dataset.rds")

