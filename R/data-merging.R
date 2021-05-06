library(tidyverse)
library(sf)

supermarkets <- read_rds("data/supermarkets.rds") %>%
  st_drop_geometry() %>%
  select(name:business_type) %>%
  mutate(id = paste(name, housenumber, street))

snap_stores <- read_rds("data/snap_stores.rds") %>%
  st_drop_geometry() %>%
  select(name:category) %>%
  mutate(street_address = str_replace(street_address, "St", "Street")) %>%
  mutate(id = paste(name, street_address))

supermarkets %>%
  left_join(snap_stores, by = "id") %>%
  drop_na(category) %>%
  view()

