

# is there a difference between food banks and food pantries?

# look at drug stores

# ethnic food options?

# double check liquor store tag

# look into geocoding for addresses

# in the popup include "confirmed has snap/wic yes or no"

# create an example for the client of what the popup might look like


# create distance matrix to join snap + supermarket


# updates by the end of the week

demographics <- read_rds(("data/api_neighborhood_data.rds"))

convenience_stores <- read_rds("data/convenience_stores_osm.rds") %>%
  st_join(demographics %>% select(nhood)) %>%
  mutate(type = "Convenience store")
food_pantries <- read_rds("data/food_pantries.rds") %>%
  st_join(demographics %>% select(nhood)) %>%
  mutate(type = "Food pantry")
