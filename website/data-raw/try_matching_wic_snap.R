library(tidyverse)
library(sf)
library(glue)

full_dataset <- read_rds("data/full_dataset.rds") %>%
  mutate(global_index = 1:n())# need to add index or rows not identifiable
#
# full_dataset <- full_dataset %>%
#   mutate(accepts_wic = FALSE,
#          accepts_snap = FALSE)
#
#
# stores <- full_dataset %>%
#   filter(category != "Stores that Accept SNAP/WIC") %>%
#   filter(!(category %in% c("Restaurants", "Restaurants (Fast Food)")))
#
# wic <- full_dataset %>% filter(category == "Stores that Accept SNAP/WIC")
#
# wic_match <- stores %>%
#   semi_join(wic, "name") #theres a couple repeat names in wic
#
# wic_unmatched <- wic %>%
#   anti_join(wic_match, "name")
#
#
# matches <- tibble(store_name = NA, store_category = NA, store_address = NA, potential_match = NA, wic_index = NA)
#
# nearest_store <- function(i) {
#   match_index <- st_nearest_feature(wic[i,], stores)
#   distance <- st_distance(wic[i, ], stores[match_index, ])
#   potential_match <- stores %>% slice(match_index) %>%
#     st_set_geometry(NULL) %>%
#     transmute(store_name = name, store_category = category, store_address = street_address, potential_match = match_index, wic_index = i, distance = distance)
#
#
#   potential_match
# }
#
#
# test_wic_matches <- map_dfr(1:250, nearest_store) %>%
#   bind_cols(wic[c(1:250),])
#
# nearest_store(10)


# first, seperate out all cvs, whole foods, trader joes, walgreens, and match seperately to locations w/in maybe 100m

# Ellen's attempt ------------------


# Prepare Data Frames ----------------

snap_wic_locations <- full_dataset %>%
  filter(category == "Stores that Accept SNAP/WIC")

remaining_locations <- full_dataset %>%
  filter(category != "Stores that Accept SNAP/WIC")

# remove resturants, can't use snap/wic on prepared food, as well as food pantries, can't use on free food

not_snap_wic_applic <- full_dataset %>%
  filter(category %in% c("Restaurants",
                         "Restaurants (Fast Food)",
                         "Food Banks/Pantries",
                         "Food Pharmacies",
                         "Free Prepared Food or Hot Meals")) %>%
  mutate(accepts_snap_wic = FALSE)

remaining_locations <- remaining_locations %>%
  anti_join(not_snap_wic_applic %>% st_drop_geometry,
            by = "global_index")

# Major Chain Matching --------------------------

match_chains <- function(chain_name) {
  chain_unmatched <- remaining_locations %>%
    filter(str_detect(str_to_lower(name), chain_name))

  chain_snap_wic <- snap_wic_locations %>%
    filter(str_detect(str_to_lower(name), chain_name))

  find_nearest <- function(snap_wic_index) {
    match_index <- st_nearest_feature(chain_snap_wic[snap_wic_index,], chain_unmatched)
    distance <- st_distance(chain_snap_wic[snap_wic_index, ], chain_unmatched[match_index, ])
    tibble(snap_wic_index = snap_wic_index,
           match_index = match_index,
           distance = distance)
  }

  matches <- map_dfr(1:nrow(chain_snap_wic), find_nearest) %>%
    filter(as.numeric(distance) < 100) # assume if further than 100 meters, its a different store

  chain_matched <- chain_unmatched %>%
    slice(matches$match_index)

  chain_unmatched <- chain_unmatched %>%
    slice(-matches$match_index) # remove locations we have a match for


  # return chain_matched to use in antijoin on remaining locations
  list(chain_unmatched = chain_unmatched,
       chain_matched = chain_matched,
       chain_snap_wic = chain_snap_wic)
}

chain_names <- c("cvs",
                 "walgreens",
                 "whole foods",
                 "trader joe",
                 "safeway",
                 "(7-eleven)|(7 eleven)",
                 "target")

chain_outputs <- map(chain_names, match_chains)


chains_unmatched <- chain_outputs %>%
  map(1) %>%  # extracts matched tables
  bind_rows()

chains_matched <- chain_outputs %>%
  map(2) %>%  # extracts matched tables
  bind_rows()


chains_snap_wic <-  chain_outputs %>%
  map(3) %>%  # extracts matched tables
  bind_rows()

remaining_locations <-  remaining_locations %>%
  anti_join(chains_matched %>% st_drop_geometry(),
            by = "global_index")

remaining_snap_wic_locations <- snap_wic_locations %>%
  anti_join(chains_snap_wic %>% st_drop_geometry(),
            by = "global_index")

# Regex Matching -----------------------------

# try stripping punctuation first
remaining_locations <- remaining_locations %>%
  mutate(name_simple = str_to_lower(name) %>%
           str_replace_all("[:punct:]","") %>%
           str_replace_all("  ", " ") %>%
           str_trim())

remaining_snap_wic_locations <- remaining_snap_wic_locations %>%
  mutate(name_simple = str_to_lower(name) %>%
           str_replace_all("[:punct:]","") %>%
           str_replace_all("  ", " ") %>%
           str_trim())

simple_regex_matches <- semi_join(remaining_locations, remaining_snap_wic_locations %>% st_drop_geometry(), by = "name_simple")
simple_regex_matches_snap_wic <- semi_join(remaining_snap_wic_locations, remaining_locations %>% st_drop_geometry(), by = "name_simple")

remaining_locations <- remaining_locations %>%
  anti_join(simple_regex_matches %>% st_drop_geometry(), by = "global_index")

remaining_snap_wic_locations <- remaining_snap_wic_locations %>%
  anti_join(simple_regex_matches_snap_wic %>% st_drop_geometry(), by = "global_index")

# lets try matching addresses next

remaining_locations <- remaining_locations %>%
  mutate(street_address_simple = str_to_lower(street_address) %>%
           str_replace_all("[:punct:]","") %>%
           str_replace_all("  ", " ") %>%
           str_trim())

remaining_snap_wic_locations <- remaining_snap_wic_locations %>%
  mutate(street_address_simple = str_to_lower(street_address) %>%
           str_replace_all("[:punct:]","") %>%
           str_replace_all("  ", " ") %>%
           str_trim())

simple_address_matches <- semi_join(remaining_locations, remaining_snap_wic_locations %>% st_drop_geometry(), by = "street_address_simple")
simple_address_matches_snap_wic <- semi_join(remaining_snap_wic_locations, remaining_locations %>% st_drop_geometry(), by = "street_address_simple")

remaining_locations <- remaining_locations %>%
  anti_join(simple_address_matches %>% st_drop_geometry(), by = "global_index")

remaining_snap_wic_locations <- remaining_snap_wic_locations %>%
  anti_join(simple_address_matches_snap_wic %>% st_drop_geometry(), by = "global_index")

nearest_store <- function(i) {
  match_index <- st_nearest_feature(remaining_snap_wic_locations[i,], remaining_locations)
  distance <- st_distance(remaining_snap_wic_locations[i, ], remaining_locations[match_index, ])
  potential_match <- remaining_locations %>%
    slice(match_index) %>%
    st_drop_geometry() %>%
    rename_with(~glue("{.x}_possible_match"))

  snap_wic_info <- remaining_snap_wic_locations %>%
    slice(i) %>%
    st_drop_geometry() %>%
    rename_with(~glue("{.x}_snap_wic"))

  bind_cols(potential_match, snap_wic_info) %>%
    mutate(distance = distance, snap_wic_index = i, possible_match_index = match_index) %>%
    select(-starts_with("city"),
           -starts_with("state"),
           -starts_with("name_simple"),
           -starts_with("street_address_simple"),
           -starts_with("global_index")) # remove to shrink table

}

# best guesses based on distance
potential_snap_wic_matches <- map_dfr(1:nrow(remaining_snap_wic_locations), nearest_store) %>%
  mutate(is_match = NA, new_category = NA) %>%
  mutate(distance = as.numeric(distance))

# potential_snap_wic_matches

write_rds(potential_snap_wic_matches,
          "data/potential_snap_wic_matches.rds")
