library(tidyverse)
library(sf)
library(glue)

reverse_geocoded <- read_rds("data/reverse_geocoded.rds") %>%
  mutate(global_index = 1:n())



nearest_store <- function(i) {
  location_1 <- reverse_geocoded[i,]
  other_locations <- reverse_geocoded[-i,]


  match_index <- st_nearest_feature(location_1, other_locations)

  location_2 <- other_locations[match_index, ]

  distance <- st_distance(location_1, location_2)
  reverse_geocoded_location_2 <- location_2 %>%
    st_drop_geometry() %>%
    rename_with(~glue("{.x}_location_2"))

  reverse_geocoded_location_1 <- location_1 %>%
    st_drop_geometry() %>%
    rename_with(~glue("{.x}_location_1"))

  bind_cols(reverse_geocoded_location_1, reverse_geocoded_location_2) %>%
    mutate(distance = distance) %>%
    select(-starts_with("city"),
           -starts_with("state"),
           -starts_with("name_simple"),
           -starts_with("street_address_simple"),
           -starts_with("accepts_snap_wic")) # remove to shrink table

}

potential_matches <- map_dfr(1:nrow(reverse_geocoded), nearest_store)

potential_matches_nearby <- potential_matches %>%
  mutate(distance = as.numeric(distance)) %>%
  filter(distance < 15) %>%
  mutate(global_indices = map2_chr(global_index_location_1, global_index_location_2, ~paste(sort(c(.x, .y)), collapse = " "))) %>%
  group_by(global_indices) %>%
  slice(1) %>%
  ungroup() %>%
  relocate(starts_with("global_index"), .after = distance) %>%
  select(-global_indices)


write_rds(potential_matches_nearby, "data/final_potential_duplicates.rds")

# I added "keep_left" by hand
client_annotated_duplicates <- read_csv("data-raw/client_annotated_duplicates.csv")

# find duplicates where we want the left store
keep_left <- client_annotated_duplicates %>%
  filter(str_detect(is_match, "x"),
         str_detect(keep_left, "x"))

# find duplicates where we want the right store
keep_right <- client_annotated_duplicates %>%
  filter(str_detect(is_match, "x")) %>%
  anti_join(keep_left)

# find duplicates where we want neither store
both_closed <- client_annotated_duplicates %>%
  filter(str_detect(is_match, "BOTH CLOSED"))

duplicates_removed <- reverse_geocoded %>%
  mutate(global_index = 1:n()) %>%
  anti_join(keep_left, by = c("global_index" = "global_index_location_2")) %>%
  anti_join(keep_right, by = c("global_index" = "global_index_location_1")) %>%
  anti_join(both_closed, by = c("global_index" = "global_index_location_1")) %>%
  anti_join(both_closed, by = c("global_index" = "global_index_location_2")) %>%
  select(-global_index)


write_rds(duplicates_removed, "data/final_dataset_no_dupes.rds")

