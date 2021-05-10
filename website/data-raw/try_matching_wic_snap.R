library(sf)

full_dataset <- read_rds("data/full_dataset.rds")

full_dataset <- full_dataset %>%
  mutate(accepts_wic = FALSE,
         accepts_snap = FALSE)


stores <- full_dataset %>%
  filter(! category %in% c("Accepts SNAP", "Accepts WIC"))

snap <- full_dataset %>% filter(category %in% c( "Accepts WIC"))

wic <- full_dataset %>% filter(category == "Accepts SNAP")

wic_match <- stores %>%
  semi_join(wic %>% st_set_geometry(NULL), "name")

wic_unmatched <- wic %>% anti_join(wic_match %>% st_set_geometry(NULL))

matches <- tibble(store_name = NA, store_category = NA, store_address = NA, potential_match = NA, wic_index = NA)

nearest_store <- function(i) {
  match_index <- st_nearest_feature(wic[i,], stores)
  potential_match <- stores %>% slice(match_index) %>%
    st_set_geometry(NULL) %>%
    transmute(store_name = name, store_category = category, store_address = street_address, potential_match = match_index, wic_index = i)


  # print(potential_match)
}


test_wic_matches <- map_dfr(1:250, nearest_store) %>%
  bind_cols(wic[c(1:250),])

nearest_store(200)

