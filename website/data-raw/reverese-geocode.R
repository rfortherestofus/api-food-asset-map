library(tidyverse)
library(sf)
library(tidygeocoder)
library(glue)

final_snap_wic_dataset <- read_rds("data/final_snap_wic_dataset.rds") %>%
  mutate(global_index = 1:n()) %>%
  filter(!(global_index %in% c(3007, 3030, 2415, 2453, 3034, 2492, 3035, 3036, 2458, 2806, 3051, 2824, 2352, 3031, 3049, 2855))) %>%  # more duplicates!!! Where do they come from???
  select(-global_index)



st_coordinates(final_snap_wic_dataset)[1,]

need_geocoding <- final_snap_wic_dataset %>%
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  filter(street_address == " ")

not_need_geocoding <- final_snap_wic_dataset %>%
  filter(street_address != " ")

geocoded <- need_geocoding %>%
  reverse_geocode(lat = lat, long = long, address = address, method = "mapbox")


# the rules:
# - If there are 4 parts, address_part_4 is the street address
# - If there are 5 parts, address_part_4 is the street address
# - If there are 6 parts, do it by hand
# - If there are 7 parts, do it by hand
partially_addressed <- geocoded %>%
  separate(address, into = glue("address_part_{1:7}"), sep = ",", fill = "left") %>%
  mutate(street_address = ifelse(is.na(address_part_3) | is.na(address_part_2),
                                 address_part_4,
                                 street_address),
         zip_code = str_extract(address_part_6, "[0-9]{5}")) %>%
  mutate(global_index = 1:n()) %>%  # for manual fixes
  relocate(street_address, .after = lat)

manual_addresses <- tribble(~global_index, ~correct_street_address,
                            120, "4 Embarcadero Center",
                            139, "333 Bush St",
                            189, "620 Broadway",
                            219, "Pier 39 M209",
                            221, "2 Pier 39",
                            236, "2 Marinal Blvd",
                            349, "Pier 39",
                            351, "Pier 39 Space 206-8",
                            354, "Pier 39 Suite J-16",
                            664, "1 Ferry Building Shop 43",
                            682, "3251 20th Ave",
                            116, "1715 Union St",
                            824, "1790 Sutter St",
                            576, "74 New Montgomery St",
                            341, "431 Jones St",
                            492, "410 Cortland Ave",
                            51,  "39 Pier Space 256",
                            225, "855 Terry A Francois Blvd",
                            222, "2 Beach St",
                            352, "Pier 39 Box M-211",
                            268, "181 3rd St",
                            609, "50 Frida Kahlo Way",
                            793, "1589 Haight St",
                            135, "49 Stevenson St",
                            658, "1 Sausalito - San Francisco Ferry Bldg",
                            218, "50 Post St",
                            109, "2 New Montgomery St",
                            350, "Pier 39  Space 255",
                            489, "One Ferry Building #19",
                            580, "2300 16th St Ste. 275",
                            498, "1100 Market St",
                            155, "225 Clement St",
                            541, "1836 Divisadero St #2",
                            699, "684 Larkin St",
                            124, "1954 Hyde St",
                            825, "661 Clement St")


addressed <- partially_addressed %>%
  left_join(manual_addresses, by = "global_index") %>%
  mutate(street_address = ifelse(!is.na(correct_street_address),
                                 correct_street_address,
                                 street_address)) %>%
  mutate(street_address = str_trim(street_address)) %>%
  select(name, category, street_address, city, state, zip_code, geometry, accepts_snap_wic)


full_geocoded <- bind_rows(addressed, not_need_geocoding) %>%
  st_as_sf()

write_rds(full_geocoded, "data/reverse_geocoded.rds")
