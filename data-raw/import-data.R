
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(rvest)
library(janitor)
library(stringr.plus)
library(sf)


# foodpantries.org --------------------------------------------------------

# Data from https://www.foodpantries.org/ci/ca-san_francisco

# Creat function to import data from singe listing

import_food_pantry_data <- function(pantry_url) {

  # Import info for each food pantry
  pantries_page_html <- read_html(pantry_url)

  pantries_page_text <- pantries_page_html %>%
    html_elements("#content > div > div > div.span8") %>%
    html_text2()

  # Name
  pantry_name <- pantries_page_html %>%
    html_elements("h1") %>%
    html_text() %>%
    as_tibble() %>%
    filter(value != "Food Pantries") %>%
    mutate(value = str_remove(value, " Details Page")) %>%
    pull(value)

  # Address
  pantry_street_address <- pantries_page_text %>%
    str_extract_between("streetAddress\\\":\\\"", "addressLocality") %>%
    str_extract_before('\\"')

  # Zip code
  # https://www.oreilly.com/library/view/regular-expressions-cookbook/9781449327453/ch04s14.html
  pantry_zip_code <- pantries_page_text %>%
    str_extract("[0-9]{5}(?:-[0-9]{4})?")

  pantry_data <- tribble(
    ~name, ~street_address, ~zip_code,
    pantry_name, pantry_street_address, pantry_zip_code
  ) %>%
    mutate(city = "San Francisco",
           state = "CA")

  pantry_data

}


# Get HTML from main page
food_pantries_html <- read_html("https://www.foodpantries.org/ci/ca-san_francisco")

# Create vector of all URLs of food pantry pages
pantries_urls <- food_pantries_html %>%
  html_elements("a") %>%
  html_attr("href") %>%
  as_tibble() %>%
  filter(str_detect(value, "/li/")) %>%
  distinct() %>%
  pull(value)

# Read in and combine all data
pantries_data <- map_df(pantries_urls, import_food_pantry_data)


# Food Pharmacies ---------------------------------------------------------

# Kathleen put these together manually
# Original here: https://docs.google.com/spreadsheets/d/1O0y0pgZgwJ8qW2Xesb2xxWazraB3qePIG7RIIcx52RM/edit?pli=1#gid=0

food_pharmacies <- tibble::tribble(
  ~Food.Pharmacy.Name,                                       ~Address,                                                ~Notes,
  "3rd Street Youth Center and Clinic",   "1728 Bancroft Ave, San Francisco, CA 94124",                                                    NA,
  "Bayview Hunters Point Clinic",         "6301 3rd St, San Francisco, CA 94124",                                                    NA,
  "Clinic by the Bay",     "4877 Mission St, San Francisco, CA 94112", "not a food pharmacy but gives out $10 food vouchers",
  "Curry Senior Center",         "333 Turk St, San Francisco, CA 94102",                                                    NA,
  "HealthRIGHT 360",     "1563 Mission St, San Francisco, CA 94103",                                                    NA,
  "UCSF Mount Zion",  "1600 Divisadero St, San Francisco, CA 94115",                                                    NA,
  "Potrero Hill Health Center",   "1050 Wisconsin St, San Francisco, CA 94107",                                                    NA,
  "Silver Avenue Family Health Center",     "1525 Silver Ave, San Francisco, CA 94134",                                                    NA,
  "Southeast Health Center",       "2401 Keith St, San Francisco, CA 94124",                                                    NA,
  "Tom Waddell Urban Health Center", "230 Golden Gate Ave, San Francisco, CA 94102", "not a food pharmacy but gives out $10 food vouchers"
) %>%
  set_names("name", "address", "notes") %>%
  separate(address,
           into = c("street_address", "city", "state"),
           sep = ", ") %>%
  separate(state,
           into = c("state", "zip_code"),
           sep = " ")


# SF Marin Food Bank ------------------------------------------------------

# https://www.sfmfoodbank.org/find-food/
# These are pop-up pantries

# pop_up_pantries <-

pop_up_pantry_html <- read_html("https://foodlocator.sfmfoodbank.org/en/site/SPUP")

pop_up_pantry_html %>%
  # html_element("h2") %>%
  html_text2() %>%
  as_tibble() %>%
  filter(str_detect(value, "Ellis"))
  view()
  str()


# Bay Area 211 ------------------------------------------------------------

# https://www.211bayarea.org/sanfrancisco/food/food-programs/brown-bag-programs/

# Problem is the links are javascript

bay_area_211_html <- read_html("https://www.211bayarea.org/sanfrancisco/food/food-programs/brown-bag-programs/")

bay_area_211_html %>%
  html_text()

# Food-related registered businesses ---------------------------------------------------------

# We can look at the registered business in San Francisco and pull out the food related ones

# from https://data.sfgov.org/Economy-and-Community/Registered-Business-Locations-San-Francisco/g8m3-pdis

businesses <- read_csv("https://data.sfgov.org/api/views/g8m3-pdis/rows.csv?accessType=DOWNLOAD") %>%
  clean_names()

businesses %>% group_by(naics_code_description) %>% count()

food_services <- businesses %>%
  filter(naics_code_description == "Food Services",
         is.na(business_end_date), # business has not ended
         is.na(location_end_date), # current location
         city == "San Francisco")

# look at the different categories of food services

food_services %>% count(lic_code_description) %>% View()


# Stores that accept EBT/SNAP ---------------------------------------------

# https://www.ebt.ca.gov/locator/index.html#/locator.page
# I downloaded Location-Details-20210419213426948.txt from the website above
# I don't know how to read in the crazy txt file it gives me

snap_stores <- read_tsv("data-raw/Location-Details-20210419213426948.txt")

snap_stores %>%
  # Drop extra variable name rows
  slice(-(1:2)) %>%
  set_names("store_info") %>%
  filter(str_detect(store_info, "Francisco"))
  separate(store_info, into = c("name", "street_address"),
           sep = "")



# WIC Stores --------------------------------------------------------------

wic_stores_html <- read_html("https://www.wicstorelocator.com/ci/ca-san_francisco")

wic_stores_names <-
  wic_stores_html %>%
  html_elements("h3") %>%
  html_text2() %>%
    as_tibble() %>%
    # Keep only all uppercase text
    # https://stackoverflow.com/questions/2323988/determine-if-string-is-all-caps-with-regular-expression
    filter(str_detect(value, "^[^a-z]*$")) %>%
  pull(value)


wic_stores <- wic_stores_html %>%
  html_elements("p") %>%
  html_text2() %>%
  as_tibble() %>%
  filter(str_detect(value, "FRANCISCO")) %>%
  mutate(street_address = str_replace_all(value, "[\r\n]" , "")) %>%
  separate(street_address,
           into = c("street_address", "drop"),
           sep = " SAN FRANCISCO") %>%
  mutate(street_address = str_trim(street_address)) %>%
  mutate(zip_code = str_extract(value, "[0-9]{5}(?:-[0-9]{4})?")) %>%
  select(street_address, zip_code) %>%
  mutate(name = wic_stores_names) %>%
  select(name, street_address, zip_code) %>%
  mutate(city = "San Francisco",
         state = "CA")



# Farmers Markets ---------------------------------------------------------

# https://sfenvironment.org/farmers-markets-in-sf

farmers_markets_html <- read_html("https://sfenvironment.org/farmers-markets-in-sf")


farmers_markets_names <- farmers_markets_html %>%
  html_elements("strong") %>%
  html_text() %>%
  as_tibble() %>%
  filter(str_detect(value, "Farmers Market\\b") | str_detect(value, "Mercantile") | str_detect(value, "Community Market")) %>%
  pull(value)

farmers_market_zips <- farmers_markets_html %>%
  html_elements("p") %>%
  html_text() %>%
  as_tibble() %>%
  # Keep only rows with zip codes
  filter(str_detect(value, "[0-9]{5}(?:-[0-9]{4})?")) %>%
  mutate(zip_code = str_extract(value, "[0-9]{5}(?:-[0-9]{4})?")) %>%
  pull(zip_code)

farmers_market_addresses <- farmers_markets_html %>%
  html_elements("p") %>%
  html_text() %>%
  as_tibble() %>%
  mutate(street_address = str_replace_all(value, "[\r\n]" , "")) %>%
  # Keep only rows with zip codes
  filter(str_detect(value, "[0-9]{5}(?:-[0-9]{4})?")) %>%
  separate(street_address,
           into = c("name", "street_address"),
           sep = "\t") %>%
  pull(street_address)


farmers_markets <- tibble(
  name = farmers_markets_names,
  street_address = farmers_market_addresses,
  zip_code = farmers_market_zips
)
