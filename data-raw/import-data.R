
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(rvest)
library(janitor)
library(stringr.plus)
library(sf)
library(tidygeocoder)
library(glue)
library(leaflet)
library(RSelenium)
library(tigris)

# load San Francisco boundary
sf_boundary <- counties(state = "California", cb = TRUE) %>%
  clean_names() %>%
  filter(name == "San Francisco") %>%
  st_transform(4326)

# foodpantries.org --------------------------------------------------------

# Data from https://www.foodpantries.org/ci/ca-san_francisco

# Create function to import data from singe listing

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
  mutate(value = str_replace(value, " ", "%20")) %>%
  pull(value)

# Read in and combine all data
pantries_data <- map_df(pantries_urls, import_food_pantry_data)

# Geocode Data

pantries_data_geocoded <- pantries_data %>%
  mutate(address = glue("{street_address}, {city}, {state} {zip_code}")) %>%
  geocode(address, method = "arcgis", lat = latitude, long = longitude) %>%  #osm misses 3 addresses
  select(-address)
pantries_data_sf <- pantries_data_geocoded %>%
  mutate(category = "food bank/pantry") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Do visual check
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = pantries_data_sf, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~name)


# Save files

write_rds(pantries_data_sf, "data/food_pantries.rds")




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

# geocode the data
food_pharmacies_geocoded <- food_pharmacies %>%
  mutate(address = glue("{street_address}, {city}, {state} {zip_code}")) %>%
  geocode(address, method = "arcgis", lat = latitude, long = longitude) %>%
  select(-address)

food_pharmacies_sf <- food_pharmacies_geocoded %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  mutate(category = "food pharmacy") # don't need to clip, already all in sf


# quick visual check
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = food_pharmacies_sf, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~name)

write_rds(food_pharmacies_sf, "data/food_pharmacies.rds")


# SF Marin Food Bank ------------------------------------------------------

# https://www.sfmfoodbank.org/find-food/
# These are pop-up pantries

# prepare links

pop_up_pantry_html <- read_html("https://www.sfmfoodbank.org/find-food/")

pop_up_pantry_links <- pop_up_pantry_html %>%
  html_elements(".link") %>%
  html_attr("href") %>%
  as_tibble() %>%
  filter(value != "https://panda.sfmfoodbank.org/application") # doesn't contain address info

# data is stored as javascript, need to use selenium to extract it
rD <- rsDriver(browser = "chrome",
         chromever = "90.0.4430.24")
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4567L,
  browserName = "chrome",
)

remDr$open()
remDr$getStatus()

pop_up_get_addr <- function(url) {
  remDr$navigate(url)
  Sys.sleep(3) # give selenium time to load all elements
  siteElements <- remDr$findElements(using = "class", "text-nowrap")
  street_address <- siteElements[[1]]$getElementText()
  city_zip <- siteElements[[2]]$getElementText()
  siteElements <- remDr$findElement(using = "css selector", "h2:not(.text-nowrap")
  name <- siteElements$getElementText()

  tibble(name = name, street_address = street_address, city_zip = city_zip)
}

siteElements <- remDr$findElement(using = "css selector", "h2:not(.text-nowrap")

# grab data
pop_up_addr <- map_df(pop_up_pantry_links$value, pop_up_get_addr)

pop_up_data <- pop_up_addr %>%
  unnest(cols = c(name, street_address, city_zip)) %>%
  mutate(zip_code = str_trim(str_extract(city_zip, "[0-9]{5}")),
         city = str_trim(str_extract(city_zip, "[^0-9]+"))) %>%
  mutate(name = str_extract(name, "\\D+") %>%
           str_sub(start = 3L)) %>%
  mutate(category = "food bank/pantry") %>%
  select(-city_zip) %>%
  mutate(state = "CA")

# geocode the addresses
pop_up_data_geocoded <- pop_up_data %>%
  mutate(address = glue("{street_address}, {city}, {state} {zip_code}")) %>%
  geocode(address, method = "arcgis", lat = latitude, long = longitude) %>%  #osm misses 3 addresses
  select(-address)

pop_up_data_sf <- pop_up_data_geocoded %>%
  mutate(category = "food bank/pantry") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_intersection(sf_boundary) # restrict to just sf county

# quick visual check
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = pop_up_data_sf, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~name)


write_rds(pop_up_data_sf, "data/pop_up_pantries.rds")

# Bay Area 211 ------------------------------------------------------------

# https://www.211bayarea.org/sanfrancisco/food/food-programs/brown-bag-programs/

# Problem is the links are javascript

# can get links by going to the iframe's page
bay_area_iframe_html <- read_html("https://www.icarol.info/Search.aspx?org=2339&Count=21&Search=Brown+Bag+Food+Programs&NameOnly=True&pst=All&sort=Proximity&TaxExact=False&Country=United%20States&StateProvince=CA&County=San%20Francisco")

# extract links to each site
bay_area_secondary_links <- bay_area_iframe_html %>%
  html_elements(".DetailsHeaderBackground") %>%
  html_children() %>%
  html_attr("onclick") %>%
  str_extract_between("open", "return") %>%
  str_sub(start = 3L, end = -4L) %>%
  str_replace_all(" ", "%20")

# crawl across each top level link
bay_area_crawl_agency <- function(url) {
  print(url)
  Sys.sleep(1)
  agency_site <- tryCatch(
    read_html(url),
    error = function(e) e
  )

  if(!inherits(agency_site, "error")) {

    agency_nums <- bay_area_sec_html_test %>%
      html_elements("#Sites a") %>%
      html_attr("id") %>%
      str_extract("[0-9]+")

    agency_resource_urls <- map_chr(agency_nums, ~glue("{url}&SiteResourceAgencyNum={.x}"))


    map_df(agency_resource_urls, bay_area_get_addr)
  } else {
    tibble(name = NA,
           street_address = NA,
           city = NA,
           zip_code = NA,
           state = NA)
  }
}


# extract data for site
bay_area_get_addr <- function(url) {

  site <- tryCatch(
    read_html(url),
    error = function(e) e
  )

  if(!inherits(site, "error")) {
    address <- site %>%
      html_elements("#lblAgencyPhysicalAddress") %>%
      html_text2()


    street_address <- address %>%
      str_extract_before("\\n")


    city <- address %>%
      str_extract_after("\\n\\n") %>%
      str_extract_before(",") %>%
      str_trim()

    zip_code <- address %>%
      str_extract("[0-9]{5}")

    state <- "CA"

    name <- site %>%
      html_elements("#hlLinkToParentAgency") %>%
      html_text() %>%
      str_extract_after("Agency: ") %>%
      str_to_title()

    tibble(name = name,
           street_address = street_address,
           city = city,
           zip_code = zip_code,
           state = state)

  } else {
    tibble(name = NA,
           street_address = NA,
           city = NA,
           zip_code = NA,
           state = NA)
  }
}

full_bay_area_data <- map_df(bay_area_secondary_links, bay_area_crawl_agency)

# geocode
full_bay_area_data_geocoded <- full_bay_area_data %>%
  distinct() %>%
  mutate(address = glue("{street_address}, {city}, {state} {zip_code}")) %>%
  geocode(address, method = "arcgis", lat = latitude, long = longitude) %>%  #osm misses 3 addresses
  select(-address)

full_bay_area_data_sf <- full_bay_area_data_geocoded %>%
  mutate(category = NA) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) #%>%
  #st_intersection(sf_boundary) none of locations actually in sf county

# Do a visual check
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = full_bay_area_data_sf, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~name)


write_rds(full_bay_area_data_sf, "data/bay_area_211.rds")

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

# pulled data from https://www.fns.usda.gov/snap/retailer-locator, it's much cleaner

snap_stores_usda <- vroom::vroom("data-raw/SNAP_Store_Locations.csv")

snap_stores_usda_sf <- snap_stores_usda %>%
  filter(Latitude > 37.613902, # do a quick filter before clipping
         Latitude < 38.678556,
         Longitude < -122.069397) %>%
  rename(name = Store_Name,
         street_address = Address,
         city = City,
         state = State,
         zip_code = Zip5,
         lon = Longitude,
         lat = Latitude) %>%
  mutate(category = "snap/wic") %>%
  select(-County, -X, -Y, -Zip4, -ObjectId, -Address_Line__2) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_intersection(sf_boundary)

write_rds(snap_stores_usda_sf, "data/snap_stores.rds")


# quick visual check
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = snap_stores_usda_sf, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~name)



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

wic_stores_geocoded <- wic_stores %>%
  mutate(address = glue("{street_address}, {city}, {state} {zip_code}")) %>%
  geocode(address, method = "arcgis", lat = latitude, long = longitude) %>%  #osm misses 3 addresses
  select(-address)

wic_stores_sf <- wic_stores_geocoded %>%
  mutate(category = "snap/wic") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) #%>%
  #st_intersection(sf_boundary) locations already in sf county

# Do a visual check
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = wic_stores_sf, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~name)

write_rds(wic_stores_sf, "data/wic_stores.rds")

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


# Prepared Food ---------------------------------------------------------

# http://www.freeprintshop.org/download/eats_english.pdf

# going to enter by hand

prepared_food <- tibble::tribble(
  ~name, ~street_address, ~zip_code,
  "Church Without Walls",    "730 Stanyan St",  "94117",
  "Curry Senior Center", "333 Turk St", "94102",
  "Food Not Bombs", "2000 Mission Street", "94110",
  "Glide Memorial Church", "330 Ellis St", "94102",
  "Homeless Church", "Brannan St & The Embarcadero", "94105",
  "Martin de Porres House of Hospitality", "225 Potrero Ave", "94103",
  "Project Open Hand", "730 Polk St", "94109",
  "St. Anthony’s Dining Room", "121 Golden Gate Ave", "94102",
  "S.F. Rescue Mission", "140 Turk St", "94102",
  "Third Baptist Church", "1399 McAllister St", "94115",
  "United Council of Human Services", "2111 Jennings St", "94124",
  "Macedonia Missionary Baptist Church", "2135 Sutter St", "94115"
) %>%
  mutate(city = "San Francisco",
         state = "CA",
         category = "prepared food")


prepared_food_geocoded <- prepared_food %>%
  mutate(address = glue("{street_address}, {city}, {state} {zip_code}")) %>%
  geocode(address, method = "arcgis", lat = latitude, long = longitude) %>%  #osm misses 3 addresses
  select(-address)

prepared_food_sf <- prepared_food_geocoded %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) #%>%
  #st_intersection(sf_boundary) locations already in sf county

# Do a visual check
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
  addCircleMarkers(data = prepared_food_sf, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~name)

write_rds(prepared_food_sf, "data/prepared_food.rds")

