
# Load Packages -----------------------------------------------------------

# packages used in web scarping and osm importing
library(tidyverse)
library(leaflet)
library(sf) # spatial data wrangling
library(janitor)
library(tigris) # census geography boundaries
library(beepr) # notify when long process finishes

# for web scraping
library(rvest) # read in and extract data from html documents online
library(stringr.plus)
library(tidygeocoder)
library(glue)
library(RSelenium)

# for osm
library(osmdata) # OSM overpass API
library(htmltools)
library(sf) # spatial data wrangling


# Web Scraping Data -------------------------------------------------------

# load San Francisco boundary
sf_boundary <- counties(state = "California", cb = TRUE) %>%
  clean_names() %>%
  filter(name == "San Francisco") %>%
  st_transform(4326)

## foodpantries.org --------------------------------------------------------

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
  geocode(address, method = "mapbox", lat = latitude, long = longitude) %>%  #osm misses 3 addresses
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




## Food Pharmacies ---------------------------------------------------------

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
  geocode(address, method = "mapbox", lat = latitude, long = longitude) %>%
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


## SF Marin Food Bank ------------------------------------------------------

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

# grab data
pop_up_addr <- map_df(pop_up_pantry_links$value, pop_up_get_addr)

remDr$quit()

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
  geocode(address, method = "mapbox", lat = latitude, long = longitude) %>%  #osm misses 3 addresses
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

## Bay Area 211 ------------------------------------------------------------

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

    agency_nums <- agency_site %>%
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
  geocode(address, method = "mapbox", lat = latitude, long = longitude) %>%  #osm misses 3 addresses
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

## Food-related registered businesses ---------------------------------------------------------

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



## Stores that accept EBT/SNAP ---------------------------------------------

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



## WIC Stores --------------------------------------------------------------

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
  geocode(address, method = "mapbox", lat = latitude, long = longitude) %>%  #osm misses 3 addresses
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

## Farmers Markets ---------------------------------------------------------

# https://sfenvironment.org/farmers-markets-in-sf

# website super unstructured, best to do by hand
farmers_markets_html <- read_html("https://sfenvironment.org/farmers-markets-in-sf")


farmers_markets_names <- farmers_markets_html %>%
  html_elements("p :nth-child(1) :nth-child(1)") %>%
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


# need to fix street address (by hand easiset), gives scrambled results otherwise

fixed_street_address <- c(
  "100 Alemany Blvd.",
  "3861 24th St.",
  "1605 Jerrold Ave",
  "1730 O'Farrell St",
  "One Ferry Building #50",
  "1182 Market St.",
  "1 Warriors Way",
  "1315 8th Ave",
  "200 Clement St",
  "3251 20th Avenue",
  "1994 37th Ave",
  "2 Marina Blvd",
  "1377 Fell St.",
  "One Ferry Building #50",
  "1 United Nations Plaza",
  "16 Veterans Dr",
  "288 Noe St",
  "One Ferry Building #50",
  "84 Bartlett St",
  "288 Noe St",
  "84 Bartlett St",
  "699 Columbus Ave",
  "46th and Taraval"
)

fixed_zips <- c(
  "94110",
  "94114",
  "94124",
  "94115",
  "94111",
  "94102",
  "94158",
  "94122",
  "94118",
  "94132",
  "94116",
  "94109",
  "94117",
  "94111",
  "94102",
  "94127",
  "94114",
  "94111",
  "94110",
  "94114",
  "94110",
  "94133",
  "94116"
)

farmers_markets <- tibble(
  name = farmers_markets_names,
  street_address = fixed_street_address,
  zip_code = fixed_zips
) %>%
  distinct() %>%
  mutate(city = "San Francisco",
         state = "CA")


farmers_markets_geocoded <- farmers_markets %>%
  mutate(address = glue("{street_address}, {city}, {state} {zip_code}")) %>%
  geocode(address, method = "mapbox", lat = latitude, long = longitude) %>%  #osm misses 3 addresses
  select(-address)

# manually fix Ferry Plaza Market
farmers_markets_geocoded$latitude[farmers_markets_geocoded$name == "Ferry Plaza Farmers Market"] <- 37.79782980366289
farmers_markets_geocoded$longitude[farmers_markets_geocoded$name == "Ferry Plaza Farmers Market"] <-  -122.39378060891158



farmers_markets_sf <- farmers_markets_geocoded %>%
  mutate(category = "farmers market") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) #%>%
#st_intersection(sf_boundary) locations already in sf county

# Do a visual check
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
  addCircleMarkers(data = farmers_markets_sf, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~name)
# filmore farmers market is in Canada?

write_rds(farmers_markets_sf, "data/farmers_markets.rds")


## Prepared Food ---------------------------------------------------------

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
  geocode(address, method = "mapbox", lat = latitude, long = longitude) %>%  #osm misses 3 addresses
  select(-address)

prepared_food_sf <- prepared_food_geocoded %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) #%>%
#st_intersection(sf_boundary) locations already in sf county

# Do a visual check
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = prepared_food_sf, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~name)

write_rds(prepared_food_sf, "data/prepared_food.rds")

## Food Banks ---------------------------------------------------------

# manually enter data
food_banks <- tibble::tribble(~name, ~street_address, ~city, ~state, ~zip_code,
                              "San Francisco-Marin Food Bank", "900 Pennsylvania Ave", "San Francisco", "CA", "94107",
                              "Food Runners", "2579 Washington St", "San Francisco", "CA", "94115",
                              "Free the Need", "827 Joost Ave", "San Francisco", "CA", "94127",
                              "San Francisco-Marin Food Bank-Illinois Warehouse", "1050 Marin St", "San Francisco", "CA", "94124") %>%
  mutate(category = "Food Bank")

# geocode
food_banks_geocoded <- food_banks  %>%
  mutate(address = glue("{street_address}, {city}, {state} {zip_code}")) %>%
  geocode(address, method = "mapbox", lat = latitude, long = longitude) %>%
  select(-address)

food_banks_sf <- food_banks_geocoded %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Do a visual check
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = food_banks_sf, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~name)

write_rds(food_banks_sf, "data/food_banks.rds")

## International food markets ---------------------------------------------------------

international_markets <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSIv3qx7IXACIQFOp6jhyZ1-9LBHkBAdHE4WnS2diosy-hfWk9nF-GDPHqW-pYR1bf1XERgyAZ_L7bs/pub?output=csv") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  mutate(zip_code = as.character(zip_code)) %>%
  mutate(category = "International Grocery Store")


write_rds(ethnic_markets, "data/international_markets.rds")


# OSM Data --------------------------------------

## Supermarket query ----------------------------

supermarket_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("shop", "supermarket") %>%
  osmdata_sf()

supermarkets <- supermarket_q$osm_polygons %>%
  st_intersection(sf_boundary) %>% # the supermarket query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()


supermarkets_pts <- supermarket_q$osm_points %>%
  st_intersection(sf_boundary) %>% # the supermarket query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()

seafood_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("shop", "seafood") %>%
  osmdata_sf()

seafood <- seafood_q$osm_polygons %>%
  st_intersection(sf_boundary) %>% # the supermarket query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()


seafood_pts <- seafood_q$osm_points %>%
  st_intersection(sf_boundary) %>% # the supermarket query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()

grocery_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("amenity", "grocery") %>%
  osmdata_sf()


grocery_pts <- grocery_q$osm_points %>%
  st_intersection(sf_boundary) %>% # the supermarket query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()

greengrocery_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("shop", "greengrocer") %>%
  osmdata_sf()


greengrocery_pts <- greengrocery_q$osm_points %>%
  st_intersection(sf_boundary) %>% # the supermarket query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()

greengrocery <- greengrocery_q$osm_polygons %>%
  st_intersection(sf_boundary) %>% # the supermarket query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()



supermarkets_clean <- supermarkets %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "supermarket") %>%
  rbind(supermarkets_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "supermarket")) %>%
  bind_rows(grocery_pts %>%
              transmute(osm_id, name, city = "San Francisco", state = "CA", business_type = "supermarket")) %>%
  rbind(greengrocery %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "supermarket")) %>%
  rbind(greengrocery_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "supermarket")) %>%
  rbind(seafood_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "supermarket")) %>%
  drop_na(name)

# visually check data

# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = supermarkets_clean, fillColor = "#840651", color = "#840651", opacity = 1, fillOpacity = 0.7, weight = 1, label = ~htmlEscape(name), radius = 2)

write_rds(supermarkets_clean, "data/supermarkets.rds")

## Convenience store query -----------------------------------------------------------

convenience_store_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("shop", "convenience") %>%
  osmdata_sf()

convenience_stores <- convenience_store_q$osm_polygons %>%
  st_intersection(sf_boundary) %>% # the convenience store query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()

convenience_stores_pts <- convenience_store_q$osm_points %>%
  st_intersection(sf_boundary) %>% # the convenience store query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()

fuel_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("amenity", "fuel") %>%
  osmdata_sf()

fuel <- fuel_q$osm_polygons %>%
  st_intersection(sf_boundary) %>% # the convenience store query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()

fuel_pts <- fuel_q$osm_points %>%
  st_intersection(sf_boundary) %>% # the convenience store query includes some from outside of the city, so this limits the data to only supermarkets in SF
  st_centroid()

convenience_stores_clean <- convenience_stores %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "convenience") %>%
  rbind(convenience_stores_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "convenience")) %>%
  drop_na(name)

# visually check data

# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = convenience_stores_clean, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~htmlEscape(name))

write_rds(convenience_stores_clean, "data/convenience_stores_osm.rds")

## Restaurants query -------------------------------------------------------

restaurant_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("amenity", "restaurant") %>%
  osmdata_sf()

restaurants <- restaurant_q$osm_polygons %>%
  st_intersection(sf_boundary) %>%
  st_centroid()

restaurants_pts <- restaurant_q$osm_points %>%
  st_intersection(sf_boundary) %>%
  st_centroid()

restaurants_clean <- restaurants %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "restaurant") %>%
  rbind(restaurants_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "restaurant")) %>%
  drop_na(name)

# visually check data

# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = restaurants_clean, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~htmlEscape(name))

write_rds(restaurants_clean, "data/restaurants_osm.rds")

## Fast food query -------------------------------------------------------------------------

fastfood_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("amenity", "fast_food") %>%
  osmdata_sf()

fast_food <- fastfood_q$osm_polygons %>%
  st_intersection(sf_boundary) %>%
  st_centroid()

fast_food_pts <- fastfood_q$osm_points %>%
  st_intersection(sf_boundary) %>%
  st_centroid()

fast_food_clean <- fast_food %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "fast food") %>%
  rbind(fast_food_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "fast food")) %>%
  drop_na(name)

# visually check data

# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = fast_food_clean, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~htmlEscape(name))

write_rds(fast_food_clean, "data/fast_food_osm.rds")


## Farmers market query -----------------------------------------------------------------

market_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("amenity", "marketplace") %>%
  osmdata_sf()

markets <- market_q$osm_polygons %>%
  st_intersection(sf_boundary) %>%  # the convenience store query includes some from outside of the city, so this limits the data to only places in SF
  st_centroid()

markets_pts <- market_q$osm_points %>%
  st_intersection(sf_boundary) %>%  # the convenience store query includes some from outside of the city, so this limits the data to only places in SF
  st_centroid()

markets_clean <- markets %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, city = "San Francisco", state = "CA", business_type = "market") %>%
  rbind(markets_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, city = "San Francisco", state = "CA", business_type = "market")) %>%
  drop_na(name)

# visually check data

# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = markets_clean, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~htmlEscape(name))

write_rds(markets_clean, "data/markets_osm.rds")

## Drugstore query ---------------------------------------------------------------------------

chemist_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("shop", "chemist") %>% # drugstore is deprecated, chemist returns subset of these results
  osmdata_sf()

chemist_pts <- chemist_q$osm_points %>%
  st_intersection(sf_boundary) %>%
  st_centroid()

drugstore_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("amenity", "pharmacy") %>%
  osmdata_sf()

drugstores <- drugstore_q$osm_polygons %>%
  st_intersection(sf_boundary) %>%
  st_centroid()

drugstores_pts <- drugstore_q$osm_points %>%
  st_intersection(sf_boundary) %>%
  st_centroid()

drugstores_clean <- drugstores %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "drugstore") %>%
  rbind(drugstores_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street , zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "drugstore")) %>%
  rbind(chemist_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street , zip = addr.postcode, city = "San Francisco", state = "CA", business_type = "drugstore")) %>%
  drop_na(name)

# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sf_boundary, fillOpacity = 0, opacity = 1, color = "#FFB55F", weight = 2) %>%
#   addCircleMarkers(data = drugstores_clean, fillColor = "#5F9AB6", color = "#5F9AB6", opacity = 1, fillOpacity = 0.7, weight = 1, radius = 2, label = ~htmlEscape(name))

write_rds(drugstores_clean, "data/drugstores_osm.rds")


## Liquor stores --------------------------------------------------------------------

liquor_q <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature("shop", "alcohol") %>%
  osmdata_sf()

liquor_pts <- liquor_q$osm_points

liquor_poly <- liquor_q$osm_polygons %>%
  st_centroid()


liquor_clean <- liquor_poly %>%
  transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street, zip = addr.postcode, city = "San Francisco", state = "CA", category = "Liquor Store") %>%
  rbind(liquor_pts %>%
          transmute(osm_id, name, housenumber = addr.housenumber, street = addr.street , zip = addr.postcode, city = "San Francisco", state = "CA", category = "Liquor Store")) %>%
  drop_na(name)

write_rds(liquor_clean, "data/liquor_stores_osm.rds")



beep(5)
