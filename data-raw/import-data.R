
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(rvest)
library(janitor)
library(stringr.plus)


# foodpantries.org --------------------------------------------------------

# Data from https://www.foodpantries.org/ci/ca-san_francisco

# Get HTML from main page
food_pantries_html <- read_html("https://www.foodpantries.org/ci/ca-san_francisco")


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
  )

  pantry_data

}


# Create vector of all URLs of food pantry pages
pantries_urls <- food_pantries_html %>%
  html_elements("a") %>%
  html_attr("href") %>%
  as_tibble() %>%
  filter(str_detect(value, "/li/")) %>%
  distinct() %>%
  pull(value)

pantries_data <- map_df(pantries_urls, import_food_pantry_data)
