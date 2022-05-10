library(tidyverse)
library(sf)
library(janitor)
library(ggfx)
library(ggirl)
library(tigris)


# Get Data ----------------------------------------------------------------

sf_boundary <- counties(state = "California", cb = TRUE, progress_bar = FALSE) %>%
  clean_names() %>%
  filter(name == "San Francisco") %>%
  st_transform(4326)

food_resources <- read_rds("data/final_dataset_no_dupes.rds") %>%
  filter(city == "San Francisco") %>%
  st_intersection(sf_boundary %>% select())

demographics <- read_rds("data/api_neighborhood_interpolated.rds") %>%
  st_transform(4326)


# Make Maps ----------------------------------------------------------------


dark_red <- "#AD1D32"
light_red <- "#E00052"
off_white <- "#FDFAF9"
gray <- "#806E6E"
light_gray <- "#ECF0F1"

sf_food_resources_map <- ggplot() +
  with_shadow(geom_sf(data = demographics,
          fill = off_white,
          color = gray,
          size = 0.04),
          color = light_gray,
          x_offset = 0.5,
          y_offset = 0.5) +
  with_shadow(geom_sf(data = food_resources,
          shape = 21,
          size = 0.5,
          fill = light_red,
          color = dark_red),
          color = light_gray,
          x_offset = 0.5,
          y_offset = 0.5) +
  theme_void() +
  theme(plot.title = element_text(family = "Catamaran",
                                  color = dark_red,
                                  size = 25,
                                  hjust = 0.5,
                                  face = "bold"),
        plot.margin = margin(t = 50,
                             b = 30))

sf_food_resources_map_contractor <- sf_food_resources_map +
  labs(title = "Thanks for all of your hard work!")

sf_food_resources_map_client <- sf_food_resources_map +
  labs(title = "Thanks for working with us!")


# Order prints ------------------------------------------------------------

katie_address <- address(name = "Katie Jolly",
                         address_line_1 = "8745 Greenwood Ave",
                         address_line_2 = "Apr 101",
                         city = "Seattle",
                         state = "WA",
                         postal_code = "98103",
                         country = "US")

ggartprint(sf_food_resources_map_contractor,
           size = "11x14",
           orientation = "landscape",
           quantity = 1,
           contact_email = "david@rfortherestofus.com",
           address = katie_address)


ellen_address <- address(name = "Ellen Graham",
                         address_line_1 = "1710 Ashland Ave",
                         address_line_2 = "Apt 2",
                         city = "St Paul",
                         state = "MN",
                         postal_code = "55105",
                         country = "US")

ggartprint(sf_food_resources_map_contractor,
           size = "11x14",
           orientation = "landscape",
           quantity = 1,
           contact_email = "david@rfortherestofus.com",
           address = ellen_address)


kathleen_address <- address(name = "Kathleen Doll",
                         address_line_1 = "536 Las Palmas",
                         city = "Irvine",
                         state = "CA",
                         postal_code = "92602",
                         country = "US")

ggartprint(sf_food_resources_map_client,
           size = "11x14",
           orientation = "landscape",
           quantity = 1,
           contact_email = "david@rfortherestofus.com",
           address = kathleen_address)
