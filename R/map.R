library(tidyverse)
library(leaflet)
library(leafem)
library(tigris)
library(here)
library(sf)
library(janitor)

## Colorscheme

dark_red <- "#AD1D32"
light_red <- "#E00052"
off_white <- "#FDFAF9"
gray <- "#806E6E"
light_gray <- "#ECF0F1"

## boundary for San Francisco

sf_boundary <- counties(state = "California", cb = TRUE) %>%
  clean_names() %>%
  filter(name == "San Francisco") %>%
  st_transform(4326)


## read in data for map

source("R/read_in_map_data.r")


## color palettes

demo_pal <- colorQuantile(
  palette = "Blues",
  domain = demographics$pct_api_clean,
  n = 5,
  na.color = light_gray)

## labels

convenience_labels <- sprintf(
  "<strong>%s</strong><br/>%s in %s</sup>",
  convenience_stores$name, convenience_stores$business_type, convenience_stores$nhood
) %>% lapply(htmltools::HTML)

pantry_labels <- sprintf(
  "<strong>%s</strong><br/>%s in %s</sup>",
  food_pantries$name, food_pantries$category, food_pantries$nhood
) %>% lapply(htmltools::HTML)

## Draft of leaflet map

leaflet(sf_boundary) %>%

  # basemap
  addProviderTiles(provider = providers$CartoDB.Positron) %>%

  # data layers
  # addPolygons(fillColor = "transparent", color = "#858585", opacity = 1, weight = 2) %>%
  addPolygons(data = demographics, group = "Demographics", fillColor = ~demo_pal(pct_api_clean), color = gray, weight = 1, fillOpacity = .8) %>%

  # legends

  addLegend(position = "bottomright", pal = demo_pal, values = demographics$pct_api_clean, opacity = .8, title = "% Asian or Pacific Islander<br>ACS 2019 (5-year)", group = "Demographics") %>%

  addCircles(data = food_pantries,
             radius = 3,
             group = "Food pantries",
             color = gray,
             opacity = .8,
             label = pantry_labels,
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px", "color" = gray),
               textsize = "15px",
               direction = "auto")) %>%

  addCircles(data = convenience_stores,
             radius = 3,
             group = "Convenience stores",
             color = light_red,
             opacity = 0.8,
             label = convenience_labels,
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px", "color" = dark_red),
               textsize = "15px",
               direction = "auto")) %>%

  # layer controls

  addLayersControl(
    overlayGroups = c("Demographics", "Food pantries", "Convenience stores"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  hideGroup("Demographics") %>%


  ## add other assets
  addLogo(img = here("assets/api_logo.png"), src = "local", width = 160, position = "bottomleft", offset.x = 10)
