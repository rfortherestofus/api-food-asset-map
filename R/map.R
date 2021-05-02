library(tidyverse)
library(leaflet)
library(leafem)
library(tigris)
library(here)

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

demographics <- read_rds(here("data/api_neighborhood_data.rds"))

demo_pal <- colorQuantile(
  palette = "Blues",
  domain = demographics$pct_api_clean,
  n = 5,
  na.color = light_gray)

## Draft of leaflet map

leaflet(sf_boundary) %>%

  # basemap
  addProviderTiles(provider = providers$CartoDB.Positron) %>%

  # data layers
  # addPolygons(fillColor = "transparent", color = "#858585", opacity = 1, weight = 2) %>%
  addPolygons(data = demographics, group = "demographics", fillColor = ~demo_pal(pct_api_clean), color = gray, weight = 1, fillOpacity = .8) %>%

  # legends

  addLegend(position = "bottomright", pal = demo_pal, values = demographics$pct_api_clean, opacity = .8, title = "% Asian or Pacific Islander<br>ACS 2019 (5-year)", group = "demographics") %>%

  # layer controls

  addLayersControl(
    overlayGroups = c("demographics"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%


  ## add other assets
  addLogo(img = here("assets/api_logo.png"), src = "local", width = 160, position = "bottomleft", offset.x = 10)
