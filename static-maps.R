library(tidyverse)
library(sf)
library(janitor)
library(tigris)

sf_boundary <- counties(state = "California", cb = TRUE) %>%
  clean_names() %>%
  filter(name == "San Francisco") %>%
  st_transform(4326)

demographics <- read_rds(("data/api_neighborhood_data.rds"))

ggplot() +
  # geom_sf(data = demographics,
  #         color = "white",
  #         size = 0.1) +
  geom_sf(data = filter(demographics, nhood == "Visitacion Valley"),
          fill = "transparent",
          color = "#AD1D32") +
  # labs(title = "Visitacion Valley") +
  # theme(plot.title = element_text(hjust = 0.5)) +
  theme_void()
