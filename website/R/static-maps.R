library(tidyverse)
library(sf)
library(janitor)
library(tigris)
library(ggfx)

sf_boundary <- counties(state = "California", cb = TRUE) %>%
  clean_names() %>%
  filter(name == "San Francisco") %>%
  st_transform(4326)

demographics <- read_rds(("data/api_neighborhood_interpolated.rds"))


neighborhoods <- demographics %>%
  filter(name %in% c("SOMA", "Richmond", "Outer Sunset", "Inner Sunset", "Chinatown", "Japantown", "Bayview Hunters Point", "Tenderloin", "Visitacion Valley", "Excelsior")) %>%
  st_drop_geometry() %>%
  pull(name)



generate_static_neighborhood_map <- function(neighborhood_name) {

  file_name <- neighborhood_name %>%
    str_replace_all(" ", "-") %>%
    str_to_lower() %>%
    str_glue(".svg")

  neighborhood_map <-
    ggplot(data = filter(demographics, name == neighborhood_name)) +
    with_shadow(geom_sf(fill = "#AD1D32",
                        color = "transparent"),
                colour = "#806E6E",
                x_offset = 2,
                y_offset = 2) +
    with_shadow(geom_sf_text(aes(label = str_wrap(neighborhood_name, 10)),
                             hjust = case_when(
                               neighborhood_name == "Bayview Hunters Point" ~ 1.1,
                               TRUE ~ NaN
                             ),
                             vjust = case_when(
                               neighborhood_name == "Outer Sunset" ~ 0,
                               TRUE ~ NaN
                             ),
                             family = "Oswald",
                             fontface = "bold",
                             color = "white",
                             size = 12),
                colour = "#806E6E",
                x_offset = 1,
                y_offset = 1) +
    theme_void() +
    theme(plot.background = element_rect(fill = "transparent",
                                         color = "transparent"))

  ggsave(neighborhood_map,
         filename = str_glue("assets/{file_name}"))

  neighborhood_map

}


generate_static_neighborhood_map(neighborhoods[6])

walk(neighborhoods, generate_static_neighborhood_map)

