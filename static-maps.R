library(tidyverse)
library(sf)
library(janitor)
library(tigris)

sf_boundary <- counties(state = "California", cb = TRUE) %>%
  clean_names() %>%
  filter(name == "San Francisco") %>%
  st_transform(4326)

demographics <- read_rds(("data/api_neighborhood_data.rds"))

neighborhoods <- demographics %>%
  filter(nhood %in% c("SOMA", "Richmond", "Sunset", "Chinatown", "Japantown", "Bayview Hunters Point", "Tenderloin", "Visitacion Valley", "Excelsior")) %>%
  st_drop_geometry() %>%
  pull(nhood)

title_position <- filter(demographics, nhood == "Japantown") %>%
  st_centroid()



generate_static_neighborhood_map <- function(neighborhood_name) {

  file_name <- neighborhood_name %>%
    str_replace_all(" ", "-") %>%
    str_to_lower() %>%
    str_glue(".svg")

  ggplot(data = filter(demographics, nhood == neighborhood_name),) +
    geom_sf(fill = "#AD1D32",
            color = "transparent") +
    geom_sf_text(aes(label = str_wrap(neighborhood_name, 10)),
                 family = "Oswald",
                 color = "white",
                 size = 12) +
    theme_void()

  ggsave(str_glue("assets/{file_name}"))

}

generate_static_neighborhood_map(neighborhoods[1])

walk(neighborhoods, generate_static_neighborhood_map)

