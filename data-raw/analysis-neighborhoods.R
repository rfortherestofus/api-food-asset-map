library(tidyverse)
library(tidycensus)
library(leaflet)
library(sf)

# from https://data.sfgov.org/Geographic-Locations-and-Boundaries/Analysis-Neighborhoods-2010-census-tracts-assigned/bwbp-wk3r
tracts <- read_sf("data-raw/analysis_nb_tracts_2010.geojson")

# list of available tidycensus data, we want a table that has both race and ethnicity to account for hispanic/latino
v19 <- load_variables(year = 2019, dataset = "acs5")

var_labels <- v19 %>%
  filter(str_detect(name, "B03002")) %>% # in the hisp/latino by race table
  mutate(label = str_remove(label, "Estimate!!")) %>% # cleaner later on when the word 'Estimate' is added back
  pull(name, name = label)

# pull table B03002 with the variable labels instead of numeric codes

race_ethnicity <- get_acs(geography = "tract", variables = var_labels, year = 2019, state = "CA", county = "San Francisco", geometry = FALSE)

# one tract per row

race_ethnicity_clean <- race_ethnicity %>%
  pivot_wider(names_from = variable, values_from = c(estimate, moe)) %>%
  janitor::clean_names()

# join tidycensus data to the tract-neighborhood pairings

race_ethnicity_nb_join <- tracts %>%
  select(nhood, geoid) %>%
  left_join(race_ethnicity_clean, by = "geoid")

# calculate summary statistics by neighborhood (plus 90% CI)

api_summary_nb <- race_ethnicity_nb_join %>%
  group_by(nhood) %>%
  summarise(total = sum(estimate_total),
            moe_total = moe_sum(moe_total, estimate = estimate_total),
            asian_alone = sum(estimate_total_not_hispanic_or_latino_asian_alone),
            moe_asian_alone = moe_sum(moe_total_hispanic_or_latino_asian_alone, estimate = estimate_total_not_hispanic_or_latino_asian_alone),
            pac_is_alone = sum(estimate_total_hispanic_or_latino_native_hawaiian_and_other_pacific_islander_alone),
            moe_pac_is_alone = moe_sum(moe_total_hispanic_or_latino_native_hawaiian_and_other_pacific_islander_alone, estimate = estimate_total_hispanic_or_latino_native_hawaiian_and_other_pacific_islander_alone),
            api = sum(asian_alone, pac_is_alone),
            moe_api = moe_sum(c(moe_asian_alone, moe_pac_is_alone), estimate = c(asian_alone, pac_is_alone))) %>%
  ungroup() %>%
  mutate(pct_api = api/total,
         moe_pct_api = moe_prop(api, total, moe_api, moe_total),
         pct_api_clean = if_else(moe_pct_api > .3 * pct_api, NA_real_, round(pct_api * 100, 2))) # make the label NA is the moe is more than 30% of the estimate. This threshold is fairly arbitrary, but we should use *some* threshold


# leaflet for basic analysis

qpal <- colorQuantile("Blues", api_summary_nb$pct_api_clean, n = 5, na.color = "#cdced1")

leaflet(api_summary_nb) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = TRUE, fillOpacity = 0.8,
              weight = 1, color = "#424242",
              fillColor = ~qpal(pct_api_clean),
              label = ~nhood
              ) %>%
  addLegend(pal = qpal, values = ~pct_api_clean, opacity = 0.7, title = "% API",
            position = "bottomright")
