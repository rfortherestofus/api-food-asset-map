library(tidyverse)
library(sf)
library(googlesheets4)

read_rds("data/snap_stores.rds") %>%
  select(name:zip_code) %>%
  st_drop_geometry() %>%
  write_sheet("https://docs.google.com/spreadsheets/d/1AnOuJMK5HHqe3mDn0aH3Mf3JYfLS61uWnvb_kHp1xFY/edit?usp=sharing",
              sheet = "SNAP/WIC Stores")
