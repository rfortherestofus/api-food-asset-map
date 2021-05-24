library(tidyverse)
library(sf)
library(googlesheets4)

read_rds("data/snap_stores.rds") %>%
  select(name:zip_code) %>%
  st_drop_geometry() %>%
  write_sheet("https://docs.google.com/spreadsheets/d/1AnOuJMK5HHqe3mDn0aH3Mf3JYfLS61uWnvb_kHp1xFY/edit?usp=sharing",
              sheet = "SNAP/WIC Stores")

read_rds("data/full_dataset.rds") %>%
  select(name:zip_code) %>%
  st_drop_geometry() %>%
  write_sheet("https://docs.google.com/spreadsheets/d/1AnOuJMK5HHqe3mDn0aH3Mf3JYfLS61uWnvb_kHp1xFY/edit?usp=sharing",
              sheet = "Full Dataset")

read_rds("data/potential_snap_wic_matches.rds") %>%
  write_sheet("https://docs.google.com/spreadsheets/d/1AnOuJMK5HHqe3mDn0aH3Mf3JYfLS61uWnvb_kHp1xFY/edit?usp=sharing",
              sheet = "Potential SNAP/WIC Matches")

read_rds("data/final_potential_duplicates.rds") %>%
  arrange(distance) %>%
  write_sheet("https://docs.google.com/spreadsheets/d/1AnOuJMK5HHqe3mDn0aH3Mf3JYfLS61uWnvb_kHp1xFY/edit?usp=sharing",
              sheet = "Potential Duplicates")
