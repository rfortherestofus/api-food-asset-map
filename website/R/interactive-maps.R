# in order to update the category labels/icons shown on the map, you'll need to edit the code in 2 distinct places.
# first, you'll need to edit the icon url in the case_when function around line 47 (this may change as other edits are made)
# second, you'll need to edit the assignment in the `groups` object. This seems redundant, but it's important for the way the labels are rendered.

draw_resource_map <- function(neighborhoods = NULL) {

  # Load packages

  library(tidyverse)
  library(leaflet)
  library(leafem)
  library(tigris)
  library(here)
  library(sf)
  library(janitor)
  library(pluralize)


  ## boundary for San Francisco

  sf_boundary <- counties(state = "California", cb = TRUE, progress_bar = FALSE) %>%
    clean_names() %>%
    filter(name == "San Francisco") %>%
    st_transform(4326)


  ## read in data for map

  clean_interval <- function(x){
    str_remove_all(x, "\\[|\\]|\\(|\\)") %>% str_replace(",", " to ") %>% paste0("%")
  }

  demographics <- read_rds("data/api_neighborhood_interpolated.rds") %>%
    mutate(pct_api_interval = cut(pct_api_clean, breaks = c(0, 14, 24, 34, 44, 100), include.lowest = TRUE, ordered = TRUE),
           pct_api_category = fct_relabel(pct_api_interval, clean_interval)) %>%
    rename(nhood = name) %>%
    arrange(nhood) %>%
    mutate(nhood = fct_inorder(factor(nhood)))

  st_crs(demographics) <- 26910
  demographics <- demographics %>%
    st_transform(4326)

  food_resources <- read_rds("data/final_dataset_no_dupes.rds")
  st_crs(food_resources) <- 4326
  food_resources <- food_resources %>%
    st_join(demographics %>% select(nhood)) %>%
    mutate(street_address = str_trim(street_address)) %>%
    filter(city == "San Francisco") %>%
    st_intersection(sf_boundary %>% select()) %>%
    arrange(category) %>%
    mutate(category = fct_inorder(category),

           icon_url = case_when(category == "Corner Stores/Convenience Stores" & accepts_snap_wic == FALSE ~ "assets/corner-stores.svg",

                                category == "Drug Stores" & accepts_snap_wic == FALSE ~ "assets/drug-stores.svg",
                                category == "Food Banks/Pantries" & accepts_snap_wic == FALSE ~ "assets/food-banks-pantries.svg",
                                category == "Food Pharmacies" & accepts_snap_wic == FALSE ~ "assets/food-pharmacy.svg",
                                category == "Free Prepared Food or Hot Meals" & accepts_snap_wic == FALSE ~ "assets/free-prepared-hot-meals.svg",
                                category == "Liquor Stores" & accepts_snap_wic == FALSE ~ "assets/liquor-stores.svg",
                                category == "International Grocery Stores" & accepts_snap_wic == FALSE ~ "assets/international-grocery-stores.svg",
                                category == "Restaurants (Fast Food)" & accepts_snap_wic == FALSE ~ "assets/restaurants-fast-food.svg",
                                category == "Restaurants" & accepts_snap_wic == FALSE ~ "assets/restaurants.svg",
                                category == "Supermarkets/Grocery Stores" & accepts_snap_wic == FALSE ~ "assets/supermarkets.svg",
                                category == "Farmers Markets" & accepts_snap_wic == FALSE ~ "assets/farmers-market.svg",
                                category == "Corner Stores/Convenience Stores" & accepts_snap_wic == TRUE ~ "assets/corner-stores-snap-wic.svg",
                                category == "Supermarkets/Grocery Stores" & accepts_snap_wic == FALSE ~ "assets/supermarkets.svg",
                                category == "Farmers Markets" & accepts_snap_wic == FALSE ~ "assets/farmers-market.svg",

                                ## SNAP/WIC has different color
                                category == "Drug Stores" & accepts_snap_wic == TRUE ~ "assets/drug-stores-snap-wic.svg",
                                category == "Food Banks/Pantries" & accepts_snap_wic == TRUE ~ "assets/food-banks-pantries-snap-wic.svg",
                                category == "Food Pharmacies" & accepts_snap_wic == TRUE ~ "assets/food-pharmacy-snap-wic.svg",
                                category == "Free Prepared Food or Hot Meals" & accepts_snap_wic == TRUE ~ "assets/free-prepared-hot-meals-snap-wic.svg",
                                category == "Liquor Stores" & accepts_snap_wic == TRUE ~ "assets/liquor-stores-snap-wic.svg",
                                category == "International Grocery Stores" & accepts_snap_wic == TRUE ~ "assets/international-grocery-stores-snap-wic.svg",
                                category == "Restaurants (Fast Food)" & accepts_snap_wic == TRUE ~ "assets/restaurants-fast-food-snap-wic.svg",
                                category == "Restaurants" & accepts_snap_wic == TRUE ~ "assets/restaurants-snap-wic.svg",
                                category == "Supermarkets/Grocery Stores" & accepts_snap_wic == TRUE ~ "assets/supermarkets-snap-wic.svg",
                                category == "Farmers Markets" & accepts_snap_wic == TRUE ~ "assets/farmers-market-snap-wic.svg",
                                TRUE ~ NA_character_
           )
    )


  # Design

  dark_red <- "#AD1D32"
  light_red <- "#E00052"
  off_white <- "#FDFAF9"
  gray <- "#806E6E"
  light_gray <- "#ECF0F1"


  show_api <- "<span style='border-bottom-width: 2px;border-bottom-style: solid #000;padding-bottom: 10px;'>Show API Population<br></span><br>"

  groups <- c(
    "Corner Stores/Convenience Stores" <- "Corner Stores/Convenience Stores",
    "Drug Stores" <- "Drug Stores",
    "Farmers Markets" <- "Farmers Markets",
    "Food Banks/Pantries" <- "Food Banks/Pantries",
    "Food Pharmacies" <- "Food Pharmacies",
    "Free Prepared Food or Hot Meals" <- "Free Prepared Food or Hot Meals",
    "International Grocery Stores" <- "International Grocery Stores",
    "Liquor Stores" <- "Liquor Stores",
    "Restaurants" <- "Restaurants",
    "Restaurants (Fast Food)" <- "Restaurants (Fast Food)",
    "Supermarkets/Grocery Stores" <- "Supermarkets/Grocery Stores"
  )

  ## color palettes

  demo_pal <- colorFactor(
    palette = c("#F5D0C9", "#E5A6A3", "#D57C7C", "#C45156", "#B4272F"),
    domain = demographics$pct_api_category,
    na.color = "#FFFFFF40")

  ## popups

  # make popup text with name, address, notes, and link to website
  food_resources <- food_resources %>%
    mutate(singular_category = singularize(category),
           popup = paste("<span style='font-family: Oswald; font-weight:400; font-size:14px; color: #6B7280;'>", singular_category, if_else(accepts_snap_wic, " (Accepts SNAP/WIC)", ""), "</span>", "<br/>",
                         "<span style='font-family: Oswald; font-weight:700; font-size:20px; color: #111827'>", name,"</span>", "<br/>",
                         "<span style='font-family: Oswald; font-weight:400; font-size:14px; color: #6B7280;'>", street_address, " (", nhood, ") </span>", "<br/>",
                         sep='')) %>%
    filter(category %in% groups) # for now, filter out the SNAP/WIC category


  demographics <- demographics %>%
    mutate(popup = case_when(
      !is.na(pct_api_clean) ~  paste("<span style='font-family: Oswald; font-weight:400; font-size:14px; color: #6B7280'>", nhood,"<br></span>",
                                     "<span style='font-family: Oswald; font-weight:700; font-size:20px; color: #111827;'>", pct_api_display, " API Population", "</span>", "<br/>",
                                     sep=''),
      is.na(pct_api_clean) ~ paste("<span style='font-family: Oswald; font-weight:700; font-size:20px; color: #111827'>", nhood,"<br></span>",
                                   sep='')))


  # Filter data

  if (!is.null(neighborhoods)) {
    demographics <- demographics %>%
      filter(nhood %in% neighborhoods)
    food_resources <- food_resources %>%
      filter(nhood %in% neighborhoods)
  }

  # Make map

  map <- leaflet() %>%
    addProviderTiles(provider = providers$CartoDB.Positron) %>%
    addPolygons(data = demographics, group = show_api, fillColor = ~ demo_pal(pct_api_category), color = "#ffffff", weight = 0.5, fillOpacity = .8, popup = ~popup, opacity = 1)

  # use walk from per to return m after adding each layer
  groups %>% # get names of resource groups
    walk(function(x) { # then walk through vector of names one at a time
      map <<-
        map %>%
        addMarkers(
          data = food_resources %>% filter(category == x),
          icon = makeIcon(iconUrl = ~icon_url, iconWidth = 24, iconHeight = 24),
          popup = ~popup,
          group = x,

          # in order for all the clusters to be green, I edited the if else statement to put any cluster with less that or equal to the total number of rows in the `small` category. Since no cluster can be larger than the dataset, there can't be anything larger than `small`
          clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                iconCreateFunction=JS(glue::glue("function (cluster) {{
    var childCount = cluster.getChildCount();
    var c = ' marker-cluster-';
    if (childCount <= {nrow(food_resources)}) {{
      c += 'small';
    }} else if (childCount > {nrow(food_resources)}) {{
      c += 'medium';
    }} else {{
      c += 'large';
    }}
    return new L.DivIcon({{ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) }});

  }}")))
        )
    })

  if (!is.null(neighborhoods)) {
    map <-
      map %>%
      addLayersControl(
        overlayGroups = c(groups),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
      )
  } else {
    map <-
      map %>%

      # legends

      addLegend(position = "bottomright", pal = demo_pal, values = demographics$pct_api_category, opacity = 1, title = "API Population", group = show_api, na.label = "Data not available") %>%


      addLayersControl(
        overlayGroups = c(show_api, groups),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(show_api)
  }

  map
}

draw_resource_map()
