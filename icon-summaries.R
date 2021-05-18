library(tidyverse)
library(ggimage)
library(rsvg)
library(grImport2)
library(ggplot2)
library(scales)
library(sf)
library(glue)


library(extrafont)
loadfonts(device = "win")

full_dataset <- read_rds("data/full_dataset.rds")

category_counts <- full_dataset %>%
  st_drop_geometry() %>%
  count(category) %>%
  filter(category != "Stores that Accept SNAP/WIC")

label_table <-tribble(~store_type, ~category,
        "corner-stores", "Corner Stores",
        "drug-stores", "Drug Stores",
        "farmers-market", "Farmers Markets",
        "food-banks-pantries", "Food Banks/Pantries",
        "food-pharmacy", "Food Pharmacies",
        "free-prepared-hot-meals", "Free Prepared Food or Hot Meals",
        "international-grocery-stores", "International Grocery Stores",
        "liquor-stores", "Liquor Stores",
        "restaurants", "Restaurants",
        "restaurants-fast-food", "Restaurants (Fast Food)",
        "supermarkets", "Supermarkets")

final_table <- label_table %>%
  left_join(category_counts, by = c("category"))

plot_icon_summary <- function(category) {
  plot_info <- final_table %>%
    filter(category == {{category}})
  print(category)
  # convert svg to correct formats
  rsvg_svg(glue("assets/{plot_info$store_type}.svg"), glue("assets/{plot_info$store_type}-cairo.svg"))

  img <- readPicture(glue("assets/{plot_info$store_type}-cairo.svg"))


  #Now create some data
  d <- data.frame(x=0.2,y=0.5)

  #Need to specify xlims and ylims of plot - this lets us calculate the
  #normalised plot coordinates
  xlims <- c(0,1)
  ylims <- c(0,1)

  #Create the plot points using symbolsGrob
  sym.grob <- symbolsGrob(img,
                          x=rescale(d$x,from=xlims),
                          y=rescale(d$y,from=ylims),
                          default.units="npc",
                          size=1)
  #Plot
  icon_summary <- ggplot(d,aes(x,y))+
    theme_void() +
    annotate("text", label = str_wrap(plot_info$category, width = 15), x = 0.45, y = 0.63,
             family = "Oswald", hjust = 0, vjust = 0,
             size = 6, color = "#565656",
             lineheight = 0.7) +
    annotate("text", label = plot_info$n, x = 0.45, y = 0.57,
             family = "Oswald", fontface = "bold", hjust = 0, vjust = 1,
             size = 12) +
    annotation_custom(sym.grob)+
    coord_cartesian(xlim=xlims,ylim=ylims,expand=FALSE) #Don't forget this!

  icon_summary

  ggsave(glue("assets/{plot_info$store_type}-icon-summary.svg"),
         width = 4, height = 1.5, units = "in")
}

walk(final_table$category, plot_icon_summary)

