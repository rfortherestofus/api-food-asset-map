library(rstudioapi)
library(rmarkdown)
library(beepr)


# Generate Assets ---------------------------------------------------------

# source("R/icon-summaries.R")
# source("R/static-maps.R")

# Generate TailwindCSS ----------------------------------------------------

# terminalExecute("NODE_ENV=production npx tailwindcss-cli build css/tailwind.css -o css/apistyle.css")

# Render Site -------------------------------------------------------------

sf::sf_use_s2(FALSE)

render_site()

# Beep --------------------------------------------------------------------

beep("coin")
