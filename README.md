# API Community Food Asset Mapping Project

This repository holds the code for a project developing a map of food assets for the San Francisco Bay Area Asian Pacific Islander (API) community. 

# Process

I've created three milestones:

1. Import Data
1. Build Map
1. Deploy Map

## Import Data

For this part of the work, I imagine us using an R script file that brings in all data, cleans it, geocodes it (where appropriate), and then saves it to an RDS file. I tend to use the structure of packages in projects as well, with data-raw and data folders. I'm not sure how much we'll need to use the data-raw for this project but it's there.

## Build Map

In this phase, we'll build out the map. This will involve design decisions (colors, fonts, etc) and the actual plotting of the geospatial data from the Import Data phase on the map. 

## Deploy Map

In the last phase we'll determine how exactly we'll deploy the map. I imagine it will be a deploy to Netlify situation but I'll confer with the client to confirm this. 
