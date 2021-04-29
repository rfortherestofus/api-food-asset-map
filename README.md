# API Community Food Asset Mapping Project

This repository holds the code for a project developing a map of food
assets for the Area Asian Pacific Islander (API) community in the City
of San Francisco.

# Process

I've created three milestones:

1.  Import Data
2.  Build Map
3.  Deploy Map

## Import Data

For this part of the work, I imagine us using an R script file that
brings in all data, cleans it, geocodes it (where appropriate), and then
saves it to an RDS file. I tend to use the structure of packages in
projects as well, with data-raw and data folders. I'm not sure how much
we'll need to use the data-raw for this project but it's there.

Each object should be saved as an RDS file with the following variables:

-   name
-   category
-   street_address
-   city
-   state
-   zip_code

The object should be geocoded with a geometry column to hold that data. 

### Food Resources

In consultation with the client, here are the categories of food
resources we're working with:

-   Providers that offer food pantries
-   Providers that offer free, prepared food/hot meals
-   Food banks
-   Food pharmacies
-   Drug stores
-   Ethnic food markets
-   Full service grocery stores/supermarkets/markets (non SNAP or WIC)
-   Stores that accept SNAP/WIC (full service grocery stores,
    supermarkets, markets, ethnic food markets, Target, Walmart)
-   Corner stores
-   Fast food restaurants
-   Restaurants
-   Farmers markets
-   Liquor Stores

### Neighborhoods

We've been asked to focus on these neighborhoods in particular as they
are where the API community is concentrated:

-   SOMA
-   Richmond
-   Sunset
-   Chinatown
-   Japantown
-   Bayview Hunters Point
-   Tenderloin
-   Visitacion Valley
-   Excelsior

## Build Map

In this phase, we'll build out the map. This will involve design
decisions (colors, fonts, etc) and the actual plotting of the geospatial
data from the Import Data phase on the map.

## Deploy Map

In the last phase we'll determine how exactly we'll deploy the map. I
imagine it will be a deploy to Netlify situation but I'll confer with
the client to confirm this.
