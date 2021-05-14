# API Community Food Asset Mapping Project

This repository holds the code for a project developing a map of food assets for the Area Asian Pacific Islander (API) community in the City of San Francisco.

# Process

I've created three milestones:

1.  Import Data
2.  Build Map
3.  Deploy Map

## Import Data

For this part of the work, I imagine us using an R script file that brings in all data, cleans it, geocodes it (where appropriate), and then saves it to an RDS file. I tend to use the structure of packages in projects as well, with data-raw and data folders. I'm not sure how much we'll need to use the data-raw for this project but it's there.

Each object should be saved as an RDS file with the following variables:

-   name
-   category
-   street_address
-   city
-   state
-   zip_code

The object should be geocoded with a geometry column to hold that data.

### Food Resources

In consultation with the client, here are the categories of food resources we're working with:

- Corner Stores
- Drug Stores
- Ethnic Markets
- Farmers Markets
- Food Banks/Pantries
- Food Pharmacies
- Free Prepared Food or Hot Meals
- Liquor Stores
- Restaurants
- Restaurants (Fast Food)
- Supermarkets

There is one attribute that can apply across categories:

- Stores that Accept SNAP/WIC


### Neighborhoods

We've been asked to focus on these neighborhoods in particular as they are where the API community is concentrated:

-   SOMA
-   Richmond
-   Sunset
-   Chinatown
-   Japantown
-   Bayview Hunters Point
-   Tenderloin
-   Visitacion Valley
-   Excelsior

### Update on Data Import Process

Below is a summary of the data that we have collected, with a brief synopsis of how it was collected. The categories come from the list above, with slight tweaks for readability.

**Free, Prepared Food or Hot Meals**

Data was copied manually from [this PDF from freeprintshop.org](http://www.freeprintshop.org/download/eats_english.pdf) and scraped from [Bay Area 211](https://www.211bayarea.org/sanfrancisco/food/food-programs/brown-bag-programs/).

**Corner Stores**

Data came from the OpenStreetMap API.

**Drug Stores**

Data came from the OpenStreetMap API.

**Restaurants**

Data came from the OpenStreetMap API.

**Supermarkets**

Data came from the OpenStreetMap API.

**Liquor Stores**

Data came from the OpenStreetMap API.

**Farmers Markets**

Data came from the OpenStreetMap API. Additional markets were collected from the [San Francisco Department of the Environment](https://sfenvironment.org/farmers-markets-in-sf).

**Fast Food Restaurants**

Data came from the OpenStreetMap API.

**Food Banks**

Data was scraped from the [San Francisco Marin Food Bank website](https://www.sfmfoodbank.org/find-food/).

**Food Pantries**

Data was scraped from [foodpantries.org](https://www.sfmfoodbank.org/find-food/).

**Food Pharmacies**

Data was provided by Kathleen and entered manually.

**Stores that Accept SNAP/WIC**

Data on stores that accept SNAP comes from the [USDA website](https://www.fns.usda.gov/snap/retailer-locator).

Data on stores that accept WIC comes from the [WIC Store Locator website](https://www.wicstorelocator.com/ci/ca-san_francisco).

**Ethnic Markets**

Data was manually collected from [SF Gate](https://www.sfgate.com/food/article/The-Bay-Area-s-best-international-markets-11157138.php#taboola-9) and a [Live Journal guide from 2014](https://ss-biggie.livejournal.com/42231.html).

## Build Map

In this phase, we'll build out the map. This will involve design decisions (colors, fonts, etc) and the actual plotting of the geospatial data from the Import Data phase on the map.

## Deploy Map

In the last phase we'll determine how exactly we'll deploy the map. I imagine it will be a deploy to Netlify situation but I'll confer with the client to confirm this.
