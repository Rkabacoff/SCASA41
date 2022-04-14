##--------------------------------------------##
## Case Study - Vaccination rates in CA
## (c) 2022, Robert I. Kabacoff, Ph.D.
##--------------------------------------------##

library(choroplethr)
library(choroplethrMaps)
library(dplyr)

# import data
data(county.map)
rates <- read.csv("vaccinationRates.csv")

# replace "NAME" with "region" in rates data frame
df<- county.map %>%
  filter(STATE == "06") %>%
  select(NAME, region) %>%
  distinct(NAME, region) %>% 
  inner_join(mydata) %>%
  select(region, value)

# create graph 
county_choropleth(df, 
                  title         = "Covid Vaccination Rates", 
                  legend        = "Rates",
                  county_zoom = df$region)

# add background
# need to get a google API
# https://cloud.google.com/maps-platform/
library(ggmap)
register_google(key="yourkeyhere")
county_choropleth(df, 
                  title         = "Covid Vaccination Rates", 
                  legend        = "Rates",
                  county_zoom = df$region,
                  reference_map=TRUE)

