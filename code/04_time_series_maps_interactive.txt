##--------------------------------------------##
## Time series, maps, and interactive plots
## (c) 2022, Robert I. Kabacoff, Ph.D.
##--------------------------------------------##

library(ggplot2)

## Time series -------------------------------------
ggplot(economics, aes(x = date, y = psavert)) +
  geom_line() 

# customized appearance
library(scales)
ggplot(economics, aes(x = date, y = psavert)) +
  geom_line(color = "indianred3", 
            size=1 ) +
  geom_smooth() +
  scale_x_date(date_breaks = '5 years', 
               labels = date_format("%b-%y")) +
  labs(title = "Personal Savings Rate",
       subtitle = "1967 to 2015",
       x = "",
       y = "Personal Savings Rate") +
  theme_minimal()


## Area chart ------------------------------
ggplot(economics, aes(x = date, y = psavert)) +
  geom_area(fill="lightblue", color="black") +
  labs(title = "Personal Savings Rate",
       x = "Date",
       y = "Personal Savings Rate")

## Filled area chart -----------------------
data(uspopage, package = "gcookbook")
ggplot(uspopage, aes(x = Year,
                     y = Thousands/1000, 
                     fill = AgeGroup)) +
  geom_area(color="black") +
  labs(title = "US Population by age",
       x = "Year",
       y = "Population in Thousands") +
  theme_minimal()

## Interactive graphs
library(plotly)

# example 1 - time series
p <- ggplot(economics, aes(x = date, y = psavert)) +
  geom_line() 
ggplotly(p)


# example 2 - cars
data(mtcars)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$name <- row.names(mtcars)

p <- ggplot(mtcars, aes(x = disp, y= mpg, color = cyl)) +
  geom_point()
ggplotly(p)

# example 3 - customized tooltip
p <- ggplot(mtcars,
            aes(x = disp, y=mpg, color=cyl,
                text = paste(name, "\n",
                             "disp:", disp))) +
  geom_point()
ggplotly(p, tooltip=c("text"))

## Maps --------------------------------------

# leaflet
library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map

m <- leaflet()
m <- addTiles(m)
m <- addAwesomeMarkers(m, lng=174.768, lat=-36.852, popup="hello")
m

awesome <- makeAwesomeIcon(
  icon = "info",
  iconColor = "black",
  markerColor = "blue",
  library = "fa"
)


leaflet(data = quakes[1:20,]) %>%
  addTiles() %>%
  addAwesomeMarkers(~long,
                    ~lat,
                    icon = awesome,
                    popup = ~as.character(mag),
                    label = ~as.character(mag))
# Dot density

library(leaflet)

# map providers
# http://leaflet-extras.github.io/leaflet-providers/preview/index.html. 


# Huston Data 2010
data(crime, package="ggmap")

# subset the data
library(dplyr)
df <- filter(crime, offense == "murder") %>%
  select(date, offense, address, lon, lat)

# get median lat and lon
summary(df)

# place blue dots
leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -95.41, lat = 29.72, zoom = 10) %>%
  addCircles(lng=df$lon, lat=df$lat, popup=df$address)

# place markers
leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -95.40, lat = 29.73, zoom = 10) %>%
  addMarkers(lng=df$lon, lat=df$lat, popup=df$address)

# markers with interactive clustering
leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -95.40, lat = 29.73, zoom = 10) %>%
  addMarkers(lng=df$lon, lat=df$lat, popup=df$address,
             clusterOptions = markerClusterOptions())


## Choropleth maps ---------------------------
library(choroplethr)
library(choroplethrMaps)

# country maps
data(df_pop_country)
country_choropleth(df_pop_country, "2012 World Bank Populate Estimates")

country_choropleth(df_pop_country, 
                   "2012 World Bank Population Estimates", 
                   num_colors=3,
                   zoom=c("united states of america", "canada", "mexico"))

# change colors
country_choropleth(df_pop_country, 
                   "2012 World Bank Population Estimates", 
                   num_colors=3,
                   zoom=c("united states of america", "canada", "mexico")) +
  scale_fill_brewer(palette="Set1")

# state maps
data(df_pop_state)
state_choropleth(df_pop_state, 
                 title  = "US 2012 State Population Estimates", 
                 legend = "Population")

# county maps
data(df_pop_county)
county_choropleth(df_pop_county, 
                  title         = "California County Population Estimates", 
                  legend        = "Population",
                  state_zoom    = "california")

