# Load packages
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))

# Load data sets. Note this assumes this .Rmd files is in the same directory as
# the .csv files.
flights <- read.csv("data/flights.csv", stringsAsFactors = FALSE) %>% 
  tbl_df() %>%
  mutate(date=as.Date(date))
weather <- read.csv("data/weather.csv", stringsAsFactors = FALSE) %>% 
  tbl_df() %>%
  mutate(date=as.Date(date))
planes <- read.csv("data/planes.csv", stringsAsFactors = FALSE) %>% 
  tbl_df()
airports <- read.csv("data/airports.csv", stringsAsFactors = FALSE) %>% 
  tbl_df()
states <- read.csv("data/states.csv", stringsAsFactors = FALSE) %>% 
  tbl_df()
``` 
head(airports)
head(flights)
head(planes)
head(states)
head(weather)
  
## Question 1:
  
# Plot a "time series" of the proportion of flights that were delayed by > 30 minutes on each day.  i.e.
# the x-axis should be some notion of time
# the y-axis should be the proportion.
# Which seasons did we tend to see the most and least delays of > 30 minutes.

# First we get the # of flights per day. 
flights_perDay <- flights %>% 
  group_by(date) %>% 
  tally() %>% 
  rename(flights_per_day = n)
flights_perDay

# Next we get the # of flights per day that were delayed  by >30 minutes.
flightsDelay_perDay <- flights %>%
  filter(dep_delay > 30) %>%
  group_by(date) %>% 
  tally() %>% 
  rename(delayed_per_day = n)
flightsDelay_perDay

# Next we join these two in order to get the proportion per day.
flightsDelay_perDay <- left_join(flightsDelay_perDay, flights_perDay, by = "date") %>% 
  mutate(prop_delay = round(delayed_per_day/flights_per_day, 3))
flightsDelay_perDay

g <- ggplot(data = flightsDelay_perDay, aes(x = date, y = prop_delay))
g + geom_point() + 
  labs(title = "Proportion of Flights Delayed More Than 30 Minutes", 
                        x     = "Date", 
                        y     = "Proportion Delayed")

# It appears that, outliers aside, the season with the most flights delayed > 30 minutes was summer (July/August), 
# and the season with the least flights delayed > 30 minutes was late fall (October/November).

## Question 2:

#Some people prefer flying on older planes.  Even though they aren't as nice,
#they tend to have more room.  Which airlines should these people favor?

# First, we find the the planes flown by each carrier.
fleet <- flights %>%
  select(carrier, plane) %>%
  group_by(carrier, plane) %>%
  summarise()

# We join the table of planes flown by each carrier to the year of each plane from the planes table.
fleet <- planes %>% 
  select(plane, year) %>%
  left_join(fleet, ., by = "plane")

# We plot the distribution of each carrier's fleet by plane manufacture year. Note that we remove planes 
# where year data is missing. We separate the histograms into separate facets to make differences clearer.

suppressPackageStartupMessages(library(RColorBrewer))

colourCount = length(unique(fleet$carrier))

g <- ggplot(data = filter(fleet, !is.na(year)), aes(x = year, fill = carrier))
g + geom_histogram(binwidth = 1) + 
  facet_wrap(~carrier) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Set3"))(colourCount)) + 
  theme_dark() + 
  labs(title = "Distribution of Airline Fleet Ages", 
       x     = "Year of Plane Manufacture", 
       fill  = "Airline")    
  
# From looking at our histogram, it appears American Airlines (AA) has very few planes built after the mid-1990's,
# and would be a great choice for our hypothetical flyers. Delta (DL) may also be acceptable, as they have a good 
# number of older planes, but they also have many planes built after 2000 as well. Finally, Envoy Air (MQ) would also
# be a good choice, but they don't have a large fleet (or number of flights).

## Question 3:

# What states did listed Southwest Airlines flights tend to fly to?
# What states did all Southwest Airlines flights tend to fly to?

# First, filter for only Southwest Airlines flights (WN).
swFlights <- filter(flights, carrier == "WN")

# Next filter for only listed flights. 
uniqueSW <- swFlights %>% 
  group_by(flight,dest) %>%
  summarise()

# Next join this to the airports table, with only the iata and state columns.
# Tally the flights to each state, and then arrange the table in order. 
destinations <- airports %>% 
  select(iata,state) %>%
  left_join(uniqueSW, ., by = c("dest" = "iata")) %>%
  group_by(state) %>%
  tally () %>%
  arrange(n)

destinations$state <- factor(destinations$state, levels = destinations$state)

g <- ggplot(filter(destinations, !is.na(state)), aes(x = state, y = n))
g + geom_bar(stat="identity") + coord_flip() + labs(title = "States to which Southwest Tends to Fly")

## Question 4:

# What weather patterns are associated with the biggest departure delays?

# Create a table of weather patterns and date/hours. 
weather_patterns <- weather %>%
  select(date, hour, events) 

# Join this table with the flights table (the date, hour, and dep_delay columns).
delay_weather <- flights %>%
  select(date, hour, dep_delay) %>%
  left_join(., weather_patterns, by=c("date", "hour"))

delay_weather %>%
  filter(!is.na(events)) %>% 
  group_by(events) %>%
  summarise(median_departure_delay = median(dep_delay)) %>%
  arrange(desc(median_departure_delay))

## Question 5:

# I want to know what proportionately regions (NE, south, west, midwest) each 
# carrier flies to from Houston in the month of July.  Consider the `month()`
# function from the `lubridate` package.

# First isolate only the flights in July, and join this to the table of airports (to get states)
# and states (to get regions) and select only the columns we need.
july_flights <- flights %>%
  mutate(month = month(date)) %>%
  filter(month == 7) %>%
  left_join(., airports, by = c("dest" = "iata")) %>%
  left_join(., states, by = "state") %>%
  select(month, carrier, state, region)
july_flights

# Compute a table of total flights per carrier for July.
totalFlights_carrier <- july_flights %>%
  group_by(carrier) %>%
  tally() %>%
  rename(total_flights = n)

# Compute a table of flights per region per carrier for July.
flightsPerRegion_carrier <- july_flights %>%
  group_by(carrier, region) %>%
  tally() %>%
  rename(flights_perRegion = n)

# Use those table to compute the proportion of flights per region per carrier for July.
prop_flightsPerRegion <- flightsPerRegion_carrier %>%
  left_join(., totalFlights_carrier, by = "carrier") %>%
  mutate(proportionFlights = flights_perRegion/total_flights) %>% 
  select(carrier, region, proportionFlights) %>%
  arrange(carrier, desc(proportionFlights))

as.data.frame(prop_flightsPerRegion)
