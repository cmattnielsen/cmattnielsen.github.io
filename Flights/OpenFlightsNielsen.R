library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(data.table)
library(ggplot2)

load("data/airports.Rdata")
load("data/airlines.Rdata")
load("data/routes.Rdata")
load("data/top50AirlinesRoutes.Rdata")
load("data/countries.Rdata")
load("data/countries.bounds.Rdata")


summary(airlines)
summary(airports)
summary(routes)
summary()




airlines_airports <- full_join(airlines,airports,by="Country")
countries_airlines_airports <- full_join(airlines_airports,countries,by="Country")



# ICAO is 4 letter codes used by the UN for international flights and govern the standards of air travel
# IATA codes are 3 letter used by non governmental trade identify airports, airlines and flight paths

# Routes has both, which one to link to?


view(airlines_airports)






ggplot(airlines, aes(x=Name, y=Country)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  labs(title="Name of Airline and Country", y="Country", x="Name of Airline")



ggplot(airlines, aes(x=Active, y=Country)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  labs(title="Country and Active Airlines", y="Country", x="ACtive, Yes or NO")


ggplot(airlines, aes(x=Name, y=Country)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  labs(title="Name of Airline and Country", y="Country", x="Name of Airline")




airports.source.summary <- routes %>% 
  group_by(Source_airport) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  left_join(airports, by = c("Source_airport" = "IATA")) %>%
  filter(!is.na(Airport_ID))

airports.info.destination <- routes %>% 
  left_join(airports, by = c("Destination_airport" = "IATA"))

















