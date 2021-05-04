library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(data.table)
library(ggmap)
library(plyr)
install.packages(ggmap)
library(ddply)
install.packages("geosphere")
library(plyr)
library(maps)
library(geosphere)

load("data/airports.Rdata")
load("./data/airlines.Rdata")
load("data/routes.Rdata")
load("data/top50AirlinesRoutes.Rdata")
load("data/countries.Rdata")
load("data/countries.bounds.Rdata")

# API key
register_google(key = "[AIzaSyAi3Ut72aLygJx5VKCldrZ8a3OpZ_3YMo8]", write = TRUE)




airlines_airports <- full_join(airlines,airports,by="Country")
countries_airlines_airports <- full_join(airlines_airports,countries,by="Country")



airports <- airports %>% 
  mutate(IATA_SOURCE = IATA,
         IATA_DEST = IATA)



routes <- routes %>% 
  rename(IATA_SOURCE = Source_airport,
         IATA_DEST = Destination_airport) 




source <- full_join(routes,airports,by=c("IATA_SOURCE")) %>% 
  mutate(Type = "SOURCE")
dest <- full_join(routes,airports,by=c("IATA_DEST")) %>% 
  mutate(Type = "DEST")



full <- full_join(source,dest)


ggplot(full,aes(x=IATA_SOURCE,fill=Type)) +
  geom_bar() + lims(y=c(0,5000))


full$Country %>% table() %>% as.data.frame() %>% arrange(desc(Freq))





airports %>% glimpse
routes %>% glimpse


dest_by_country <- full %>% 
  group_by(Country) %>% 
  summarize(N = n(),
            Destinations = unique(IATA_DEST.y))


dest_by_country %>% 
  filter(Country == "Peru")


ggplot(aes(x=Destinations)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle=60,hjust=1))


airports$IATA[airports$IATA == "\\N"] <- NA

x <- dest_by_country %>% 
  filter(Country == "Peru") %>%
  select(Destinations)

full[full$IATA %>% na.omit(x$Destinations), "Country"]





# break 







departures <- ddply(routes, .(Source_airport_ID), "nrow")
names(departures)[2] <- "flights"
arrivals <- ddply(routes, .(Destination_airport_ID), "nrow")
names(arrivals)[2] <- "flights"


head(departures)

head(routes)

head(airports)



# cant figure this out

airportC <- merge(x = routes, y = departures, by.x = "ID", by.y = "Source_airport_ID", all = FALSE)


airportB <- merge(airports, departures, by.x = "ID", by.y = "Source_airport_ID")

airportA <- merge(airports, arrivals, by.x = "ID", by.y = "Destination_airport_ID")








# ggmap location

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% ggmap() 




airportD$type <- "departures"
airportA$type <- "arrivals"
airportAB <- rbind(airportB, airportA)


mapPointsDA <- ggmap(us) + geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airportAB, alpha = .5) 
mapPointsLegendDA <- mapPointsDA + scale_size_area(breaks = sqrt(c(1, 5, 10, 50, 100, 500)), labels = c(1, 5, 10, 50, 100, 500), name = "routes")
mapPointsFacetsDA <- mapPointsLegendDA + facet_grid(. ~ type)
mapPointsFacetsDA








c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
  "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", 
  "Nicaragua", "Honduras", "El Salvador", "Belize", "Guatemala", "Mexico", "Trinidad and Tobago",
  "Caribe", "Puerto Rico", "Dominican Republic", "Haiti", "Jamaica", "Cuba", "Bahamas", "Antiles",
  "Dominica", "Saba")

















#peru airports Chavez



peruairports <- filter(airports, Latitude < 45)
peruairports <- filter(airports, Longitude > -130)
peruairports <- filter(peruairports, IATA!="LIM")
LIM <- filter(airports, IATA=="LIM")

map("world", regions=c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
                       "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", 
                       "Nicaragua", "Honduras", "El Salvador", "Belize", "Guatemala", "Mexico", "Trinidad and Tobago",
                       "Caribe", "Puerto Rico", "Dominican Republic", "Haiti", "Jamaica", "Cuba", "Bahamas", "Antiles",
                       "Dominica", "Saba"), fill=T, col="grey8", bg="grey15")
points(peruairports$Longitude,peruairports$Latitude, pch=3, cex=0.1, col="chocolate1")



for (i in (1:dim(peruairports)[1])) { 
  inter <- gcIntermediate(c(LIM$Longitude[1], LIM$Latitude[1]), c(peruairports$Longitude[i], peruairports$Latitude[i]), n=1)
  lines(inter, lwd=0.1, col="turquoise2")    
}


# trying to find out what region south america is in
world <- map("world", plot = FALSE, namesonly = T)  # Assign the world's regions' names to a new object we called 'world'
canaryIslands <- world[grep("[Cc]anary.*", world)]  # Search for all the regions containing the 'canary' word
canaryIslands



# Chicago OHare 


usairports <- filter(airports, Latitude < 48.5)
usairports <- filter(usairports, Longitude > -130)
usairports <- filter(usairports, IATA!="ORD") #filter out jfk
ORD <- filter(airports, IATA=="ORD") #separate df for jfk





map("world", regions=c("usa"), fill=T, col="grey8", bg="grey15", ylim=c(21.0,50.0), xlim=c(-130.0,-65.0))
points(usairports$Longitude,usairports$Latitude, pch=3, cex=0.1, col="chocolate1")




for (i in (1:dim(usairports)[1])) { 
  inter <- gcIntermediate(c(ORD$Longitude[1], ORD$Latitude[1]), c(usairports$Longitude[i], usairports$Latitude[i]), n=100)
  lines(inter, lwd=0.1, col="turquoise2")    
}





















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






colnames(airports) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST")
colnames(routes) <- c("airline", "airlineID", "sourceAirport", "sourceAirportID", "destinationAirport", "destinationAirportID", "codeshare", "stops", "equipment")


map <- get_map(location = 'EUROPE', zoom = 3) 
# Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=Europe&zoom=4&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
# Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Europe&sensor=false










