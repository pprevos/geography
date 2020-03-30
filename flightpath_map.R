## Flightpath map
## https://lucidmanager.org/create-air-travel-route-map

## Init
library(tidyverse)
library(ggmap)
api <- readLines("google.api") # Text file with the API key
register_google(key = api)
library(ggrepel)

## Read flight and airports lists 
#flights <- read_csv("flights.csv")
flights <- read_csv("flights.csv")
airports_file <- "airports.csv"

if (file.exists(airports_file)) {
  airports <- read_csv(airports_file)
  } else {
  airports <- tibble(airport = NA, lon = NA, lat= NA)
}

## Lookup coordinates
## Some airports need country names to ensure Google finds the correct location
## The geocoding keeps looping till all coordinates have been found
destinations <- unique(c(flights$From, flights$To))
new_destinations <- destinations[!destinations %in% airports$airport]

while (length(new_destinations) > 0) {
    new_airports <- geocode(new_destinations) %>%
      mutate(airport = new_destinations) %>%
      select(airport, lon, lat)
    airports <- rbind(airports, new_airports) %>%
      filter(!is.na(lon) | !is.na(lat))
    new_destinations <- destinations[!destinations %in% airports$airport]
}
write_csv(airports, "airports.csv")

## Remove country names
airports$airport <- as.character(airports$airport)
comma <- regexpr(",", airports$airport)
airports$airport[which(comma > 0)] <- substr(airports$airport[which(comma > 0)], 1, comma[comma > 0] - 1)

## Remove return flights
d <- vector()
for (i in 1:nrow(flights)) {
    d <- which(paste(flights$From, flights$To) %in%
               paste(flights$To[i], flights$From[i]))
    flights$From[d] <- "R"
}
flights2 <- flights %>%
  filter(From != "R") %>%
  select(From, To)

## Add coordinates to flight list
flights <- merge(flights, airports, by.x = "From", by.y = "airport")
flights <- merge(flights, airports, by.x = "To", by.y = "airport")
flights <- flights %>% 
  select(From, To, lon.x, lat.x, lon.y, lat.y) %>% 
  as_data_frame()

## Split Circumnaviation Flights at -180/180 degrees
circ <- which(abs(flights$lon.y - flights$lon.x) > 180)
flights[circ,]
flights$lon.y[circ] <- ifelse(flights$lon.y[circ] < 0, 180, -180)
flights$lat.y[circ] <- rowSums(flights[circ, c("lat.x", "lat.y")]) / 2
leg2 <- airports %>%
  filter(airport %in% flights$To[circ]) %>%
  mutate(From = rep("", length(circ))) %>%
  mutate(lon.x = -flights$lon.y[circ], lat.x = flights$lat.y[circ]) %>%
  select(From, To = airport, lon.x, lat.x, lon.y = lon, lat.y = lat)
flights <- rbind(flights, leg2)

## Plot flight routes
worldmap <- borders("world2", colour="#efede1", fill="#efede1") 
ggplot() + worldmap + 
    geom_point(data = airports, aes(x = lon, y = lat), col = "#970027") + 
    geom_text_repel(data=airports, aes(x = lon, y = lat, label = airport), col = "black", size = 2, segment.color = NA) + 
    geom_curve(data = flights, aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y), col = "#b29e7d", size = .4) + 
    theme_void()
ggsave("flights_map.png", dpi = 300)


