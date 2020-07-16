library(ggmap)
library(ggplot2)
library(ggrepel)
library(geosphere)
library(tidyverse)
library(stringr)

## Google maps api
api <- readLines("google.api") # Text file with the API key
register_google(key = api)

## Read flight list and airport list
flights <- read_csv("pacific/pacific-flights.csv")
f <- "pacific/pacific-airports.csv"
if (file.exists(f)) {
  airports <- read.csv(f)
} else
  airports <- data.frame(airport = NA, lat = NA, lon = NA)

## Lookup coordinates for new airports
all_airports <- unique(c(paste(flights$From, flights$From.Country, sep = ", "),
                         paste(flights$To, flights$To.Country, sep = ", ")))
new_airports <- all_airports[!(all_airports %in% airports$airport)]

while (length(new_airports) != 0) {
  coords <- geocode(new_airports)
  temp_airports <- data.frame(airport = new_airports, coords)
  airports <- rbind(airports, temp_airports) %>%
    filter(!is.na(lat), !is.na(lon))
  new_airports <- all_airports[!(all_airports %in% airports$airport)]
}
## Remove countries and save
airports$airport <- str_remove_all(airports$airport, ",.*")
write_csv(airports, f)

## Add coordinates to flight list
flights <- merge(flights, airports, by.x = "From", by.y = "airport")
flights <- merge(flights, airports, by.x = "To", by.y = "airport")

## Pacific centric
flights$lon.x[flights$lon.x < 0] <- flights$lon.x[flights$lon.x < 0] + 360
flights$lon.y[flights$lon.y < 0] <- flights$lon.y[flights$lon.y < 0] + 360
airports$lon[airports$lon < 0] <- airports$lon[airports$lon < 0] + 360

## Plot flight routes
worldmap <- borders("world2", fill = "grey", col = NA)
ggplot() + worldmap +
  geom_point(data = airports, aes(x = lon, y = lat), col = "#970027") +
  geom_text_repel(data = airports, aes(x = lon, y = lat, label = airport), col = "black", size = 2,
                  segment.color = NA) +
  geom_curve(data = flights, aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y, col = Airline), size = 1,
             curvature = .2) +
  xlim(90, 300) + ylim(-50, 50) +
  theme_void() +
  labs(title = "Pacific Island Hopping", subtitle = "lucidmanager.org")
ggsave("pacific-flights.png", width = 6, height = 4)

library(igraph)
g <- graph_from_edgelist(as.matrix(flights[,1:2]), directed = FALSE)

png("pacific-network.png", width = 600, height = 600)
par(mar = rep(0, 4))
plot(g, layout = layout.fruchterman.reingold,
     vertex.size = 0,
     vertex.label.cex = 1.2,
     edge.width = 2)
dev.off()

shortest_paths(g, "Auckland", "Saipan")
