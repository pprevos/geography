## Flat Earth Mathematics
library(ggplot2)

world <- map_data("world")

worldmap <- ggplot(world) +
  geom_path(aes(x = long, y = lat, group = group), size = .2) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(x = "", y = "")

worldmap + 
  coord_map("azequidistant", orientation = c(90, 0, 270))

ggsave("azequidistant.png")

## Round the World Itinerary
library(ggmap)
library(dplyr)

api <- readLines("google.api") # Text file with the secret API key
register_google(key = api)
airports <- c("Melbourne", "Tokyo", "Amsterdam", "San Francisco")
itinerary <- geocode(airports)
itinerary <- rbind(itinerary, itinerary[1, ]) %>%
    mutate(location = c(airports, airports[1]))

## Split travel past dateline
dl <- which(diff(itinerary$lon) > 180)
dr <- ifelse(itinerary$lon[dl] < 0, -180, 180)
dateline <- tibble(lon = c(dr, -dr),
                   lat = rep(mean(itinerary$lat[dl:(dl + 1)]), 2),
                   location = "dateline")
itinerary <- rbind(itinerary[1:dl, ], dateline,
                   itinerary[(dl + 1):nrow(itinerary), ])
itinerary

## Visualise
worldmap +
    geom_point(data = itinerary, aes(lon, lat), colour = "red", size = 4) +
    geom_path(data = itinerary, aes(lon, lat), colour = "red", size = 1) +
    coord_map("azequidistant", orientation = c(90, 0, 270))

## Great Circle Distance
library(geosphere)
sapply(1:(nrow(itinerary) - 1), function(l)
    distVincentyEllipsoid(itinerary[l, 1:2], itinerary[(l + 1), 1:2]) / 1000) %>%
    sum()

library(mapproj)
flatearth.coords <- mapproject(world$long, world$lat,
                        "azequidistant", orientation = c(90, 0, 270))
r <- 6378.137
flatearth.coords <- mutate(world,
                x = flatearth.coords$x * r,
                y = flatearth.coords$y * r) %>%
    select(x, y, group, order, region, subregion)

flatearth <- ggplot(flatearth.coords) +
    geom_path(aes(x, y, group = group), size = .2) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank()) +
    labs(x = "", y = "")

## Australia - South America
airports <- tibble(city = c("Sydney", "Santiago de Chili"))
itinerary <- geocode(airports$city) %>%
    mutate(location = airports$city)
itinerary
coords <- mapproject(itinerary$lon, itinerary$lat, "azequidistant",
                     orientation = c(90, 0, 270))
coords <- tibble(x = coords$x * r, y = coords$y * r)
sum(sqrt(diff(coords$x)^2 + diff(coords$y)^2))

flatearth + 
    geom_point(data = coords, aes(x, y), colour = "red", size = 4) +
    geom_path(data = coords, aes(x, y), colour = "red", size = 1)
