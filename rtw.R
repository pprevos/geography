library(tidyverse)
world <- map_data("world")
worldmap <- ggplot(world) +
    geom_path(aes(x = long, y = lat, group = group)) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank()) +
    labs(x = "", y = "")

## Define itinerary
library(ggmap)
airports <- tibble(city = c("Melbourne", "Townsville", "Brisbane", "Tokyo", "Amsterdam", "San Francisco", "Melbourne"))

## Find coordinates
## Where you receive an OVER_QUERY_LIMIT error, repeat the code
itinerary <- geocode(airports$city) %>%
    mutate(location = airports$city)

## Split travel past dateline
dl <- which(diff(itinerary$lon) > 180)
dr <- ifelse(itinerary$lon[dl] < 0, -180, 180)
dateline <- tibble(lon = c(dr, -dr),
                   lat = rep(mean(itinerary$lat[dl:(dl + 1)]), 2),
                   location = "dateline")
itinerary <- rbind(itinerary[1:dl, ], dateline,
                   itinerary[(dl + 1):nrow(itinerary), ])
itinerary$c <- 2
itinerary$c[itinerary$location == "dateline"] <- NA

## Visualise
worldmap +
    geom_path(data = itinerary, aes(lon, lat), colour = "red", size = 1) +
    coord_map("azequidistant", orientation = c(-90, 0, 270))
ggsave("rtw_trip.png", dpi = 300)
