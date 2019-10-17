## Mapping antipodes using the ggmap package
## https://lucidmanager.org/mapping-antipodes/

library(tidyverse)
## Antipodean globe
world <- map_data("world")
anti_world <- world %>%
    mutate(long = long - 180,
           lat = - lat)           
ggplot() +
    geom_polygon(data = world,  aes(long, lat, group = group), fill = "grey") +
    geom_polygon(data = anti_world, aes(long, lat, group = group),
                 fill = "blue", alpha = 0.2) + 
    coord_map("ortho", orientation = c(0, 29, 31)) +
    theme_void()

ggsave("antipodes.png", dpi = 150)

library(gridExtra)
library(ggmap)
api <- readLines("google.api") # Text file with the API key
register_google(key = api)
## Antipode function
antipode <- function(location, zm = 6) {
    # Map location
    lonlat <- geocode(location)
    loc1 <- get_map(lonlat, zoom = zm)
    map1 <- ggmap(loc1) +
        geom_point(data = lonlat, aes(lon, lat, col = "red", size = 10)) + 
        theme(legend.position = "none")
    # Define antipode
    lonlat$lon <- lonlat$lon-180
    if (lonlat$lon < -180) 
        lonlat$lon <- 360 + lonlat$lon
    lonlat$lat <- -lonlat$lat
    loc2 <- get_map(lonlat, zoom = zm)
    map2 <- ggmap(loc2) +
        geom_point(data = lonlat, aes(lon, lat, col = "red", size = 10)) + 
        theme(legend.position = "none")
    grid.arrange(map1, map2, nrow = 1)
}

antipode("20 Alpina Place, Kangaroo Flat", 6)
antipode("Rector Nelissenstraat 47 Hoensbroek", 4)
ggsave("AntipodeHoensbroek.png", dpi = 150)
antipode ("Chicago,IL", 4)


