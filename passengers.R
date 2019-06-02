## Libraries
library(ggmap)
api <- readLines("google.api") # Text file with the API key
register_google(key = api)
library(rvest)
library(tidyverse)
library(ggrepel)

## Passegener volumes
url <- "https://en.wikipedia.org/wiki/List_of_the_busiest_air_routes_in_Australia_by_passenger_traffic"
xpath <- '//*[@id="mw-content-text"]/div/table[1]'
sydneytraffic <- url %>%
    read_html() %>%
    html_nodes(xpath = xpath) %>%
    html_table()
sydneytraffic <- sydneytraffic[[1]]
sydneytraffic <- sydneytraffic[, c(2, 3, 14)]
names(sydneytraffic) <- c("From", "To", "Passengers")
sydneytraffic$Passengers <- as.numeric(gsub(",", "", as.character(sydneytraffic$Passengers)))
sydneytraffic <- filter(sydneytraffic, To == "Sydney")
sydneytraffic$linew <- sydneytraffic$Passengers / min(sydneytraffic$Passengers)

## Lookup coordinates
airports <- unique(c("Sydney", as.character(sydneytraffic$From)))
coords <- geocode(paste(airports, "Australia"))
airports <- data.frame(airport = airports, coords)

## Add coordinates to flight list
traffic <- merge(sydneytraffic, airports, by.x = "To", by.y = "airport")
traffic <- merge(traffic, airports, by.x = "From", by.y = "airport")

# Plot flight routes
worldmap <- borders("world", colour="#efede1", fill="#efede1") # create a layer of borders
ggplot() + worldmap + 
    geom_curve(data = traffic, aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y), col = "#b29e7d", 
               size = sydneytraffic$linew/2, curvature = .2) + 
    geom_point(data=airports, aes(x = lon, y = lat), col = "#970027") + 
    geom_text_repel(data=airports, aes(x = lon, y = lat, label = airport), col = "black", size = 2, segment.color = NA) + 
    xlim(110, 155) + ylim(-45, -10) + coord_fixed() + 
    theme_void()
ggsave("Sydney_Passengers.png")
