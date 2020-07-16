## Flat Earth mathematics
## https://lucidmanager.org/flat-earth-mathematics/

## Recreate Gleason's Map
library(tidyverse)
library(maps)
library(mapproj)

world <- map_data("world")
ggplot(world, aes(x = long, y = lat, group = group, fill = region)) +
    geom_polygon(size = .2, col = "grey10") +
    scale_fill_discrete(guide = FALSE) + 
    coord_map("azequidistant", orientation = c(-90, 0, 0)) +
    scale_x_continuous(name = "", labels = NULL,
                       breaks = seq(-180, 180, 15)) +
    scale_y_continuous(name = "", labels = NULL,
                       breaks = seq(-180, 180, 15)) +
    theme(panel.background = element_rect(fill = "lightblue")) +
    labs(title = "Gleason Map",
         subtitle = "Downunder version")

ggsave("flatearth_au.png")

ggplot(world, aes(x = long, y = lat, group = group)) +
    geom_path(size = .5, col = "black") +
    coord_map("azequidistant", orientation = c(90, 0, 270)) +
    scale_x_continuous(name = "", labels = NULL,
                       breaks = seq(-180, 180, 30)) +
    scale_y_continuous(name = "", labels = NULL,
                       breaks = seq(-180, 180, 30)) +
    theme(panel.grid.major = element_line(size = 0.5,
                                          colour = "black"),
          panel.background = element_rect(fill = "#00a8c6"))

ggsave("flatearth.png")
