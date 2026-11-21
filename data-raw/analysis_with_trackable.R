## paclages
library(tidyverse)
library(trackable)
library(adehabitatHR)
library(sf)
library(gganimate)
library(readr)

## readdata
elephant_fixes_2025 <- read_csv("data-raw/elephant_fixes_2025.csv")
head(elephant_fixes_2025)

## withtrackable
ele <- elephant_fixes_2025 |>
  select(elephant, datetime, lon, lat, month, period)
eletrack <- trackable( data=ele, time = datetime,
                       x = lon,
                       y = lat,
                       key = c("elephant"),
                       crs = 4326)
eletrack

p <- ggplot() +
  annotation_map_tile(type = "osm") +
  geom_sf(data = eletrack, aes(col=as.factor(month)), size = 2)

p
