# Packages loading ---- 
library(osmdata)
library(sf)
library(tidyverse)
library(tidygeocoder)

bbx <- getbb(paste0('Ljubljana, Slovenia')) #"Oujda", ",", "Morocco"

## Reference to map features
## https://wiki.openstreetmap.org/wiki/Map_features#Highway

# Get motorway, trunk, primary, secondary, tertiary ways ----
highways <- bbx %>%
  opq(timeout = 100) %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "trunk",
      "primary",
      "secondary",
      "tertiary",
      "motorway_link",
      "trunk_link",
      "primary_link",
      "secondary_link",
      "tertiary_link"
    )
  ) %>%
  osmdata_sf()



# Get small streets, pedestrian paths, living streets ----
streets <- bbx %>%
  opq(timeout = 50) %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "residential",
      "living_street",
      "service",
      "unclassified",
      "pedestrian",
      "footway",
      "track",
      "path"
    )
  ) %>%
  osmdata_sf()


# Get landuse ---- 
landuse <- opq(bbx, timeout = 50) |> 
  add_osm_feature(key = "landuse") |> 
  osmdata_sf() |> 
  unname_osmdata_sf()

# Get buildings ---- 
buildings <- opq(bbx, timeout = 50) |> 
  add_osm_feature(key = "building") %>%
  osmdata_sf() |> 
  unname_osmdata_sf()

buildings_unname <- opq(bbx, timeout = 50) |> 
  add_osm_feature(key = "building") %>%
  osmdata_sf() 


# Center and circle ----
city_coords <- tibble(address = "Oujda, Morocco") |> 
  tidygeocoder::geocode(address, method = 'osm', long = long, lat = lat)


long <- city_coords$long[1]
lat <- city_coords$lat[1]

crs2 <- 6384 # https://epsg.io/6384
center_proj <-
  tibble(long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)


# circle to crop in ----
dist <-  3500
circle <- tibble(long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  #st_transform(crs = crs2) %>%
  st_buffer(dist = dist) #%>%
  #st_transform(crs = 4326)

streets_lines <- st_intersection(circle, streets$osm_lines)
highways_lines <- st_intersection(circle, highways$osm_lines)
buildings_polygons <- st_intersection(circle, buildings$osm_polygons)
landuse_polygons <- st_intersection(circle, landuse$osm_polygons)



