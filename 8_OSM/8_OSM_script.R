library(tidyverse)
library(osmdata)
library(sf)
#library(raster)
library(showtext)


font_add_google(name = 'Barlow', family = 'Barlow')
showtext_auto()


#choose area
bbx <- getbb("Birmingham, UK")

# large roads
roads <- bbx %>%
  opq(timeout = 50) %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "trunk", "primary",
                            "secondary", "tertiary", "motorway_link",
                            "trunk_link", "primary_link", "secondary_link",
                            "tertiary_link")) %>%
  osmdata_sf()

#saveRDS(roads, "roads_osm.rds")


# small roads
streets <- bbx %>%
  opq(timeout = 50) %>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street", "service",
                            "unclassified", "pedestrian", "footway",
                            "track", "path")) %>%
  osmdata_sf()

#saveRDS(streets, "streets_osm.rds")


# Other features
buildings <- opq(bbx) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()

#saveRDS(buildings, "buildings_osm.rds")



landuse <- opq(bbx) %>%
  add_osm_feature(key = 'landuse', value = c('grass','forest')) %>%
  osmdata_sf()

#saveRDS(landuse, "landuse_osm.rds")


leisure <- opq(bbx) %>%
  add_osm_feature(key = 'leisure', value = c('garden','park')) %>%
  osmdata_sf()

#saveRDS(leisure, "leisure_osm.rds")

natural <- opq(bbox = bbx) |>
  add_osm_feature(key = 'natural') |>
  osmdata_sf()

#saveRDS(natural, "natural_osm.rds")


river <- opq(bbox = bbx, timeout = 50) %>%
  add_osm_feature(key = 'water') %>%
  osmdata_sf()

#saveRDS(river, "river_osm.rds")



#roads <- readRDS(here::here("8_OSM/osm_features/roads_osm.rds"))
#landuse <- readRDS(here::here("8_OSM/osm_features/landuse_osm.rds"))
#river <- readRDS(here::here("8_OSM/osm_features/river_osm.rds"))
#leisure <- readRDS(here::here("8_OSM/osm_features/leisure_osm.rds"))



# draw a circle
centre = c(long = mean(bbx[1,]), lat = mean(bbx[2,]))
centre_proj <-
  tibble(lat = centre["lat"], long = centre["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

circle <- tibble(lat = centre["lat"], long = centre["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  #st_transform(crs = 4277) %>%
  st_buffer(dist = 3500, nQuadSegs = 200) %>%
  st_transform(crs = 4326)


# intersection
roads_lines <- st_intersection(circle, roads$osm_lines )  #%>%  st_transform(crs = 4326)
streets_lines <- st_intersection(circle, streets$osm_lines)
river_mpoly <- st_intersection(circle, river$osm_multipolygons)
buildings_poly <- st_intersection(circle, buildings$osm_polygons)
landuse_poly <- st_intersection(circle, landuse$osm_polygons) 
leisure_poly <- st_intersection(circle, leisure$osm_polygons)
natural_poly <- st_intersection(circle, natural$osm_polygons)




# plot
bham_cir_map <- ggplot() +
  geom_sf(data = roads_lines, color = 'grey60', fill = 'grey60', size = .4) +
  geom_sf(data = streets_lines, color = 'grey40', fill = 'grey40', size = .3) +
  geom_sf(data = river_mpoly, color = '#47a9e6', fill = '#47a9e6') +    ##0f3b5f
  geom_sf(data = landuse_poly, color = '#3c6f57', fill = '#3c6f57') +
  geom_sf(data = leisure_poly, color = '#5f7c45', fill = '#5f7c45') +
  geom_sf(data = natural_poly, color = '#47a9e6', fill = '#47a9e6') +   ##3c6f57
  geom_sf(data = buildings_poly, color = NA, fill = '#f3b49f') +
  theme_void() +
  theme(plot.title = element_text(family = "Barlow", size = 24, hjust = 0.5, margin = margin(t = 10), color = '#ede6ed'),   ##fee3eb
        plot.caption = element_text(family = "Barlow", size = 10, hjust = 0.5, margin = margin(b = 10), color = '#ede6ed'),
        plot.background = element_rect(fill = "#2e3133", color = NA)) +    ##3a3b3c
  labs(title = "Birmingham, UK",
              caption = "Data: {osmdata} | Graphic: JohnnyLau") 
  
  
  #theme(plot.caption = element_text(hjust = .5, color = '#fee3eb', 
  #                                  family = 'Barlow', size = 55), 
  #      plot.background = element_rect(fill = "#3a3b3c", color = NA)) +
  #labs(caption = 'Birmingham, UK')

ggsave('8_OSM/day8_OSM.png', plot=bham_cir_map, width = 6, height = 6.5, dpi = 300, bg="#2e3133")     ##3a3b3c




#ggplot() +
#  geom_sf(data = roads_new,
#          size = 0.6, 
#          colour = alpha("black", 0.9)) +
#  geom_sf(data = river_lines,
#          size = 0.6, 
#          colour = alpha("blue", 0.5)) +
#  geom_sf(data = landuse_lines,
#          size = 0.6, 
#          colour = alpha("green", 0.5)) +
#  geom_sf(data = leisure_lines,
#          size = 0.6, 
#          colour = alpha("green", 0.5)) +
#  labs(title = "Birmingham",
#       caption = "Johnny Lau | Data: {osmdata}") +
#  theme_void() +
#  theme(plot.title = element_text(family = "mono", size = 24, hjust = 0.5, margin = margin(t = 10)),
#        plot.caption = element_text(family = "mono", size = 10, hjust = 0.5, margin = margin(b = 10)))

# save
#ggsave("8_OSM/day_08.png", height = 6, width = 6, bg = "white")


