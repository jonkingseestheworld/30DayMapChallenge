# title: "Day1_point"
# subtitle: "pubs and starbucks"
# created by: "Johnny Lau"
# date: "2022-11-02"


# Data source: Kaggle - Beer or Coffee in London data
## https://www.kaggle.com/code/gpreda/beer-or-coffee-in-london-tough-choice-no-more/data


# Load packages ----
library(ggplot2)
library(here)
library(readr)
library(tidyr)
library(dplyr)
library(cowplot)
library(grid)

library(showtext)

font_add_google(name = "Ubuntu", family = "Ubuntu")
showtext_auto()

#library(geojsonio)
#library(sf)


# Data files
## List of UK pub locations
uk_pubs <- readr::read_csv(here::here("1_Point_pubs/uk_open_pubs.csv"))
world_starbucks <- readr::read_csv(here::here("1_Point_pubs/starbucks_directory.csv"))



# Data Preprocessing
uk_starbucks <- world_starbucks %>%
  filter(Country == "GB") %>%
  rename( lon = Longitude, 
          lat = Latitude ) %>%
  mutate( site = "starbucks") %>%
  drop_na(lon, lat)  %>%
  filter(City != "Isle of Man")


uk_pubs <- uk_pubs %>% 
  rename( lon = longitude, 
          lat = latitude) %>% 
  mutate(across(c(lon, lat), as.numeric)) %>%
  mutate(site = "pubs") %>%
  drop_na(lon, lat) %>%
  filter(local_authority != "Isles of Scilly")



## UK map from https://geoportal.statistics.gov.uk/datasets/countries-december-2018-boundaries-uk-bfc
# uk_map <- sf::st_read(here::here("1_Point_pubs/UK_NUTS_Level2/NUTS_Level_2_2018_Boundaries.shp")) 


uk_map <- map_data("world") %>% 
  filter(region == "UK")

ukmap_plot <- ggplot() +
  geom_polygon(data = uk_map,
               aes(x = long, y = lat,   group = group), 
               fill = "#28334AFF"   #mint cream "#FCF6F5FF"
               ) +
  coord_fixed(1.3) +
  geom_point(data = uk_pubs,
             aes(x = lon, y = lat),
             color = "#078282FF",
             size = 0.1, alpha=0.2) +
  geom_point(data = uk_starbucks,
             aes(x = lon, y = lat),
             color = "#F65058FF",
             size = 0.1, alpha=0.4) +
  theme_void() +
  theme( plot.background = element_rect(fill = "#011936FF", colour = "#011936FF"),
         panel.background = element_rect(fill = "#011936FF", colour= "#011936FF"),
         plot.margin = unit(c(t=1, r=1.5, b=1, l=1), unit = "cm"),
         plot.title = element_text(color = "white", size = "15", 
                                   family = "Ubuntu", margin=margin(b=0.5, unit="cm"), hjust=0),
         plot.subtitle = element_text(color = "white", size = "8", 
                                   family = "Ubuntu", margin=margin(b=2, unit="cm"), hjust=0),
         plot.caption = element_text(color = "#aaaaaa", size = "6", 
                                   family = "Ubuntu", margin=margin(t=1.5, unit="cm"), hjust=0)
         )  +
   labs(title="Pubs & Cafes in the UK", 
        subtitle = "Where to find drinks venues day & night?",
        caption = "Note: Data on pubs in Northern Ireland are missing \nData Source: https://www.kaggle.com/code/gpreda/\nbeer-or-coffee-in-london-tough-choice-no-more \n\nGraphic: Johnny Lau")
  


circle_sb <- circleGrob(x=0.7, 
                      y=0.70,
                      r=0.005,
                      gp = gpar(fill = "#F65058FF", col="#F65058FF"))

circle_p <- circleGrob(x=0.7, 
                      y=0.67,
                      r=0.005,
                      gp = gpar(fill = "#078282FF", col="#078282FF"))


p <- ggdraw(ukmap_plot) +
  draw_grob(circle_sb) + 
  draw_label(label = "Starbucks", x= 0.72, y=0.7, hjust=0,
             colour="white", size=10, fontfamily = "ubuntu") +
  draw_grob(circle_p) +
  draw_label(label = "Pubs", x= 0.72, y=0.67, hjust=0,
             colour="white", size=10, fontfamily = "ubuntu") 




ggsave( here::here("1_Point_pubs/1_Point_uk_pubs_starbucks.png"), plot = p, height = 6, width = 4, bg = "#011936FF")




#uk_starbucks <- world_starbucks %>%
#  filter(Country == "GB")

#pubs_starbucks_lonlat <- uk_starbucks %>%
#  select( longitude = Longitude, latitude = Latitude, ownership = `Ownership Type` ) %>%
#  mutate( site = "starbucks") %>%
#  bind_rows( uk_pubs %>% 
#               select(longitude, latitude) %>% 
#               mutate_all(as.numeric) %>%
#               mutate(site = "pubs") ) %>%
#  drop_na(longitude, latitude) %>%
#  #sf::st_as_sf(coords = c("longitude", "latitude")) %>%
#  #sf::st_set_crs(4277) %>% 
#  #sf::st_transform(crs = 4277) %>%
#  mutate(color = if_else( site == "pubs", "#F65058FF", "#078282FF" ),   #red crayola #teal blue
#         shape = if_else( site == "pubs", 20, 18))  #"#FC766AFF" 



#ukmap_plot <- ggplot() +
#  geom_polygon(data = uk_map,
#               aes(x = long, y = lat,   group = group, 
#                   #fill = color, colour = color
#               ), fill = "#FCF6F5FF",   #mint cream,
#               
#  ) +
#  coord_fixed(1.4) +
#  geom_point(data = pubs_starbucks_lonlat,
#             aes(x = longitude, y = latitude, color = color, shape=shape), 
#             size = 0.01, alpha=0.2) +
#  theme_void() +
#  scale_color_identity() +
#  guides(shape = "none") +
#  theme( plot.background = element_rect(fill = "#2A2B2DFF", colour = "#2A2B2DFF"),
#         panel.background = element_rect(fill = "#2A2B2DFF", colour= "#2A2B2DFF"),
#         plot.margin = unit(c(0.5, 3.5, 0.5, 3.5), unit = "cm")
#         )
  
  
  

