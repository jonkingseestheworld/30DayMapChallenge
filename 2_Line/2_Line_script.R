# Libraries Loading -------------------------------------------------------
library(tidyverse)
library(sf)
library(nngeo)

# Data Reading and Wrangling ----------------------------------------------
## For definitions of col names, refer here: https://openflights.org/data.html
airports <- read_csv("https://gist.githubusercontent.com/fletchjeff/b60f46ca3c1aa51e9aea9fd86d0ab433/raw/81ee446939110daf8b5c3a79425ccf1100070578/airports.csv") |> 
  mutate(row_num= row_number()) |>
  ## convert to 'sf' objects by specifying coords
  st_as_sf(coords =c("Longitude", "Latitude"))

# Airports in Japanese main islands
airports_jpn <- airports |> 
  filter(str_detect(`Country`,'Japan')) %>%
  filter(!str_detect(`Name`,'Minami|Iwo Jima')) ## remove very far airports from the main islands
  #filter(str_detect(`Tz database time zone`, 'Asia/Tokyo'))


# Nearest Neighbours
## Returns the indices of layer y which are nearest neighbors of each feature of layer x. 
## The number of nearest neighbors k and the search radius maxdist can be modified. 

nn <- st_nn(airports_jpn, airports_jpn, k = 5, progress = F,  returnDist = TRUE)
airports_connexions <- st_connect(airports_jpn, airports_jpn, ids = nn$nn, progress = F)
## ids -- A sparse list representation of features to connect such as returned by function st_nn. 
## If NULL the function automatically calculates ids using st_nn
## st_connect returns LINESTRING type

# Graphic -----------------------------------------------------------------
## geom_sf() uses stat_sf() and adds coord_sf() for you.
jpn_airports_5nn <- airports_connexions |> 
  ggplot() + 
  geom_sf(size = .75) + 
  labs(
    title = "JAPAN'S AIRPORTS",
    subtitle = "Airports on main islands connecting to 5 nearest neighbours", 
    caption = "30DayMapChallenge (Day 2 : Lines)\nGraphic: github.com/jonkingseestheworld\nData: gist.github.com/fletchjeff"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(family = "Gotham Black", size = rel(4)),
    plot.subtitle = element_text(family = "Gotham Narrow", size = rel(1.4), margin = margin(t=.15, b = .5, unit = "cm")),
    plot.caption = element_text(family = "Gotham Narrow", size = rel(1), margin = margin(t = .5, unit = "cm"), lineheight = rel(1.1)),
    panel.grid = element_blank(), 
    axis.text = element_blank(),
    plot.background = element_rect(fill = "#c3dbd4", color = NA),  
    plot.margin = margin(c(1, 1, 1, 1), unit = "cm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("2_Line") 
ggsave(plot = jpn_airports_5nn, filename = glue::glue("{path}/2_Line_jpn_airports.png"), width=8, height=10, dpi=300, bg="#c3dbd4")  

# units="in", device=ragg::agg_png, 


#ggsave(glue::glue("{path}.pdf"), width = 9.5, height = 10.5, device = cairo_pdf)

#pdftools::pdf_convert(
#  pdf = glue::glue("{path}.pdf"), 
#  filenames = glue::glue("{path}_twitter.png"),
#  dpi = 196
# )