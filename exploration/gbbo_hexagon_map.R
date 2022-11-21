# Load packages
library(ggplot2) # For plotting
library(dplyr) # Data wrangling
library(skimr) # Explor data
library(showtext) # Custom fonts
library(ggmap) # Get coordinates from Google API
library(rcartocolor) # Colour palette for map
library(png) # Read in PNG file
library(grid) # Convert png to grob
library(cowplot) # Combine plots with annotation
library(rnaturalearth) # To get UK border data

showtext::showtext_auto()
showtext_opts(dpi = 600) # Makes sure the text is same res as plot
sysfonts::font_add_google(family = "Sacramento", name = "Sacramento")
## https://fonts.adobe.com/fonts/dolly
#sysfonts::font_add(family = "Dolly",
#                   regular = "DollyPro1.otf")


## read in data
bakers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/bakers.csv')

## Explore data
skim(bakers)

