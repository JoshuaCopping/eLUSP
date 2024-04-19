# 0 SETUP ######################

# LOAD PACKAGES ---------------------------------------------
# install.packages("install.load")
library(install.load)

install_load(
  # essentials --
  "tidyverse",
  "magrittr",
  "here",
  "glue",
  "janitor",

  # Spatial --
  "terra",
  "raster",
  "rasterDT",
  "broom",
  "GADMTools",
  "sp",
  "sf",
  "rgdal",
  "exactextractr",
  "fasterize",

  # Plotting --
  "cowplot",
  "patchwork",
  "paletteer",
  "gt",
  "gtExtras",
  "ggtext",
  "ggnewscale",
  "shadowtext",
  "ungeviz"
  
)


# DEFINE CUSTOM FUNCTIONS ---------------------------------------------
# To avoid clashes
select <- dplyr::select

# For consistency in subsequent analysis
wind_footprint <- 0.02
solar_footprint <- 0.05

# Set BGN projection
bng <- CRS('+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs') 

