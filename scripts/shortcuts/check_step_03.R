library(sf)
library(tidyverse)
library(here)

layers <- st_layers(here("data/geopackage/reag_2010_boundaries.gpkg"))
files <- layers$name

fips <- data.frame(FIPS = substr(files,19,20))%>%
  arrange()
fips

list <- list(substr(files,19,20))
