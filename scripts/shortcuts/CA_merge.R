library(sf)
library(here)



wells90 <- st_read(here("data/geopackage/reagg_2010_boundaries.gpkg"),layer = "2010_Block_Groups_wd90_06")
hu90 <- st_read(here("data/geopackage/reagg_2010_boundaries.gpkg"),layer = "2010_Block_Groups_hu90_06")%>%
  dplyr::select(GISJOIN,hu_km2_90)%>%
  st_drop_geometry()

hu00 <- st_read(here("data/geopackage/reagg_2010_boundaries.gpkg"),layer = "2010_Block_Groups_hu00_06")%>%
  dplyr::select(GISJOIN,hu_km2_00)%>%
  st_drop_geometry()

join <- wells90%>%
  left_join(hu90)%>%
  left_join(hu00)

st_write(join, here("data/geopackage/reagg_2010_boundaries.gpkg"), layer = "2010_Block_Groups_06")
