library(sf)
library(tidyverse)
library(here)
library(units)


sf <- st_read(here("data/geopackage/reag_2010_boundaries_w_wells.gpkg"), layer = "All_Block_Groups_w_Wells")

## Add in the missing housing unit densities for 2000
missing <- read.csv(here("data/csv/missing_hu_2000.csv"))%>%
  select(GISJOIN, MEAN)

sf1 <- sf%>%
  filter(!GISJOIN %in% missing$GISJOIN)

sf2 <- sf%>%
  filter(GISJOIN %in% missing$GISJOIN)%>%
  left_join(missing)%>%
  mutate(hu_km2_00 = MEAN)%>%
  select(!MEAN)

sf <- rbind(sf1,sf2)

st_write(sf,here("data/geopackage/reag_2010_boundaries_w_wells.gpkg"), layer = "All_Block_Groups_w_Wells", append = FALSE)

# RW Method (Equation 1)
rwSf <- sf%>%
  dplyr::filter(as.character(STATEFP10) %in% c("04","05","08","16","20","21","22","23","24","26","27","29","30","32","34","35","39","40","50","56"))
rwSf <- rwSf%>%
  mutate(Area = st_area(rwSf)%>%
      set_units(km^2))%>%
  mutate(wells90 = wells_km2_90 * as.numeric(Area),
         hu90 = hu_km2_90 * as.numeric(Area),
         hu00 = hu_km2_00 * as.numeric(Area),
         hu_loss_00 = ifelse((hu90 - hu00)>0, hu90 - hu00,0),
         hu_loss_10 = ifelse((hu00 - Housing_Units)> 0, hu00 - Housing_Units,0),
         RW_2000 = wells_km2_90 + (Wells_Cnstrctd_90_00 / as.numeric(Area)) - ((wells_km2_90/hu_km2_90)*(hu_loss_00 / as.numeric(Area))),
         RW_2010 = RW_2000 + (Wells_Cnstrctd_00_10 / as.numeric(Area)) - ((RW_2000/hu_km2_00)*(hu_loss_10 / as.numeric(Area))))%>%
  dplyr::select(GISJOIN,hu90,hu00,Wells_Cnstrctd_90_00,Wells_Cnstrctd_00_10,RW_2000,RW_2010)%>%
  st_drop_geometry()


# NHU Method (Equation 2)
nhuSf <- sf%>%
  mutate(Area = st_area(sf)%>%
           set_units(km^2))%>%
  mutate(hu90 = hu_km2_90 * as.numeric(Area),
         hu00 = hu_km2_00 * as.numeric(Area),
         NHU_2000 = wells_km2_90 + ((wells_km2_90 / hu_km2_90)*((hu00 - hu90)/as.numeric(Area))),
         NHU_2010 = NHU_2000 + ((NHU_2000 / hu_km2_00)*((Housing_Units - hu00)/as.numeric(Area))))%>%
  dplyr::select(GISJOIN,STATEFP10, COUNTYFP10,Population, Housing_Units, wells_km2_90, hu_km2_90, hu_km2_00, Area, NHU_2000, NHU_2010)

# Join to make one complete data set with all estimates
dfOut <- left_join(nhuSf, rwSf)%>%
  mutate(hybrd_2000 = ifelse(is.na(RW_2000),NHU_2000,RW_2000),
         hybrd_2010 = ifelse(is.na(RW_2010),NHU_2010,RW_2010))

st_write(dfOut, here("data/geopackage/final_estimates.gpkg"), layer = "All_Estimates_Blk_Grps", append = FALSE)
