library(tidyverse)
library(here)

sf <- st_read(here("data/geopackage/final_estimates.gpkg"), layer = "All_Estimates_Blk_Grps")

filt <- sf%>%
  filter(RW_2000 > 0 &
           (wells_km2_90/hu_km2_90)<1.01 &
           (RW_2000/hu_km2_00)<1.01 &
           (RW_2010/(Housing_Units/Area))<1.01)%>%
  mutate(wells_90 = wells_km2_90*Area)


ggplot(filt)+
  geom_point(aes(x = hu_km2_90, y = wells_km2_90/hu_km2_90))+
  geom_point(aes(x = hu_km2_00, y = RW_2000/hu_km2_00), color = 'red')+
  geom_point(aes(x = Housing_Units/Area, y = RW_2010/(Housing_Units/Area)), color = 'green')


pivot <- filt%>%
  mutate('1990' = hu_km2_90,
         '2000' = hu_km2_00,
         '2010' = Housing_Units / Area)%>%
  dplyr::select(GISJOIN,'1990','2000','2010')%>%
  st_drop_geometry()%>%
  pivot_longer(-GISJOIN, names_to = 'YEAR', values_to = 'HU_Density')%>%
  mutate(Join = paste0(GISJOIN,YEAR))%>%
  dplyr::select(Join, HU_Density)

pivotWells <- filt%>%
  mutate('1990' = wells_km2_90,
         '2000' = RW_2000,
         '2010' = RW_2010)%>%
  dplyr::select(GISJOIN, '1990','2000','2010')%>%
  st_drop_geometry()%>%
  pivot_longer(-GISJOIN, names_to = 'YEAR', values_to = "Well_Density")%>%
  mutate(Join = paste0(GISJOIN,YEAR))%>%
  dplyr::select(GISJOIN,Join, Well_Density, YEAR)

pivotJoin <- left_join(pivot,pivotWells)%>%
  filter(Well_Density > 10)

ggplot(pivotJoin)+
  geom_boxplot(aes(x = YEAR, y = Well_Density))
  geom_line(aes(x = YEAR, y = HU_Density/13, group = GISJOIN), color = 'red')
