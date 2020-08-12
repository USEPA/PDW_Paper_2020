library(lubridate)
library(tidyverse)
library(sf)
library(here)
library(units)


ca <- st_read(here("data/geopackage/nhgis_block_groups.gpkg"), layer = "US_block_groups_1990")%>%
  filter(ST_FIPS == "06")

# Number of domestic wells in CA in 1990
sum(ca$Drill_sow)+sum(ca$Dug_sow)


# Number of wells Well logs we include
sf <- st_read(here("data/geopackage/final_estimates.gpkg"), layer= "All_Estimates_Blk_Grps")

sum(sf$Wells_Cnstrctd_90_00)+sum(sf$Wells_Cnstrctd_00_10)

wells_blt <- sf%>%
  filter(Wells_Cnstrctd_00_10 > 0 |
           Wells_Cnstrctd_90_00 > 0)

# Area
wells_blt$Area <- st_area(wells_blt)%>%
  set_units(km^2)

# Housing unit density increase
wells_blt$hu_inc_00 <- wells_blt$hu_km2_00-wells_blt$hu_km2_90
wells_blt$hu_inc_10 <- (wells_blt$Housing_Units/as.numeric(wells_blt$Area)) -wells_blt$hu_km2_00

# Well Density Increase
wells_blt$wd_inc_00 <- wells_blt$RW_2000-wells_blt$wells_km2_90
wells_blt$wd_inc_10 <- wells_blt$RW_2010-wells_blt$RW_2000


# Plot it

ggplot(wells_blt)+
  geom_point(aes(x = hu_inc_00, y = wd_inc_00), color = 'blue')+
  geom_point(aes(x = hu_inc_10, y = wd_inc_10), color = 'purple')


# Regress it
lm00 <- lm(wells_blt$wd_inc_00~wells_blt$hu_inc_00)
summary(lm00)
