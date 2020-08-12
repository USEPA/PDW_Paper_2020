library(tidyverse)
library(sf)
library(ggplot2)
library(here)


# CHECK NA VALUES

# CHECK 1167 VALUES


# Import Estimates
sf <- st_read(here("data/geopackage/final_estimates.gpkg"), layer= "All_Estimates_Blk_Grps_QA")


# Total Estimated Wells
sum(sf$Wells_2010_Est, na.rm = TRUE)

# Count the number of block groups with at least 1 flag
flagCount <- sf%>%
  filter(T1_Pass == FALSE |
           T2_Pass == FALSE |
           T3_Pass == FALSE |
           T4_Pass == FALSE)


# Count the flags by category
flag1 <- sf%>%
  filter(T1_Pass == FALSE)

flag2 <- sf%>%
  filter(T2_Pass == FALSE)

flag3 <- sf%>%
  filter(T3_Pass == FALSE)

flag4 <- sf%>%
  filter(T4_Pass == FALSE & Wells_HYBRD >= 1)

#flag5 <- sf%>%
#  filter(T5_Valid == FALSE)



# Bar Plot showing all block groups vs. only Valid
allvalid <- sf%>%
  filter(T1_Pass == TRUE &
           T2_Pass == TRUE &
           T3_Pass == TRUE &
           T4_Pass == TRUE)

sum(allvalid$Wells_2010_Est)



# RW filter
rw <- sf%>%
  filter(!is.na(RW_2010))

RWW <- rw%>%
  filter(Wells_HYBRD >= 1)

rwval <- RWW%>%
  filter(T1_Valid == TRUE&
           T2_Valid == TRUE &
           T3_Valid == TRUE &
           T4_Valid == TRUE)

# Estimate population served
sf$pop_per_HU <- ifelse(sf$Housing_Units>0, sf$Population/sf$Housing_Units,0)
sf$popServed <- sf$pop_per_HU*sf$Wells_HYBRD
sum(sf$popServed, na.rm = TRUE)

# Bar chart of block groups flagged vs not-flagged
ggplot()+
  geom_bar(data = sf, aes(x = STATEFP10, y = Wells_HYBRD), stat = "sum", fill = "#b4b8b5")+
  geom_bar(data = allvalid, aes(x = STATEFP10, y = Wells_HYBRD), stat = "sum", fill = '#35b858')+
  coord_flip()



# Export Block Groups for GeoPlatform Layer
sfOut <- sf%>%
  select(GISJOIN,STATE,COUNTY,Population,Housing_Units,hybrd_2010,Wells_HYBRD,popServed,T1_Valid,T2_Valid,T3_Valid,T4_Valid)
