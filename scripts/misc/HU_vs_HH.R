library(tidyverse)
library(sf)
library(here)

sf <- st_read(here("data/geopackage/nhgis_block_groups.gpkg"), layer = "US_block_groups_1990")

df <- read.csv(here("data/tables/nhgis0253_ds120_1990_blck_grp.csv"))

join <- sf%>%
  left_join(df, by="GISJOIN")%>%
  filter(pct_Well > 0)


lm <- lm(join$Housing_Units~join$EUO001)

p <- ggplot(join)+
  geom_point(aes(x = Housing_Units, y = EUO001, color = pct_Well))+
  labs(x = "Housing Units", y = "Households")

plotly::ggplotly(p, hoverinfo = "text",
                 text = ~paste("Households: ",EUO001,"<br>",
                               "Housing Units: ",Housing_Units,"<br>",
                               "County: ", COUNTY.x))






sf1 <- st_read(here("data/geopackage/nhgis_block_groups.gpkg"), layer = "US_block_groups_1990")