library(tidyverse)
library(leaflet)
library(here)

sfQA <- st_read(here("data/geopackage/final_estimates.gpkg"), layer= "All_Estimates_Blk_Grps_QA")

# Map
sf84 <- sfQA%>%
  filter(STATEFP10 == '37')%>%
  st_transform(crs = 4326)

labels <- sprintf(
  "<strong>%s</strong><br/>Population: %g<br/>Wells: %g<br/>T1 Pass: %s<br/>T2 Pass: %s<br/>T3 Pass: %s<br/>T4 Pass: %s<br/>T5 Pass: %s",
  sf84$GISJOIN, sf84$Population, sf84$hybrd_2010/sf84$Area,sf84$T1_Valid,sf84$T2_Valid,sf84$T3_Valid,sf84$T4_Valid,sf84$T5_Valid
) %>% lapply(htmltools::HTML)

bins <- c(0, 5, 10, 25, 50, 100, 250, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = sf84$Wells_HYBRD, bins = bins)
leaflet(sf84)%>%
  addTiles()%>%
  addPolygons( fillColor = ~pal(Wells_HYBRD),
               weight = 1,
               opacity = 1,
               color = "white",
               dashArray = "3",
               fillOpacity = 0.7,
               highlight = highlightOptions(
                 weight = 5,
                 color = "#666",
                 dashArray = "",
                 fillOpacity = 0.7,
                 bringToFront = TRUE),
               label = labels,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto"))
