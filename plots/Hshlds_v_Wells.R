library(tidyverse)
library(sf)
library(plotly)
library(here)


blkGrps <- st_read(here("data/geopackage/final_estimates.gpkg"), layer= "All_Estimates_Blk_Grps_QA")%>%
  st_drop_geometry()%>%
  mutate(STATEFP10 = as.character(STATEFP10))

sel <- blkGrps%>%
  select(GISJOIN, State, County)

#plot_ly(blkGrps)%>%
  add_markers(x = ~Well_Density_2010_Est, y = ~Housing_Units / Area)

sf90 <- st_read(here("data/geopackage/reag_2010_boundaries_w_wells.gpkg"), layer = "All_Block_Groups_w_Wells")%>%
  left_join(sel, by="GISJOIN")


lines1 <- blkGrps%>%
  select(GISJOIN, Well_Density_2010_Est,Housing_Units,Area)%>%
  mutate(X = Well_Density_2010_Est, Y = Housing_Units/Area)%>%
  select(GISJOIN,X,Y)%>%
  drop_na()

lines2 <- sf90%>%
  st_drop_geometry()%>%
  select(GISJOIN, wells_km2_90, hu_km2_90)%>%
  drop_na()
colnames(lines2) <- c("GISJOIN","X","Y")

linesdf <- rbind(lines1,lines2)%>%
  group_by(GISJOIN)



plot_ly()%>%
  add_lines(data = linesdf, x = ~X, y = ~Y, group = ~GISJOIN, name = "Change",line = list(color = 'rgb(161, 163, 166)'), width = 1)%>%
  add_markers(data = sf90, x = ~wells_km2_90, y = ~hu_km2_90,name = "1990",
              hoverinfo = "text",
              text = ~paste("County: ", County, "<br>",
                            "State: ", State,"<br>",
                            "1990 Population: ", Population,"<br>",
                            "1990 Housing Units: ",Housing_Units,"<br>",
                            "1990 Well Density: ", wells_km2_90, "<br>",
                            "1990 Housing Unit Density: ", hu_km2_90))%>%
  add_markers(data = blkGrps, x = ~Well_Density_2010_Est, y = ~Housing_Units / Area, name = "2010", colors = 'green',
              hoverinfo = "text",
              text = ~paste("County: ", County, "<br>",
                            "State: ", State,"<br>",
                            "2010 Population: ", Population_BlkGrp,"<br>",
                            "2010 Housing Units: ",Housing_Units,"<br>",
                            "Est 2010 Well Density: ", Well_Density_2010_Est, "<br>",
                            "2010 Housing Unit Density: ", Housing_Units/Area))%>%
    layout(xaxis = list(title = "Well Density"), yaxis = list(title = "Housing Unit Density"))

  