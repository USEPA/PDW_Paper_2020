library(tidyverse)
library(sf)
library(here)
library(units)

bg90 <- st_read(here("data/geopackage/nhgis_block_groups.gpkg"), layer = "US_block_groups_1990")%>%
  mutate(Area = st_area(bg90)%>%
           set_units(km^2))%>%
  mutate(HUD = Housing_Units / as.numeric(Area))

# Determine the max housing unit density where people use wells

filt1 <- bg90%>%
  filter((Drill_sow + Dug_sow) > 0)

summary(filt1$HUD)

filt2 <- filt1%>%
  filter(HUD > 1000)

# Look at the realtionship between housing unit density and wells
df <- data.frame()
for(n in seq(0,2000,10)){
  filt <- filt1%>%
    filter(HUD > n)
  wells <- sum(filt$Drill_sow+filt$Dug_sow)
  new <- data.frame("HUD" = n, "Wells" = wells)
  df <- rbind(df,new)
}

ggplot(df)+
  geom_line(aes(x = HUD, y = Wells))+
  geom_vline(aes(xintercept = 1667), color = 'red')


# Keep 99.9% of 15,131,691 means we can cutoff 15,131.69 = HUD of 1,667


sum(filt2$Drill_sow+filt2$Dug_sow)

# Find limit
filt3 <- bg90%>%
  filter((Drill_sow + Dug_sow) > 0)%>%
  filter(HUD > 274)

# Plot housing unit density vs wells
ggplot(filt1)+
  geom_point(aes(x = log10(HUD), y = log10(Drill_sow+Dug_sow)))

lm <- lm(filt1$Drill_sow+filt1$Dug_sow~filt1$HUD)
summary(lm)

# Plot geometry based on 100 ft (30.48 m) setbacks
c <- rbind(c(0,0), c(0,82.51), c(82.51,82.51), c(82.51,0), c(0,0))
pol <-st_polygon(list(c))
plot(pol)

p1 <- rbind(c(30.48,30.48))
pt1 <- st_multipoint(p1)
plot(pt1, add=TRUE)

p2 <- rbind(c(52.03,52.03))
pt2 <- st_multipoint(p2)

buf1 <- st_buffer(pt1,30.48)
buf2 <- st_buffer(pt2,30.48)

ggplot()+
  geom_sf(data=pol)+
  geom_sf(data=buf1, alpha = 0)+
  geom_sf(data=buf2, color='red', alpha = 0)+
  geom_sf(data=pt1,size=3)+
  geom_sf(data=pt2, size = 3)+
  geom_sf_label(data = pt1,aes(label = "Well"),nudge_x = 8, nudge_y = 5)+
  geom_sf_label(data = pt2,aes(label = "Septic"),nudge_x = 10, nudge_y = 5)+
  labs(title = "Minimum Well Spacing using 30m (100ft) setback",
       x = "Meters",
       y = "Meters")
  
# This suggests housing unit density no greater than 3280.84/

# Plot housing unit density of all block groups versus those using wells
ggplot()+
  geom_histogram(data = bg90, aes(x = HUD),color = 'black', fill = 'red')+
  geom_histogram(data = filt1, aes(x = HUD),color = 'black', fill = 'blue')+
  xlim(c(0,10000))
