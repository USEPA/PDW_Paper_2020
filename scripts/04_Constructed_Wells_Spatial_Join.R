# In this script, we will take all of the constructed well data obtained from states
# that have required reporting of domestic well drilling since 1990 and spatially join
# their locations to 2010 Census block groups. We will create two new columns; the first
# will show wells constructed between April 1, 1990 and March 31, 2000. The second will
# show wells constructed between April 1, 2000 and Marcdh 31, 2010.

library(lubridate)
library(tidyverse)
library(sf)
library(here)


# Load geopackage with all (constructed) domestic well locations
wells <- st_read(here("data/Well Log Data/all_domestic_logs.gpkg"), layer = "All_Domestic")


# Create a list of the geopackage layers which we merge prior to the spatial join
gpkg <- st_layers(here("data/geopackage/reag_2010_boundaries.gpkg"))
layers <- gpkg$name


# Combine all of the states block group layers
sf <- st_read(here("data/geopackage/reag_2010_boundaries.gpkg"), layer = layers[1])

for(n in 2:length(layers)){
  sf2 <- st_read(here("data/geopackage/reag_2010_boundaries.gpkg"), layer = layers[n])
  sf <- rbind(sf,sf2)
}

# Run a join to determine which 2010 Census block group each well was built within
join <- st_join(wells,sf)

# Subset the wells by date and determine total number per block group constructed within that time period
wells_1990_2000 <- join%>%
  st_drop_geometry()%>%
  dplyr::select(GISJOIN, Date_Constructed)%>%
  dplyr::filter(Date_Constructed > ymd("1990-03-31") & 
           Date_Constructed < ymd("2000-04-01"))

tbl_1990_2000 <- as.data.frame(table(wells_1990_2000$GISJOIN))
colnames(tbl_1990_2000) <- c("GISJOIN","Wells_Cnstrctd_90_00")

wells_2000_2010 <- join%>%
  st_drop_geometry()%>%
  dplyr::select(GISJOIN, Date_Constructed)%>%
  dplyr::filter(Date_Constructed > ymd("2000-03-31") & 
           Date_Constructed < ymd("2010-04-01"))

tbl_2000_2010 <- as.data.frame(table(wells_2000_2010$GISJOIN))
colnames(tbl_2000_2010) <- c("GISJOIN","Wells_Cnstrctd_00_10")

# Arkansas Wells
arWells <- st_read(here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Arkansas_Domestic")%>%
  st_transform(crs=4326)

arJoin <- st_join(arWells,arSF)

ar_wells_1990_2000 <- arJoin%>%
  st_drop_geometry()%>%
  dplyr::select(GISJOIN, Date_Constructed)%>%
  dplyr::filter(Date_Constructed > ymd("1990-03-31") & 
                  Date_Constructed < ymd("2000-04-01"))
ar_tbl_1990_2000 <- as.data.frame(table(ar_wells_1990_2000$GISJOIN))
colnames(ar_tbl_1990_2000) <- c("GISJOIN","Wells_Cnstrctd_90_00")

ar_tbl_1990_2000$State <- substr(ar_tbl_1990_2000$GISJOIN,2,3)
arSel90 <- ar_tbl_1990_2000%>%
  filter(State == "05")%>%
  select(GISJOIN, Wells_Cnstrctd_90_00)

ar_wells_2000_2010 <- arJoin%>%
  st_drop_geometry()%>%
  dplyr::select(GISJOIN, Date_Constructed)%>%
  dplyr::filter(Date_Constructed > ymd("2000-03-31") & 
                  Date_Constructed < ymd("2010-04-01"))

ar_tbl_2000_2010 <- as.data.frame(table(ar_wells_2000_2010$GISJOIN))
colnames(ar_tbl_2000_2010) <- c("GISJOIN","Wells_Cnstrctd_00_10")

ar_tbl_2000_2010$State <- substr(ar_tbl_2000_2010$GISJOIN,2,3)
arSel00 <- ar_tbl_2000_2010%>%
  filter(State == "05")%>%
  select(GISJOIN, Wells_Cnstrctd_00_10)

tbl_1990_2000 <- tbl_1990_2000%>%
  mutate(STATE = substr(GISJOIN,2,3))%>%
  filter(!STATE == "05")%>%
  select(GISJOIN,Wells_Cnstrctd_90_00)%>%
  rbind(arSel90)

tbl_2000_2010 <- tbl_2000_2010%>%
  mutate(STATE = substr(GISJOIN,2,3))%>%
  filter(!STATE == "05")%>%
  select(GISJOIN,Wells_Cnstrctd_00_10)%>%
  rbind(arSel00)

# Join the well counts to the 2010 Census boundaries
sfJoin <- sf%>%
  left_join(tbl_1990_2000)%>%
  left_join(tbl_2000_2010)

# Count the results
tapply(sfJoin$Wells_Cnstrctd_90_00, sfJoin$STATEFP10, sum)



# Save the file
st_write(sfJoin, here("data/geopackage/reag_2010_boundaries_w_wells.gpkg"), layer = "All_Block_Groups_w_Wells", append = FALSE)
