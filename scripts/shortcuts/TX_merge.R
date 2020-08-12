library(sf)
library(here)

#################################################
# Bind together first geopackage into one layer #
#################################################

#List the layers in the first geopackage
l1 <- st_layers(here("data/geopackage/Texas_reagg_1.gpkg"))
lNames <- l1$name

# Import the first layer to bind other layers to
sf1 <- st_read(here("data/geopackage/Texas_reagg_1.gpkg"), layer = lNames[1])

# iterate through all of the layers and bind them
for( i in 2:length(lNames)){
  sf <- st_read(here("data/geopackage/Texas_reagg_1.gpkg"), layer = lNames[i])
  sf1 <- rbind(sf1,sf)
}

##################################################
# Bind together second geopackage into one layer #
##################################################

#List the layers in the first geopackage
l2 <- st_layers(here("data/geopackage/Texas_reagg_2.gpkg"))
lNames <- l2$name

# Import the first layer to bind other layers to
sf2 <- st_read(here("data/geopackage/Texas_reagg_2.gpkg"), layer = lNames[1])

# iterate through all of the layers and bind them
for( i in 2:length(lNames)){
  sf <- st_read(here("data/geopackage/Texas_reagg_2.gpkg"), layer = lNames[i])
  sf2 <- rbind(sf2,sf)
}

#################################################
# Bind together third geopackage into one layer #
#################################################

#List the layers in the first geopackage
l3 <- st_layers(here("data/geopackage/Texas_reagg_3.gpkg"))
lNames <- l3$name

# Import the first layer to bind other layers to
sf3 <- st_read(here("data/geopackage/Texas_reagg_3.gpkg"), layer = lNames[1])

# iterate through all of the layers and bind them
for( i in 2:length(lNames)){
  sf <- st_read(here("data/geopackage/Texas_reagg_3.gpkg"), layer = lNames[i])
  sf3 <- rbind(sf3,sf)
}


##################################################
# Bind together fourth geopackage into one layer #
##################################################

#List the layers in the first geopackage
l4 <- st_layers(here("data/geopackage/Texas_reagg_4.gpkg"))
lNames <- l4$name

# Import the first layer to bind other layers to
sf4 <- st_read(here("data/geopackage/Texas_reagg_4.gpkg"), layer = lNames[1])

# iterate through all of the layers and bind them
for( i in 2:length(lNames)){
  sf <- st_read(here("data/geopackage/Texas_reagg_4.gpkg"), layer = lNames[i])
  sf4 <- rbind(sf4,sf)
}

#################################################
# Bind together fifth geopackage into one layer #
#################################################

#List the layers in the first geopackage
l5 <- st_layers(here("data/geopackage/Texas_reagg_5.gpkg"))
lNames <- l5$name

# Import the first layer to bind other layers to
sf5 <- st_read(here("data/geopackage/Texas_reagg_5.gpkg"), layer = lNames[1])

# iterate through all of the layers and bind them
for( i in 2:length(lNames)){
  sf <- st_read(here("data/geopackage/Texas_reagg_5.gpkg"), layer = lNames[i])
  sf5 <- rbind(sf5,sf)
}

#################################################
# Bind together sixth geopackage into one layer #
#################################################

#List the layers in the first geopackage
l6 <- st_layers(here("data/geopackage/Texas_reagg_6.gpkg"))
lNames <- l6$name

# Import the first layer to bind other layers to
sf6 <- st_read(here("data/geopackage/Texas_reagg_6.gpkg"), layer = lNames[1])

# iterate through all of the layers and bind them
for( i in 2:length(lNames)){
  sf <- st_read(here("data/geopackage/Texas_reagg_6.gpkg"), layer = lNames[i])
  sf6 <- rbind(sf6,sf)
}


# Bind everything together
sfTX <- rbind(sf1,sf2,sf3,sf4,sf5,sf6)

# Check with a plot
#plot(st_geometry(sfTX))

# Save it to the geopackage with all of the other states
st_write(sfTX, here("data/geopackage/reag_2010_boundaries.gpkg"), layer = "2010_Block_Groups_48")
