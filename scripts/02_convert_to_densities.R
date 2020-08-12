library(sf)
library(dplyr)
library(raster)
library(units)
library(here)


### 1990 Block Groups ###
sf <- st_read(here("data/geopackage/nhgis_block_groups.gpkg"), layer = "US_block_groups_1990") # Import prepared 1990 data
# Project to equal area
sfEA <- st_transform(sf, crs = 2163)

# Calculate area of each block group in square km
sfEA$Area <- st_area(sfEA)%>%
  set_units(km^2)

# Calculate well density
sfEA$well_Density <- (sfEA$Drill_sow+sfEA$Dug_sow) / sfEA$Area

# Calculate housing unit density
sfEA$hu_Density <- sfEA$Housing_Units / sfEA$Area

###############################################################################################
# The next step is to rasterize the data. This is a very computationally intense operation.   #
# Here, we iterate through one county at a time. If we tried to do the whole country at once, #
# we would needlessly include a large chunk of the Pacific Ocean in our raster, which would   #
# obviously be much more computationally intense. If you still find that your comuter cannot  #
# handle this, you could replace the county FIPS with GISJOIN in the loop to iterate through  #
# through block groups instead of counties. This for loop is designed so that if you do not   #
# finish in one sn, it will pick up where you left off next time. It does this by checking    #
# output folder and removing any counties that have already been run from the list of         #
# counties that will be run in the loop.                                                      #
###############################################################################################

#####################################
# 1990 Well Density & Housing Units #
#####################################

# Create list of all County fips codes
sfEA$STCO <- as.factor(paste0(sfEA$ST_FIPS,sfEA$CO_FIPS))   # Create Column for State/County FIPS
counties <- levels(sfEA$STCO)                 # Make a list of all State/county fips combinations

# Create a list of all of the counties that have already been completed
files <- list.files(here("data/rasters/County_HU_Densities_1990"))
run <- substr(files,15,19)

# Refine county list by removing counties that have already run
counties <- setdiff(counties, run) # returns values in 'counties' that are not in 'run'

# Create for loop
for(n in counties){
  sub <- sfEA%>%
    filter(STCO == n)                                # Subset one county at a time
  print(paste0("Starting ",n," (",sub$COUNTY[1],")"," at: ",Sys.time()))    # Print the start time for each county
  extent <- st_bbox(sub)                             # Find the spatial extent of that county to make an empty raster
  rows <- round(as.numeric(extent$ymax - extent$ymin)/20,0)  # Do the math to figure out how many cells 
  cols <- round(as.numeric(extent$xmax - extent$xmin)/20,0)  # the empty raster should be
  r <- raster(ncol = cols, nrow = rows)              # Create the empty raster
  extent(r) <- extent(sub)                           # Make the extent of the empty raster identical to the county
  rast1 <- rasterize(sub,r, field = 'well_Density')   # Rasterize the Well density into the empty raster
  projection(rast1) <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
  origin(rast1) <- c(0,0)
  writeRaster(rast1,paste0(here("data/rasters/County_Well_Densities_1990"),"/well_density_90_",n),format = "GTiff") # Write the raster to a folder
  rast2 <- rasterize(sub,r, field = 'hu_Density')   # Rasterize the Housing Unit density into the empty raster
  projection(rast2) <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
  origin(rast2) <- c(0,0)
  writeRaster(rast2,paste0(here("data/rasters/County_HU_Densities_1990"),"/hu_density_90_",n),format = "GTiff") # Write the raster to a folder
  print(paste0("Finished ",n," at: ",Sys.time()))
}


#####################################
#        2000 Housing Units         #
#####################################

### 2000 Block Groups ###
sf2 <- st_read(here("data/geopackage/nhgis_block_groups.gpkg"), layer = "US_block_groups_2000") # Import prepared 1990 data
# Project to equal area
sfEA2 <- st_transform(sf2, crs = 2163)

# Calculate area of each block group in square km
sfEA2$Area <- st_area(sfEA2)%>%
  set_units(km^2)

# Calculate housing unit density
sfEA2$hu_Density <- sfEA2$Housing_Units / sfEA2$Area

# Create list of all County fips codes
sfEA2$STCO <- as.factor(paste0(sfEA2$STATEFP00,sfEA2$COUNTYFP00))   # Create Column for State/County FIPS
counties <- levels(sfEA2$STCO)                 # Make a list of all State/county fips combinations

# Create a list of all of the counties that have already been completed 
# (This helps if you need more than one session to complete the for loop)
files <- list.files(here("data/rasters/County_HU_Densities_2000"))
run <- substr(files,15,19)

# Refine county list by removing counties that have already run
counties <- setdiff(counties, run) # returns values in 'counties' that are not in 'run'

# Create for loop
for(n in counties){
  sub <- sfEA2%>%
    filter(STCO == n)                                # Subset one county at a time
  print(paste0("Starting ",n," at: ",Sys.time()))    # Print the start time for each county
  extent <- st_bbox(sub)                             # Find the spatial extent of that county to make an empty raster
  rows <- round(as.numeric(extent$ymax - extent$ymin)/20,0)  # Do the math to figure out how many cells 
  cols <- round(as.numeric(extent$xmax - extent$xmin)/20,0)  # the empty raster should be
  r <- raster(ncol = cols, nrow = rows)              # Create the empty raster
  extent(r) <- extent(sub)                           # Make the extent of the empty raster identical to the county
  rast <- rasterize(sub,r, field = 'hu_Density')   # Rasterize the Housing Unit density into the empty raster
  projection(rast) <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs" #Match the projection to the input polygons
  origin(rast) <- c(0,0) # Set the origin point to 0,0 to facilitate merging later
  writeRaster(rast,paste0(here("data/rasters/County_HU_Densities_2000"),"/hu_density_00_",n),format = "GTiff") # Write the raster to a folder
  print(paste0("Finished ",n," at: ",Sys.time()))
}