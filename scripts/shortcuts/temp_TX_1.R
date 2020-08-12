library(sf)
library(tidyverse)
library(raster)
library(here)

# STEPS:
#
# 1. Mosaic county rasters into states
# 2. Extract raster values to 2010 boundaries
# 3. Save as a geopackage


# Import the 2010 boundaries 
sf <- st_read(here("data/geopackage/nhgis_block_groups.gpkg"),layer = "US_block_groups_2010")%>%
  st_transform(crs = 2163)

# We created the rasters by county, but we want to do the conversions at the state level
# so we need to mosaic together all of the rasters by state

# Create a list of state fips codes
states <- levels(sf$STATEFP10)

# Link: https://gis.stackexchange.com/questions/226351/combine-multiple-partially-overlapping-rasters-into-a-single-raster-in-r

# Write a function that will iterate through files to mosaic multiple rasters at once Credit ^^^
mosaicList <- function(rasList){
  
  #Internal function to make a list of raster objects from list of files.
  ListRasters <- function(list_names) {
    raster_list <- list() # initialise the list of rasters
    for (i in 1:(length(list_names))){ 
      grd_name <- list_names[i] # list_names contains all the names of the images in .grd format
      raster_file <- raster::raster(grd_name)
    }
    raster_list <- append(raster_list, raster_file) # update raster_list at each iteration
  }
  
  #convert every raster path to a raster object and create list of the results
  raster.list <-sapply(rasList, FUN = ListRasters)
  
  # edit settings of the raster list for use in do.call and mosaic
  names(raster.list) <- NULL
  #####This function deals with overlapping areas
  raster.list$fun <- mean
  
  #run do call to implement mosaic over the list of raster objects.
  mos <- do.call(raster::mosaic, raster.list)
  
  #set crs of output
  crs(mos) <- crs(x = raster(rasList[1]))
  return(mos)
}

# Next, we need to make a list of all of the rasters we have created in the previous step. We create a data frame that
# has a column for the file path of each raster and a colum denoting the state fips code for that raster.

# 1990 Well Densities

# Create a list of all of the states that have already been completed. Also remove Washington DC, since it is already one
# raster, it does not need to be mosaiced and will cause an error if you try
layers <- st_layers(here("data/geopackage/reagg_2010_boundaries.gpkg"))
run <- substr(layers$name,19,20)

# Refine state list by removing states that have already run
#states <- setdiff(states, run) # returns values in 'counties' that are not in 'run'
#states <- states[states != "11"]

# Or you can make a filter to just run one or some of the states
states <- c("48")

# Using full.names = FALSE here will give us a short name which we can extract the state fips code from
stateFips <- data.frame(file = list.files(here("data/rasters/County_Well_Densities_1990_100m"),full.names = FALSE,pattern = '.tif$'))%>%
  mutate(STATE_FP = substr(file,17,18))%>%
  dplyr::select(STATE_FP)

# Using full.names = TRUE here will give us the full file path, which will be different on different computers.
filesDF <- data.frame(wells90_file = list.files(here("data/rasters/County_Well_Densities_1990_100m"),full.names = TRUE,pattern = '.tif$'),
                      hu90_file = list.files(here("data/rasters/County_HU_Densities_1990_100m"),full.names = TRUE,pattern = '.tif$'),
                      hu00_file = list.files(here("data/rasters/County_HU_Densities_2000_100m"),full.names = TRUE,pattern = '.tif$'))%>%
  cbind(stateFips)%>%
  mutate(wells90_file = as.character(wells90_file),
         hu90_file = as.character(hu90_file),
         hu00_file = as.character(hu00_file))

# Now we write a loop which will subset our rasters by each state, then feed that subset to the mosaic function
# we wrote at the beginning of this script. We can then use the raster::extract() function to reaggregate the data

for(n in states){
  print(paste0("Starting ",n," at: ",Sys.time()))
  sub <- filesDF%>%
    filter(STATE_FP == n)
  
  # Mosaic 1990 wells
  wells90mos <- mosaicList(sub$wells90_file)
  print(paste0("Finished 1990 Well Density Mosaic for ",n," at: ",Sys.time()," ... Starting 1990 Housing Units ..."))
  
  # Mosaic 1990 Housing Units
  hu90mos <- mosaicList(sub$hu90_file)
  print(paste0("Finished 1990 Housing Unit Density Mosaic for ",n," at: ",Sys.time()," ... Starting 2000 Housing Units ..."))
  
  # Mosaic 2000 Housing Units
  hu00mos <- mosaicList(sub$hu00_file)
  print(paste0("Finished 2000 Housing Unit Density Mosaic for ",n," at: ",Sys.time()," ... Starting Raster Extractions ..."))
  
  # subset features to the state
  sfSub <- sf%>%
    dplyr::filter(STATEFP10 == n )%>%
    mutate(COUNTYFP10 = as.character(COUNTYFP10))
  
  # Make a data frame of county fips codes
  cntyFP <- sfSub%>%
    dplyr::select(COUNTYFP10)%>%
    st_drop_geometry()%>%
    distinct()
  
  # Let's create subsets of these so we can run them accross multiple nodes
  cntySub <- cntyFP[1:42,1]
  
  # Write a for loop to iterate by county
  for( i in cntySub){
    cnty <- sfSub%>%
      dplyr::filter(COUNTYFP10 == i)
    
    # Well Density 1990
    print(paste0("Extracting 1990 Well Density ...", Sys.time()))
    cnty$wells_km2_90 <- raster::extract(wells90mos,cnty, fun = mean, na.rm = TRUE)
    print(paste0("1990 Well Density Extraction Completed for ",i," at: ", Sys.time(), " ...Moving to 1990 Housing Unit Density..."))
    
    # Housing Unit Density 1990
    cnty$hu_km2_90 <- raster::extract(hu90mos,cnty, fun = mean, na.rm = TRUE)
    print(paste0("1990 Housing Unit Density Extraction Completed for ",i," at: ", Sys.time(), " ...Moving to 2000 Housing Unit Density..."))
    
    # Housing Unit Density 2000
    cnty$hu_km2_00 <- raster::extract(hu00mos,cnty, fun = mean, na.rm = TRUE)
    print(paste0("2000 Housing Unit Density Extraction Completed for ",i," at: ", Sys.time(), " ...SAVING..."))
    
    st_write(cnty, here("data/geopackage/Texas_reagg_1.gpkg"), layer = paste0("Texas_",i,"_reagg_2010_boundaries_1"))
  }
  

}
