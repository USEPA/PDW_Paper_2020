library(sf)
library(dplyr)
library(raster)
library(units)
library(here)
library(foreach)
library(doParallel)

### 1990 Block Groups ###
sf <- st_read(here("data/geopackage/nhgis_block_groups.gpkg"), layer = "US_block_groups_1990") # Import prepared 1990 data
# Project to equal area (NAD83 CONUS ALBERS)
sfEA <- st_transform(sf, crs = 5070)

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
# obviously be much more computationally intense. If you still find that your computer cannot #
# handle this, you could replace the county FIPS with GISJOIN in the loop to iterate through  #
# block groups instead of counties. This for loop is designed so that if you do not           #
# finish in one session, it will pick up where you left off next time. It does this by        #
# checking output folder and removing any counties that have already been run from the list   #
# of counties that will be run in the loop.                                                   #
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


n_cores <- detectCores() - 5  
cl <- makeCluster(n_cores)  
registerDoParallel(cl)  

start <- Sys.time()

convert90 <-foreach(n=1:length(counties), .combine='rbind', .packages = c("R.utils","tidyverse","sf","units","raster","here")) %:%
  foreach(i=1:length(counties), .combine='rbind', .packages = c("R.utils","tidyverse","sf","units","raster","here")) %dopar% {
    sub <- sfEA%>%
      filter(STCO == counties[i])                                # Subset one county at a time
    #print(paste0("Starting ",counties[i]," (",sub$COUNTY[1],")"," at: ",Sys.time()))    # Print the start time for each county
    extent <- st_bbox(sub)                             # Find the spatial extent of that county to make an empty raster
    rows <- round(as.numeric(extent$ymax - extent$ymin)/20,0)  # Do the math to figure out how many cells 
    cols <- round(as.numeric(extent$xmax - extent$xmin)/20,0)  # the empty raster should be
    r <- raster(ncol = cols, nrow = rows)              # Create the empty raster
    extent(r) <- extent(sub)                           # Make the extent of the empty raster identical to the county
    rast1 <- rasterize(sub,r, field = 'well_Density')   # Rasterize the Well density into the empty raster
    projection(rast1) <- 5070
    origin(rast1) <- c(0,0)
    writeRaster(rast1,paste0(here("data/rasters/County_Well_Densities_1990"),"/well_density_90_",counties[i]),format = "GTiff",overwrite = TRUE) # Write the raster to a folder
    rast2 <- rasterize(sub,r, field = 'hu_Density')   # Rasterize the Housing Unit density into the empty raster
    projection(rast2) <- 5070
    origin(rast2) <- c(0,0)
    writeRaster(rast2,paste0(here("data/rasters/County_HU_Densities_1990"),"/hu_density_90_",counties[i]),format = "GTiff",overwrite = TRUE) # Write the raster to a folder
    
    time <- Sys.time()-start
    
    write(c(paste0("Completed ", round(100*(as.numeric(countLines(here("data/rasters/progress/Dasymetric_1990_report.txt")))/4)/length(counties),2),"% of counties"),
                 paste0("Counties Remaining: ", length(counties)- as.numeric(countLines(here("data/rasters/progress/Dasymetric_1990_report.txt")))/4),
                 paste0("Elapsed Time: ",round(time,3)," [",units(time),"]"),
                 "----"),
               file = here("data/rasters/progress/Dasymetric_1990_report.txt"),append = TRUE)
    sub <- data.frame("County" = sub$COUNTY[1],"Status"="Complete")
  
  }


#####################################
#        2000 Housing Units         #
#####################################
start2 <- Sys.time()
### 2000 Block Groups ###
sf2 <- st_read(here("data/geopackage/nhgis_block_groups.gpkg"), layer = "US_block_groups_2000") # Import prepared 1990 data
# Project to equal area
sfEA2 <- st_transform(sf2, crs = 5070)

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
convert00 <-foreach(n=1:length(counties), .combine='rbind', .packages = c("R.utils","tidyverse","sf","units","raster","here")) %:%
  foreach(i=1:length(counties), .combine='rbind', .packages = c("R.utils","tidyverse","sf","units","raster","here")) %dopar% {

  sub <- sfEA2%>%
    filter(STCO == counties[i])                                # Subset one county at a time
  extent <- st_bbox(sub)                             # Find the spatial extent of that county to make an empty raster
  rows <- round(as.numeric(extent$ymax - extent$ymin)/20,0)  # Do the math to figure out how many cells 
  cols <- round(as.numeric(extent$xmax - extent$xmin)/20,0)  # the empty raster should be
  r <- raster(ncol = cols, nrow = rows)              # Create the empty raster
  extent(r) <- extent(sub)                           # Make the extent of the empty raster identical to the county
  rast <- rasterize(sub,r, field = 'hu_Density')   # Rasterize the Housing Unit density into the empty raster
  projection(rast) <- 5070 #Match the projection to the input polygons
  origin(rast) <- c(0,0) # Set the origin point to 0,0 to facilitate merging later
  writeRaster(rast,paste0(here("data/rasters/County_HU_Densities_2000"),"/hu_density_00_",counties[i]),format = "GTiff") # Write the raster to a folder

  time <- Sys.time()-start2
  
  # Write Status messages
  write(c(paste0("Completed ", round(100*(as.numeric(countLines(here("data/rasters/progress/Dasymetric_2000_report.txt")))/4)/length(counties),2),"% of counties"),
          paste0("Counties Remaining: ", length(counties)- (as.numeric(countLines(here("data/rasters/progress/Dasymetric_2000_report.txt")))/4)),
          paste0("Elapsed Time: ",round(time,3)," [",units(time),"]"),
          "----"),
        file = here("data/rasters/progress/Dasymetric_2000_report.txt"),append = TRUE)
  sub <- data.frame("County" = sub$COUNTY[1],"Status"="Complete")
  }
