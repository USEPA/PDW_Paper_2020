library(tidyverse)
library(sf)
library(here)
library(units)
library(vroom)

files <- list.files(here("data/shapefiles/nhgis_blocks"), pattern = ".shp$", full.names = TRUE)
names <- list.files(here("data/shapefiles/nhgis_blocks"), pattern = ".shp$", full.names = FALSE)

# Import Geopackage (slower)
#blkGrps <- st_read(here("data/geopackage/final_estimates.gpkg"), layer= "All_Estimates_Blk_Grps_QA")%>%
#  st_drop_geometry()%>%
#  mutate(STATEFP10 = as.character(STATEFP10))

# Import csv (much faster & same data)
blkGrps <- vroom(here("data/Well_Estimates/final_estimates_block_groups.csv"))%>%
  mutate(STATEFP10 = str_pad(as.character(STATEFP10),2,pad = "0"))


tbl <- read.csv(here("data/tables/nhgis0247_ds172_2010_block.csv"))%>%
  select(GISJOIN,H7V001,IFC001)

colnames(tbl) <- c("GISJOIN","Population","Housing_Units")

dfOut <- data.frame()

for(n in 1:length(files)){
  print(paste0("Starting ", substr(names[n],1,2)," at: ",Sys.time()))
  sf <- st_read(files[n])%>%
    mutate("BlkGrp_ID" = substr(GISJOIN,1,15))%>%
    left_join(tbl)
  
  sf$Area <- st_area(sf)%>%
    set_units(km^2)
  
  StateNum <- as.character(sf$STATEFP10[1])
  
  filt <- blkGrps%>%
    filter(STATEFP10 == StateNum)%>%
    select(GISJOIN,GEOID,State,County, Housing_Units,Well_Density_2010_Est,Wells_2010_NHU,
           Wells_2010_Est,Method,Wells_1990,Wells_2000_Est,Area)
  
  colnames(filt) <- c("GISJOIN","GEOID","State","County","HU_BlkGrp",
                      "Well_Density_BlkGrp2010","Wells_2010_NHU","Wells_2010_Hybrd",
                      "Method","Wells_1990_BlkGrp","Wells_2000_BlkGrp","Area_BlkGrp")
  
  sfOut <- sf%>%
    left_join(filt, by = c("BlkGrp_ID" = "GISJOIN"))%>%
    mutate("Wells_Hybrd" = round((Housing_Units/HU_BlkGrp)*Wells_2010_Hybrd),
           "Wells_NHU" = round((Housing_Units/HU_BlkGrp)*Wells_2010_NHU))%>%
    mutate("Population_Served" = round((Population/Housing_Units))*Wells_Hybrd,
           "Percent_Served" = Population_Served/Population,
           "GEOID" = paste0(substr(GISJOIN,2,3), substr(GISJOIN,5,7), substr(GISJOIN,9,18)),
           "Wells_1990_Est" = (Wells_1990_BlkGrp/Area_BlkGrp)*as.numeric(Area),
           "Wells_2000_Est" = (Wells_2000_BlkGrp/Area_BlkGrp)*as.numeric(Area))%>%
    select(GISJOIN,GEOID,State,County,Population,Housing_Units,Well_Density_BlkGrp2010,
           Wells_NHU,Wells_Hybrd,Method,Percent_Served,Population_Served,Wells_1990_Est,Wells_2000_Est)%>%
    st_transform(crs = 4326)

  colnames(sfOut) <- c("GISJOIN","GEOID","State","County","Population_Block","Housing_Units_Block","Well_Density_2010_Est","Wells_NHU","Wells_2010_Hybrd","Method",
                       "Well_Usage_Rate_Est","Population_Served_Est","Wells_1990_Est","Wells_2000_Est","geometry")
  #st_write(sfOut, here("data/geopackage/final_estimates_blocks.gpkg"), layer= paste0(substr(names[n],1,2),"_Estimates_Blocks_QA"), append = FALSE)
  
  csvOut <- sfOut%>%
    st_drop_geometry()%>%
    mutate(Well_Density_2010_Est = as.numeric(Well_Density_2010_Est),
           Wells_NHU = as.numeric(Wells_NHU),
           Wells_2010_Hybrd = as.numeric(Wells_2010_Hybrd),
           Well_Usage_Rate_Est = as.numeric(Well_Usage_Rate_Est),
           Population_Served_Est = as.numeric(Population_Served_Est),
           Wells_1990_Est = as.numeric(Wells_1990_Est),
           Wells_2000_Est = as.numeric(Wells_2000_Est))
  
  dfOut <- rbind(dfOut, csvOut)
  
  #write.csv(csvOut, paste0(here("data/csv/blocks/final_estimates_blocks_"),substr(names[n],1,2),".csv"))
  
  print(paste0("Finished ", substr(names[n],1,2)," at: ",Sys.time()))
}

dfOut$Well_Density_2010_Est[is.na(dfOut$Well_Density_2010_Est)] <- -999
dfOut$Wells_NHU[is.na(dfOut$Wells_NHU)] <- -999
dfOut$Wells_2010_Hybrd[is.na(dfOut$Wells_2010_Hybrd)] <- -999
dfOut$Well_Usage_Rate_Est[is.na(dfOut$Well_Usage_Rate_Est)] <- -999
dfOut$Population_Served_Est[is.na(dfOut$Population_Served_Est)] <- -999
dfOut$Wells_1990_Est[is.na(dfOut$Wells_1990_Est)] <- -999
dfOut$Wells_2000_Est[is.na(dfOut$Wells_2000_Est)] <- -999

write.csv(dfOut, here("data/csv/blocks/Final_Estimates_All_Blocks.csv"))

#write.csv(blkGrps,here("data/csv/final_estimates_blockGroups/All_Estimates_Blk_Grps_QA.csv"))
# COLUMN NAMES FOR BLOCKS

# GEOID
# STATE
# COUNTY
# POPULATION (BLOCK)
# HOUSING UNITS (Block)
# WELL DENSITY
# 2010 WELLS (EST)
# METHOD
# WELL USAGE RATE (EST)
# POPULATION SERVED (EST)
# 1990 WELLS (EST)
# 2000 WELLS (EST)