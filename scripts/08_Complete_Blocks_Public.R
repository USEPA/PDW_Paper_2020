library(tidyverse)
library(sf)
library(here)
library(units)
library(vroom)

# Read in Block Group data
blkGrps <- st_read("D:/data/Wells_Public/Wells_Public.gdb", layer = "Block_Groups_2010")%>%
  st_drop_geometry()%>%
  mutate(Flag_1 = ifelse(Unit_Chg_00 <= 1000,"Pass","Fail"),
         Flag_2 = ifelse(Unit_Chg_10 <= 1000,"Pass","Fail"),
         Flag_3 = ifelse(Unit_Dnsty_10 <=1167,"Pass","Fail"),
         Flag_4 = ifelse(Hybd_Dnsty_10 <= 1167,"Pass","Fail"),
         Flag_5 = ifelse(HUC_GT_400 == "Y","Fail","Pass"))

blk_layers <- st_layers("D:/data/nhgis/Block_Boundaries.gdb")$name

# Import block level data (Population & Housing Units)
blkPop <- vroom(here("data/tables/nhgis0291_ds172_2010_block.csv"))

# Function to replace NaN values
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

# List RW states
rwBlkGrps <- blkGrps%>%
  filter(Result_Cat == "RW Method")
rwStates <- unique(rwBlkGrps$Name)

blksOut <- data.frame()

for (n in 1:length(blk_layers)) {
  blks <- st_read("D:/data/nhgis/Block_Boundaries.gdb", layer = blk_layers[n])%>%
    mutate(BlkGrp_ID = substr(GISJOIN,1,15))%>%
    left_join(blkPop)%>%
    select(GEOID10,NAME10,GISJOIN,STATEFP10,COUNTYFP10,BlkGrp_ID,STATE,COUNTY,H7V001,IFC001)
    
  colnames(blks)[9:10] <- c("Population","Housing_Units")  
  
  blks$Area <- st_area(blks)%>%
    set_units(km^2)
  
  join <- blks%>%
    #st_drop_geometry()%>%
    left_join(blkGrps, by = c("BlkGrp_ID" = "GISJOIN"))%>%
    mutate(Pct_Units = Housing_Units / Units_10,
           NHU_Wells_10 = round(Tot_Wells_10 * Pct_Units),
           NHU_Wells_10_Dens = round(NHU_Wells_10 / as.numeric(Area),2),
           NHU_ROU_10 = round(NHU_Wells_10 / Housing_Units,2),
           RW_Wells_10 = round(RW_Total_Wells * Pct_Units),
           RW_Wells_10_Dens = round(RW_Wells_10 / as.numeric(Area),2),
           RW_ROU_10 = round(RW_Wells_10 / Housing_Units,2),
           Hybrid_Wells_10 = round(Hybd_Tot_10 * Pct_Units),
           Hybrid_Wells_10_Dens = round(Hybrid_Wells_10 / as.numeric(Area),2),
           Hybrid_ROU_10 = round(Hybrid_Wells_10 / Housing_Units,2),
           Pop_Served_NHU = round((Population.x / Housing_Units)*NHU_Wells_10),
           Pop_Served_RW = round((Population.x / Housing_Units)*RW_Wells_10),
           Pop_Served_Hybrid = round((Population.x / Housing_Units)*Hybrid_Wells_10),
           RW_State = ifelse(STATE %in% rwStates,"Y","N"),
           Area = round(as.numeric(Area),3))%>%
    select(GEOID10.x,STATE,COUNTY,Population.x,Housing_Units,Area,
           NHU_Wells_10,NHU_Wells_10_Dens,NHU_ROU_10,
           RW_Wells_10,RW_Wells_10_Dens,RW_ROU_10,
           Hybrid_Wells_10,Hybrid_Wells_10_Dens,Hybrid_ROU_10,
           Pop_Served_NHU,Pop_Served_RW,Pop_Served_Hybrid,
           RW_State,Flag_1,Flag_2,Flag_3,Flag_4,Flag_5,
           Correction)%>%
    mutate(RW_Wells_10 = ifelse(RW_State == "N",NA,RW_Wells_10),
           RW_Wells_10_Dens = ifelse(RW_State == "N",NA,RW_Wells_10_Dens),
           RW_ROU_10 = ifelse(RW_State == "N",NA,RW_ROU_10),
           Pop_Served_RW = ifelse(RW_State == "N",NA,Pop_Served_RW))
  
  colnames(join) <- c("GEOID","STATE","COUNTY","Population","Housing_Units","Area",
                     "NHU_Wells_10", "NHU_Wells_10_Dens", "NHU_ROU_10",
                     "RW_Wells_10", "RW_Wells_10_Dens", "RW_ROU_10",
                     "Hybrid_Wells_10","Hybrid_Wells_10_Dens", "Hybrid_ROU_10",
                     "Pop_Served_NHU", "Pop_Served_RW","Pop_Served_Hybrid",
                     "RW_State","Flag_1", "Flag_2", "Flag_3", "Flag_4","Flag_5",
                     "Correction_2010","Shape")
  
  
  # Replace NaN and NA with 0
  df <- join%>%
    st_drop_geometry()
  
  df[is.nan(df)] <- NA
  #df[is.na(df)] <- 0
  
  geoms <- join%>%
    select(Shape)
  
  output <- bind_cols(df,geoms)%>%
    st_as_sf()
  
  # Write shapefile
  st_write(output,
           paste0(here('data/Well_Estimates/Blocks_By_State_shp'),"/",join$STATE[1],"_Blocks_2010.shp"),
           append=FALSE)
  
  # Write table
  tbl <- output%>%
    st_drop_geometry()
  vroom_write(tbl,paste0(here('data/Well_Estimates/Blocks By State'),"/",join$STATE[1],"_Blocks_2010.csv"),delim = ",")
  
  
  joindf <- output%>%
    st_drop_geometry()
  
  blksOut <- bind_rows(blksOut,joindf)
  
  print(paste0("Completed: ",blk_layers[n]," at ",Sys.time()))

  
}

vroom_write(blksOut, here("data/Well_Estimates/final_estimates_blocks.csv"))

sf <- st_read("D:/data/nhgis/Block_Boundaries.gdb",layer = blk_layers[1])%>%
  mutate(BlkGrp_ID = substr(GISJOIN,1,15))
# Save a geopackage
for (n in 2:length(blk_layers)) {
  sfnew <- st_read("D:/data/nhgis/Block_Boundaries.gdb",layer = blk_layers[n])%>%
    mutate(BlkGrp_ID = substr(GISJOIN,1,15))
  
  sf <- bind_rows(sf,sfnew)
  
  print(paste0("Added ",blk_layers[n]," at: ", Sys.time()))
}

blksSf <- sf%>%
  left_join(blksOut, by = "GISJOIN")

st_write(blksSf, here("data/Well_Estimates/Estimates_Public.gpkg"),layer = "Blocks_2010")




