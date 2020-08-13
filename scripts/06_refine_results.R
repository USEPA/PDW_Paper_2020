#############################################################
# Here, we apply the five tests shown in our paper as a way #
# to parse out spatial inaacuracies which may have occured  #
# while reaggregating 1990 & 2000 boundaries to match 2010  #
# Census boundaries.                                        #
#############################################################

library(tidyverse)
library(sf)
library(here)
library(units)


# Import data set with final estimates
sf <- st_read(here("data/geopackage/final_estimates.gpkg"), layer= "All_Estimates_Blk_Grps")

df <- sf%>%
  st_drop_geometry()


# Test #1: (1)	Housing change from 1990 – 2000:  Realistic housing unit density change was 
# determined for block groups between 1990 and 2000 by identifying block groups which maintained 
# the exact same boundaries between census collection years and then finding the range of housing 
# unit density changes for these block groups. Due to the fact that census boundary files for 1990 
# and 2000 are slightly different, block groups were determined to be identical if they had less 
# than .1 percent change in land area between 1990 and 2000.
t1 <- sf%>%
  mutate(T1_Valid = ifelse(((hu_km2_00-hu_km2_90)/hu_km2_90)<10,TRUE,FALSE))%>%
  select(GISJOIN,T1_Valid)%>%
  st_drop_geometry()

# Test #2: (2)	Housing change from 2000 - 2010:  Realistic housing unit density change was 
# determined for block groups between 2000 and 2010 by identifying block groups which maintained 
# the exact same boundaries between census collection years and then finding the range of housing 
# unit density changes for these block groups. The boundary files for block groups in 2000 and 2010
# use the same spatial basis (2010 Tiger Line files), i.e. if they have not been redrawn, their 
# geometries are identical. It could therefore be determined if block groups were identical by 
# using the function 'st_equals_exactly' (sf package)
t2 <- sf%>%
  mutate(T2_Valid = ifelse((((Housing_Units/Area)-hu_km2_00)/hu_km2_00)<10,TRUE,FALSE))%>%
  select(GISJOIN,T2_Valid)%>%
  st_drop_geometry()


# This section is a way to iterate through geometries to check for identical polygons, however
# it will take multiple days to complete so we use the above method instead.

#library(doParallel)
#registerDoParallel(100)

#start <- Sys.time()
#df <- data.frame()
#ident <- foreach(k = 1:1000,.combine=rbind,.packages=c("sf","tidyverse"))%dopar%{
#  row <- join10[k,]
#  id00 <- bg00%>%
#    filter(GISJOIN == row$GISJOIN00)
#  id10 <- bg10%>%
#    filter(GISJOIN == row$GISJOIN10)
#  equals <- st_equals(id00,id10,sparse = FALSE)
#  out <- data.frame("GISJOIN" = row$GISJOIN10, "Identical" = equals[1,1])
#  df <- rbind(df,out)
#}
#end <- Sys.time()
#end-start

# Test #3: (3)	Ratio of wells to housing units:  Results were determined to be unrealistic if there were 
# far more wells predicted in a block group than existing housing units. The predicted number of wells was 
# compared to the known number of housing units (2010 census) for each block group. If a block group had five 
# percent or more wells than it had housing units, the results for that block group were considered unrealistic.

#t3 <- sf%>%
#  filter(hybrd_2010 > 0)%>%
#  mutate(T3_Valid = ifelse(hybrd_2010/(Housing_Units/as.numeric(Area))<= 1, TRUE, FALSE))%>%
#  select(GISJOIN,T3_Valid)%>%
#  st_drop_geometry()


# Removed the original test three and knocked four and five down to three and four respectively

# Test #3:
t3 <- sf%>%
  mutate(T3_Valid = ifelse(Housing_Units/as.numeric(Area)<1667,TRUE,FALSE))%>%
  select(GISJOIN,T3_Valid)%>%
  st_drop_geometry()


# Test #4: 
t4 <- sf%>%
  mutate(T4_Valid = ifelse(hybrd_2010<1667,TRUE,FALSE))%>%
  select(GISJOIN, T4_Valid)%>%
  st_drop_geometry()


sfQA <- sf%>%
  left_join(t1)%>%
  left_join(t2)%>%
  left_join(t3)%>%
  left_join(t4)%>%
  mutate(Wells_NHU = ifelse(T1_Valid == FALSE & NHU_2010>Housing_Units/Area,Housing_Units,
                        ifelse(T2_Valid == FALSE & NHU_2010>Housing_Units/Area,Housing_Units,
                               ifelse(T3_Valid == FALSE & NHU_2010>Housing_Units/Area,Housing_Units,
                                             ifelse(T4_Valid == FALSE, 1167, NHU_2010*Area)))))%>%
  mutate(Wells_HYBRD = ifelse(T1_Valid == FALSE & hybrd_2010>Housing_Units/Area,Housing_Units,
                        ifelse(T2_Valid == FALSE & hybrd_2010>Housing_Units/Area,Housing_Units,
                        ifelse(T3_Valid == FALSE & hybrd_2010>Housing_Units/Area,Housing_Units,
                                      ifelse(T4_Valid == FALSE, 1167, hybrd_2010*Area)))))%>%
  mutate(T1_Valid = ifelse(Wells_HYBRD < .5, TRUE,T1_Valid))%>%
  mutate(T2_Valid = ifelse(Wells_HYBRD < .5, TRUE,T2_Valid))%>%
  mutate(T3_Valid = ifelse(Wells_HYBRD < .5, TRUE,T3_Valid))%>%
  mutate(T4_Valid = ifelse(Wells_HYBRD < .5, TRUE,T4_Valid))


# Organize the final dataset:
# COLUMN NAMES FOR BLOCK GROUPS

# GEOID x
# STATE x
# COUNTY x
# POPULATION (BLOCK GROUP) x
# HOUSING UNITS x
# WELL DENSITY x
# 2010 WELLS (EST) x
# METHOD X

# WELL USAGE RATE (1990)    (Move >100% = 100%)
# WELL USAGE RATE (2000)    (Move >100% = 100%)

# WELL USAGE RATE (EST) x   (Move >100% = 100%)

# Population Served 1990 (EST)
# Population Served 2000 (EST)


# POPULATION SERVED 2010 (EST) x
# TEST 1 PASS x
# TEST 2 PASS x
# TEST 3 PASS x
# TEST 4 PASS x
# TEST 5 PASS x
# 1990 WELLS x
# 2000 WELLS (EST) x


# Need to add in 1990 / 2000 Population
#pop90 <- st_read(here("data/geopackage/nhgis_block_groups.gpkg"),layer = "US_block_groups_1990")

#pop00 <- st_read(here("data/geopackage/nhgis_block_groups.gpkg"),layer = "US_block_groups_2000")

cnty <- st_read(here("data/geopackage/nhgis_counties.gpkg"), layer = "2010_Counties")%>%
  mutate(STCO = paste0(STATEFP,COUNTYFP))%>%
  select(STCO,STATE,COUNTY)%>%
  st_drop_geometry()


sfFinal <- sfQA%>%
  mutate("GEOID" = paste0(substr(GISJOIN,2,3), substr(GISJOIN,5,7), substr(GISJOIN,9,15)),
         "STCO" = paste0(STATEFP10,COUNTYFP10))%>%
  left_join(cnty, by = "STCO")%>%
  mutate("Method" = ifelse(is.na(NHU_2010)&is.na(RW_2010),NA,
                           ifelse(is.na(RW_2010),"NHU","RW")),
         "Population_Served" = (Population/Housing_Units)*Wells_HYBRD,
         "Well_Usage_Rate" = Population_Served/Population,
         "1990_Wells" = round(wells_km2_90*Area),
         "2000_Wells_Est"= round(hybrd_2000*Area),
         "Wells_2010_NHU" = round(NHU_2010*Area))%>%
  select(GISJOIN,GEOID,STATE,STATEFP10,COUNTY,Population,Housing_Units,hybrd_2010,Wells_HYBRD,Wells_2010_NHU,Method,Well_Usage_Rate,
         Population_Served,T1_Valid,T2_Valid,T3_Valid,T4_Valid,"1990_Wells","2000_Wells_Est",Area)

colnames(sfFinal) <- c("GISJOIN","GEOID","State","STATEFP10","County","Population_BlkGrp","Housing_Units",
                       "Well_Density_2010_Est","Wells_2010_Est","Wells_2010_NHU","Method","Well_Usage_Rate_2010_Est",
                       "Population_Served_2010_Est","T1_Pass","T2_Pass","T3_Pass","T4_Pass","Wells_1990","Wells_2000_Est","Area","geom")


#Recalculate density and population served
sfQA <- sfFinal%>%
  mutate(Well_Density_2010_Est = Wells_2010_Est/Area,
         Well_Usage_Rate_2010_Est = Wells_2010_Est / Housing_Units)%>%
  mutate(Well_Usage_Rate_2010_Est = ifelse(Well_Usage_Rate_2010_Est < 0,0,
                                           ifelse(Well_Usage_Rate_2010_Est>1,1,Well_Usage_Rate_2010_Est)),
         Population_Served_2010_Est = (Population_BlkGrp / Housing_Units)*Wells_2010_Est)%>%
  mutate(Population_Served_2010_Est = ifelse(is.infinite(Population_Served_2010_Est) & Wells_2010_Est > 0, Wells_2010_Est*2,
                                             ifelse(is.infinite(Population_Served_2010_Est),0,Population_Served_2010_Est)))


csvFinal <- sfQA%>%
  st_drop_geometry()

csvFinal_OnlyWells <- csvFinal%>%
  filter(Wells_2010_Est >= .5)


write.csv(csvFinal, here("data/csv/final_estimates_block_groups.csv"))

write.csv(csvFinal_OnlyWells, here("data/csv/final_estimates_block_groups_only_wells.csv"))

st_write(sfQA, here("data/geopackage/final_estimates.gpkg"), layer= "All_Estimates_Blk_Grps_QA", append = FALSE)

