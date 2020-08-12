library(sf)
library(dplyr)
library(here)


# *** This script imports oiriginal files downloaded from NHGIS,
# *** and joins them together by year, exporting them into a geopackage


##############################
# --------- 1990 ----------- #
##############################

## Import Tabular Data
## 1990 tabular data - Make sure file names are correct, as NHGIS appends a download ID to their files
# If downloaded all at once, this table should contain both housing units and population
tblHU_POP90 <- read.csv(here("data/tables/nhgis_ds120_1990_blck_grp.csv"))%>%
  dplyr::select(GISJOIN,ET1001,ESA001)
colnames(tblHU_POP90) <- c("GISJOIN","Population","Housing_Units")

# Import the 1990 Source of Water Table
tblSOW90 <- read.csv(here("data/tables/nhgis_ds123_1990_blck_grp.csv"))%>%
  dplyr::select(GISJOIN,YEAR,COUNTY,EX5001,EX5002,EX5003,EX5004)
colnames(tblSOW90) <- c("GISJOIN","YEAR","COUNTY","Public_sow","Drill_sow","Dug_sow","Other_sow")

## Import Spatial Data &
## join all of the 1990 tabular data to the spatial data,
## then create a new column for percent of housing units
## using wells
bg1990 <- st_read(here("data/shapefiles/US_blck_grp_1990.shp"))%>%  # Import shapefile of block groups
  dplyr::select(GISJOIN,FIPSSTCO)%>%                                       # Keep only the join and state/county columns
  left_join(tblHU_POP90)%>%                                         # Join the population and housing unit tables
  left_join(tblSOW90)%>%                                            # Join the source of water table
  mutate(ST_FIPS = substr(as.character(FIPSSTCO),1,2))%>%           # Create a State FIPS column
  mutate(CO_FIPS = substr(as.character(FIPSSTCO),3,5))%>%           # Create a County FIPS column
  dplyr::select(GISJOIN,YEAR,ST_FIPS,CO_FIPS,COUNTY,Population,Housing_Units, 
         Public_sow,Drill_sow,Dug_sow,Other_sow)%>%                 # Keep only the needed columns after the joins
  mutate(pct_Well = 100 * (Drill_sow + Dug_sow)/(Public_sow+Drill_sow+Dug_sow+Other_sow))  # Create a new column with percent of housing units using wells

bg1990$pct_Well[is.na(bg1990$pct_Well)] <- 0       # Replace NA values with 0

# Export prepared data to a geopackage
st_write(bg1990, here("data/geopackage/nhgis_block_groups.gpkg"),layer = "US_block_groups_1990",driver = "GPKG")

##############################
# --------- 2000 ----------- #
##############################

## Import Tabular Data

# If downloaded all at once, tihs table should contain both housing units and population
tblHU_POP00 <- read.csv(here("data/tables/nhgis_ds147_2000_blck_grp.csv"))%>%
  dplyr::select(GISJOIN,FXS001,FV5001)
colnames(tblHU_POP00) <- c("GISJOIN","Population","Housing_Units")

## Import Spatial Data
# Join all of the 1990 tabular data to the spatial data
bg2000 <- st_read(here("data/shapefiles/US_blck_grp10_2000.shp"))%>%
  dplyr::select(GISJOIN,STATEFP00,COUNTYFP00,ALAND00,AWATER00)%>%
  left_join(tblHU_POP00)%>%
    mutate(YEAR = "2000")%>%
  dplyr::select(GISJOIN,YEAR,STATEFP00,COUNTYFP00,ALAND00,AWATER00,
         Population,Housing_Units)

# Export prepared data to a geopackage
st_write(bg2000, here("data/geopackage/nhgis_block_groups.gpkg"),"US_block_groups_2000",driver = "GPKG")

##############################
# --------- 2010 ----------- #
##############################

## Import Tabular Data

# If downloaded all at once, tihs table should contain both housing units and population
tblHU_POP10 <- read.csv(here("data/tables/nhgis_ds172_2010_blck_grp.csv"))%>%
  dplyr::select(GISJOIN,H7V001,IFC001)
colnames(tblHU_POP10) <- c("GISJOIN","Population","Housing_Units")

## Import Spatial Data
# Join all of the 1990 tabular data to the spatial data
bg2010 <- st_read(here("data/shapefiles/US_blck_grp_2010.shp"))%>%
  dplyr::select(GISJOIN,STATEFP10,COUNTYFP10,ALAND10,AWATER10)%>%
  left_join(tblHU_POP10)%>%
  mutate(YEAR = "2010")%>%
  dplyr::select(GISJOIN,YEAR,STATEFP10,COUNTYFP10,ALAND10,AWATER10,
         Population,Housing_Units)

# Export prepared data to a geopackage
st_write(bg2010, here("data/geopackage/nhgis_block_groups.gpkg"),"US_block_groups_2010",driver = "GPKG")

