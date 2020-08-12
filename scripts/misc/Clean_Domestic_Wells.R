library(lubridate)
library(sf)
library(tidyverse)
library(here)

# Arizona
AZ <- st_read(here("data/Well Log Data/By State/Arizona/Arizona.gdb"), layer = "Arizona_Wells_All")%>%
  dplyr::select(WELLTYPE, DATE)%>%
  filter(WELLTYPE == "EXEMPT")%>%
  mutate(DATE = as.Date(DATE))%>%
  st_transform(crs = 2163)

colnames(AZ) <- c("Well_Type","Date_Constructed","Shape")

st_write(AZ,here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Arizona_Domestic")

# Arkansas
AR <- st_read(here("data/Well Log Data/By State/Arkansas/Arkansas_Wells.gdb"), layer = "All_Wells_Pts")%>%
  mutate("Well_Type" = substr(as.character(USE_CD__),1,2),
         "Date_Constructed" = ymd_hms(Date_))%>%
  select(Well_Type,Date_Constructed)%>%
  filter(Well_Type == "DO")%>%
  st_transform(crs = 2163)

st_write(AR,here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Arkansas_Domestic")

# Colorado
CO <- st_read(here("data/Well Log Data/By State/Colorado/Colorado.gdb"), layer = "Colorado_Domestic")%>%
  dplyr::select(use1,Date)%>%
  mutate(Date = as.Date(Date))%>%
  st_transform(crs = 2163)

colnames(CO) <- c("Well_Type","Date_Constructed","Shape")

st_write(CO, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Colorado_Domestic")

# Idaho
ID <- st_read(here("data/Well Log Data/By State/Idaho/Idaho.gdb"), layer = "Idaho_Wells_All")%>%
  select(WellUse, Constructi)%>%
  filter(WellUse == "Domestic" |
           WellUse == "Domestic-Multiple Residence" |
           WellUse == "Domestic-Single Residence")%>%
  mutate(Constructi = as.Date(Constructi))%>%
  st_transform(crs = 2163)

colnames(ID) <- c("Well_Type","Date_Constructed","Shape")

st_write(ID, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Idaho_Domestic")

# Kansas
KS <- st_read(here("data/Well Log Data/By State/Kansas/Kansas.gdb"), layer = "Kansas_Wells_Domestic")%>%
  select(WELL_USE, COMPLE_DATE)%>%
  mutate(COMPLE_DATE = dmy(COMPLE_DATE))%>%
  st_transform(crs = 2163)

colnames(KS) <- c("Well_Type","Date_Constructed","Shape")

st_write(KS, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Kansas_Domestic")

# Kentucky
KY <- st_read(here("data/Well Log Data/By State/Kentucky/Kentucky.gdb"), layer = "Kentucky_Wells_All")%>%
  select(Usage, EndDate)%>%
  filter(Usage == "Domestic - Multiple Domestic Households"|
           Usage == "Domestic - Single Household")%>%
  mutate(EndDate = as.Date(EndDate))%>%
  st_transform(crs = 2163)

colnames(KY) <- c("Well_Type","Date_Constructed","Shape")

st_write(KY, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Kentucky_Domestic")

# Lousiana
LA <- st_read(here("data/Well Log Data/By State/Louisianna/Water_Wells_Registration.gdb"), layer = "Water_Wells_Registration")%>%
  filter(WELL_STATUS == "Active")%>%
  filter(USE_DESCRIPTION == "domestic")%>%
  select(USE_DESCRIPTION, Date)%>%
  mutate(Date = as.Date(Date))%>%
  st_transform(crs = 2163)

colnames(LA) <- c("Well_Type","Date_Constructed","Shape")

st_write(LA, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Louisiana_Domestic")

# Maine
ME <- st_read(here("data/Well Log Data/By State/Maine/Maine.gdb"), layer = "Maine_Geolocated_Wells_All")%>%
  select(WELL_USE, DRILL_DATE)%>%
  filter(WELL_USE == "DOMESTIC")%>%
  mutate(DRILL_DATE = as.Date(DRILL_DATE))%>%
  st_transform(crs = 2163)

colnames(ME) <- c("Well_Type","Date_Constructed","Shape")

st_write(ME, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Maine_Domestic")

# Maryland
MD <- st_read(here("data/Well Log Data/By State/Maryland/Maryland.gdb"), layer = "All_Wells_geocorrected")%>%
  select(USER_Use_for_water_sim,USER_COMPLETION_DATE)%>%
  filter(USER_Use_for_water_sim == "DW")%>%
  mutate(USER_COMPLETION_DATE = as.Date(USER_COMPLETION_DATE))%>%
  st_transform(crs = 2163)

colnames(MD) <- c("Well_Type","Date_Constructed","Shape")

st_write(MD, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Maryland_Domestic")


# Michigan
MI <- st_read(here("data/Well Log Data/By State/Michigan/Michigan.gdb"), layer = "Water_Wells_All1")%>%
  select(WELL_TYPE, CONST_DATE)%>%
  filter(WELL_TYPE == "HOSHLD")%>%
  mutate(CONST_DATE = as.Date(CONST_DATE))%>%
  st_transform(crs = 2163)

colnames(MI) <- c("Well_Type","Date_Constructed","Shape")

st_write(MI, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Michigan_Domestic")

# Minnesota
MN <- st_read(here("data/Well Log Data/By State/Minnesota/Minnesota.gdb"), layer = "wells_All")%>%
  select(USE_C,DATE_DRLL)%>%
  filter(USE_C == "DO")%>%
  st_transform(crs = 2163)%>%
  mutate(DATE_DRLL = ymd(DATE_DRLL))

colnames(MN) <- c("Well_Type","Date_Constructed","Shape")

st_write(MN, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Minnesota_Domestic")


# Missouri
MO <- st_read(here("data/Well Log Data/By State/Missouri/Missouri.gdb"), layer = "Missouri_Wells_All")%>%
  select(WELL_USE, DATE_COMPL)%>%
  filter(WELL_USE == "Domestic")%>%
  mutate(DATE_COMPL = as.Date(DATE_COMPL))%>%
  st_transform(crs = 2163)

colnames(MO) <- c("Well_Type","Date_Constructed","Shape")

st_write(MO, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Missouri_Domestic")

# Montana
MT <- st_read(here("data/Well Log Data/By State/Montana/Montana.gdb"), layer = "Montana_Wells_All")%>%
  select(Well_Use,Date_Completed)%>%
  filter(Well_Use == "DOMESTIC")%>%
  mutate(Date_Completed = as.Date(Date_Completed))%>%
  st_transform(crs = 2163)

colnames(MT) <- c("Well_Type","Date_Constructed","Shape")

st_write(MT, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Montana_Domestic")

# Nevada
NV <- st_read(here("data/Well Log Data/By State/Nevada/Nevada.gdb"), layer = "Nevada_Wells_All")%>%
  select(proposed_use, well_finish_date)%>%
  filter(proposed_use == "H")%>%
  mutate(well_finish_date = as.Date(well_finish_date))%>%
  st_transform(crs = 2163)

colnames(NV) <- c("Well_Type","Date_Constructed","Shape")

st_write(NV, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Nevada_Domestic")

# New Jersey
NJ <- st_read(here("data/Well Log Data/By State/New Jersey/New_Jersey.gdb"), layer = "New_Jersey_Wells_All_Clip")%>%
  select(Well_Use, Date)%>%
  mutate(Well_Use = as.character(Well_Use),
         Date = as.Date(Date))%>%
  filter(Well_Use == "Domestic ")%>%
  st_transform(crs = 2163)

colnames(NJ) <- c("Well_Type","Date_Constructed","Shape")

st_write(NJ, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "New_Jersey_Domestic")

# New Mexico
NM <- st_read(here("data/Well Log Data/By State/New Mexico/New_Mexico.gdb"), layer = "New_Mexico_Wells_Domestic")%>%
  select(use_,finish_dat)%>%
  mutate(finish_dat = ymd_hms(finish_dat))%>%
  st_transform(crs = 2163)

colnames(NM) <- c("Well_Type","Date_Constructed","Shape")

st_write(NM, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "New_Mexico_Domestic")

# Ohio
OH <- st_read(here("data/Well Log Data/By State/Ohio/Ohio.gdb"), layer = "Ohio_Wells_All")%>%
  select(WELL_USE_CODE, DATE_OF_COMPLETION)%>%
  filter(WELL_USE_CODE == "D")%>%
  mutate(DATE_OF_COMPLETION = as.Date(DATE_OF_COMPLETION))%>%
  st_transform(crs = 2163)

colnames(OH) <- c("Well_Type","Date_Constructed","Shape")

st_write(OH, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Ohio_Domestic")

# Oklahoma
OK <- st_read(here("data/Well Log Data/By State/Oklahoma/Oklahoma.gdb"), layer = "Oklahoma_Wells_All")%>%
  select(USE_CLASS,CONST_DATE)%>%
  filter(USE_CLASS == "Domestic")%>%
  mutate(CONST_DATE = as.Date(CONST_DATE))%>%
  st_transform(crs = 2163)

colnames(OK) <- c("Well_Type","Date_Constructed","Shape")

st_write(OK, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Oklahoma_Domestic")

# Vermont
VT <- st_read(here("data/Well Log Data/By State/Vermont/Vermont.gdb"), layer = "Vermont_Wells")%>%
  select(WellUseCod, DateComple)%>%
  filter(WellUseCod == "Domestic")%>%
  mutate(DateComple = ymd_hms(DateComple))%>%
  st_transform(crs = 2163)

colnames(VT) <- c("Well_Type","Date_Constructed","Shape")

st_write(VT, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Vermont_Domestic")

# Wyoming
WY <- st_read(here("data/Well Log Data/By State/Wyoming/Wyoming.gdb"), layer = "Wyoming_Wells_Domestic_w_latlon")%>%
  select(Uses, PriorityDate)%>%
  filter(Uses == "DOM_GW")%>%
  mutate(PriorityDate = as.Date(PriorityDate))%>%
  st_transform(crs = 2163)

colnames(WY) <- c("Well_Type","Date_Constructed","Shape")

st_write(WY, here("data/Well Log Data/domestic_logs_by_state.gpkg"), layer = "Wyoming_Domestic")


# Combine one simplified file
all_domestic <- rbind(AZ,CO,ID,KS,KY,LA,MD,ME,MI,MN,MO,MT,NJ,NM,NV,OH,OK,VT,WY)
st_write(all_domestic,here("data/Well Log Data/all_domestic_logs.gpkg"), layer = "All_Domestic")
