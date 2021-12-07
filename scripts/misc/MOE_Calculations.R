library(tidyverse)
library(vroom)


id <- "G060081061372"

tbl1 <- vroom("D:/data/nhgis/tables/Block_Groups/nhgis0292_ds123_1990_blck_grp.csv")

tbl2 <- vroom("D:/data/nhgis/tables/Block_Groups/nhgis0293_ds120_1990_blck_grp.csv")

error <- vroom("D:/data/nhgis/tables/Block_Groups/nhgis0294_ds123_1990_blck_grp.csv")%>%
  select(GISJOIN,E2X001,E3J001,EX1001,EZX001,EZX002,EZY001,EZY002)

join <- left_join(tbl1,tbl2, by = "GISJOIN")%>%
  left_join(error, by = "GISJOIN")%>%
  select(GISJOIN,EX5001,EX5002,EX5003,EX5004,EX6001,EX6002,EX6003,ET1001,ESA001,
         E2X001,E3J001,EX1001,EZX001,EZX002,EZY001,EZY002)

colnames(join) <- c("GISJOIN","Public","Drilled","Dug","Other_Water","Sewer","Septic",
                    "Other_Disposal","Population","Housing_Units",
                    "Unweighted_Persons","Percent_Persons","Unweighted_Units",
                    "SOW_Imputation_Allocated","SOW_Imputation_NotAllocated",
                    "Sewage_Imputation_Allocated","Sewage_Imputation_Not_Allocated")

percent <- join%>%
  mutate(Pct_Population_Response = round(100*(Unweighted_Persons/Population),2),
         Pct_HousingUnit_Response = round(100*(Unweighted_Units/Housing_Units),2),
         Pop_MOE = 100*(1.645 *sqrt((Percent_Persons/100)*(1-(Percent_Persons/100)))/
           sqrt((Population-1)*Unweighted_Persons/(Population-Unweighted_Persons))))

vroom_write(percent,"D:/data/temp/Percent_Response_1990.csv", delim = ",")

id2 <- "G060081061349"
sub <- percent%>%
  filter(GISJOIN == id)%>%
  pivot_longer(!GISJOIN, names_to = "Variable",values_to = "Value")

huResponse <- 32/315

public <- 275 / 41
Drilled <- 12 / 41
Other <-  6 / 41
