library(tidyverse)
library(here)


# Calculate linear correlation between population and housing units
df90 <- read.csv(here("data/tables/nhgis_ds120_1990_blck_grp.csv"))%>%
  select(GISJOIN,ET1001,ESA001)
colnames(df90) <- c("GISJOIN","Population","Housing_Units")

cor90 <- lm(df90$Housing_Units~df90$Population)

# 2000
df00 <- read.csv(here("data/tables/nhgis_ds147_2000_blck_grp.csv"))%>%
  select(GISJOIN,FXS001,FV5001)
colnames(df00) <- c("GISJOIN","Population","Housing_Units")

cor00 <- lm(df00$Housing_Units~df00$Population)

# 2010
df10 <- read.csv(here("data/tables/nhgis_ds172_2010_blck_grp.csv"))%>%
  select(GISJOIN,H7V001,IFC001)
colnames(df10) <- c("GISJOIN","Population","Housing_Units")

cor10 <- lm(df10$Housing_Units~df10$Population)

summary(cor90)
summary(cor00)
summary(cor10)
