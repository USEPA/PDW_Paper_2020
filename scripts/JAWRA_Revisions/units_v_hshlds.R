library(tidyverse)
library(here)

housing <- read.csv(here("data/tables/misc/nhgis0243_ds120_1990_blck_grp.csv"))%>%
  dplyr::select(GISJOIN,EUO001,ESA001)
colnames(housing) <- c("GISJOIN","Households","Housing_Units")

sow <- read.csv(here("data/tables/misc/nhgis0243_ds123_1990_blck_grp.csv"))%>%
  dplyr::select(GISJOIN,EX5001,EX5002,EX5003,EX5004)
colnames(sow) <- c("GISJOIN","Public","Drill","Dug","Other")

df <- left_join(housing,sow)%>%
  mutate("Wat_Use" = Public+Drill+Dug+Other)


ggplot(df)+
  geom_segment(aes(x = 0, y = 0, xend = 12000, yend = 12000),linetype = "dashed")+
  geom_point(aes(x = Wat_Use, y = Households), color = 'black', shape = 1)+
  geom_point(aes(x = Wat_Use, y = Housing_Units), color = '#4B9CD3', shape = 2)+
  labs(title = "Water Users vs Housing Units or Households by Census Block Groups", x = "1990 Water Use Responses", y = "Households / Housing Units")


lmhshld <- lm(df$Wat_Use~df$Households)
lmhu <- lm(df$Wat_Use~df$Housing_Units)

