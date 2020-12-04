library(tidyverse)
library(here)

#substr right function
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

results <- read.csv(here("data/Well_Estimates/final_estimates_block_groups.csv"))

byState <- results%>%
  mutate(Population = sum(Population_BlkGrp),
         Well_Use_NHU = Wells_2010_NHU * (Population_BlkGrp / Housing_Units))%>%
  filter(!is.infinite(Well_Use_NHU))%>%
  group_by(State)%>%
  mutate(Use_NHU = sum(Well_Use_NHU, na.rm = TRUE),
         Use_Hybrd = sum(Population_Served_2010_Est, na.rm = TRUE),
         State_Pop = sum(Population_BlkGrp, na.rm = TRUE))%>%
  ungroup()%>%
  select(State,STATEFP10,Use_NHU,Use_Hybrd,State_Pop)%>%
  distinct()%>%
  mutate(ROU_NHU = round(100*(Use_NHU/State_Pop),1),
         ROU_Hybrd = round(100*(Use_Hybrd/State_Pop),1),
         STATEFP10 = substrRight(paste0("0",STATEFP10),2))



# Make it spatial
library(USAboundaries)

states <- us_states()%>%
  select(geoid)%>%
  left_join(byState,by = c("geoid" = "STATEFP10"))

sf::st_write(states, here("figures/data/RateOfUse.shp"))
