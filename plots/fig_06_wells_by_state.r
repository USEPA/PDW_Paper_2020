library(tidyverse)
library(here)

# Import and correct the NAPA infinite block group
df <- read.csv(here("data/csv/final_estimates_block_groups.csv"))%>%
  mutate(Wells_2010_NHU = ifelse(is.infinite(Wells_2010_NHU),4,Wells_2010_NHU))

# Format table into long format
byState <- df%>%
  select(State, Wells_2010_Est, Wells_2010_NHU, Method)%>%
  group_by(State)%>%
  mutate(Total = sum(Wells_2010_Est, na.rm = TRUE),
         Total_NHU = sum(Wells_2010_NHU, na.rm = TRUE))%>%
  ungroup()%>%
  select(State,Total,Total_NHU,Method)%>%
  distinct()%>%
  mutate(Total = ifelse(Method == "NHU",0,Total))

state_nhu <- byState%>%
  select(State, Total_NHU)

long <- pivot_longer(byState,cols = c(Total,Total_NHU))%>%
  mutate(Method = ifelse(name == "Total", "RW", "NHU"))%>%
  select(State,Method,value)%>%
  left_join(state_nhu)
colnames(long) <- c("State","Method","Total","Total_NHU")

# Grayscale HOusing Units
ggplot()+
  geom_col(data = long[long$Method == "NHU", ], aes(x = reorder(State,Total_NHU), y = Total/1000000, fill = Method),width = .8, position = "dodge")+
  geom_col(data = long[long$Method == "RW", ], aes(x = reorder(State,Total_NHU), y = Total/1000000, fill = Method), width = 0.5, position = position_dodge(width = 0.9))+
  coord_flip()+
  labs(x = "", y = "Housing Units Using Private Wells (Millions)", title = "Estimated Housing Units Using Wells\n by State & Method")+
  scale_fill_manual(values = c("#262626","#b8b8b8"))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
  theme_bw()+
  theme(panel.grid.major.y = element_blank())


####################################
# Alternate Population Served Plot #
####################################

# Population Served
pop <- df%>%
  mutate(Pop_Served_NHU = round((Population_BlkGrp/Housing_Units)*Wells_2010_NHU))

popByState <- pop%>%
  select(State, Pop_Served_NHU, Population_Served_2010_Est, Method)%>%
  group_by(State)%>%
  mutate(Total = sum(Population_Served_2010_Est, na.rm = TRUE),
         Total_NHU = sum(Pop_Served_NHU, na.rm = TRUE))%>%
  ungroup()%>%
  select(State,Total,Total_NHU,Method)%>%
  distinct()%>%
  mutate(Total = ifelse(Method == "NHU",0,Total))

state_nhu <- popByState%>%
  select(State, Total_NHU)

popLong <- pivot_longer(popByState,cols = c(Total,Total_NHU))%>%
  mutate(Method = ifelse(name == "Total", "RW", "NHU"))%>%
  select(State,Method,value)%>%
  left_join(state_nhu)
colnames(popLong) <- c("State","Method","Total","Total_NHU")

# Green / Orange
ggplot(popLong)+
  geom_col(aes(x = reorder(State,Total_NHU), y = Total/1000000, fill = Method),colour = "#000000",position = position_dodge2(reverse = TRUE))+
  coord_flip()+
  labs(x = "", y = "Population Using Private Wells (Millions)", title = "Est. Population Using Wells\n by State & Method")+
  scale_fill_manual(values = c("#00A08A","#F98400"))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
  theme_bw()

# EPA Colors
ggplot(popLong)+
  geom_col(aes(x = reorder(State,Total_NHU), y = Total/1000000, fill = Method),colour = "#000000",position = position_dodge2(reverse = TRUE))+
  coord_flip()+
  labs(x = "", y = "Population Using Private Wells (Millions)", title = "Est. Population Using Wells\n by State & Method")+
  scale_fill_manual(values = c("#00cc33","#3399ff"))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
  theme_bw()


# RW and NHU seperately 