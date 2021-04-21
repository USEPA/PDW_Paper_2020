library(tidyverse)
library(sf)

# Check layer names
st_layers("D:/data/wells/National Files.gdb")

bgs <- st_read("D:/data/wells/National Files.gdb", layer = "US_Blk_Grps_2010")


rw <- bgs%>%
  filter(Result_Cat == "RW Method")


# Filter on rou greater in 2010 then in 1990
filt <- rw%>%
  filter(Hybd_ROU_10 > Well_RT_90 & Unit_Dnsty_10  >= 14.2)

filt2 <- rw%>%
  filter(Hybd_ROU_10 > Well_RT_90 & Unit_Dnsty_10  < 14.2)


nabove14 <- rw%>%
  filter(Unit_Dnsty_10  >= 14.2)

nrow(filt) / nrow(rw)

nrow(filt) / nrow(nabove14)

ggplot(nabove14)+
  geom_point(aes(x = Unit_Dnsty_10, y= Hybd_ROU_10 - Well_RT_90 ), alpha = .3)+
  geom_smooth(aes(x = Unit_Dnsty_10, y= Hybd_ROU_10 - Well_RT_90 ),method='lm', formula= y~x, color = 'red')+
  geom_vline(xintercept = 14.2, size = 1, color = "blue")+
  xlim(0,250)+
  ylim(-1,.6)+
  labs(title = "1990-2010 Change in Domestic Ratio vs. Housing Unit Denstiy", x = "2010 Housing Unit Density", y = "2010 - 1990 Domestic Ratio")+
  theme(text = element_text(size = 16))


lm <- lm(nabove14$Hybd_ROU_10 - nabove14$Well_RT_90 ~ nabove14$Unit_Dnsty_10)


# While we acknowledge that there is a density effect in the data, there is no linear trend between housing unit density and the domestic ratio from driller logs.
# For example, we found that 15% of block groups in RW states,


# Plot 2010 DR vs 2010 Housing unit density.