library(tidyverse)
library(sf)
library(plotly)
library(ggpubr)
library(cowplot)
library(here)

df <- read.csv(here("data/Well_Estimates/final_estimates_block_groups.csv"))%>%
  filter(Method == "RW")%>%
  select(GISJOIN,State,Housing_Units,Wells_2010_Est,Wells_2010_NHU)%>%
  mutate(DR_RW = round(Wells_2010_Est/Housing_Units,4),
         DR_NHU = round(Wells_2010_NHU/Housing_Units,4))%>%
  drop_na()
colnames(df) <- c("GISJOIN","State","H_Units","RW_Wells","NHU_Wells","DR_RW","DR_NHU")

#write.csv(df, here("figures/data/block_groups_RW_NHU_noNA.csv"))


# Plot the Domestic Ratios
#filt1 <- df%>%
#  filter(DR_NHU < 1.1 & DR_RW < 1.1)

# Add in housing unit density change to color it. 
huDF <- st_read("C:/Users/HP/OneDrive - University of North Carolina at Chapel Hill/EPA_12_13_2017/Groundwater Well Use/Final Well Estimates/National Files.gdb", layer = "US_Blk_Grps_2010")%>%
  st_drop_geometry()%>%
  select(GISJOIN, Well_RT_90, HU_Chg_90_10)%>%
  right_join(df)%>%
  mutate(Class = as.factor(ifelse(HU_Chg_90_10 < -1.2, "< -1.2",
                        ifelse(HU_Chg_90_10 < 0, "-1.2 : 0",
                               ifelse(HU_Chg_90_10 < 1.2, "0 - 1.2",
                                      ifelse(HU_Chg_90_10 < 85, "1.2 : 85",
                                             ifelse(HU_Chg_90_10 >= 85, "> 85", NA)))))))

# Add in a 95% confidence interval by getting the residuals and finding the 95% residual distance threshold
lm <- lm(huDF$DR_RW~huDF$DR_NHU)
huDF$Residuals <- residuals(lm)
int95 <- 1.96*sd(huDF$Residuals) # 95% confidence interval


# Plot 95% of residuals seperately.
c95 <- huDF%>%
  mutate(absResid = abs(Residuals))%>%
  arrange(absResid)
i95 <- c95[1:59813,]
o95 <- c95[59814:nrow(c95),]



p1 <- ggplot(huDF,aes(x = DR_NHU, y = DR_RW))+
  geom_point( aes(color = fct_reorder(Class,HU_Chg_90_10)),alpha = .3, size = 1,shape = 16)+
  geom_abline(slope = 0.9143231, intercept = -0.0041607, color = "black")+
  labs(x = "NHU Domestic Ratio", y = "RW Domestic Ratio", title = "Change in Housing Unit Density")+
  scale_color_manual(name = bquote('Housing Unit Change'~(km^2)),
                     values = c("#d7191c","#fdae61","#d9ef8b","#a6d96a","#1a9641"))+
  scale_x_continuous(expand = c(0,0), limits = c(0,1.1))+
  scale_y_continuous(expand = c(0,0), limits = c(0,1.4))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 0, vjust = 0, hjust=.5, size=12),
        axis.text.x = element_text(angle = 0, vjust = 0, hjust=.5, size=12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
#p1

p2 <- ggplot()+
  geom_point(data = i95, aes(x = DR_NHU, y = DR_RW),color = "#c2aa53",shape = 16,size = 1, alpha = .3)+
  geom_point(data = o95, aes(x = DR_NHU, y = DR_RW),color = "black",shape = 16,size = 1, alpha = .5)+
  geom_abline(slope = 0.9143231, intercept = -0.0041607, color = "black")+
  labs(x = "NHU Domestic Ratio", y = "RW Domestic Ratio", title = "95% Nearest Block Groups to Regression Line")+
  scale_x_continuous(expand = c(0,0), limits = c(0,1.1))+
  scale_y_continuous(expand = c(0,0), limits = c(0,1.4))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0, hjust=.5, size=12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
p2


## Cowplot

plot <- plot_grid(p1,p2)

# Add title
title <- ggdraw() +
  draw_label(
    "Domestic Ratio (NHU v RW)",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

# All together now

fig1 <- plot_grid(
  title, plot,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)





## SECOND FIGURE ##
### Bins of housing units

df2 <- read.csv(here("figures/data/bins.csv"))%>%
  arrange(order)%>%
  mutate(pct_BGs = 100*(BG_Count / sum(BG_Count)),
         group = ifelse(order < 12, "1", "2"))

bgCount <- sum(df2$BG_Count)  

df2 <- df2%>%
  group_by(group)%>%
  mutate(pct_group = 100*sum(BG_Count)/bgCount)%>%
  ungroup()

fig2 <- ggplot(df2)+
  geom_col(aes(x = fct_reorder(Range,order), y = BG_Count/27000,fill = group),color = "black",width = 1)+
  geom_col(aes(x = fct_reorder(Range, order), y = r), width = .3, fill = "white", col = "black", lwd = 1)+
  theme_bw()+
  scale_fill_manual(name = "Block Groups", labels = c("93% of Block Groups","7% of Block Groups"), values = c("#4d4d4d","#adadad"))+
  scale_x_discrete(expand = c(0,0))+
  scale_y_continuous(name = "Correlation [r]",
                     sec.axis = sec_axis(trans=~.*2700, name="Number of Block Groups", labels = scales::comma, breaks = c(500,1000,1500,2000,2500)),
                     expand = c(0,0), limits = c(0,1))+
  labs(x =bquote('1990 - 2010 Housing Unit Density Change ['~HU/km^2~']'), title = "NHU vs. RW Binned by Housing Unit Change")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1.1, size=12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
fig2

### AUXILARY PLOTS ###

## OLS Regressions by state
# Run OLS regressions for each state
states <- levels(as.factor(df$State))

statsDf <- data.frame(state = character(0), y_int = numeric(0),slope = numeric(0),r2 = numeric(0),pVal = numeric(0))

for(i in states){
  sub <- df%>%
    filter(State == i)
  lm <- lm(sub$DR_RW~sub$DR_NHU)
  sub$predicted <- predict(lm)
  sub$residuals <- residuals(lm)
  #write.csv(sub,paste0(here("figures/data/OLS_Outputs"),"/",i,"_Predicted.csv"))
  stats <- data.frame(state = i ,y_int = as.numeric(lm$coefficients[1]),slope = as.numeric(lm$coefficients[2]),r2 = summary(lm)$r.squared,pVal = round(summary(lm)$coefficients[2,4],10))
  statsDf <- rbind(statsDf,stats)
}

# write.csv(statsDf, here("figures/data/OLS_Outputs/statsByState.csv")) # Write output statistics


# Import OLS regression outputs and combine into a single data frame

olsFiles <- list.files(here("figures/data/OLS_Outputs/"), pattern = "Predicted", full.names = TRUE)

dfAll <- read.csv(olsFiles[1])

for(n in 2:length(olsFiles)){
  csv <- read.csv(olsFiles[n])
  dfAll <- rbind(dfAll,csv)
}

dfFilt <- dfAll%>%
  filter(DR_NHU < 1.1 & DR_RW <1.1)


# Scatter plot of residuals

ggplot(dfFilt, aes(x = DR_NHU, y = DR_RW)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = DR_NHU, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals)), size = 2) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_line(aes(y = predicted), shape = 1) +
  theme_bw()+
  xlim(0,1)+
  ylim(0,1)+
  facet_wrap(~State)

# Map residuals
  # Make a State Selection Here
stateName <- "Ohio"

selection <- dfFilt%>%
  filter(State == stateName)

# import spatial dataset
blkGrps <- st_read("C:/Users/HP/OneDrive - University of North Carolina at Chapel Hill/EPA_12_13_2017/Data/NHGIS/Boundaries.gdb", layer = "US_blck_grp_2010")%>%
  filter(STATEFP10 == substr(selection$GISJOIN[1],2,3))%>%
  select(GISJOIN)%>%
  st_transform(4326)%>%
  left_join(selection)

plot_ly(blkGrps, fillcolor = ~abs(residuals))


# Export the full dataset for use in other GIS
#blkGrpsEX <- st_read("C:/Users/HP/OneDrive - University of North Carolina at Chapel Hill/EPA_12_13_2017/Data/NHGIS/Boundaries.gdb", layer = "US_blck_grp_2010")%>%
#  filter(STATEFP10 %in% substr(dfFilt$GISJOIN,2,3))%>%
#  select(GISJOIN)%>%
#  st_transform(4326)%>%
#  left_join(dfFilt)

#st_write(blkGrpsEX, here("figures/data/spatial/DR_Predictions_BlkGrps.shp"))
