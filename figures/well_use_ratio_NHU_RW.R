library(tidyverse)
library(sf)
library(plotly)
library(here)

df <- read.csv(here("data/Well_Estimates/final_estimates_block_groups.csv"))%>%
  filter(Method == "RW")%>%
  select(GISJOIN,State,Housing_Units,Wells_2010_Est,Wells_2010_NHU)%>%
  mutate(DR_RW = round(Wells_2010_Est/Housing_Units,4),
         DR_NHU = round(Wells_2010_NHU/Housing_Units,4))%>%
  drop_na()
colnames(df) <- c("GISJOIN","State","H_Units","RW_Wells","NHU_Wells","DR_RW","DR_NHU")

#write.csv(df, here("figures/data/block_groups_RW_NHU_noNA.csv"))




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
blkGrpsEX <- st_read("C:/Users/HP/OneDrive - University of North Carolina at Chapel Hill/EPA_12_13_2017/Data/NHGIS/Boundaries.gdb", layer = "US_blck_grp_2010")%>%
  filter(STATEFP10 %in% substr(dfFilt$GISJOIN,2,3))%>%
  select(GISJOIN)%>%
  st_transform(4326)%>%
  left_join(dfFilt)

st_write(blkGrpsEX, here("figures/data/spatial/DR_Predictions_BlkGrps.shp"))
