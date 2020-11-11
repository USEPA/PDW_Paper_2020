library(tidyverse)
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
  write.csv(sub,paste0(here("figures/data/OLS_Outputs"),"/",i,"_Predicted.csv"))
  stats <- data.frame(state = i ,y_int = as.numeric(lm$coefficients[1]),slope = as.numeric(lm$coefficients[2]),r2 = summary(lm)$r.squared,pVal = round(summary(lm)$coefficients[2,4],10))
  statsDf <- rbind(statsDf,stats)
}







ggplot(df)+
  geom_point(aes(x = Wells_2010_NHU / Housing_Units, y = Wells_2010_Est / Housing_Units))+
  geom_segment(aes(x=0,xend=1,y=0,yend=1), color = "red")+
  xlim(0,1)+
  ylim(0,1)+
  labs(title = "NHU vs. RW Well Use Ratio",
       y = "RW Method", x = "NHU Method")+
  facet_wrap(~State)


oh <- df%>%
  filter(State == "Ohio")

lm <- lm(oh$Wells_2010_Est/oh$Housing_Units ~ oh$Wells_2010_NHU/oh$Housing_Units)
summary(lm)

co <- df%>%
  filter(State == "Colorado")

colm <- lm(co$Wells_2010_Est/co$Housing_Units ~ co$Wells_2010_NHU/co$Housing_Units)
summary(lm)

write.csv(oh, here("figures/"))