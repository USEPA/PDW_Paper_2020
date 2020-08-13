library(tidyverse)
library(ggpmisc)
library(here)
library(DT)
library(gt)

df <- read.csv(here("data/csv/final_estimates_block_groups.csv"))

df$Wells_2010_NHU[is.infinite(df$Wells_2010_NHU)] <- 0
# NHU_All
sum(df$Wells_2010_NHU, na.rm=TRUE)

# RW All
sum(df$Wells_2010_Est, na.rm = TRUE)


# NHU No Flags
nf <- df%>%
  filter(T1_Pass == TRUE & T2_Pass == TRUE & T3_Pass == TRUE & T4_Pass == TRUE)
sum(nf$Wells_2010_NHU)

sum(nf$Wells_2010_Est, na.rm = TRUE)



# Plot by State
rw <- df%>%
  filter(Method == "RW")

my.formula <- y ~ x

ggplot(rw, aes(x=Wells_2010_Est, y = Wells_2010_NHU))+
  geom_point(color = "#878787", fill = "#3a9cde", alpha = .4, shape = 21)+ 
  geom_smooth(method=lm, color = "#565657", formula = my.formula)+
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..rr.label.., sep = "*plain(\",\")~")), 
               parse = TRUE)+
  facet_wrap(~ State)+
  labs(x = "Estimated Wells (RW Method)", y = "Estimated Wells (NHU Method)",title = "NHU vs. RW Methods by State")+
  scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        plot.margin=unit(c(2,.5,2,.5),"in"))


# Make a table of coefficients
summary_lm <- data.frame()

rw2 <- rw%>%
  droplevels()

for(i in levels(rw2$State)){
  df <- rw2%>%
    filter(State == i)
  lm <- lm(df$Wells_2010_Est~df$Wells_2010_NHU)
  new <- data.frame(State = i, r2 = round(summary(lm)$r.squared,2), y_int = round(summary(lm)$coefficients[1],1), p_val = round(summary(lm)$coefficients[8],8))
  summary_lm <- rbind(summary_lm,new)
}

datatable(summary_lm, colnames = c("State", "r2","p-value"), options = list(
  pageLength = 20
))


# gt package
table <- summary_lm%>%
  select(State,r2,y_int)%>%
  gt()%>%
  tab_header("NHU vs. RW Methods")%>%
  cols_label(r2 = html("R<sup>2</sup>"), y_int = "Y-int")

#gtsave(table,"C:/Users/HP/OneDrive - University of North Carolina at Chapel Hill/EPA_12_13_2017/Groundwater Well Use/Andrew Paper/Submission_2/figures/table_01.pdf")
