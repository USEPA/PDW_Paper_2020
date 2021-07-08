library(tidyverse)
library(here)

df <- read.csv(here("figures/data/bins.csv"))


# Original
ggplot(df)+
  geom_col(aes( x = fct_reorder(Range,order), y = BG_Count), width = .8,
           fill = c("#3b3838","#3b3838","#3b3838","#3b3838","#3b3838","#3b3838","#3b3838",
                    "#3b3838","#3b3838","#3b3838","#3b3838","#878282","#878282","#878282","#878282","#878282"),
           color = 'black')+
  geom_col(aes( x = fct_reorder(Range,order), y = r * 30000), color = 'black', fill = 'white', width = .2)+
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),
    # Features of the first axis
    name = "Number of Block Groups",
        # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~./30000, name="Correlations [r]")
  )+
  labs(x = "", title = "NHU vs. RW Binned by Housing Unit Change")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), text = element_text(size = 14))


# Dots
p <- ggplot(df)+
  geom_col(aes( x = fct_reorder(Range,order), y = BG_Count), width = .8,
           fill = c("#3b3838","#3b3838","#3b3838","#3b3838","#3b3838","#3b3838","#3b3838",
                    "#3b3838","#3b3838","#3b3838","#3b3838","#878282","#878282","#878282","#878282","#878282"),
           color = 'black')+
  geom_point(aes( x = fct_reorder(Range,order), y = r * 30000),shape = 18, color = 'black', fill = 'black', size = 5)+
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                     # Features of the first axis
                     # Add a second axis and specify its features
                     sec.axis = sec_axis( trans=~./30000, name="Correlations [r]")
  )+
  labs(x = expression(paste("Housing Units / ",km^2)),
       title = "NHU vs. RW Binned by Housing Unit Change",
       y = "Block Groups")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), text = element_text(size = 16))

p
ggsave(here("figures/Fig03_binned_correlations.png"),plot = p, dpi = 600, width = 6)

# Side by side
ggplot(df)+
  geom_col(aes( x = fct_reorder(Range,order), y = BG_Count), width = .4,
           fill = c("#3b3838","#3b3838","#3b3838","#3b3838","#3b3838","#3b3838","#3b3838",
                    "#3b3838","#3b3838","#3b3838","#3b3838","#878282","#878282","#878282","#878282","#878282"),
           color = 'black',
           position = position_nudge(x = -0.1))+
  geom_col(aes( x = fct_reorder(Range,order), y = r * 30000), color = 'black', fill = 'white', width = .4,
           position = position_nudge(x = 0.3))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                     # Features of the first axis
                     name = "Number of Block Groups",
                     # Add a second axis and specify its features
                     sec.axis = sec_axis( trans=~./30000, name="Correlations [r]")
  )+
  labs(x = "", title = "NHU vs. RW Binned by Housing Unit Change")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), text = element_text(size = 14))
