# look at 1990 land area to water area

bg00 <- st_read(here("data/geopackage/nhgis_block_groups.gpkg"), layer = "US_block_groups_2000")


bg00$pctWat <- bg00$AWATER00/(bg00$ALAND00+bg00$AWATER00)

plot(bg00$ALAND00~bg00$AWATER00)

ggplot(bg00)+
  geom_histogram(aes(pctWat),bins=40)+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
  labs(title = "Percent Area of Block Groups Covered in Water",
       x = "Percent Water",
       y = "Number of Block Groups")

