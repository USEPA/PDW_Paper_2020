library(RSQLite)
db = SQLite()
con = dbConnect(db,here("data/geopackage/reagg_2010_boundaries.gpkg"))


dbRemoveTable(con, "2010_Block_Groups_hu00_06")
