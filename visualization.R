library(dplyr)
library(leaflet)
library(rgdal)

zh_rg <- readOGR("/home/mirai_user/Downloads/stzh.adm_stadtkreise_v.json")
color <- c("white",
           "cyan",
           "chocolate",
           "blue", 
           "brown",
           "darkorchid",
           "gold",
           "yellowgreen",
           "lightpink",
           "seagreen",
           "orangered",
           "gray")
# pal <- colorNumeric("OrRd", length(zh_rg$kname))
leaflet(zh_rg) %>%
  addPolygons(color = color, fill = TRUE, fillColor = color, weight = 2, fillOpacity = 0.9, 
              opacity = 1) %>%
  addTiles() %>% 
  addLegend(colors = color, labels = as.list(zh_rg$kname), title = "Zurich Districts")
