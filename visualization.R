library(dplyr)
library(leaflet)
library(rgdal)

load('dogs.RData')

######################################
# Owner Age - Dog Breed Relationship #
######################################

# Generate breeds table for totals
breeds <- table(breed=dogs2020$BREED, age=dogs2020$AGE)
breeds <- cbind(breeds, total = rowSums(breeds)) %>%
  as.data.frame()

# Use pie charts to visualise
par(mfrow = c(2,5))
pie(breeds$`11-20`, dogs2020$BREED, main="11-20")
pie(breeds$`21-30`, dogs2020$BREED, main="21-30")
pie(breeds$`31-40`, dogs2020$BREED, main="31-40")
pie(breeds$`41-50`, dogs2020$BREED, main="41-50")
pie(breeds$`51-60`, dogs2020$BREED, main="51-60")
pie(breeds$`61-70`, dogs2020$BREED, main="61-70")
pie(breeds$`71-80`, dogs2020$BREED, main="71-80")
pie(breeds$`81-90`, dogs2020$BREED, main="81-90")
pie(breeds$`91-100`, dogs2020$BREED, main="91-100")
pie(breeds$total, dogs2020$BREED, main="All ages")

# Delete generated table
rm(breeds)

zh_rg <- readOGR("/home/mirai_user/Downloads/stzh.adm_stadtkreise_v.json")

# group sub-districts together
list_districts <- list(
  District_1 <- c(
    "Rathaus",
    "Hochschulen",
    "Lindenhof",
    "City"
  ),
  District_2	<- c(
    "Wollishofen",
    "Leimbach",
    "Enge"
  ),
  District_3 <- c(
    "Alt-Wiedikon",
    "Friesenberg",
    "Sihlfeld"
  ),
  District_4 <- c(
    "Werd",
    "Langstrasse",
    "Hard"
  ),
  District_5 <- c(
    "Gewerbeschule",
    "Escher Wyss"
  ),
  District_6	<- c(
    "Unterstrass",
    "Oberstrass",
    "Unterstrass"
  ),
  District_7 <- c(		
    "Fluntern",
    "Hottingen",
    "Hirslanden",
    "Witikon"
  ),
  District_8 <- c(
    "Seefeld",
    "Mühlebach",
    "Weinegg"
  ),
  District_9 <- c(	
    "Albisrieden",
    "Altstetten"
  ),
  District_10 <- c(	
    "Höngg",
    "Wipkingen"
  ),
  District_11 <- c(	
    "Affoltern",
    "Oerlikon",
    "Seebach"
  ),
  District_12 <- c(
    "Saatlen",
    "Schwamendingen-Mitte",
    "Hirzenbach"
  )
)
avg_wealth <- unlist(lapply(seq_len(length(list_districts)), function (z) {
  mean(unlist(lapply(list_districts[[z]], function (y) {
    mean(unlist(lapply(y, function (x) {
      dogs2020[which(x == dogs2020$DISTRICT_NAME),]$`wealth (T. CHF)`
    })))
  })))
}))

bins <- c(0,50, 100, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = avg_wealth, bins = bins)

# color <- c("white",
#            "cyan",
#            "chocolate",
#            "blue", 
#            "brown",
#            "darkorchid",
#            "gold",
#            "yellowgreen",
#            "lightpink",
#            "seagreen",
#            "orangered",
#            "gray")
# pal <- colorNumeric("OrRd", length(zh_rg$kname))
leaflet(zh_rg) %>%
  addPolygons(fillColor = ~pal(unlist(avg_wealth)), weight = 2, fillOpacity = 0.9, 
              opacity = 1) %>%
  addTiles() %>% 
  addLegend(colors = pal(unlist(avg_wealth)), labels = as.list(zh_rg$kname), title = "Zurich Districts")
