# Mexico-Choropleth-map
Mexico Choropleth Map using R and Leaflet

library(rgdal)
library(leaflet)
library(maptools)
library(RColorBrewer)
library(geosphere)
library(ggplot2)

S_MPOS <- readOGR(".", layer = "subset_mpos", encoding = "UTF-8")
DFS_MPOS<-S_MPOS@data

S_MPOS@data$ESTABLECIMIENTOS<-ESTABLECIMIENTOS
pal <- colorNumeric("Blues", NULL, n = 17) #n numero de variables a colorear

choropleth_map<-leaflet(data = S_MPOS) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(S_MPOS$ESTABLECIMIENTOS), 
              fillOpacity = 0.9, 
              color = "#BDBDC3",
              weight = 1, 
              popup = S_MPOS@data$NOM_MUN)%>%
  addLegend(pal = pal, values = ~S_MPOS$ESTABLECIMIENTOS, opacity = 1)

pal <- colorNumeric("Oranges", NULL, n = 17) #n numero de variables a colorear

oranges<-leaflet(data = S_MPOS) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(as.numeric(S_MPOS$CVE_MUN)), 
              fillOpacity = 0.9, 
              color = "#BDBDC3",
              weight = 1, 
              popup = S_MPOS@data$NOM_MUN)%>%
  addLegend(pal = pal, values = ~as.numeric(S_MPOS$CVE_MUN), opacity = 1)
