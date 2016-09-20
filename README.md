# Mexico-Choropleth-map
Mexico Choropleth Map using R and Leaflet

libraries 
```{r}
library(rgdal)
library(leaflet)
library(maptools)
library(RColorBrewer)
```
We have to read the shp files. All data was taken from the INEGI website (Data is available, [here](https://github.com/edroga/Mexico-Choropleth-map/blob/master/mexico_shp.rar))
```{r}
S_MPOS <- readOGR(".", layer = "subset_mposPOP", encoding = "UTF-8")
```
once the file is read, then we asigne a palette for the 141 entities
```{r}
pal <- colorNumeric("YlOrRd", NULL, n = 141) #n numero de variables a colorear
```

finally leaflet is gonna take care of the Choropleth map
```{r}
leaflet(data = S_MPOS) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(S_MPOS$POBLACION), 
              fillOpacity = 0.9, 
              color = "#BDBDC3",
              weight = 1, 
              popup = S_MPOS@data$NOM_MUN)%>%
  addLegend(pal = pal, values = ~S_MPOS$POBLACION, opacity = 1)
```
you can click on any coloured region and the name of it will be displayed.

#![alt tag](https://github.com/edroga/Mexico-Choropleth-map/blob/master/choropleth.png)
