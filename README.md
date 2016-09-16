# Mexico-Choropleth-map
Mexico Choropleth Map using R and Leaflet

```{r,echo = FALSE,include=FALSE}
library(rgdal)
library(leaflet)
library(maptools)
library(RColorBrewer)
library(networkD3)
library(geosphere)
library(ggplot2)
library(chorddiag)
library(highcharter)


S_MPOS <- readOGR(".", layer = "subset_mpos", encoding = "UTF-8")
DFS_MPOS<-S_MPOS@data
ESTABLECIMIENTOS<-c(9,17,4,10,8,16,3,0,7,0,12,3,17,22,20,10,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,
                    1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,18,0,0,16,5,0,0,2,0,0,0,0,0,0,7,0,0,0,0,8,0,0,0,9,0,0,0,0,0,0,0,0,0,0,0,0,0,26,0,0,0,0,0,4,0,0)

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


src <- c("A", "A", "A", "A", "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J", "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)
# Plot
snd3<-simpleNetwork(networkData)

####

establecimientos<-read.csv("ESTABLECIMIENOS_CDMX.csv")
pal <- colorNumeric("Dark2", NULL, n = 19) #n numero de variables a colorear

bubble<-leaflet(establecimientos) %>%
  addProviderTiles("CartoDB.DarkMatter")%>%
  addCircleMarkers(lng = ~longitude, 
             lat = ~latitude,
             weight = 1,
             radius= ~sqrt(OBS)*1/7,
             color = ~pal(COL_CADENA),
             popup = ~CADENA,
             group = ~CADENA,
             stroke = FALSE, fillOpacity = 0.45
             )%>% 
  # Layers control
  addLayersControl(
    overlayGroups = ~CADENA,
    options = layersControlOptions(collapsed = FALSE)
    )

##############################
##### CONECCIONES 

adli<-c(-99.167735,19.427035) #INVERTIR COORDENADAS
estabs_inv<- cbind(establecimientos[,2],establecimientos[,1]) # invertimos las otras coordenadas

inter1 <- as.data.frame(gcIntermediate(adli, estabs_inv[45,], n=100, addStartEnd=TRUE))
inter2 <- as.data.frame(gcIntermediate(adli, estabs_inv[13,], n=100, addStartEnd=TRUE))
inter3 <- as.data.frame(gcIntermediate(adli, estabs_inv[25,], n=100, addStartEnd=TRUE))
inter4 <- as.data.frame(gcIntermediate(adli, estabs_inv[5,], n=100, addStartEnd=TRUE))
inter5 <- as.data.frame(gcIntermediate(adli, estabs_inv[52,], n=100, addStartEnd=TRUE))
inter6 <- as.data.frame(gcIntermediate(adli, estabs_inv[34,], n=100, addStartEnd=TRUE))
inter7 <- as.data.frame(gcIntermediate(adli, estabs_inv[31,], n=100, addStartEnd=TRUE))
inter8 <- as.data.frame(gcIntermediate(adli, estabs_inv[32,], n=100, addStartEnd=TRUE))
inter9 <- as.data.frame(gcIntermediate(adli, estabs_inv[35,], n=100, addStartEnd=TRUE))
inter10 <- as.data.frame(gcIntermediate(adli, estabs_inv[36,], n=100, addStartEnd=TRUE))

##########################

CONNECTIONS<-leaflet() %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolylines(data = inter1, lng = ~lon, lat = ~lat,
             fill = F, 
             weight = 2, 
             color = "#FFFFCC")%>%
  addPolylines(data = inter2, lng = ~lon, lat = ~lat,
               fill = F, 
               weight = 2, 
               color = "#FFFFCC")%>%
  addPolylines(data = inter3, lng = ~lon, lat = ~lat,
               fill = F, 
               weight = 2, 
               color = "#FFFFCC")%>%
  addPolylines(data = inter4, lng = ~lon, lat = ~lat,
               fill = F, 
               weight = 2, 
               color = "#FFFFCC")%>%
  addPolylines(data = inter5, lng = ~lon, lat = ~lat,
               fill = F, 
               weight = 2, 
               color = "#FFFFCC")%>%
  addPolylines(data = inter6, lng = ~lon, lat = ~lat,
               fill = F, 
               weight = 2, 
               color = "#FFFFCC")%>%
  addPolylines(data = inter7, lng = ~lon, lat = ~lat,
               fill = F, 
               weight = 2, 
               color = "#FFFFCC")%>%
  addPolylines(data = inter8, lng = ~lon, lat = ~lat,
               fill = F, 
               weight = 2, 
               color = "#FFFFCC")%>%
  addPolylines(data = inter9, lng = ~lon, lat = ~lat,
               fill = F, 
               weight = 2, 
               color = "#FFFFCC")%>%
  addPolylines(data = inter10, lng = ~lon, lat = ~lat,
               fill = F, 
               weight = 2, 
               color = "#FFFFCC")
###################
rds <- readRDS("ARROZ.Rds")


p<-ggplot(rds,aes(x=factor(YEARMONTH),
                 y=PRECIO_PROMEDIO,
                 col=ESPECIFICACION))
bx<-geom_boxplot(fill="white",
               #outlier.colour = NA, 
               position = position_dodge(width=0.9))
jt<-geom_point(position=position_jitterdodge(dodge.width=0.9), 
             size=2,
             shape=21,
             aes(alpha=.3,fill=PRECIO_PROMEDIO))
#######
### https://github.com/mattflor/chorddiag
m <- matrix(c(11975,  5871, 8916, 2868,
              1951, 10048, 2060, 6171,
              8010, 16145, 8090, 8045,
              1013,   990,  940, 6907),
            byrow = TRUE,
            nrow = 4, ncol = 4)
haircolors <- c("negro", "rubio", "cafe", "rojo")
dimnames(m) <- list(have = haircolors,
                    prefer = haircolors)
groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223")
#######
### https://www.santoshsrinivas.com/bullet-graphs-using-r/
library(d3Dashboard)

ytd2005 <- list(  
  title=list("Revenue", "Profit", "Order Size", "New Customers", "Satisfaction"),
  subtitle=list("US$, in thousands", "%", "US$, average", "count", "out of 5"),
  range=list(c(150, 225, 300),
             c(20, 25, 30),
             c(350, 500, 600),
             c(1400, 2000, 2500),
             c(3.5, 4.25, 5)),
  measures=list(c(220, 270),
                c(21, 23),
                c(100, 320),
                c(1000, 1650),
                c(3.2, 4.7)),
  markers=list(250, 26, 550, 2100, 4.2)
)

##### RADARS

SRS<-read.csv("SelloR_SUMMARY.csv")

#saveRDS(SRS,"SRS.rds")
#SRS<-readRDS("SRS.rds")


Rad<-highchart() %>% 
  hc_chart(polar = TRUE, type = "area") %>% #type= "colum","line","area"
  hc_title(text = "CIUDAD DE MEXICO") %>%
  #hc_pane(size= '85%' )%>%
  hc_xAxis(categories =SRS$CADENA_FOLIO ,
           tickmarkPlacement = 'on',
           lineWidth = 0) %>% 
  hc_yAxis(gridLineInterpolation = 'polygon',
           lineWidth = 0,
           min = 0) %>% 
  hc_series( 
    list(name ="ALP2M_CLASICA_EP_DESC_C1LT_AZUL",data = as.numeric(SRS$ALP2M_CLASICA_EP_DESC_C1LT_AZUL),pointPlacement = 'on'),
    list(name ="ALP2M_LT_EP_DESC_C1LT",data = as.numeric(SRS$ALP2M_LT_EP_DESC_C1LT),pointPlacement = 'on'),
    list(name ="ALP2M_S_COLEST_CON_GRASA_VEGETAL_C1LT",data = as.numeric(SRS$ALP2M_S_COLEST_CON_GRASA_VEGETAL_C1LT),pointPlacement = 'on'),
    list(name ="ALP2M_SEMI_EP_DESC_C1LT",data = as.numeric(SRS$ALP2M_SEMI_EP_DESC_C1LT),pointPlacement = 'on'),
    list(name ="ALP_CLASICA_ENT_ENVASE_1LT",data = as.numeric(SRS$ALP_CLASICA_ENT_ENVASE_1LT),pointPlacement = 'on'),
    list(name ="ALP_SEMI_EP_DESC_ENVASE_1LT",data = as.numeric(SRS$ALP_SEMI_EP_DESC_ENVASE_1LT),pointPlacement = 'on'),
    list(name ="LALA_ENT_C1LT",data = as.numeric(SRS$LALA_ENT_C1LT),pointPlacement = 'on'),
    list(name ="LALA_LT_EP_DESC_B_EN_GRASA_C1LT",data = as.numeric(SRS$LALA_LT_EP_DESC_B_EN_GRASA_C1LT),pointPlacement = 'on'),
    list(name ="LALA_LT_EP_DESC_C1LT",data = as.numeric(SRS$LALA_LT_EP_DESC_C1LT),pointPlacement = 'on'),
    list(name ="LALA_SEMI_C1LT_SEMIDESC",data = as.numeric(SRS$LALA_SEMI_C1LT_SEMIDESC),pointPlacement = 'on'),
    list(name ="LALA_SIL_PLUS_0por_DESC_C1LT",data = as.numeric(SRS$LALA_SIL_PLUS_0por_DESC_C1LT),pointPlacement = 'on')
  ) 


```
