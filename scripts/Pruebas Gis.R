setwd("C:\\Users\\OK\\Documents\\Tesia\\Gorbea2020")
tr<-read.csv("trans_gorbea_20_clean.csv", sep=",", header=TRUE)
str(tr)

library(dplyr)
coord <- read.csv("GIS_wgs84\\ptos_trans_Fichas_wgs84.csv")
coord <- coord %>%
  rename(Bosque=id)


tr <- left_join(tr,coord, by="Bosque")

tr <- tr %>%
  group_by(Fecha,Bosque) %>%
  mutate(n_visitas_dia=n())

tr<- tr %>%
  select("Fecha", "Bosque", "x_geo", "y_geo", "n_visitas_dia", "Periodo")

library(sf)
##marcar coordenadas geograficas, junta columna x e y

tr.sf <- st_as_sf(tr, coords = c("x_geo", "y_geo"))
tr.sf

## CRS pone NA --> las coordenadas estan como WGS84, hay que decir
tr.geo <- st_set_crs(tr.sf, value= 4326)
tr.geo

## ahora si pone CRS WGS84
st_coordinates(tr.geo)

library(mapview)
mapview(tr.geo, zcol="n_visitas_dia", cex="n_visitas_dia")

library(leaflet)
library(rgdal)
library(RColorBrewer)

leaflet(tr.geo) %>% 
  addTiles()  %>% 
  setView(lat = -2.9, lng = 43.5, zoom=2) %>%
addCircleMarkers(lng = tr$x_geo,
                 lat = tr$y_geo,
                 radius = ~log(tr$n_visitas_dia) * 2.5,
                 weight = 1,
                 opacity = 1,
                 color = "red",
                 fillColor = "red",
                 fillOpacity = 0.8,
                 )
  

sliderInput("dateSel", "Date",
            min = min(tr$Fecha),
            max = max(tr$Fecha),
            value = min(tr$Fecha),
            step = 1,
            timeFormat = "%d %b %y",
            animate = animationOptions(interval = 500, loop = FALSE))


library(maptiles)
library(terra)
library(tmap)

class(tr$Fecha)
tr$Fecha<-as.Date(tr$Fecha, format="%y-%m-%d")
help(as.Date)

bmap <- get_tiles(tr.geo, provider = "Esri.WorldImagery", zoom = 15, crop = TRUE)
plot(bmap)

bmap <- get_tiles(tr.geo, provider = "Esri.WorldShadedRelief", zoom = 20, crop = TRUE)
plot(bmap)


tr.anim <- tm_shape(bmap) +
  tm_rgb(alpha = 0.7) +
  tm_shape(tr.geo) +
  tm_symbols(size = "n_visitas_dia", col="red", scale = 2.5, title.size = "Número de visitas 2020") +
  tm_layout(legend.position = c("right", "bottom"),
            inner.margins = c(0.03, 0.01, 0.02, 0.01)) +
  tm_credits(get_credit("Esri.WorldImagery"), size = 0.4, position = c(0.02, 0))+
  tm_facets(along="Periodo", free.coords=FALSE)

install.packages("gifski")
library(gifski)
tmap_animation(tr.anim, filename="tr.anim.gif", delay=120)
tmap_mode("view")
tr.anim

help(tm_symbols)
               
 
library(rayshader)
devtools::install_github("h-a-graham/rayvista", dependencies=TRUE)
library(rayvista)
devtools::install_github("tylermorganwall/rayrender")
library(rayrender)

### 3D intentando poner los puntos. no consigo
tr.3D <- plot_3d_vista(req_area = tr.geo, zscale=5, zoom=1.2,overlay_detail = 14, theta=-65,
                       windowsize=1000, phi=25)+  
             render_highquality(lightdirection = 220, clear=TRUE)+
  generate_point_overlay(tr.geo$geometry, color="red", size=12)

point_object <- generate_object(tr.geo$geometry, 
                                height = tr.geo$elev, 
                                color = "red", 
                                radius = 0.01)

# Overlay the point object on your 3D map
tr.3D <- add_object(tr.3D, point_object)

# Render the 3D plot with the added points
render_highquality(tr.3D, 
                   lightdirection = 220, 
                   clear = TRUE, 
                   windowsize = 1000)

geom1 <- sf::st_sfc(sf::st_point(c(-2.809897, 43.0781)))
geom2 <- sf::st_sfc(sf::st_point(c(-2.819285,  43.08122)))
multipoint_sfg <- st_multipoint(rbind(c(-2.809897, 43.0781),c(-2.819285,  43.08122)))
str(multipoint_sfg)
st_sfc(multipoint_sfg)

tr.3D <- plot_3d_vista(req_area = tr.geo)
geom <- tr.geo[4]
class(geom1)
geom1 <-st_sf(geom)

tr.3D <-tr.3D %>% plot_3d_vista(req_area = tr.geo, zscale=5, zoom=1.2,overlay_detail = 14, theta=-65,
                        windowsize=1000, phi=25)
render_points(lat=43.0781, long = -2.809897, color="red", size="3", zscale=50)
render_snapshot()
  
generate_point_overlay(geometry= geom1, attr(tr.3D, "extent"), color="red", size=12 )

  
  
###mapa ggplot animado
  library(gganimate)
  library(ggplot2)
  install.packages("ggiraph")
  library(ggiraph)
  
  help(borders)
tr.map <- map_data(tr$Bosque)
trmap <- ggplot() +
  borders(tr, xlim = x_geo, ylim= y_geo)+
  geom_sf(colour="blue", aes(size=n_visitas_dia))+
  theme_minimal()+
  transition_time(Fecha)

  animationOfYears <- function(tr) {
    
    p <- ggplot(data, aes(x=ABUNDANCE,
                          y=GENUS_SPECIES,
                          color = YEAR))+
      geom_point() +
      scale_color_distiller(palette = "Dark2") +
      labs(x = "Abundancia",
           y = "Especies",
           title = "Abundancia ~ Especies",
           subtitle =  "Evolución de la abundancia con los años",
           color = "Años")
    
    return(p + transition_time(YEAR) +
             labs(title = "Año: {frame_time}")+
             shadow_wake(wake_length = 0.1, alpha = FALSE))
    
  }


## para visitas en cuadrados

  library(dplyr)
  coord <- read.csv("GIS_wgs84\\ptos_trans_Fichas_wgs84.csv")
  coord <- coord %>%
    rename(Bosque=id)

visitas <- read.csv("C:/Users/OK/Documents/Tesia/GorBEE_data/GorBEEa_data-main/Cuad/seg_totales.csv", sep = ",", header=T)

vis<- left_join(visitas,coord, by="Bosque")

vis <- vis %>%
  group_by(Bosque) %>%
  mutate(n_visitas_año=n())

vis<- vis %>%
  select("Año", "Bosque", "x_geo", "y_geo", "n_visitas_año")

library(sf)
##marcar coordenadas geograficas, junta columna x e y
vis.sf <- st_as_sf(vis, coords = c("x_geo", "y_geo"))
vis.sf

## CRS pone NA --> las coordenadas estan como WGS84, hay que decir
vis.geo <- st_set_crs(vis.sf, value= 4326)
vis.geo

## ahora si pone CRS WGS84
st_coordinates(vis.geo)

library(mapview)
mapview(vis.geo)

library(leaflet)
library(rgdal)
library(RColorBrewer)

leaflet(vis.geo) %>% 
  addTiles()  %>% 
  setView(lat = -2.9, lng = 43.5, zoom=2) %>%
  addCircleMarkers(lng = vis$x_geo,
                   lat = vis$y_geo,
                   radius = ~log(vis$n_visitas_año) * 0.5,
                   weight = 1,
                   opacity = 1,
                   color = "red",
                   fillColor = "red",
                   fillOpacity = 0.8,
  )

st_coordinates(vis.geo)

library(maptiles)
library(terra)
library(tmap)
bmap <- get_tiles(vis.geo, provider = "Esri.WorldImagery", zoom = 15, crop = TRUE)
plot(bmap)

bmap <- get_tiles(vis.geo, provider = "Esri.WorldShadedRelief", zoom = 20, crop = TRUE)
plot(bmap)

tr.anim <- tm_shape(bmap) +
  tm_rgb(alpha = 1) +
  tm_shape(vis.geo) +
  tm_symbols(size = "n_visitas_año", col="red", scale = 1.8, title.size = "Bisita kopurua", sizes.legend = c(500,1500,2500)) +
  tm_layout(title="2020 eta 2021ko udaberrian behaturiko\npolinizatzaileen bisitak",title.size = 1.5, title.fontface = "bold", title.color = "white",
            title.position=c(0.05,0.9), legend.position = c("right", "bottom"), legend.text.size=0.8, legend.text.color = "white", legend.title.color = "white",
            legend.title.size=1.5, legend.title.fontface = "bold", 
            legend.text.fontface = "bold",
            legend.width = 0.35, legend.height = 0.3,  inner.margins = c(0.03, 0.01, 0.02, 0.01))+
  tm_credits(get_credit("Esri.WorldImagery"), size = 0.4, position = c(0.02, 0))+
  tm_compass(type="8star", position=c(0.05, 0.15), size=1.5, text.color = "white")+
  tm_scale_bar( position=c(0.2,0.04), breaks =c(0,0.5,1),text.size = 0.6, text.color = "white")

tr.anim


###mapa EAE

install.packages("mapSpain", dependencies = TRUE)
library(mapSpain)
library(tidyverse)
install.packages("tidyterra")
library("tidyterra")
esp_set_cache_dir("~/R/mapslib/mapSpain", install = TRUE, verbose = TRUE)

library(MetBrewer)
library(ggsci)
library(RColorBrewer)
library(ggthemes)
pv <- esp_get_ccaa(ccaa="País Vasco")
ggplot(pv) + geom_sf() + geom_sf(data=vis.geo$geometry, colour="#800000FF")+
  labs(title="Gorbeako Parke Naturaleko laginketa puntuak")+
  scale_fill_discrete(whitebox.colors(3))+
  theme_map()
