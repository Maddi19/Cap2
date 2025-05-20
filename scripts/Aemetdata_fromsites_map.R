####AEMET DATA
install.packages("climaemet")
library(climaemet)
install.packages("geosphere")
library(geosphere)

browseURL("https://opendata.aemet.es/centrodedescargas/obtencionAPIKey")
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYWRkaS5hcnRhbWVuZGlAZWh1LmV1cyIsImp0aSI6IjUwZWE3MDlkLTg3YjMtNGE2Zi04MzY0LTU2OTk3OGQ0OWM1MyIsImlzcyI6IkFFTUVUIiwiaWF0IjoxNzQ3MTI0ODIzLCJ1c2VySWQiOiI1MGVhNzA5ZC04N2IzLTRhNmYtODM2NC01Njk5NzhkNDljNTMiLCJyb2xlIjoiIn0.mIaWkb8xsoG6WvAbSdTadFmZ1psuZtm6JykmOkWbBuE",
              install=TRUE, overwrite = TRUE)


stations <- aemet_stations()
knitr::kable(head(stations))
# 2. Coordenadas aproximadas de Doñana
ref_donana <- c(lon = -6.487, lat = 36.986)

# 3. Calcular distancia de cada estación al punto de Doñana
stations_dist <- stations %>%
  filter(!is.na(latitud) & !is.na(longitud)) %>%
  mutate(dist_m = distHaversine(
    p1 = cbind(longitud, latitud),
    p2 = matrix(ref_donana, nrow = n(), ncol = 2, byrow = TRUE)
  ))

est_donana <- stations_dist %>%
  arrange(dist_m) %>%
  slice(1)

# 5. Seleccionar la estación de Orozko
orozko <- stations %>%
  filter(nombre == "OROZKO, IBARRA")

# 6. Extraer códigos indicativos
id_donana <- est_donana$indicativo
id_orozko <- orozko$indicativo

message("Estación elegida para Doñana: ", est_donana$nombre,
        " (a ", round(est_donana$dist_m), " m del punto)")

library(lubridate)
resumir_periodo <- function(station_id, year, start_month, end_month) {
  start_date <- as.Date(sprintf("%d-%02d-01", year, start_month))
  end_date   <- as.Date(sprintf("%d-%02d-01", year, end_month)) %m+% months(1) - days(1)
  
  datos <- aemet_daily_clim(
    station = station_id,
    start   = as.character(start_date),
    end     = as.character(end_date)
  )
  
  datos %>%
    summarise(
      precipitacion_total = sum(prec,    na.rm = TRUE),
      temperatura_media   = mean(tmed,   na.rm = TRUE),
      tmed_min            = min(tmed,    na.rm = TRUE),
      tmed_max            = max(tmed,    na.rm = TRUE),
      tmin_min            = min(tmin,    na.rm = TRUE),
      tmax_max            = max(tmax,    na.rm = TRUE)
    ) %>%
    mutate(
      rango_temperatura  = tmax_max - tmin_min,
      rango_tmed         = tmed_max - tmed_min,
      estacion           = station_id,
      año                = year,
      inicio_extraccion  = start_date,
      fin_extraccion     = end_date
    )
}

# 8. Preparar combinaciones para 2020–2022
años <- 2020:2022
parametros <- bind_rows(
  tibble(estacion   = id_donana, año = años, start_month = 2, end_month = 5),
  tibble(estacion   = id_orozko, año = años, start_month = 3, end_month = 6)
)

# 9. Ejecutar descarga y resumen
resultados <- parametros %>%
  rowwise() %>%
  do(resumir_periodo(.$estacion, .$año, .$start_month, .$end_month)) %>%
  ungroup()

# 10. Añadir nombre legible y ordenar columnas
resultados <- resultados %>%
  left_join(select(stations, indicativo, nombre),
            by = c("estacion" = "indicativo")) %>%
  select(
    nombre, año, inicio_extraccion, fin_extraccion,
    precipitacion_total, temperatura_media,
    tmed_min, tmed_max, rango_tmed,
    tmin_min, tmax_max, rango_temperatura
  )

# 11. Imprimir tabla completa en consola con kable (formato pandoc)
options(width = 200)  # ampliar ancho de consola
cat(knitr::kable(
  resultados,
  format    = "pandoc",
  col.names = c(
    "Estación", "Año", "Inicio ext.", "Fin ext.",
    "Prec. total", "Tmed media",
    "Tmed mín", "Tmed máx", "Rango Tmed",
    "Tmin mín", "Tmax máx", "Rango Tmin/Tmax"
  ),
  digits = c(NA, 0, NA, NA, 1, 1, 1, 1, 1, 1, 1, 1)
), sep = "\n")
#####INFO SACADA DE TEMPERATURA  Y PRECIPITACION MEDIA

#####2022
data_daily <- aemet_daily_clim(station=id_orozko,
                               start = "2022-04-01",
                               end = "2022-06-30"
)

knitr::kable(head(data_daily))

temp_media <- mean(data_daily$tmed, na.rm = TRUE)
temp_minima <- min(data_daily$tmin, na.rm = TRUE)
temp_maxima <- max(data_daily$tmax, na.rm = TRUE)


indice_max <- which.max(data_daily$tmax)
indice_min <- which.min(data_daily$tmin)

# Fecha y temperatura máxima
fecha_max <- data_daily$fecha[indice_max]
temp_maxima <- data_daily$tmax[indice_max]

# Fecha y temperatura mínima
fecha_min <- data_daily$fecha[indice_min]
temp_minima <- data_daily$tmin[indice_min]


install.packages(c("ggplot2", "gganimate", "sf", "dplyr", "gifski"))
library(ggplot2)
library(gganimate)
library(sf)
library(dplyr)
library(gifski)

data_daily$fecha <- as.Date(data_daily$fecha)

data_daily <- data_daily %>%
  mutate(longitud = orozko$longitud, latitud = orozko$latitud)



# Datos de Doñana
donana <- data.frame(
  fecha = seq(as.Date("2022-04-01"), as.Date("2022-06-30"), by = "1 day"),
  tmed = runif(91, min = 10, max = 35), # Temperaturas medias simuladas
  estacion = "Doñana",
  longitud = -6.487,  # Coordenada aproximada de Doñana
  latitud = 36.986
)

# Combinar los datos de las dos estaciones
estaciones <- bind_rows(est_orozko, donana)

# Convertir a formato espacial
estaciones_sf <- st_as_sf(estaciones, coords = c("longitud", "latitud"), crs = 4326)


install.packages("rnaturalearthdata")

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Descargar datos del mapa base
mapa <- ne_countries(scale = "medium", country = "Spain", returnclass = "sf")


mapa_animado <- ggplot() +
  geom_sf(data = mapa, fill = "grey90", color = "white") +
  geom_sf(data = estaciones_sf, aes(color = tmed), size = 3) +
  scale_colour_gradientn(
    colours = hcl.colors(10, "RdBu", rev = TRUE),
    breaks = c(-5, 0, 5, 10, 15, 20,25,30,35,40),
    guide = "legend"
  ) +
  theme_minimal() +
  labs(
    title = "Temperatura Media en OROZKO y DOÑANA - {frame_time}",
    subtitle = "Datos diarios de abril a junio de 2022",
    color = "Temp. Media (°C)",
    x = "Longitud", y = "Latitud"
  ) +
  transition_time(fecha) +
  ease_aes("linear")


animacion <- animate(
  mapa_animado,
  nframes = 91,  
  fps = 3,      
  width = 800, height = 600
)
# Guardar el GIF
anim_save("mapa_orozko_temperatura.gif", animation = animacion)



## Abrir el GIF en tu navegador (si usas RStudio)
browseURL("mapa_orozko_temperatura.gif")


mapa_sites <- ggplot() +
  geom_sf(data = mapa, fill = "#EDEDED", color = "white") +
  geom_sf(data = mapa, fill = NA, color = "black", linewidth = 0.2) +
  geom_sf(data = estaciones_sf, color = "#275982", size = 5) +
  theme_minimal() +
  labs(
    title = "Study areas: Gorbea (N) and Doñana (S)",
    x = "Longitud", y = "Latitud"
  ) +
  coord_sf(
    xlim = c(-10, 5),   # Límites de longitud (aproximados para la Península Ibérica)
    ylim = c(35, 44)    # Límites de latitud
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(color = "grey80", linewidth= 0.2)
  )
ggsave("Figs/mapa_sites.png", mapa_sites, dpi=300, width=10, height = 6, bg= "white") 

###MAPA ELEVACIÓN

install.packages("ggspatial")
library(ggspatial)
install.packages("prettymapr")
library(prettymapr)

library(ggplot2)
install.packages("ggmap")
library(ggmap)
library(sf)
library(tmap)


library(terra)
install.packages("raster")
library(raster)
library(patchwork)


register_stadiamaps(key = "d2f4a2c1-45d2-459c-8bca-8ac5835a9b7c")


mapa_sites

bbox_gorbea <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -3.12, ymin = 42.95, xmax = -2.65, ymax = 43.15), crs = 4326)))
bbox_donana <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -6.8, ymin = 36.99, xmax = -6, ymax = 37.4), crs = 4326)))

library(elevatr)
d <- get_elev_raster(locations = bbox_gorbea, z = 11, clip = "locations")
rast_d <- terra::rast(d)
coord <- read.csv("gis/ptos_trans_Fichas_wgs84.csv")  # Asegúrate de que las columnas son "x_geo" y "y_geo"
tr.sf <- st_as_sf(coord, coords = c("x_geo", "y_geo"), crs = 4326)
tr.sf <- st_transform(tr.sf, crs = crs(rast_d))

print(st_coordinates(tr.sf))

install.packages("cols4all")
library(cols4all)
cols4all::c4a_palettes()

bbox_gorbea_zoom <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -3.02, ymin = 43.0, xmax = -2.7, ymax = 43.13), crs = 4326)))
d <- get_elev_raster(locations = bbox_gorbea_zoom, z = 11, clip = "locations")
rast_d <- terra::rast(d)


zoom_nivel <- 2 
library(tmap)

map <- tm_shape(rast_d) +
  # 1) Raster con paleta y cortes fijos
  tm_raster(
    palette  = "carto.earth",
    style    = "fixed",
    breaks   = breaks_personalizados,
    midpoint = NA,
    title    = "Elevation (m)"
  ) +
  # 2) Puntos de muestreo
  tm_shape(tr.sf) +
  tm_symbols(size = 0.7, col = "darkred", fill.legend = tm_legend(title = "Sampling points")) +  # Añadir los puntos
  tm_compass(type = "8star", position = c("right", "bottom"), size=2) +             # Añadir una brújula
  tm_scalebar(position = c("left", "bottom"))+
  tm_layout(inner.margins = c(0.05, 0.02, 0.01, 0.02),
            frame = FALSE)

print(map)

map <- map + tm_view(zoom = zoom_nivel)



do <- get_elev_raster(locations = bbox_donana, z = 9, clip = "locations")
rast_do <- terra::rast(do)
gpx_file <- "gis/puntos-donana.gpx"
gpx_data <- st_read(gpx_file, 
                    layer = c("waypoints", "routes", "tracks"))

# Verificar las capas disponibles
print(names(gpx_data))
gpx_sf <- st_as_sf(gpx_data)
print(names(gpx_sf))
library(dplyr)
gpx_resumido <- gpx_sf %>%
  group_by(name) %>%  # Reemplaza "name" con el nombre real de tu columna
  dplyr::summarise(geometry = st_union(geometry)) %>% # Combina geometrías en una sola
  st_centroid()

coordenadas_redondeadas <- st_coordinates(gpx_sf) %>% round(2)
gpx_sf_redondeado <- st_sf(
  geometry = st_sfc(
    st_multipoint(coordenadas_redondeadas), # Convertir la matriz a multipoint
    crs = st_crs(gpx_sf)  # Usar el mismo CRS que el objeto original
  ) %>% st_cast("POINT"),
  data = gpx_sf # Añadir los datos originales
)

gpx_sitios_unicos <- gpx_sf_redondeado %>%
  distinct(geometry, .keep_all = TRUE)


limites_elevacion <- c(-200, 1400)
breaks_personalizados <- c(0, 50, 100, 200, 400, 700, 1000, 1300, 1600)

map.do <- tm_shape(rast_do) +
  tm_raster(col.scale = tm_scale(values = "carto.earth", 
                                 breaks = breaks_personalizados,
                                 midpoint = NA),
            col.legend = tm_legend(title = "Elevation (m)"))+
  tm_shape(gpx_sitios_unicos) +
  tm_symbols(size = 0.7, col = "darkred", fill.legend = tm_legend(title = "Sampling points")) +  # Añadir los puntos
  tm_compass(type = "8star", position = c("right", "bottom"), size=2) +             # Añadir una brújula
  tm_scalebar(position = c("left", "bottom")) +
  tm_layout(inner.margins = c(0.05, 0.02, 0.01, 0.02),
            frame = FALSE)

library(cowplot)
map_go.leyenda <- map 

# 2. Extraer la leyenda como un grob
leyenda.g <- tmap_grob(map_go.leyenda + tm_layout(legend.only = TRUE))

map_do_sin_leyenda <- map.do + tm_layout(legend.show = FALSE)
map_go_sin_leyenda <- map + tm_layout(legend.show = FALSE)

map_do_ggplot <- tmap_grob(map_do_sin_leyenda)
map_go_ggplot <- tmap_grob(map_go_sin_leyenda )


columna_elevacion <- plot_grid(map_go_ggplot, map_do_ggplot, ncol = 1)
elev <- plot_grid(columna_elevacion, NULL, leyenda.g, ncol=3, nrow=1,rel_widths = c(1.4, 1.5, 1.2) )
diseno <- plot_grid(mapa_sites,NULL, elev, nrow = 1, rel_widths = c(1,0.25, 1), rel_heights = c(1,1,1))


ggsave2("Figs/Map.png", diseno, width=10, height = 4, bg= "white", dpi=300)

# Descargar mapas en zoom
map_donana <- get_stadiamap(bbox = bbox_donana, zoom = 10, maptype = "stamen_terrain_background")
map_gorbea <- get_stadiamap(bbox= bbox_gorbea, zoom = 10, maptype = "stamen_terrain")

install.packages("geodata")
library(geodata)
# Obtener datos de altitud
alt_donana <- geodata::getData("SRTM", lon = -6.5, lat = 37)
alt_gorbea <- getData("SRTM", lon = -2.8, lat = 43)

# Convertir a dataframe
df_donana <- as.data.frame(rasterToPoints(alt_donana))
df_gorbea <- as.data.frame(rasterToPoints(alt_gorbea))

# Mapa de detalle Doñana con altitud
mapa_donana <- ggmap(map_donana) +
  geom_raster(data = , aes(x = x, y = y, fill = layer), alpha = 0.5) +
  scale_fill_viridis_c(name = "Altitude (m)") +
  labs(title = "Doñana: Elevation") +
  theme_minimal()


coord <- read.csv("gis/ptos_trans_Fichas_wgs84.csv")
tr.sf <- st_as_sf(coord, coords = c("x_geo", "y_geo"), crs = 4326)
tr.sf_coords <- st_coordinates(tr.sf)
tr.sf <- cbind(tr.sf, tr.sf_coords)
st_geometry(tr.sf) <- "geometry"

mapa_gorbea <- ggmap(map_gorbea) +
  geom_point(data = tr.sf, aes(x = X, y = Y), size = 3, color = "red") +  # Usa las coordenadas X y Y extraídas
  labs(title = "Gorbea: Study Sites") +
  theme_minimal()



# Amplía 1000 metros en todas direcciones
library(maptiles)
library(terra)
library(tmap)
library(sf)


gc() 
bmap <- get_tiles(tr.sf_expanded, provider = "Esri.WorldShadedRelief", zoom = 15, crop = TRUE)
tr.geo_coords <- st_coordinates(tr.sf)
tiles <- get_tiles(tr.sf, provider = "Esri.WorldShadedRelief", zoom = 20, crop = TRUE)
writeRaster(tiles, "local_map.tif", overwrite = TRUE)

# Leerlos desde el archivo guardado
bmap <- raster("local_map.tif")
crs_bmap <- crs(bmap)
tr.sf <- st_transform(tr.sf, crs =crs_bmap)
tr.sf_expanded <- st_buffer(tr.sf, dist = 500)


tr.anim <- tm_shape(bmap) +
  tm_rgb() +  # Mostrar la imagen satelital en su forma RGB
  tm_shape(tr.sf_expanded) +  # Capa de los puntos de muestreo
  tm_symbols(size = 1, col = "red")+
  tm_credits(get_credit("Esri.WorldImagery"), size = 0.4, position = c(0.02, 0)) +
  tm_compass(type = "8star", position = c(0.05, 0.15), size = 1.5, text.color = "white") +
  tm_scalebar(position = c(0.2, 0.04), breaks = c(0, 0.5, 1), text.size = 0.6, text.color = "white")


print(bmap)






ggsave("Figs/mapa_terrain.png", mapa_sites, dpi=300, width=10, height = 6, bg= "white") 
