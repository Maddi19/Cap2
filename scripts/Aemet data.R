####AEMET DATA
install.packages("climaemet")
library(climaemet)

browseURL("https://opendata.aemet.es/centrodedescargas/obtencionAPIKey")
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYWRkaS5hcnRhbWVuZGlAZWh1LmV1cyIsImp0aSI6ImI4Njk4MGYwLTM2OGYtNDE5Yy1iYWRjLWFiMWYzZTNiZjIyMiIsImlzcyI6IkFFTUVUIiwiaWF0IjoxNzM0Njg0NTUzLCJ1c2VySWQiOiJiODY5ODBmMC0zNjhmLTQxOWMtYmFkYy1hYjFmM2UzYmYyMjIiLCJyb2xlIjoiIn0.blE0VrZLLimhnr-F54c7J7eOy0DHNLn_IA02Jqxs0mA",
              install=TRUE)


stations <- aemet_stations()
knitr::kable(head(stations))

orozko<- subset(stations, nombre %in% c("OROZKO, IBARRA"))
id_orozko <- orozko$indicativo

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
estaciones <- bind_rows(data_daily, donana)

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



# Abrir el GIF en tu navegador (si usas RStudio)
browseURL("mapa_orozko_temperatura.gif")
