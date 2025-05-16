rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,here, ggpubr, dplyr,ggplot2,ggsci,Polychrome, RColorBrewer,ggpubr)


all_df<-read.csv(here("data","useful", "all_data.csv"))


plantas_visitas <- all_df %>%
  group_by(Planta, Site) %>%
  summarise(visitas = n(), .groups = "drop") %>%
  arrange(desc(visitas))

especies_seleccionadas <- c("Lotus corniculatus", "Helleborus viridis subsp. occidentalis", "Helianthemum nummularium",
                            "Vicia pyrenaica", "Lathyrus linifolius", "Pedicularis sylvatica subsp. sylvatica",
                            "Scilla verna",  "Cardamine pratensis", "Hepatica nobilis", "Erysimum gorbeanum", 
                            "Halimiun calycinum","Cistus salviifolius", "Cistus ladanifer", "Cistus crispus","Halimiun halimifolium",
                            "Cistus libanotis", "Bellis perennis","Rosmarinus officinalis","Taraxacum sp.",
                            "Potentilla erecta", "Ranunculus sp.", "Lavandula stoechas", "Leontodon hispidus",
                            "Ranunculus repens", "Halimium halimifolium", "Leontodon longirostris", "Echium plantagineum",
                            "Halimium calycinum", "Medicago lupulina", "Trifolium repens", "Erica cinerea",
                            "Thymus praecox subsp. polytrichus", "Veronica officinalis", "Andryala arenaria","Scorzonera humilis",
                            "Andryala integrifolia", "Trifolium pratense", "Thymus praecox")


datos_rangos <- all_df %>%
  filter(Planta %in% especies_seleccionadas) %>%
  group_by(Planta, Site) %>%
  summarise(inicio = min(Periodo), fin = max(Periodo))


all_df$Fecha2 <- as.Date(all_df$Fecha2, format = "%Y-%m-%d")
rosmarinus_df <- all_df %>%
  filter(Planta == "Rosmarinus officinalis" & Site == "Doñana" & year(Fecha2) == 2020)
fecha_inicio <- min(rosmarinus_df$Fecha2)
fecha_fin <- max(rosmarinus_df$Fecha2)
fecha_inicio
fecha_fin
dias_diferencia <- as.numeric(fecha_fin - fecha_inicio)

all_df <- all_df %>%
  mutate(
    Fecha2 = case_when(
      Site == "Doñana" & Year == 2020 & year(Fecha2) > 2020 ~ update(Fecha2, year = 2020), 
      TRUE ~ Fecha2  
    )
  )


datos_floracion <- all_df %>%
  group_by(Site, Year) %>%
  mutate(inicio = min(Fecha2),
            fin = max(Fecha2),
            floracion = as.numeric((fin - inicio)+1)) %>%
  ungroup()

datos_floracion <- datos_floracion %>%
  group_by(Planta, Site, Year) %>%
  mutate(inicio = min(Fecha2),
            fin = max(Fecha2),
            duracion_floracion = as.numeric((fin - inicio)+1))

datos_floracion <- datos_floracion %>%
  mutate(Planta = ifelse(Planta == " Thymus praecox subsp. polytrichus", "Thymus praecox subsp. polytrichus", Planta))




datos_pielou <- datos_floracion %>%
  group_by(Site, Year, Planta) %>%
  mutate(prop_floracion = duracion_floracion / floracion)

datos_pielou <- datos_pielou %>%
  group_by(Site, Year, Planta) %>%   
  mutate(H = -sum(prop_floracion * log(prop_floracion), na.rm = TRUE)) %>%
  ungroup()

datos_pielou <- datos_pielou%>%
  group_by(Site, Year) %>%
  mutate(S = n_distinct(Planta))

datos_pielou<- datos_pielou %>%
  mutate(J = H / log(S))
write.csv(datos_pielou, "data/useful/datos_floracion.csv")



length(unique(datos_rangos$Planta))
colors<-get_palette("jama",55)


presence_plot <- ggplot(datos_rangos %>% filter(Planta %in% especies_seleccionadas),
                aes(y = Planta)) +
                geom_linerange(aes(xmin = inicio, xmax = fin, color = Planta), size = 4) +
                scale_color_manual(values = colors) +
                labs(x = "Sampling period", y = "Species") +
                facet_wrap(~ Site, scales = "free_y")+
                theme_minimal() +
                theme(
                   axis.text.y = element_text(size = 10),
                   axis.title.x = element_text(size = 12),
                   legend.position = "none"
                 )+
                 scale_x_continuous(breaks = 1:9) 


all_df <- all_df %>%
  mutate(Pollinator_id = recode(Pollinator_id, "Platycheirus  sp" = "Platycheirus sp"))

all_df$Periodo <- as.numeric(all_df$Periodo)

datos_ordenados <- all_df %>%
  group_by(Pollinator_id, Site, Periodo) %>%
  summarise(
    abundancia = n(),  # Número de observaciones (abundancia) por especie y periodo
    .groups = "drop"  # Evitar advertencias de agrupamiento
  ) %>%
  arrange(Site, Pollinator_id, Periodo)  # Ordenar por sitio, especie y periodo

datos_ordenados_top20 <- datos_ordenados %>%
  group_by(Site, Pollinator_id) %>%
  summarise(
    presencia_total = n_distinct(Periodo),  # Cuántos períodos tiene presencia la especie
    .groups = "drop"
  ) %>%
  group_by(Site) %>%
  top_n(20, presencia_total) %>%  # Seleccionar las 20 especies con más presencia
  ungroup()

# Filtrar los datos para incluir solo las 20 especies seleccionadas por sitio
datos_filtrados <- datos_ordenados %>%
  semi_join(datos_ordenados_top20, by = c("Site", "Pollinator_id"))

datos_rep <- datos_filtrados %>%
  uncount(weights = abundancia)

colors<-get_palette("jama",45)


violin_pol <- ggplot(datos_rep, aes(x = Periodo, y = Pollinator_id, fill = Pollinator_id)) +
  geom_violin(color = NA, bw = 0.35) +  # Gráfico de violín con borde negro, la anchura se ajusta automáticamente
  scale_fill_manual(values = colors) +  # Usar la paleta de colores especificada (jama de ggsci)
  labs(x = "Sampling period", y = "Species", fill = "Polinizador") +  # Etiquetas
  facet_wrap(~ Site, scales = "free_y") +  # Un gráfico para cada Site
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    legend.text = element_blank(),
    legend.position = "none"
  )+
  scale_x_continuous(breaks = 1:9) 

ggsave(violin_pol, file="Figs/violin_pol.jpg", dpi=300, width=12, height = 7)
save(presence_plot, violin_pol, file="scripts/presence_plot.RData")
