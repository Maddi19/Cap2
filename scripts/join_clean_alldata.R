##preparar consola
rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,dplyr,ourr,iNEXT,wesandersom,ggplot2,bipartite)
installed.packages("maxnodf")
library(maxnodf)
library(vegan)
library(bipartite)
source("./scripts/toolbox.R")

d<-read.csv("./data/clean/trans_donana_20_clean.csv")
d.21<-read.csv("./data/clean/trans_doñana_21_clean.csv")
d.gorb<-read.csv("./data/clean/trans_gorbea_20_clean.csv")
d.21.gorb<-read.csv("./data/clean/trans_G21_clean.csv")
d.22.gorb<-read.csv("./data/clean/trans_gorbea_22_clean.csv")
glimpse(d)
glimpse(d.21)
glimpse(d.gorb)
glimpse(d.22.gorb)
glimpse(d.21.gorb)

unique(d$Pollinator_id)
unique_sorted <- d %>%
  distinct(Pollinator_id) %>%  
  arrange(Pollinator_id)

d$Pollinator_id <- recode(d$Pollinator_id ,"Nomada  sp"= "Nomada sp",
                        "Panurgus  calcaratus"="Panurgus calcaratus",
                        "Xylocopa  cantabrita"="Xylocopa cantabrita")

write.csv(d,"data/doñana_2020_nwclean.csv")
d.21$capturado <- str_replace(d.21$capturado, "idem ", "")

d.21[d.21 == " "] <- NA
pollinator_info <- d.21 %>%
  filter(!is.na(Pollinator_id) & Pollinator_id != " ") %>%
  select(Codigo, Pollinator_id, Pollinator_genus, Pollinator_family, Order)
pollinator_info[pollinator_info == ""] <- NA

info_complete <- pollinator_info %>%
  filter(!is.na(Pollinator_id) & !is.na(Pollinator_genus) & !is.na(Pollinator_family) & !is.na(Order)) %>%
  select(Codigo, Pollinator_id, Pollinator_genus, Pollinator_family, Order)

# 3. Actualizar columnas en d.21 usando match
d.21$Pollinator_id <- ifelse(
  is.na(d.21$Pollinator_id),
  info_complete$Pollinator_id[match(d.21$capturado, info_complete$Codigo)],
  d.21$Pollinator_id
)

d.21$Pollinator_genus <- ifelse(
  is.na(d.21$Pollinator_genus),
  info_complete$Pollinator_genus[match(d.21$capturado, info_complete$Codigo)],
  d.21$Pollinator_genus
)

d.21$Pollinator_family <- ifelse(
  is.na(d.21$Pollinator_family),
  info_complete$Pollinator_family[match(d.21$capturado, info_complete$Codigo)],
  d.21$Pollinator_family
)

d.21$Order <- ifelse(
  is.na(d.21$Order),
  info_complete$Order[match(d.21$capturado, info_complete$Codigo)],
  d.21$Order
)

d.21 <- d.21 %>%
  mutate(
    Pollinator_id = case_when(
      (is.na(Pollinator_id) | Pollinator_id == "") & !is.na(Pollinator_genus) & Pollinator_genus != "" ~ paste(trimws(Pollinator_genus), "sp."),  # Si Pollinator_id está vacío y Pollinator_genus no está vacío
      TRUE ~ Pollinator_id  # Mantener el valor original si Pollinator_id no está vacío
    )
  )

d.21 <- d.21 %>%
  mutate(
    Pollinator_genus = if_else(capturado == "AT1-02", "Lasioglossum", Pollinator_genus),
    Pollinator_family = if_else(capturado == "AT1-02", "Halictidae", Pollinator_family),
    Pollinator_id = case_when(
      Codigo == "AT1-02" | capturado == "AT1-02" ~ "Lasioglossum immunitum",
      Codigo == "HT6-16" | capturado == "HT6-16" ~ "Andrena allaudi",
      Codigo == "HT8-02" | capturado == "HT8-02" ~ "Lobonix aeneus",
      Codigo == "PC1-01" | capturado == "PC1-01" ~ "Empis tesellata",
      Codigo == "VET2-01" | capturado == "VET2-01" ~ "Lasioglossum immunitum",
      Codigo == "VET4-01" | capturado == "VET4-01" ~ "Lasioglossum immunitum",
      Codigo == "VEC4-04" | capturado == "VEC4-04" ~ "Lasioglossum immunitum",
      Codigo == "VEC5-06" | capturado == "VEC5-06" ~ "Eucera elongatula",
      Codigo == "VEC5-08" | capturado == "VEC5-08" ~ "Eucera elongatula",
      Codigo == "VET9-01" | capturado == "VET9-01" ~ "Anthrenus verbasci",
      Codigo == "VST2-03" | capturado == "VST2-03" ~ "Lasioglossum immunitum",
      Codigo == "VST4-02" | capturado == "VST4-02" ~ "Lobonix aeneus",
      Codigo == "VST4-04" | capturado == "VST4-04" ~ "Episyrphus balteatus",
      Codigo == "VST5-02" | capturado == "VST5-02" ~ "Eupeodes corollae",
      Codigo == "AT3-09" | capturado == "AT3-09" ~ "Eupeodes corollae",
      Codigo == "PT2-01" | capturado == "PT2-01" ~ "Lobonix aeneus",
      Codigo == "PT2-07" | capturado == "PT2-07" ~ "Rhyncomyia cuprea",
      Codigo == "PT3-01" | capturado == "PT3-01" ~ "Lobonix aeneus",
      Codigo == "PT7-09" | capturado == "PT7-09" ~ "Panurgus perezi",
      Pollinator_id == "Empis " ~ "Empis sp",
      TRUE ~ Pollinator_id  # Mantener el valor original si no coincide
    ),
    Pollinator_sps = if_else(capturado == "AT1-02", "immunitum", Pollinator_sps),
    Order = if_else(str_starts(Pollinator_id, "Andrena") |
                      str_starts(Pollinator_id, "Eucera") |
                      str_starts(Pollinator_id, "Lasioglossum")|
                      str_starts(Pollinator_id, "Panurgus" ),
                    "Hymenoptera", 
                    if_else(str_starts(Pollinator_id, "Empis") |
                              str_starts(Pollinator_id, "Episyrphus") |
                              str_starts(Pollinator_id,  "Eupeodes" )|
                              str_starts(Pollinator_id, "Rhyncomyia"),
                            "Diptera", 
                            if_else(str_starts(Pollinator_id, "Lobonix") |
                                      str_starts(Pollinator_id, "Anthrenus"), 
                                    "Coleoptera",        
                                    Order)
                    )))

unique(d.21$Pollinator_id)
d.21<- d.21 %>%
  filter(!is.na(Pollinator_id))

write.csv(d.21,"data/doñana_2021_nwclean.csv")

d.22.gorb <- d.22.gorb %>%
  filter(!is.na(Planta) & Planta != "")

d.22.gorb<-d.22.gorb%>%
  rename(Periodo=Ronda,
         Pollinator_id=Polinizador_curro,
         Transecto = Transecto..1.2.3.,
         Fecha2=Fecha)%>%
  mutate(Year=first(year(Fecha2))) %>% 
  ungroup%>%
  mutate(Site="Gorbea")
d.22.gorb$Bosque<-as.character(d.22.gorb$Bosque)
d.22.gorb$Codigo_frasco<-as.character(d.22.gorb$Codigo_frasco)
d.22.gorb <- d.22.gorb %>%
  mutate(Pollinator_id = if_else(!is.na(Codigo_frasco) & Pollinator_id == "", Codigo_frasco, Pollinator_id))
d.22.gorb <- d.22.gorb%>%
  mutate(Pollinator_id = str_replace(Pollinator_id, "^\\?=", ""))
d.22.gorb$Pollinator_id  <- recode(d.22.gorb$Pollinator_id ,
                        "1"= "Anthomyia sp",
                        "3"= "Anthomyia sp",
                        "9"= "Anthomyia sp",
                        "11"= "Calliphora vicina",
                        "16"= "Eupeodes luniger",
                        "21"= "Empis sp",
                        "31"= "Dasyrhamphis ater",
                        "32"= "Empis sp",
                        "38"= "Meliscaeva sp",
                        "50"= "Sphex sp",
                        "65"= "Tipula sp",
                        "69"= "Diptera",
                        "70"= "Melanomya sp",
                        "74"= "Systoechus sp",
                        "83"= "Melanomya sp",
                        "92"= "Diptera",
                        "111"= "Diptera",
                        "124"= "Diptera",
                        " Syrphus sp."= "Syrphus sp.",
                        "Bombus sp" ="Bombus sp.",
                        "Bombus terrestris reina" ="Bombus terrestris")

unique_sorted <- d.22.gorb %>%
  distinct(Pollinator_id) %>%  
  arrange(Pollinator_id)
d.22.gorb <- d.22.gorb %>%
  mutate(Pollinator_id = if_else(Planta == "Scilla verna" & (is.na(Pollinator_id) | Pollinator_id == ""), 
                                 "Lepidoptera", 
                                 Pollinator_id))


write.csv(d.22.gorb,"data/gorbea_2022_nwclean.csv")

d.21.gorb<-d.21.gorb%>%
  rename(Pollinator_id=Polinizador_curro)%>%
  mutate(Year=first(year(Fecha2))) %>% 
  ungroup%>%
  mutate(Site="Gorbea")
d.21.gorb$Bosque<-as.character(d.21.gorb$Bosque)
unique_sorted <- d.21.gorb %>%
  distinct(Pollinator_id) %>%  
  arrange(Pollinator_id)
write.csv(d.21.gorb,"data/gorbea_2021_nwclean.csv")

unique(d.gorb$Pollinator_id)

d.gorb<-d.gorb%>%
  mutate(Year=first(year(Fecha2))) %>% 
  ungroup%>%
  mutate(Site="Gorbea")
d.gorb$Bosque<-as.character(d.gorb$Bosque)
write.csv(d.gorb,"data/gorbea_2020_nwclean.csv")

d.21<-d.21%>%
  mutate(Year=first(year(Fecha2))) %>% 
  ungroup%>%
  mutate(Site="Doñana")
write.csv(d.21,"data/doñana_2021_nwclean.csv")

d<-d%>%
  mutate(Year=first(year(Fecha2))) %>% 
  ungroup%>%
  mutate(Site="Doñana")
write.csv(d,"data/doñana_2020_nwclean.csv")

###group all data and select columns
list_dataframes <- list(d, d.21, d.gorb, d.21.gorb, d.22.gorb)

all_df<-map_df(list_dataframes, ~select(., any_of(c("X", "Site","Year", "Fecha2", "Hora","Bosque",
                                                    "Periodo","Transecto","Planta","Pollinator_id", "Order",
                                                    "Pollinator_genus","Pollinator_family"))))


all_df$Bosque <- recode(all_df$Bosque,"Pinar Villamanrique Este (Chaparral)"= "Villamanrique Chaparral",
                        "Pinar Villamanrique Sur"="Villamanrique Sur")

unique(all_df$Pollinator_id)
all_df$Pollinator_id <- recode(all_df$Pollinator_id,"Xylocopa  cantabrita"= "Xylocopa cantabrita",
                               "Bombylius  torquatus"="Bombylius torquatus",
                               "Eucera  elongatula"="Eucera elongatula",
                               "Andrena  cinerea"="Andrena cinerea",
                               "Apis melifera"="Apis mellifera",
                               "Andrena  flavipes"="Andrena flavipes",
                               "Andrena  chrysosceles"="Andrena chrysosceles",
                               "Nomada agrestis"="Nomada agrestis",
                               "Empis  sp"="Empis sp",
                               "Osmia  bicornis"="Osmia bicornis",
                               "Calliphora  sp"="Calliphora sp",
                               "Empis  tesellata"="Empis tesellata",
                               "Chasmatopterus  illigeri"="Chasmatopterus illigeri",
                               "Parageron  sp"="Parageron sp",
                               "Merodon  sp"="Merodon sp",
                               "Nomada  sp"="Nomada sp",
                               " Syrphus sp."= "Syrphus sp.",
                               "Eristalis "="Eristalis sp",
                               "Eristalis sp"="Eristalis sp.",
                               "Empis "="Empis sp",
                               "Lasioglossum  prasinum"="Lasioglossum prasinum",
                               "avispa"="Vespidae",
                               "Usia  sp"="Usia sp",
                               "Dasypoda  iberica"="Dasypoda iberica",
                               "Panurgus  calcaratus"="Panurgus calcaratus",
                               "Tropinota  squalida"="Tropinota squalida",
                               "Syrphidae 2"="Syrphidae",
                               "Syrphidae 3"="Syrphidae",
                               "Bombylidae 2"="Bombylidae",
                               "Anthophora  retusa"="Anthophora retusa",
                               "Nomada  agrestis"="Nomada agrestis",
                               "Lasioglossum"="Lasioglossum sp",
                               "Lasioglossum "="Lasioglossum sp",
                               "Osmia  submicans"="Osmia submicans",
                               "Machimus  sp"="Machimus sp",
                               "Panurgus  dentipes"="Panurgus dentipes",
                               "Episyrphus "="Episyrphus sp",
                               "Dasypoda "="Dasypoda sp",
                               "Syrphidae NA"="Syrphidae",
                               "Formicidae NA"="Formicidae",
                               "Lycaenidae NA"="Lycaenidae",
                               "mosca peque\x96a"="Diptera",
                               "Syrphidae 1"="Syrphidae",
                               "Anthophora"="Anthophora sp",
                               "Dilophus  sp"="Dilophus sp",
                               "Eucera"="Eucera sp",
                               "Platycheirus  albimanus"="Platycheirus albimanus",
                               "polilla marron mediana"="Lepidoptera",
                               "Cerambycidae NA"="Cerambycidae",
                               "Pyrgus sp"="Pyrgus sp.",
                               "Bombus sp"="Bombus sp.",
                               "Bombylius sp"="Bombylius sp.",
                               "Eupeodes sp"="Eupeodes sp.",
                               "Eucera "="Eucera sp.")


all_df <- all_df %>%
  filter(Planta != "")

all_df <- all_df %>%
  mutate(Order = case_when(
    Pollinator_family %in% c("Apidae", "Megachilidae", "avispa", "abeja", "Andrenidae", "Halictidae", "Avispa", "Melittidae", 
                             "Abeja", "Formicidae", "Anthopora", "Colletidae", "Vespidae", "Argidae", 
                             "Pompiilidae") ~ "Hymenoptera",
    Pollinator_family %in% c("Esfingidae", "Lepidoptera", "Zygaenidae", "Nymphalidae-Satyrinae", "Hesperiidae", 
                             "Lycaenidae", "Pieridae") ~ "Lepidoptera",
    Pollinator_family %in% c("Pyrrhocoridae", "Miridae") ~ "Hemiptera",
    Pollinator_family %in% c("Orthoptera") ~ "Orthoptera",
    Pollinator_family%in% c("Diptera", "Tabanidae", "mosca", "Calliphoridae", "Conopidae", "Asilidae", 
                            "Stratiomyidae", "Tachinidae", "Muscidae", "Bombyliidae", "Bombylidae", 
                            "Empididae", "Bibionidae", "Mosca", "Syrphidae") ~ "Diptera",
    Pollinator_family %in% c("Oedemeridae", "Coleoptera", "Cerambycidae", "Cerambicidae", "Cetonidae", 
                             "Chrysomelidae", "Buprestidae", "Tenebrionidae", "Melyridae", 
                             "Scarabaeidae") ~ "Coleoptera",
    Pollinator_genus %in% c("Cerambycidae","Curculionoidea")~ "Coleoptera",
    Pollinator_genus %in% c("Anthocharis")~ "Lepidoptera",
    Pollinator_id %in% c("Syrphus sp.")~ "Diptera",
    TRUE ~ Order  # Para casos que no coincidan con ninguna familia listada
  ))

unknown_order <- all_df %>% 
  filter(is.na(Order)| Order=="")
unique(unknown_fams$Pollinator_id)

all_df <- all_df %>%
  mutate(Order = if_else(str_starts(Pollinator_id, "Andrena") |
                           str_starts(Pollinator_id, "Eucera") |
                           str_starts(Pollinator_id, "Lasioglossum")|
                           str_starts(Pollinator_id, "Panurgus")|
                           str_starts(Pollinator_id, "Dasypoda") |
                           str_starts(Pollinator_id, "Anthophora") |
                           str_starts(Pollinator_id, "Hoplitis") |
                           str_starts(Pollinator_id, "Ceratina")|
                           str_starts(Pollinator_id, "Colletes")|
                           str_starts(Pollinator_id, "Sphex"),
                         "Hymenoptera", 
                         if_else(str_starts(Pollinator_id, "Empis") |
                                   str_starts(Pollinator_id, "Episyrphus") |
                                   str_starts(Pollinator_id,  "Eupeodes" )|
                                   str_starts(Pollinator_id, "Platycheirus")|
                                   str_starts(Pollinator_id, "Lomatia")|
                                   str_starts(Pollinator_id, "Rhyncomyia")|
                                   str_starts(Pollinator_id, "Eristalis") |
                                   str_starts(Pollinator_id, "Usia")|
                                   str_starts(Pollinator_id, "Bombylius")|
                                   str_starts(Pollinator_id, "Platynochaetus")|
                                   str_starts(Pollinator_id, "Eristalinus") |
                                   str_starts(Pollinator_id, "Sphaerophoria")|
                                  str_starts(Pollinator_id, "Anthomyia") |
                                  str_starts(Pollinator_id, "Calliphora") |
                                  str_starts(Pollinator_id, "Dasyrhamphis") |
                                    str_starts(Pollinator_id, "Meliscaeva")|
                                    str_starts(Pollinator_id, "Melanomya") |
                                    str_starts(Pollinator_id, "Systoechus")|
                                    str_starts(Pollinator_id, "Diptera")|
                                   str_starts(Pollinator_id, "Tipula"), 
                                 "Diptera", 
                                 if_else(str_starts(Pollinator_id, "Lobonix") |
                                           str_starts(Pollinator_id, "Anthrenus")|
                                           str_starts(Pollinator_id,"Chasmatopterus")|
                                           str_starts(Pollinator_id,"Exosoma")|
                                           str_starts(Pollinator_id,"Cerocoma")|
                                           str_starts(Pollinator_id,"Oxythyrea") , 
                                         "Coleoptera",  
                                  if_else(str_starts(Pollinator_id, "Lepidoptera"),
                                          "Lepidoptera",
                                         Order)))))

all_df <- all_df %>%
  mutate(Pollinator_family = if_else(
    str_starts(Pollinator_id, "Andrena") | str_starts(Pollinator_id, "Panurgus"), "Andrenidae",
    if_else(
      str_starts(Pollinator_id, "Eristalis") | str_starts(Pollinator_id, "Platycheirus") |
        str_starts(Pollinator_id, "Meliscaeva") |
        str_starts(Pollinator_id, "Episyrphus") | str_starts(Pollinator_id, "Eupeodes") |
        str_starts(Pollinator_id, "Platynochaetus") | str_starts(Pollinator_id, "Eristalinus") |
        str_starts(Pollinator_id, "Sphaerophoria") | str_starts(Pollinator_id, "Syrphus"), 
      "Syrphidae",
      if_else(
        str_starts(Pollinator_id, "Lasioglossum"), "Halictidae",
        if_else(
          str_starts(Pollinator_id, "Lomatia") | str_starts(Pollinator_id, "Usia") |
            str_starts(Pollinator_id, "Bombylius") | str_starts(Pollinator_id, "Systoechus"), "Bombyliidae",
          if_else(
            str_starts(Pollinator_id, "Eucera") | str_starts(Pollinator_id, "Ceratina") |
              str_starts(Pollinator_id, "Anthophora"), "Apidae",
            if_else(
              str_starts(Pollinator_id, "Dasypoda"), "Melittidae",
              if_else(
                str_starts(Pollinator_id, "Hoplitis"), "Megachilidae",
                if_else(
                  str_starts(Pollinator_id, "Colletes"), "Colletidae",
                  if_else(
                    str_starts(Pollinator_id, "Anthocharis"), "Pieridae",
                    if_else(str_starts(Pollinator_family, "mosca"), "Diptera",
                    if_else(str_starts(Pollinator_family, "avispa"), "Vespidae",
                    if_else(str_starts(Pollinator_family, "abeja"), "Hymenoptera",        
                    Pollinator_family
                  )))))))))))))

all_df <- all_df %>%
  mutate(Pollinator_family = if_else(
    is.na(Pollinator_family)|Pollinator_family=="", Order, 
    Pollinator_family))

unknown_order <- all_df %>% 
  filter(is.na(Order)| Order=="")

unknown_fam <- all_df %>% 
  filter(is.na(Pollinator_family)| Pollinator_family=="")
unique(unknown_fams$Pollinator_id)

unique(all_df$Pollinator_id)
all_df <- all_df %>%
  mutate(Pollinator_id = if_else(
    Pollinator_id == " 169" | str_starts(Pollinator_id, "coleopteros"),
    Pollinator_family,  # Asignar el valor de Pollinator_family a Pollinator_id
    Pollinator_id       # Mantener el valor original de Pollinator_id si no cumple las condiciones
  ))

all_df <- all_df %>%
  filter(!(Pollinator_id == "" | str_detect(Pollinator_id, "^\\d+$")))

gor<-all_df%>%
  filter(Site=="Gorbea")
unique(gor$Pollinator_id)
#168
doñ<-all_df%>%
  filter(Site=="Doñana")
unique(doñ$Pollinator_id)
#151
unique(all_df$Pollinator_family)

total_visits <- nrow(gor)
unknown_order <- gor %>% 
  filter(is.na(Order)| Order=="")


all_df <- bind_rows(doñ, gor)
write.csv(all_df, "data/all_data.csv")
all_df<-read.csv("./data/all_data.csv")


# Calcular el número de visitas por cada orden y su porcentaje
gor<-all_df%>%
  filter(Site=="Gorbea")

doñ<-all_df%>%
  filter(Site=="Doñana")

visits_by_order <- gor %>%
  group_by(Order) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / total_visits) * 100)

total_visits.d <- nrow(doñ)
visits_by_order.d <- doñ %>%
  group_by(Order) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / total_visits.d) * 100)

apidae <- gor %>%
  group_by(Pollinator_family)%>%
  summarise(Count=n())%>%
  mutate(perc.ap =(Count / total_visits) * 100) 

apidae.d <- doñ %>%
  group_by(Pollinator_family)%>%
  summarise(Count=n())%>%
  mutate(perc.ap =(Count / total_visits.d) * 100) 

apis<- gor %>%
  group_by(Pollinator_id)%>%
  summarise(Count=n())%>%
  mutate(perc.ap =(Count / total_visits) * 100) 
apis.d<- doñ %>%
  group_by(Pollinator_id)%>%
  summarise(Count=n())%>%
  mutate(perc.ap =(Count / total_visits.d) * 100) 

bombus<- gor %>%
  filter(str_starts(Pollinator_id, "Bombus")) %>%  # Aplica str_starts a la columna Pollinator_id
  group_by(Pollinator_id) %>%
  summarise(Count = n()) %>%
  mutate(perc.ap = (Count / total_visits) * 100)%>%
  summarise(sum=sum(perc.ap))

bombus.d <- doñ %>%
  filter(str_starts(Pollinator_id, "Bombus")) %>%  # Aplica str_starts a la columna Pollinator_id
  group_by(Pollinator_id) %>%
  summarise(Count = n()) %>%
  mutate(perc.ap = (Count / total_visits.d) * 100)
