###ABUNDANCIA DE FLORES POR ESPECIE
library(dplyr)
library(tidyr)
library(bipartite)

variables<- read.csv("Todas_variables.csv", sep=",", header=T)
flora <-read.csv("data/clean/cuad_flora_21_clean.csv")
flora_subset<- flora[!is.na(flora$Flores),] 
#eliminar planta 0
flora_subset<- subset(flora_subset, flora_subset$Planta != "0")
flora_subset1 <-flora_subset %>%
  group_by(Periodo,Bosque,Planta)%>%
  summarise(abundancia=sum(Flores)) %>%
  mutate(Año=2021)%>%relocate(Año, .after=2)

flora20 <-read.csv("data/clean/cuad_flora_20_clean.csv")
#eliminar planta 0
flora_subset20<- subset(flora20, flora20$Planta != "0")
flora_subset20.1 <-flora_subset20%>%
  group_by(periodo,Bosque,Planta)%>%
  summarise(abundancia=sum(flores))%>%
  mutate(Año=2020)%>%relocate(Año, .after=2)%>%
  rename(Periodo=periodo)

abundancia.sp.Planta<- rbind(flora_subset20.1,flora_subset1)
abundancia.sp.Planta<- abundancia.sp.Planta%>%
  rename(Periodo_fecha=Periodo)
abundancia.sp.Planta[is.na(abundancia.sp.Planta)] <- 0

abundancia.sp.Planta1 <- abundancia.sp.Planta%>%
  group_by(Periodo_fecha,Bosque,Año)%>%
  summarise(abundancia=sum(abundancia))
  
variables_todas <- left_join(variables,abundancia.sp.Planta, by=c("Año","Bosque","Periodo_fecha","Planta"))

##añadir abundancia por especie a todas las variables
variables_todas <- left_join(variables_todas,abundancia.sp.Planta1, by=c("Año","Bosque","Periodo_fecha"))

### MODELOAK AZKENAK
library(lme4)
variables_t<- variables_todas[!is.na(variables_todas$revisita_binario),]
variables_t<- variables_t%>% group_by(Periodo_fecha, Bosque, Polinizador,Año,Planta) %>%
  dplyr:: mutate(revisitaPlanta=mean(revisita_binario))

variables_t <- unite(variables_t, col="Periodofecha_Año", c("Periodo_fecha", "Año"), sep="_")
#reescalar variables
library(scales)

App <- rescale(variables_t$App)
Comp.pol<- rescale(variables_t$Comp.pol)
moran.I<- rescale(variables_t$moran.I)
plant_richness <- rescale(variables_t$plant_richness)
pol_richness <- rescale(variables_t$pol_richness)
shannon<- rescale(variables_t$shannon)
shannon.pol<- rescale(variables_t$shannon.pol)
abundancia<- rescale(variables_t$abundancia)
abundanciaTotal<- rescale(variables_t$abundanciaTotal)

data_norm <- data.frame(App, Comp.pol,moran.I,shannon,
                       shannon.pol,plant_richness,pol_richness,abundancia, abundanciaTotal)

data_norm$Periodofecha_Año <- variables_t$Periodofecha_Año
data_norm$Bosque <- variables_t$Bosque
data_norm$Polinizador <- variables_t$Polinizador
data_norm$Planta <- variables_t$Planta
data_norm$revisita_binario <- variables_t$revisita_binario
data_norm$revisitaPlanta<- variables_t$revisitaPlanta
data_norm$Codigo_vuelo <- variables_t$Codigo_vuelo
data_norm$Periodo_hora<- variables_t$Periodo_hora
data_norm$n_plantsp<- variables_t$n_plantsp


##NIVEL COMUNIDAD
m8<-glmer(revisita_binario ~ plant_richness+ pol_richness + App + abundanciaTotal +(1|Periodofecha_Año)+ (1|Bosque) + (1|Polinizador),  family="binomial",data=data_norm)
summary(m8)
car::Anova(m8)
m9<-glmer(revisita_binario ~ shannon+ shannon.pol + App  +(1|Periodofecha_Año)+ (1|Bosque) + (1|Polinizador),  family="binomial",data=data_norm)
summary(m9)
car::Anova(m9)
AIC(m8,m9)#m9
##NIVEL ESPECIE
library(glmmTMB)
m5<-glmer(revisitaPlanta ~ shannon+ shannon.pol + moran.I+ Comp.pol + abundancia +(1|Periodofecha_Año)+ (1|Bosque) + (1|Polinizador),  family="binomial",data=data_norm)
summary(m5)
car::Anova(m5)

m4<-glmer(revisitaPlanta ~ pol_richness+ plant_richness + moran.I+ Comp.pol + abundancia +(1|Periodofecha_Año)+ (1|Bosque) + (1|Polinizador),  family="binomial",data=data_norm)
summary(m4)
car::Anova(m4)###hau hoberen

m6<-glmer(revisitaPlanta ~ shannon+ shannon.pol + moran.I+ Comp.pol  +(1|Periodofecha_Año)+ (1|Bosque) + (1|Polinizador),  family="binomial",data=data_norm)
summary(m6)
car::Anova(m6)
m7<-glmer(revisitaPlanta ~ plant_richness+ pol_richness + moran.I+ Comp.pol + abundancia+(1|Periodofecha_Año)+ (1|Bosque) + (1|Polinizador),  family="beta",data=data_norm)
summary(m7)
car::Anova(m7)
AIC(m5,m4) #m4

###cantidad plantas dif visitadas
m1<-lmer(n_plantsp ~ Periodo_fecha + plant_richness + Año+ (1|Bosque) + (1|Polinizador), data=seg_totales)

m7<-glmer(n_plantsp ~ plant_richness+ pol_richness + abundanciaTotal+ App +(1|Periodofecha_Año)+ (1|Bosque) + (1|Polinizador),  family="poisson",data=data_norm)
summary(m7)


#####PARA SACAR DF DE RECURSOS FLORALES PARA USO/DISPONIBILIDAD
##2020
#una fila por censo
floral_resources.20 <- spread(flora_subset20.1, Planta, abundancia)
#NAs por 0s
floral_resources.20[is.na(floral_resources.20)]<-0
floral_resources.20<- floral_resources.20%>%mutate(Año=2020)%>%
  relocate(Año, .after=2)
##2021
#una fila por censo
floral_resources <- spread(flora_subset1, Planta, abundancia)
#NAs por 0s
floral_resources[is.na(floral_resources)]<-0
floral_resources<- floral_resources%>%mutate(Año=2021)%>%
  relocate(Año, .after=2)
##Recursos florales totales, Disponibilidad de recursos.
floral_res_tot <- rbind(floral_resources,floral_resources.20)


###web de visitas--> matriz con plantas como filas y polinizadores como columnas
###2021 --> web,web1,web2 --> periodo 1,2,3
install.packages("adehabitatHR")
install.packages("adehabitatHS")
library(adehabitatHR)
library(adehabitatHS)

flora21.1 <-subset(floral_resources, floral_resources$Periodo=="1")

jacobs_index <- widesI(web, flora21.1)
