library(readr)
setwd("C:/Users/OK/Documents/Tesia/GorBEE_data/GorBEEa_data-main/Cuad")
seg_totales <- read_csv("seg_totales.csv")

library(dplyr)
library(tidyr)
pol.richness <- seg_totales%>%
  group_by(Periodo_fecha,Bosque,Año)%>%
  summarise(pol_richness=n_distinct(Polinizador))

##abundancia polinizadores
pol.richness1 <-seg_totales %>%
  group_by(Periodo_fecha,Bosque,Año, Polinizador)%>%
  summarise(abundancia.pol=n())

shannon.pol <- spread(pol.richness1, Polinizador, abundancia.pol)
#NAs por 0s
shannon.pol[is.na(shannon.pol)]<-0
##calcula automaticamente teniendo en cuenta cantidad de sp y su abundancia,
## hay que quitar columnas 1 y 2 que no son plantas
library(vegan)
shannon.index <- diversity(shannon.pol[-c(1:3)])

#creamos cuatro columnas con periodo, bosque, año y los indices shannon
key_shannon <-cbind(Periodo_fecha=shannon$Periodo_fecha, Bosque=shannon$Bosque,
                    Año=shannon$Año,shannon.pol=shannon.index)

seg_pol <- merge(seg_totales,pol.richness, by=c("Periodo_fecha","Bosque","Año"))
seg_pol<-merge(seg_pol, key_shannon, by=c("Periodo_fecha","Bosque","Año"))


##GLMM
seg_pol1<- unite(seg_pol, col="BosqueAño", c("Bosque", "Año"), sep="_")
seg_pol1$BosqueAño<-as.factor(seg_pol1$BosqueAño)
library(lme4)
str(seg_pol1)

m4<-glmer(n_plantsp ~ Periodo_fecha + pol_richness+ shannon.pol+ (1|Bosque)+ (1|Año) + (1|Polinizador),family="poisson",data=seg_pol)
summary(m4)
car::Anova(m4)
library(easystats)
model_dashboard(m4)

##fidelidad para todas las sp.

fid_tot<- seg_pol %>% group_by(Codigo_vuelo, Año) %>% 
  mutate(revisita_binario= ifelse(Planta == lag(Planta), 1, 0))
str(fid_tot)
fid_tot$Periodo_fecha<-as.factor(fid_tot$Periodo_fecha)
fid_tot$Bosque<-as.factor(fid_tot$Bosque)
fid_tot$Año<-as.factor(fid_tot$Año)

write.csv(fid_tot,"C:/Users/OK/Documents/Tesia/GorBEE_data/GorBEEa_data-main/Cuad/indices_tot.csv" )

fid_tot<- fid_tot[!is.na(fid_tot$revisita_binario),]

fid_tot1<- fid_tot %>% group_by(Periodo_fecha, Bosque, Polinizador, Año) %>%
  dplyr::summarise(revisita=mean(revisita_binario))

fid_tot.1 <- unite(fid_tot, col="BosqueAño", c("Bosque", "Año"), sep="_")
fid_tot.1$BosqueAño<-as.factor(fid_tot.1$BosqueAño)

m5<-glmer(revisita_binario ~ Periodo_fecha + pol_richness + shannon.pol + Año +(1|Bosque) + (1|Polinizador), family="binomial", data=fid_tot)
summary(m5)
car::Anova(m5)
model_dashboard(m5)
library(effects)
plot(allEffects(m5))

m6<-glmer(revisita_binario ~ Periodo_fecha + pol_richness + shannon.pol  +(1|BosqueAño) + (1|Polinizador), family="binomial", data=fid_tot.1)
car::Anova(m6)
model_dashboard(m6)

m7<-glmer(revisita_binario ~ Periodo_fecha + pol_richness + shannon.pol  +(1|Bosque) + (1|Año)+(1|Polinizador), family="binomial", data=fid_tot)
car::Anova(m7)
model_dashboard(m7)
##m5 el mejor ajustado --> efecto periodo, pol rich y año. shannon pol no.

m8<-glmer(revisita_binario ~ Periodo_fecha + pol_richness+ shannon.pol+plant_richness + shannon + Año +(1|Bosque) + (1|Polinizador), family="binomial", data=fid_tot)
car::Anova(m8)
model_dashboard(m8)

m0<-glmer(n_plantsp~ Periodo_fecha + pol_richness+ shannon.pol+plant_richness + shannon + Año +(1|Bosque) + (1|Polinizador), family="poisson", data=fid_tot)
car::Anova(m0)
m9<-glmer.nb(n_plantsp~ Periodo_fecha + pol_richness+ shannon.pol+plant_richness + shannon + Año +(1|Bosque) + (1|Polinizador),  data=fid_tot)
car::Anova(m9)

m1<-glmer(n_plantsp~  pol_richness+ shannon.pol+plant_richness + shannon + (1|Periodofecha_Año)+ (1|Bosque) + (1|Polinizador),  data=fid_tot.1)
library(car)
vif(m1)

seg_pol.1 <- unite(seg_pol, col="Periodofecha_Año", c("Periodo_fecha", "Año"), sep="_")
seg_pol.1$Periodofecha_Año<-as.factor(seg_pol.1$Periodofecha_Año)
fid_tot.1 <- unite(fid_tot, col="Periodofecha_Año", c("Periodo_fecha", "Año"), sep="_")
fid_tot.1$Periodofecha_Año<-as.factor(fid_tot.1$Periodofecha_Año)

m1<-glmer(n_plantsp~   shannon.pol+ shannon + (1|Periodofecha_Año)+ (1|Bosque) + (1|Polinizador), family="poisson", data=seg_pol.1)
model_dashboard(m1)
summary(m1)
car::Anova(m1)

m2<-glmer(n_plantsp~  plant_richness+ pol_richness + (1|Periodofecha_Año)+ (1|Bosque) + (1|Polinizador), family = "poisson", data=seg_pol.1)
model_dashboard(m2)
summary(m2)
car::Anova(m2)

AIC(m1,m2)

m8<-glmer(revisita_binario ~ shannon.pol+ shannon +(1|Periodofecha_Año)+ (1|Bosque) + (1|Polinizador),  family="binomial",data=fid_tot.1)
summary(m8)
car::Anova(m8)

m9<-glmer(revisita_binario ~ plant_richness+ pol_richness +(1|Periodofecha_Año)+ (1|Bosque) + (1|Polinizador),  family="binomial",data=fid_tot.1)
summary(m9)
car::Anova(m9)
AIC(m8,m9)

fid_subset20<- subset(seg_pol, Año %in% c("2020"))
fid_subset21<- subset(seg_pol, Año %in% c("2021"))

m3<-glmer(n_plantsp~  pol_richness + shannon.pol + plant_richness + shannon + (1|Periodo_fecha)+ (1|Bosque) + (1|Polinizador),  family="poisson", data=fid_subset20)
library(car)
vif(m1)

m1<-glmer(n_plantsp~   shannon.pol+ shannon + (1|Periodo_fecha)+ (1|Bosque) + (1|Polinizador), family="poisson", data=fid_subset20)
model_dashboard(m1)
summary(m1)
car::Anova(m1)

m2<-glmer(n_plantsp~  plant_richness+ pol_richness + (1|Periodo_fecha)+ (1|Bosque) + (1|Polinizador), family = "poisson" , data=fid_subset20)
model_dashboard(m2)
summary(m2)
car::Anova(m2)

AIC(m1,m2,m3) ##m2, efecto riqueza de plantas y polinizadores en el numero de sp plantas visitadas por vuelo

m4<-glmer(n_plantsp~  pol_richness + shannon.pol + plant_richness + shannon + (1|Periodo_fecha)+ (1|Bosque) + (1|Polinizador),  family="poisson", data=fid_subset21)
library(car)
vif(m1)

m5<-glmer(n_plantsp~   shannon.pol+ shannon + (1|Periodo_fecha)+ (1|Bosque) + (1|Polinizador), family="poisson", data=fid_subset21)
model_dashboard(m5)
summary(m5)
car::Anova(m5)

m6<-glmer(n_plantsp~  plant_richness+ pol_richness + (1|Periodo_fecha)+ (1|Bosque) + (1|Polinizador), family = "poisson" , data=fid_subset21)
model_dashboard(m6)
summary(m6)
car::Anova(m6)

AIC(m4,m5,m6) ##m5, no efecto

##fidelidad
fid_subset2020<- subset(fid_tot, Año %in% c("2020"))
fid_subset2021<- subset(fid_tot, Año %in% c("2021"))

m8<-glmer(revisita_binario ~ shannon.pol+ shannon +(1|Periodo_fecha)+ (1|Bosque) + (1|Polinizador),  family="binomial",data=fid_subset2020)
summary(m8)
car::Anova(m8)

m9<-glmer(revisita_binario ~ plant_richness+ pol_richness +(1|Periodo_fecha)+ (1|Bosque) + (1|Polinizador),  family="binomial",data=fid_subset2020)
summary(m9)
car::Anova(m9)
AIC(m8,m9)

m10<-glmer(revisita_binario ~ shannon.pol+ shannon +(1|Periodo_fecha)+ (1|Bosque) + (1|Polinizador),  family="binomial",data=fid_subset2021)
summary(m10)
car::Anova(m10)

m11<-glmer(revisita_binario ~ plant_richness+ pol_richness +(1|Periodo_fecha)+ (1|Bosque) + (1|Polinizador),  family="binomial",data=fid_subset2021)
summary(m11)
car::Anova(m11)
AIC(m10,m11)
