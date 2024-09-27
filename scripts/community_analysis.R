######COMMUNITY ANALYSIS
rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 
pacman::p_load(tidyverse,dplyr, lme4, carData,effects, easystats, lmertTest,
               performance,see,gridExtra,car, lattice,ggplot2,bipartite,
               glmmTMB)



sitems<-read.csv("data/sitems_meanfs.csv")

#FRUITset
sitems.fs <- sitems %>%
  filter(!is.na(mean.fs)& !is.nan(mean.fs))
sitems.fs <- sitems.fs %>%
  mutate(mean.fs = if_else(mean.fs == 1, 0.9999, mean.fs))


m1<-lm( mean.fs ~  poll.sp + total.visits + Anidamiento + comp.fun.pol + Year + Periodo , data=sitems.fs)
summary(m1)

vif(m1)


m2<-lm( mean.fs ~  poll.sp  + Anidamiento + comp.fun.pol + Year + Periodo , data=sitems.fs)

summary(m2)
vif(m2)


m3<-lm( mean.fs ~   poll.sp + total.visits + Anidamiento + Year + Periodo , data=sitems.fs)

summary(m3)
vif(m3)

str(sitems.fs)
#sitems.fs$Periodo<-as.factor(sitems.fs$Periodo)
sitems.fs$Bosque<-as.factor(sitems.fs$Bosque)
sitems.fs$Year<-as.factor(sitems.fs$Year)


m4<-lmerTest::lmer( mean.fs ~  poll.sp + plant.sp + Anidamiento + comp.fun.pol +  Periodo + (1|Site_id) + (1|Year), data=sitems.fs)
summary(m4)


m5<-lmerTest::lmer( mean.fs ~  poll.sp + plant.sp + Anidamiento + total.visits +  Periodo + (1|Site_id) + (1|Year), data=sitems.fs)
summary(m5)

m6<-glmmTMB( mean.fs ~  poll.sp + plant.sp + Anidamiento + total.visits +  Periodo + (1|Year) + (1|Site_id) , family = "beta_family", data=sitems.fs)
summary(m6)


#separar gorbea y do

str(sitems.fs)

sitems.fs.d<-sitems.fs%>%
  filter(sitems.fs$Site_id=="Doñana")
sitems.fs.g<-subset(sitems.fs, sitems.fs$Site_id=="Gorbea")


sitems.fs.d$Year<-as.factor(sitems.fs.d$Year)

m7<-glmmTMB(mean.fs ~  poll.sp + plant.sp + Anidamiento + total.visits  + Periodo , family = "beta_family", data=sitems.fs.d)
summary(m7)

m8<-glmmTMB( mean.fs ~  poll.sp + plant.sp + Anidamiento + total.visits  + (1|Bosque) , family = "beta_family", data=sitems.fs.d)
summary(m8)

m9<-glmmTMB( mean.fs ~  poll.sp + plant.sp + Anidamiento + comp.fun.pol+  Periodo , family = "beta_family", data=sitems.fs.d)
summary(m9)

m9.b<-glmmTMB( mean.fs ~  poll.sp + plant.sp + Periodo *(Anidamiento + comp.fun.pol) , family = "beta_family", data=sitems.fs.d)
summary(m9.b)

library(AICcmodavg)
AIC(m7, m8, m9, m9.b)
##m7 eta m9

m10<-glmmTMB( mean.fs ~  poll.sp + plant.sp + Anidamiento + total.visits +  Periodo + (1|Year) , family = "beta_family", data=sitems.fs.g)
summary(m10)

m11<-glmmTMB( mean.fs ~  poll.sp + plant.sp + Anidamiento + comp.fun.pol+  Periodo + (1|Year) , family = "beta_family", data=sitems.fs.g)
summary(m11)
AIC(m10, m11)



## TABLA RED: HAGO FACTOR LO QUE DEDE SER FACTOR
# (SITIO, RECORRIDO Y PERIODO).
sitems$Site_id<-as.factor(sitems$Site_id)
sitems$Year<-as.factor(sitems$Year)
sitems$Bosque<-as.factor(sitems$Bosque)
sitems$Periodo<-as.factor(sitems$Periodo)

# HISTOGRAMAS DE MIS VARIABLES.
hist(sitems$poll.sp)
hist(sitems$plant.sp)
hist(sitems$Anidamiento)
hist(sitems$connectance)
hist(sitems$asymmetry)
hist(sitems$robustness.pol)
hist(sitems$robustness.pl)
hist(sitems$comp.fun.pol)
hist(sitems$comp.fun.pl)

####subset doñana and gorbea
sitems.d<-sitems%>%
  filter(sitems$Site_id=="Doñana")
sitems.g<-subset(sitems, sitems$Site_id=="Gorbea")

sitems.g$Year <- as.factor(sitems.g$Year)
sitems.g$Periodo <- as.factor(sitems.g$Periodo)

# MODELO 1
m1.g<-lmer(poll.sp ~ Periodo + Year + (1|Bosque),  data=sitems.g)
# COMPRUEBO NORMALIDAD RESIDUOS.
install.packages("insight")
install.packages("performance")
library(performance)
check_model(m1.g)
summary(m1.g)
# SIGUEN DISTRIBUCION NORMAL.
anova(m1.g)
# GRAFICO RESULTADOS
library(effects)
allEffects(m1.g)
plot(allEffects(m1.g))

sitems.d$Year <- as.factor(sitems.d$Year)
m1.d<-lmer(poll.sp ~ Periodo  + Year+ (1|Bosque),  data=sitems.d)
# COMPRUEBO NORMALIDAD RESIDUOS.
performance::check_model(m1.d)
# SIGUEN DISTRIBUCION NORMAL.
anova(m1)
# GRAFICO RESULTADOS
allEffects(m1.d)
plot(allEffects(m1.d))

install.packages("visreg")
library(visreg)
visreg::visreg(m1.g)

install.packages("easystats")
library(easystats)
model_dashboard(m1.g)

hist(residuals(m1.g))
qqnorm(resid(m1.g))
qqline(residuals(m1.g))


install.packages("report")
library(report)
report::report(m1.g) 

# MODELO 2
m2.g<-lmer(plant.sp ~ Year + Periodo + (1|Bosque), data=sitems.g)
# COMPRUEBO NORMALIDAD RESIDUOS
check_model(m2.g)
# SIGUEN DISTRIBUCION NORMAL
Anova(m2.g)
# GRAFICO RESULTADOS
allEffects(m2.g)
plot(allEffects(m2.g))
model_dashboard(m2.g)

m2.d<-lmer(plant.sp ~ Year + Periodo + (1|Bosque), data=sitems.d)
# COMPRUEBO NORMALIDAD RESIDUOS
check_model(m2.d)
# SIGUEN DISTRIBUCION NORMAL
Anova(m2.d)
# GRAFICO RESULTADOS
allEffects(m2.d)
plot(allEffects(m2.d))

# MODELO 3 - ANIDAMIENTO
m3.g<-lmer(Anidamiento ~ Year + Periodo + (1|Bosque), data = sitems.g)
# COMPRUEBO NORMALIDAD RESIDUOS
check_model(m3.g)
report(m3.g)
model_dashboard(m3.g)
library(lattice)
qqmath(m3.g)
allEffects(m3.g)
plot(allEffects(m3.g))
summary(m3.g)

m3.d<-lmer(Anidamiento ~ Year + Periodo + (1|Bosque), data = sitems.d)
# COMPRUEBO NORMALIDAD RESIDUOS
check_model(m3.d)
model_dashboard(m3.d)
qqmath(m3.d)


effects_list <- allEffects(m3.g)
# Convertimos las listas a data.frames
df_year <- as.data.frame(effects_list$Year)
df_year$Periodo <- NA  # Añadimos columna Periodo vacía para combinar
df_year$Type <- "Year"

df_periodo <- as.data.frame(effects_list$Periodo)
df_periodo$Year <- NA  # Añadimos columna Year vacía para combinar
df_periodo$Type <- "Periodo"

# Combinamos los dos data.frames
df_combined <- bind_rows(df_year, df_periodo) %>%
  mutate(Periodo = ifelse(is.na(Periodo), as.integer(as.character(Year)), Periodo)) %>%
  mutate(Year = ifelse(is.na(Year), NA, Year)) %>%
  filter(!is.na(Periodo))



###Grafico Anidamiento
j <- ggplot(df_combined, aes(x = Periodo, y = fit, color = as.factor(Year), group = as.factor(Year)))+
  geom_point(size=1.9) +
  geom_line(size=0.7) +
  theme_bw() +
  xlab("Periodo") +
  ylab("Nestedness") +
  labs(
    title="Gorbea",
    x="Periodo", 
    y="Nestedness"
  )+
  scale_color_manual(values=c("#DF8F44FF", "#B24745FF", "#79AF97FF"))

# Ajustar tema y añadir límites a los ejes
hamar <- j + 
  theme(
    axis.text.x = element_text(colour="black", size=12, face="bold"),
    axis.text.y = element_text(
      colour="black", 
      size=12, 
      angle=90, 
      hjust=0.5, 
      face="bold"
    )
  ) +
  ylim(0, 22) + 
  theme(
    axis.title.x = element_text(face="bold", size=15), 
    axis.title.y = element_text(face="bold", size=15)
  )

# Añadir barras de error
se1 <- df$fit + df$se
se2 <- df$fit - df$se

hamar3 <- hamar + geom_errorbar(
  aes(ymin=se2, ymax=se1), 
  width=0.1, 
  color="palevioletred2"
)

# Mostrar el gráfico final
hamar3

# MISMO MODELO QUE EL ANTERIOR PERO CON VARIABLE
# "ESPECIES PLANTAS" COMO VARIABLE EXPLICATIVA.
m4.g<-lmer(Anidamiento ~ Year + Periodo + plant.sp + (1|Bosque), data=sitems.g)
# RESUMEN MODELO 6
summary(m4.g)
anova(m4.g)
plot(allEffects(m4.g))
qqmath(m3.g)

# MODELO 5 - conectancia
m5.g<-lmer(connectance ~ Year + Periodo + (1|Bosque), data=sitems.g)
# COMPRUEBO NORMALIDAD RESIDUOS
check_model(m5.g)
# SIGUEN DISTRIBUCION NORMAL
anova(m5.g)
# GRAFICO RESULTADOS
allEffects(m5.g)
plot(allEffects(m5.g))
qqmath(m5.g)

m5.d<-lmer(connectance ~ Year + Periodo + (1|Bosque), data=sitems.d)
# COMPRUEBO NORMALIDAD RESIDUOS
check_model(m5.d)
anova(m5.d)
# GRAFICO RESULTADOS
allEffects(m5.d)
plot(allEffects(m5.d))
qqmath(m5.d)
model_dashboard(m5.d)

sitems.g$Periodo <- as.factor(sitems.g$Periodo)
# MODELO 6 - COMPLEMENTARIEDAD FUNC. POL.
m6.g<-lmer(comp.fun.pol ~ Periodo + plant.sp + (1|Year) + (1|Bosque), data=sitems.g)
# COMPRUEBO NORMALIDAD RESIDUOS
check_model(m6.g)
model_dashboard(m6.g)
qqmath(m6.g)
#NO SIGUEN DISTRIBUCION NORMAL
allEffects(m6.g)
plot(allEffects(m6.g))

sitems.d$Periodo <- as.factor(sitems.d$Periodo)
m6.d<-lmer(comp.fun.pol ~  Periodo + plant.sp + (1|Year) + (1|Bosque), data=sitems.d)
# COMPRUEBO NORMALIDAD RESIDUOS
check_model(m6.d)
model_dashboard(m6.d)
qqmath(m6.d)
allEffects(m6.d)
plot(allEffects(m6.d))

library(dplyr)
# Obtener las predicciones del modelo
sitems.g$predicted <- predict(m6.g, type="response", re.form = NA)
mean_predictions <- sitems.g %>%
  group_by(Year, Periodo) %>%
  summarise(mean_predicted = mean(predicted, na.rm = TRUE), .groups = 'drop')

sitems.d$predicted <- predict(m6.d, type="response", re.form = NA)
mean_predictions.d <- sitems.d %>%
  group_by(Year, Periodo) %>%
  summarise(mean_predicted = mean(predicted, na.rm = TRUE), .groups = 'drop')

library(ggplot2)
library(ggsci)
library(ggpubr)
library(purrr)

# Define la función para crear el gráfico
create_plot <- function(data, mean_predictions, plot_title) {
  ggplot(data, aes(x = Periodo, y = comp.fun.pol)) +
    geom_point(aes(color = as.factor(Year)), alpha = 0.6) +  # Puntos originales
    geom_line(aes(y = predicted), color = "#3C5488FF", alpha=0.75,size = 0.7) +  # Línea de valores predichos
    geom_point(data = mean_predictions, aes(x = Periodo, y = mean_predicted), color = "#3C5488FF", size = 2.5, shape = 21, fill = "#3C5488FF", alpha = 0.75) +  # Punto de la media de los valores predichos
    geom_line(data = mean_predictions, aes(x = Periodo, y = mean_predicted, group = Year), color = "#3C5488FF", size = 0.7, alpha = 0.75) +  # Línea conectando medias de predicciones
    facet_wrap(~ Year) +  # Facetar por año
    scale_color_npg() +  # Paleta de colores Nature
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x = "Periodo", y = "Functional complementarity", color = "Year") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          strip.text = element_text(size = 12),
          legend.text = element_text(size = 12)) +
    ggtitle(plot_title)
}

# Lista de datos y títulos
data_list <- list(
  g = list(data = sitems.g, mean_predictions = mean_predictions, title = "Gorbea"),
  d = list(data = sitems.d, mean_predictions = mean_predictions.d, title = "Doñana")
)

# Generar gráficos para cada conjunto de datos usando map
plots <- map(data_list, ~ create_plot(.x$data, .x$mean_predictions, .x$title))

# Mostrar los gráficos
print(plots$g)
print(plots$d)

library(glmmTMB)
# MODELO 11 - COMPLEMENTARIEDAD FUNC. POL.
m11 <- glmmTMB(comp.fun.pol ~ Year + Periodo + plant.sp + (1|Bosque), family = nbinom2, data = sitems.g)
# RESIDUOS
qqmath(m11)
anova(m11)
# RESUMEN MODELO 11 Y GRAFICOS
summary(m11)
allEffects(m11)
plot(allEffects(m11))

# MODELO 12 - ROBUSTEZ A LA PERDIDA DE SPP.
m12<-lmer(Robustez ~ Recorrido + Periodo + (1|Site_id), data = sitems)
# COMPRUEBO NORMALIDAD RESIDUOS
check_model(m12)
# SIGUEN DISTRIBUCION NORMAL
car::Anova(m12)
# GRAFICOS
allEffects(m12)
plot(allEffects(m12))

