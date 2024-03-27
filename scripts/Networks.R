####CUADRADOS
####Gorbea 2020 y 2021 redes


library(bipartite)
library(dplyr)

cuad<- read.csv("data/clean/cuad_gorbea_2020_clean.csv")



df.1<- subset(cuad, cuad$Periodo_fecha=="1")

#intentar crear variable tipo polinizador
df.1 <- df.1 %>%
  mutate(Tipo.polinizador= recode(Polinizador,"Bombus pascuorum"= "Abejorro",
                                  "Bombus hortorum"="Abejorro",
                                  "Bombus lapidarius"="Abejorro",
                                  "Bombus pratorum"="Abejorro",
                                  "Bombus terrestris"="Abejorro",
                                  "Bombus sylvestris"= "Abejorro",
                                  "Diptera"= "Dipteros",
                                  "Episyrphus balteatus"= "Sirfido",
                                  "Eristalis sp."= "Sirfido",
                                  "Eristalis tenax" = "Sirfido",
                                  "Eupeodes corollae"= "Sirfido",
                                  "Lasioglossum calceatum" ="Abeja",
                                  "Lasioglossum punctatissimum"="Abeja",
                                  "Lasioglossum sp"="Abeja",
                                  "Osmia andrenoides" ="Abeja",
                                  "Melanostoma scalare"="Sirfido",
                                  "Pieris sp."= "Lepidoptero",
                                  "Platycheirus albimanus"="Sirfido",
                                  "Platycheirus sp"="Sirfido",
                                  "Seladonia confusa"="Abeja",
                                  "Sphaerophoria scripta"= "Sirfido",
                                  "Syrphidae"= "Sirfido",
                                  "Thymelicus sp."= "Lepidoptero",
                                  "Andrena minutuloides"="Abeja",
                                  "Andrena rhyssonota"="Abeja",
                                  "Andrena wilkella"= "Abeja",
                                  "Andrena wikella"= "Abeja",
                                  "Apis mellifera"= "Abeja de la miel"
                                  ))

unique(df.1$Tipo.polinizador)
##eliminar no identificados
df.1 <- subset(df.1, df.1$Polinizador != "?=S28")
df.1 <- subset(df.1, df.1$Polinizador != "?=S30")
df.1 <- subset(df.1, df.1$Polinizador != "S28")
unique(df.1$Tipo.polinizador)

###Repetir proceso periodo2
df.2<- subset(cuad, cuad$Periodo_fecha=="2")
df.2 <- subset(df.2, df.2$Polinizador != "?=S108")
df.2 <- subset(df.2, df.2$Polinizador != "?=S120")
df.2 <- subset(df.2, df.2$Polinizador != "S118")
df.2 <- subset(df.2, df.2$Polinizador != "S136")
df.2 <- df.2 %>%
  mutate(Tipo.polinizador= recode(Polinizador,"Bombus pascuorum"= "Abejorro",
                                  "Bombus hortorum"="Abejorro",
                                  "Bombus lapidarius"="Abejorro",
                                  "Bombus pratorum"="Abejorro",
                                  "Bombus terrestris"="Abejorro",
                                  "Bombus sylvestris"= "Abejorro",
                                  "Diptera"= "Dipteros",
                                  "Episyrphus balteatus"= "Sirfido",
                                  "Eristalis sp."= "Sirfido",
                                  "Eristalis tenax" = "Sirfido",
                                  "Eupeodes corollae"= "Sirfido",
                                  "Lasioglossum calceatum" ="Abeja",
                                  "Lasioglossum punctatissimum"="Abeja",
                                  "Lasioglossum sp"="Abeja",
                                  "Osmia andrenoides" ="Abeja",
                                  "Melanostoma scalare"="Sirfido",
                                  "Pieris sp."= "Lepidoptero",
                                  "Platycheirus albimanus"="Sirfido",
                                  "Platycheirus sp"="Sirfido",
                                  "Seladonia confusa"="Abeja",
                                  "Sphaerophoria scripta"= "Sirfido",
                                  "Syrphidae"= "Sirfido",
                                  "Thymelicus sp."= "Lepidoptero",
                                  "Andrena minutuloides"="Abeja",
                                  "Andrena rhyssonota"="Abeja",
                                  "Andrena wilkella"= "Abeja",
                                  "Andrena wikella"= "Abeja",
                                  "Apis mellifera"= "Abeja de la miel",
                                  "Cheilosia nigripes" ="Sirfido",
                                  "Eristalis similis"="Sirfido",
                                  "Lassioglossum calceatum"= "Abeja",
                                  "Erebia meolans"="Lepidoptero",
                                  "Eupeodes sp"="Sirfido",
                                  "Lampides boeticus"= "Lepidoptero",
                                  "Melanostoma sp."= "Sirfido",
                                  "Nimphalidae"="Lepidoptero",
                                  "Bombus ruderatus"="Abejorro",
                                  "Chrysotoxum sp"="Sirfido",
                                  "Eupeodes latifasciatus"="Sirfido",
                                  "Tachinidae"="Dipteros",
                                  "Lepidoptera"="Lepidoptero",
                                  "Andrena bucephala"="Abeja",
                                  "Syrphus ribesii"="Sirfido",
                                  "Zygaena lonicerae"="Lepidoptero"
  ))
unique(df.2$Tipo.polinizador)
df.2 <- subset(df.2, df.2$Polinizador != "Abeja foto")

df.1$Planta<-as.factor(df.1$Planta)
df.1$Polinizador<-as.factor(df.1$Polinizador)

df.2$Planta<-as.factor(df.2$Planta)
df.2$Polinizador<-as.factor(df.2$Polinizador)

web=matrix(table(df.1$Planta,df.1$Polinizador),nrow=length(levels(df.1$Planta)),ncol=length(levels(df.1$Polinizador)))

web1=matrix(table(df.2$Planta,df.2$Polinizador),nrow=length(levels(df.2$Planta)),ncol=length(levels(df.2$Polinizador)))

rownames(web)=levels(df.1$Planta)
colnames(web)=levels(df.1$Polinizador)

rownames(web1)=levels(df.2$Planta)
colnames(web1)=levels(df.2$Polinizador)

##para ver la matrz que hemos creado
edit(web)

#comprobar que es una matriz numerica

str(web)
is.matrix(web)
is.numeric(web)

#La función dim nos devuelve un vector con dos elementos: el primero indica el número de filas, en nuestro caso 
#son plantas, y el segundo indica el número de columnas, en nuestro caso son animales.

dim(web)

#¿Cuántos registros, es decir visitas, contiene la matriz de adyacencia? ¿Y cuántas interacciones entre pares de 
#especies planta-animal únicos?

(V=sum(web)) #Frecuencia de la interacción, en este caso número total de visitas
(I=sum(web!=0))#Número total de interacciones --> riqueza de interacciones. Cuenta cuantas interacciones hay de mas de 0

#Calculamos la densidad de interacciones, es decir el número medio de interaciones por especie, para plantas y para 
#animales por separado.
(I.plants=I/nrow(web))
(I.animals=I/ncol(web))

##La conectancia (o llenado) es la proporción (o porcentaje) de interacciones que se observaron del total de 
#interacciones posibles.
(C=I/length(web))
(C.percentage=C*100)

##¿Cuántas visitas recibió cada especie de planta? Para calcularlo, sumamos los totales de las filas de la 
#matriz cuantitativa.
(rowvisits=rowSums(web))

#¿Cuántas visitas realizó cada especie de animal? Para calcularlo sumamos los totales de las columnas de la matriz 
#cuantitativa.
(colvisits=colSums(web))

##Transformamos la matriz cuantitativa o ponderada en su correspondiente matriz cualitativa o binaria.
binary.web=web

### a todos los valores diferentes a 0 les damos el valor 1. Matriz cualitativa, numero de distintas interacciones
binary.web[which(web!=0)]=1

sum(binary.web)
sum(binary.web!=0)

##Y calculamos el número de interacciones de cada especie de planta, es decir, los totales por filas.
(rowinteractions=rowSums(binary.web))

## las distintas interacciones que tiene cada planta y cada polinizador
(colinteractions=colSums(binary.web))

#Podemos dibujar los gráficos de barras correspondientes, numero total de visitas y numero de distintas visitas.
par(mfrow=c(2,2),mar=c(5,5,5,1))
barplot(rowinteractions,las=2,main="Plant interactions")
barplot(colinteractions,las=2,main="Animal interactions")
barplot(rowvisits,las=2,main="Plant visits")
barplot(colvisits,las=2, main="Animal visits")

##ordenadas de mas a menos
par(mfrow=c(2,2),mar=c(5,5,5,1))
barplot(rowinteractions[order(rowinteractions,decreasing=T)],las=2,main="Plant interactions")
barplot(colinteractions[order(colinteractions,decreasing=T)],las=2,main="Animal interactions")
barplot(rowvisits[order(rowvisits,decreasing=T)],las=2,main="Plant visits")
barplot(colvisits[order(colvisits,decreasing=T)],las=2,main="Animal visits")


####VISUALIZACION DE REDES
library(bipartite)
library(tidyr)
par(mfrow=c(1,1))


install.packages("colorspace")
library(MetBrewer)
library(viridis)

subset.default(cuad$Planta[cuad$Planta=="Helleborus viridis"])

##Un color a cada especie de polinizador
par(mfrow=c(2,1))
cols <- hcl.colors(n=27, palette="Temps")
red <- plotweb(sortweb(web, sort.order="dec"), method = "normal",labsize = 2.8, col.interaction= cols, text.rot=90, high.y=2.2, low.y= 1.2, adj.low=1, bor.col.high= rep(cols), bor.col.low = "deepskyblue4",
        bor.col.interaction= cols,arrow="up.center", ybig=1.8,  col.high = cols, col.low= "deepskyblue4")

cols <- hcl.colors(n=35, palette="Temps")
red1<-plotweb(sortweb(web1, sort.order="dec"), method = "normal",labsize = 2.8, col.interaction= cols, text.rot=90, high.y=2.2, low.y= 1.2, adj.low=1, bor.col.high= rep(cols), bor.col.low = "deepskyblue4",
        bor.col.interaction= cols,arrow="up.center", ybig=1.8,  col.high = cols, col.low= "deepskyblue4")


## paletas con colores bonitos
cols <- met.brewer(name="Nizami", n=27)
cols <- met.brewer(name="Hiroshige", n=27)
cols <- hcl.colors(n=5, palette="Spectral")

##Intentar agrupar por Tipo.polinizador para los colores

unique(df.1$Tipo.polinizador) ## 6 grupos
patch <- unique(df.1[,c("Tipo.polinizador", "Polinizador")])

library(ggsci)
cols <- c("#5B8FA8FF", "#6A6599FF", "#5B8FA8FF","#5B8FA8FF","#6A6599FF", "#6A6599FF", "#6A6599FF","#DF8F44FF","#5B8FA8FF",
                    "#B24745FF","#DF8F44FF","#5B8FA8FF", "#DF8F44FF","#DF8F44FF","#6A6599FF","#DF8F44FF","#DDCC77",
                    "#DF8F44FF","#6A6599FF","#6A6599FF","#6A6599FF", "#5B8FA8FF", "#6A6599FF","#DF8F44FF", "#DF8F44FF",
                    "#882255", "#DDCC77")

patch1 <- unique(df.2[,c("Tipo.polinizador", "Polinizador")])
cols1 <- c("#5B8FA8FF", "#6A6599FF", "#B24745FF","#5B8FA8FF","#5B8FA8FF", "#6A6599FF", "#DF8F44FF","#6A6599FF","#6A6599FF",
                     "#6A6599FF","#5B8FA8FF","#DDCC77", "#882255","#6A6599FF","#DF8F44FF","#5B8FA8FF","#6A6599FF","#6A6599FF",
                     "#6A6599FF","#DF8F44FF","#6A6599FF","#6A6599FF", "#6A6599FF", "#6A6599FF","#DF8F44FF", "#DDCC77", "#6A6599FF",
                     "#882255", "#5B8FA8FF", "#DF8F44FF", "#DF8F44FF","#6A6599FF","#DDCC77","#DDCC77","#DDCC77")
                     

legend <- unique(patch[c("Tipo.polinizador", "color")])
par(xpd = T)
par(mfrow=c(2,1))
plotweb(sortweb(web,sort.order="dec"), method = "normal",labsize = 2, col.interaction= rep(cols), text.rot=90, high.y=2.2, low.y= 1.2, adj.low=1, bor.col.high= rep(as.character(cols)), bor.col.low = "#374E66FF",
        bor.col.interaction= rep(as.character(cols)),arrow="up.center", ybig=1.8,  col.high = cols, col.low= "#374E66FF")

plotweb(sortweb(web1,sort.order="dec"), method = "normal",labsize = 2, col.interaction= rep(cols1), text.rot=90, high.y=2.2, low.y= 1.2, adj.low=1, bor.col.high= rep(as.character(cols1)), bor.col.low = "#374E66FF",
        bor.col.interaction= rep(as.character(cols1)),arrow="up.center", ybig=1.8,  col.high = cols1, col.low= "#374E66FF")


plot(NULL,xaxt="n",yaxt="n",bty="n",ylab="",xlab="", xlim=0:1,ylim=0:1)
legend(x=0, y=1, legend=c("Sirfidoak","Erle bakartiak", "Ezti erlea", "Erlastarrak","Dipteroak","Lepidopteroak"), pch=21,
       col="#777777", pt.bg = c ("#6A6599FF", "#DF8F44FF", "#B24745FF","#5B8FA8FF", "#882255","#DDCC77"),pt.cex=2, cex=0.7, bty="n", ncol=4, title="Polinizatzaile motak", title.adj=0.02, title.cex=0.9)
       

#####2021
cuad<- read.csv("data/clean/cuad_gorbea_2021_clean.csv")
unique(cuad$Polinizador)
cuad$Polinizador[cuad$Polinizador == "Andrena nigroaenea"]<- "Andrena nigroaena"
cuad$Polinizador[cuad$Polinizador == "Cheilosia sp"]<- "Cheilosia sp."
cuad$Polinizador[cuad$Polinizador == "Bombylius sp"]<- "Bombylius sp."
cuad$Polinizador[cuad$Polinizador == "Eupeodes sp"]<- "Eupeodes sp."
library(dplyr)
cuad<- cuad %>%
  mutate(Tipo.polinizador= recode(Polinizador,"Bombus pascuorum"= "Abejorro",
                                  "Bombus hortorum"="Abejorro",
                                  "Bombus sp."="Abejorro",
                                  "Bombus lapidarius"="Abejorro",
                                  "Bombus pratorum"="Abejorro",
                                  "Bombus terrestris"="Abejorro",
                                  "Bombus sylvestris"= "Abejorro",
                                  "Bombus ruderatus"="Abejorro",
                                  "Bombus magnus"="Abejorro",
                                  "Pieris brassicae"="Lepidoptero",
                                  "Eupeodes sp."="Sirfido",
                                  "Diptera"= "Dipteros",
                                  "Episyrphus balteatus"= "Sirfido",
                                  "Eristalis sp."= "Sirfido",
                                  "Eristalis tenax" = "Sirfido",
                                  "Eristalis similis"="Sirfido",
                                  "Eupeodes corollae"= "Sirfido",
                                  "Eupeodes niger"="Sirfido",
                                  "Lasioglossum immunitum" ="Abeja",
                                  "Lasioglossum tricinctum"="Abeja",
                                  "Lasioglossum sp"="Abeja",
                                  "Lasioglossum morio"="Abeja",
                                  "Lasioglossum nitidulum"="Abeja",
                                  "Diptera"="Dipteros",
                                  "Episyrphus sp"="Sirfido",
                                  "Sphaerophoria sp"= "Sirfido",
                                  "Ancistrocerus sp"="Avispa",
                                  "Lucilia silvarum"="Dipteros",
                                  "Lucilia caesar"="Dipteros",
                                  "Osmia andrenoides" ="Abeja",
                                  "Osmia emarginata"="Abeja",
                                  "Vanessa cardui"="Lepidoptero",
                                  "Pieris napi"="Lepidoptero",
                                  "Anthocharis cardamines"="Lepidoptero",
                                  "Melanostoma scalare"="Sirfido",
                                  "Pieris sp."= "Lepidoptero",
                                  "Platycheirus albimanus"="Sirfido",
                                  "Platycheirus sp"="Sirfido",
                                  "Seladonia confusa"="Abeja",
                                  "Sphaerophoria scripta"= "Sirfido",
                                  "Syrphidae"= "Sirfido",
                                  "Dasyrhamphis atra"="Dipteros",
                                  "Thymelicus sp."= "Lepidoptero",
                                  "Andrena minutuloides"="Abeja",
                                  "Andrena cineraria"="Abeja",
                                  "Andrena nitida"="Abeja",
                                  "Andrena nigroaena"="Abeja",
                                  "Andrena nigroaenea"="Abeja",
                                  "Andrena bicolor"="Abeja",
                                  "Andrena rhyssonota"="Abeja",
                                  "Andrena wilkella"= "Abeja",
                                  "Andrena wikella"= "Abeja",
                                  "Apis mellifera"= "Abeja de la miel",
                                  "Empis sp" ="Dipteros",
                                  "Andrena cf. Morio"="Abeja",
                                  "Macroglossum stellatarum"="Lepidoptero",
                                  "Orthoptera"="Ortopteros",
                                  "Andrena agilissima"="Abeja",
                                  "Bombylius sp."="Dipteros",
                                  "Vanessa atalanta"="Lepidoptero",
                                  "Cheilosia sp."="Sirfido",
                                  "Cheilosia sp"="Sirfido",
                                  "Coenoninpha panfilus"= "Lepidoptero",
                                  "Eucera nigrescens"="Abeja",
                                  "Formicidae"="Hormigas",
                                  "Zygaena lonicerae"="Lepidoptero",
                                  "Lepidoptera"="Lepidoptero",
                                  "Erebia meolans"="Lepidoptero",
                                  "Calliphoridae"="Dipteros",
                                  "Cheilosia personata"="Sirfido"
                                  
                                  
                                  
  ))
##eliminar no identificados
cuad <- subset(cuad, cuad$Polinizador != "?=C22")
cuad <- subset(cuad, cuad$Polinizador != "C22")
cuad <- subset(cuad, cuad$Polinizador != "C19")
cuad<- subset(cuad, cuad$Polinizador != "C44")
cuad<- subset(cuad, cuad$Polinizador != "?=C35")
cuad<- subset(cuad, cuad$Polinizador != "?=C47")
unique(cuad$Tipo.polinizador)

df.1<- subset(cuad, cuad$Periodo_fecha=="1")
df.2<- subset(cuad, cuad$Periodo_fecha=="2")
df.3<- subset(cuad, cuad$Periodo_fecha=="3")

df.1$Planta<-as.factor(df.1$Planta)
df.1$Polinizador<-as.factor(df.1$Polinizador)

df.2$Planta<-as.factor(df.2$Planta)
df.2$Polinizador<-as.factor(df.2$Polinizador)

df.3$Planta<-as.factor(df.3$Planta)
df.3$Polinizador<-as.factor(df.3$Polinizador)

web=matrix(table(df.1$Planta,df.1$Polinizador),nrow=length(levels(df.1$Planta)),ncol=length(levels(df.1$Polinizador)))

web1=matrix(table(df.2$Planta,df.2$Polinizador),nrow=length(levels(df.2$Planta)),ncol=length(levels(df.2$Polinizador)))

web2=matrix(table(df.3$Planta,df.3$Polinizador),nrow=length(levels(df.3$Planta)),ncol=length(levels(df.3$Polinizador)))

rownames(web)=levels(df.1$Planta)
colnames(web)=levels(df.1$Polinizador)

rownames(web1)=levels(df.2$Planta)
colnames(web1)=levels(df.2$Polinizador)

rownames(web2)=levels(df.3$Planta)
colnames(web2)=levels(df.3$Polinizador)

##info de la web
dim(web)
(V=sum(web)) 
(I=sum(web!=0))

(I.plants=I/nrow(web))
(I.animals=I/ncol(web))

(C=I/length(web))
(C.percentage=C*100)

(rowvisits=rowSums(web))
(colvisits=colSums(web))

binary.web=web
binary.web[which(web!=0)]=1
sum(binary.web)
sum(binary.web!=0)
(rowinteractions=rowSums(binary.web))
(colinteractions=colSums(binary.web))

dim(web1)
(V=sum(web1)) 
(I=sum(web1!=0))

(I.plants=I/nrow(web1))
(I.animals=I/ncol(web1))

(C=I/length(web1))
(C.percentage=C*100)

(rowvisits=rowSums(web1))
(colvisits=colSums(web1))

binary.web1=web1
binary.web1[which(web1!=0)]=1
sum(binary.web1)
sum(binary.web1!=0)
(rowinteractions=rowSums(binary.web1))
(colinteractions=colSums(binary.web1))

dim(web2)
(V=sum(web2)) 
(I=sum(web2!=0))

(I.plants=I/nrow(web2))
(I.animals=I/ncol(web2))

(C=I/length(web2))
(C.percentage=C*100)

(rowvisits=rowSums(web2))
(colvisits=colSums(web2))

binary.web2=web2
binary.web2[which(web2!=0)]=1
sum(binary.web2)
sum(binary.web2!=0)
(rowinteractions=rowSums(binary.web2))
(colinteractions=colSums(binary.web2))

##visualizacion
library(bipartite)
library(tidyr)
par(mfrow=c(1,1))

install.packages("colorspace")
library(MetBrewer)

##un color a cada especie de polinizador
cols <- hcl.colors(n=20, palette="Temps")
red <- plotweb(sortweb(web, sort.order="dec"), method = "normal",labsize = 2.8, col.interaction= cols, text.rot=90, high.y=2.2, low.y= 1.2, adj.low=1, bor.col.high= rep(cols), bor.col.low = "deepskyblue4",
               bor.col.interaction= cols,arrow="up.center", ybig=1.8,  col.high = cols, col.low= "deepskyblue4")

cols <- hcl.colors(n=37, palette="Temps")
red <- plotweb(sortweb(web1, sort.order="dec"), method = "normal",labsize = 2.8, col.interaction= cols, text.rot=90, high.y=2.2, low.y= 1.2, adj.low=1, bor.col.high= rep(cols), bor.col.low = "deepskyblue4",
               bor.col.interaction= cols,arrow="up.center", ybig=1.8,  col.high = cols, col.low= "deepskyblue4")

cols <- hcl.colors(n=28, palette="Temps")
red <- plotweb(sortweb(web2, sort.order="dec"), method = "normal",labsize = 2.8, col.interaction= cols, text.rot=90, high.y=2.2, low.y= 1.2, adj.low=1, bor.col.high= rep(cols), bor.col.low = "deepskyblue4",
               bor.col.interaction= cols,arrow="up.center", ybig=1.8,  col.high = cols, col.low= "deepskyblue4")


unique(df.1$Tipo.polinizador) ## 6grupos
patch <- unique(df.1[,c("Tipo.polinizador", "Polinizador")])
library(ggsci)
library(ggthemes)
cols <- c("#B24745FF",  "#5B8FA8FF","#5B8FA8FF","#6A6599FF", "#DF8F44FF", "#882255","#6A6599FF","#DF8F44FF","#DF8F44FF",
                     "#5B8FA8FF","#DF8F44FF","#6A6599FF" ,"#DDCC77","#DF8F44FF","#882255", "#DF8F44FF","#DF8F44FF",
                      "#5B8FA8FF","#5B8FA8FF", "#6A6599FF")

unique(df.2$Tipo.polinizador) ## 9 grupos
patch1 <- unique(df.1[,c("Tipo.polinizador", "Polinizador")])
cols1 <- c("#B24745FF","#6A6599FF","#5B8FA8FF","#DF8F44FF","#882255", "#882255","#DF8F44FF","#5B8FA8FF","#6A6599FF", "#6A6599FF","#DDCC77","#6A6599FF", "#882255",
                     "#DDCC77","#DF8F44FF","#882255","#DDCC77","#DF8F44FF","#DDCC77","#DF8F44FF","#DF8F44FF","#5B8FA8FF","#6A6599FF" ,"#DF8F44FF","#DF8F44FF","#8A9045FF", "#5B8FA8FF", 
                     "#2d6d66","#6A6599FF","#350E20FF","#DDCC77", "#882255","#6A6599FF", "#6A6599FF", "#6A6599FF", "#6A6599FF","#DDCC77" )


unique(df.3$Tipo.polinizador) ## 7grupos
patch2 <- unique(df.3[,c("Tipo.polinizador", "Polinizador")])
library(ggsci)
library(ggthemes)
cols2 <- c(  "#5B8FA8FF","#6A6599FF","#DDCC77","#6A6599FF" ,"#5B8FA8FF", "#DF8F44FF", "#DF8F44FF","#DF8F44FF","#6A6599FF","#5B8FA8FF","#DF8F44FF","#B24745FF","#882255",
                     "#5B8FA8FF","#882255","#6A6599FF" ,"#5B8FA8FF","#DDCC77","#6A6599FF","#6A6599FF","#DDCC77", "#6A6599FF","#DDCC77","#882255",
                     "#6A6599FF","#350E20FF", "#DDCC77","#DF8F44FF")
                     

par(mfrow=c(2,1))
plotweb(sortweb(web,sort.order="dec"), method = "normal",labsize = 2, col.interaction= rep(cols), text.rot=90, high.y=2.2, low.y= 1.2, adj.low=1, bor.col.high= rep(as.character(cols)), bor.col.low = "#374E66FF",
        bor.col.interaction= rep(as.character(cols)),arrow="up.center", ybig=1.8,  col.high = cols, col.low= "#374E66FF")

plotweb(sortweb(web1,sort.order="dec"), method = "normal",labsize = 2, col.interaction= rep(cols1), text.rot=90, high.y=2.2, low.y= 1.2, adj.low=1, bor.col.high= rep(as.character(cols1)), bor.col.low = "#374E66FF",
        bor.col.interaction= rep(as.character(cols1)),arrow="up.center", ybig=1.8,  col.high = cols1, col.low= "#374E66FF")

par(mfrow=c(1,1))
plot(NULL,xaxt="n",yaxt="n",bty="n",ylab="",xlab="", xlim=0:1,ylim=0:1)
legend(x=0, y=1, legend=c("Sirfidoak","Erle bakartiak", "Ezti erlea", "Erlastarrak","Dipteroak","Lepidopteroak","Inurriak","Liztorrak","Ortopteroak"), pch=21,
       col="#777777", pt.bg = c ("#6A6599FF", "#DF8F44FF", "#B24745FF","#5B8FA8FF", "#882255","#DDCC77", "#2d6d66","#8A9045FF","#350E20FF"),pt.cex=2, cex=0.7, bty="n", ncol=4, title="Polinizatzaile motak", title.adj=0.02, title.cex=0.9)

plotweb(sortweb(web2,sort.order="dec"), method = "normal",labsize = 2, col.interaction= rep(cols2), text.rot=90, high.y=2.2, low.y= 1.2, adj.low=1, bor.col.high= rep(as.character(cols2)), bor.col.low = "#374E66FF",
        bor.col.interaction= rep(as.character(cols2)),arrow="up.center", ybig=1.8,  col.high = cols2, col.low= "#374E66FF")

