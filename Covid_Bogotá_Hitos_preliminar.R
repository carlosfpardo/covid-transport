#limpiar RAM y caché
gc()
rm(list=ls())

#Instalar Librerias
library(ggplot2)
library(dplyr)
library(readxl)
library(viridis)
library("colorspace")

#Se cargan las bases de excel desde GitHub Desktop
##Cargar casos promedio semanales
Bog <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                  range = "E2:F60")
View(Bog)

##Cargar stringency index para Colombia
stringency_index <- read_excel("Bogota_daily_cases.xlsx", 
                                 range = "S2:T442")
View(stringency_index)

##Cargar hitos legislación y transporte en Colombia
Hitos <- read_excel("Bogota_daily_cases.xlsx", 
                                 range = "H2:L24")
View(Hitos)

##Cambio de nombres de columnas por practicidad
colnames(Bog)=c("Semana","Casos")
colnames(stringency_index)=c("Dia","Index")
colnames(Hitos)=c("Fecha","Clase","Entidad","Hito","Altura")

#Ajustes de de base de hitos dataframe-gráfica
baseline = min(Bog$Casos)
delta = 0.05 * diff(range(Bog$Casos))
Hitos$ymin = baseline
Hitos$timelapse = c(diff(Hitos$Fecha),2020-03-01)
Hitos$bump = Hitos$timelapse < 2*370 #~2 años
offsets <- rle(Hitos$bump)
Hitos$offset <- unlist(mapply(function(l,v) {if(v){(l:1)+1}else{rep(1,l)}}, l=offsets$lengths, v=offsets$values, USE.NAMES=FALSE))
Hitos$ymax <- Hitos$Altura
Hitos$ymin + Hitos$offset * delta

#Realizar el Gráfico
##Crear curva de casos
base_line <- geom_line(data=Bog,aes(Semana,Casos))

##Integrar Stringency Index de Oxford
stringency <- geom_bar(data=stringency_index,stat="identity",aes(Dia,max(Bog$Casos),fill=Index))

##Integrando el gráfico
ggplot() + stringency + base_line + theme_classic() + scale_fill_viridis_c("Stringency Index
      Colombia",begin=0.4,end=1,option="magma",direction=-1,aesthetics = "fill") + # escala del mapa de stringency
  labs(x="Semana",y="Casos semanales promedio",
       title="Curva de contagios por COVID-19",
       subtitle="Casos promedio semanales en Bogotá, Colombia",
       caption=("Fuente de datos: saludata.saludcapital.gov.co")) + ###labels
  #scale_x_date(date_labels="%b %y",date_breaks = "1 month",limits=c(as.Date("2020-03-01"),as.Date("2021-05-01")))+
  scale_y_continuous(limits=c(-4300,5400)) + # change y axis scale
  geom_segment(data = Hitos, mapping=aes(x=Fecha, y=ymin, xend=Fecha, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = Hitos, mapping=aes(x=Fecha,y=ymax), size=1.2) + ###Agregar puntos de hitos
  geom_text(data = Hitos, mapping=aes(x=Fecha, y=ymax, label=Hito), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  guides(alpha=FALSE)

##Ahora con Google Mobility
###Cargando la base
Google_mob <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                  range = "S2:U448", col_types=c("date","skip","numeric"))
View(Google_mob)

###Integrar el indice de restricción de movilidad de Google para el caso de tiendas
mobility_index <- geom_bar(data=Google_mob,stat="identity",aes(Dia,max(Bog$Casos),fill=Google_index))

###Integrando el gráfico
ggplot() + mobility_index + base_line + theme_classic() + scale_fill_viridis_c("Google mobility Index
        Bogotá",begin=0.4,end=1,option="magma",aesthetics = "fill") + # escala del mapa de stringency
  labs(x="Semana",y="Casos semanales promedio",
       title="Curva de contagios por COVID-19",
       subtitle="Casos promedio semanales en Bogotá, Colombia",
       caption=("Fuente de datos: saludata.saludcapital.gov.co")) + ###labels
  #scale_x_date(date_labels="%b %y",date_breaks = "1 month",limits=c(as.Date("2020-03-01"),as.Date("2021-05-01")))+
  scale_y_continuous(limits=c(-4300,5400)) + # change y axis scale
  geom_segment(data = Hitos, mapping=aes(x=Fecha, y=ymin, xend=Fecha, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = Hitos, mapping=aes(x=Fecha,y=ymax), size=1.2) + ###Agregar puntos de hitos
  geom_text(data = Hitos, mapping=aes(x=Fecha, y=ymax, label=Hito), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  guides(alpha=FALSE)

##scale_color_gradient
###Cargar datos
Fusion_test <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                         range = "AS2:AU60", col_types=c("date","numeric","numeric"))
View(Fusion_test)

###Nombres
colnames(Fusion_test)=c("Dia","casos","Index")

###Configuración de escala cromática
pal <- choose_palette()
pal(10)

###Generación de la gráfica
ggplot(Fusion_test, aes(x=Dia, y=casos)) +
  geom_line(aes(colour = Index)) +
  scale_color_gradientn(colors = c("#ADF84E", "#CCEE18", "#E2DF00", "#EECB00", "#F2B300", "#EE9700", "#E37830", "#D15544", "#B9264E", "#A00052"))