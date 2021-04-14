#limpiar RAM y caché
gc()
rm(list=ls())

#Instalar Librerias
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)

#Se cargan las bases de excel desde GitHub Desktop
##Cargar casos promedio semanales
Bog <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                  range = "E2:F58")
View(Bog)

##Cargar stringency index para Colombia
stringency_index <- read_excel("Bogota_daily_cases.xlsx", 
                                 range = "S2:T428")
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

##Crear el mapa base
base_line <- geom_line(data=Bog,aes(Semana,Casos,alpha=0.8))

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

##Integrando el gráfico
#() + stringency + base_line + theme_classic() + scale_fill_viridis_c("Stringency Index
#      Colombia",begin=0.4,end=1,option="magma",aesthetics = "fill") + # escala del mapa de stringency
#  labs(x="Semana",y="Casos semanales promedio",
#       title="Curva de contagios por COVID-19",
#       subtitle="Casos promedio semanales en Bogotá, Colombia",
#       caption=("Fuente de datos: saludata.saludcapital.gov.co")) + ###labels
#  #scale_x_continuous(breaks = c(1:14),labels = c("Marzo 2020","Abril 2020","Mayo 2020","Junio 2020","Julio 2020","Agosto 2020","Septiembre 2020","Octubre 2020","Noviembre 2020","Diciembre 2020","Enero 2021","Febrero 2021","Marzo 2021","Abril 2021"))+
#  scale_y_continuous(limits=c(-4300,5400)) + # change y axis scale
#  geom_segment(data = Hitos, mapping=aes(x=Fecha, y=ymin, xend=Fecha, yend=ymax)) + ###Agregar lineas de Hitos
#  geom_point(data = Hitos, mapping=aes(x=Fecha,y=ymax, col=Clase), size=1.2) + ###Agregar puntos de hitos
#  geom_text(data = Hitos, mapping=aes(x=Fecha, y=ymax, label=Hito), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
#  guides(alpha=FALSE)

##Grafica preliminar de casos semanales
###Solo borrador de curva general
ggplot(Bog, aes(x=Semana)) + 
  geom_line(aes(y=Casos)) + 
  labs(title="Curva de contagios por COVID-19",
       subtitle="Casos promedio semanales en Bogotá, Colombia", 
       caption="Fuente de datos: saludata.saludcapital.gov.co", 
       y="Casos semanales promedio", 
       color=NULL) +
  scale_x_date(date_labels="%b %y",date_breaks = "1 month",limits=c(as.Date("2020-03-01"),as.Date("2021-05-01")))+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8), 
        panel.grid.minor = element_blank())

#scale_color_gradient
