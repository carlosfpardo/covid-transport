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
##Cargar casos promedio semanales en Buenos Aires
ICA <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                  sheet = "air_quality", range = "G1:I70", col_types=c("date","numeric","numeric"))
View(ICA)

##Cambio de nombres de columnas por practicidad
colnames(ICA)=c("Dia","Paris","Delhi")

#Realizar el Gráfico para Paris
##Crear el mapa base
base_line_Delhi <- geom_line(data=ICA,aes(Dia,Delhi,colour=Delhi), size = 1.5)
base_line_Paris <- geom_line(data=ICA,aes(Dia,Paris,colour=Paris), size = 1.5)

##Integrando el gráfico
ggplot() + base_line_Delhi + base_line_Paris + theme_minimal() + # escala del mapa de stringency
  labs(x="Fecha",y="ICA PM 2.5",
       title="Índice de Calidad del aire en Paris, Francia y Delhi, India",
       subtitle="ICA para material particulado menor a 2.5 micras por metro cúbico",
       color = "Índice de 
Calidad del Aire",
       caption=("aqicn.org")) + ###labels
  scale_color_gradientn(colours = c("#00FF00", "#FFFF00", "#FFFF00", "#FFA07A","#FFA07A", "#FF0000", "#800080", "#800080", "#800080", "#A0522D"))+
  scale_y_continuous(limits=c(0,350)) + # change y axis scale
  guides(alpha=FALSE)

leyenda <- c('0-50','51-100','101-150','151-200','201-300','301-500')
levels(ICA$Delhi) <- leyenda
