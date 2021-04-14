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
##Cargar casos promedio semanales en Buenos Aires
WDC <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                 sheet = "Washington_DC_cases", range = "E2:F60")
View(WDC)

##Cargar stringency index para Argentina
stringency_index <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                               sheet = "Washington_DC_cases", range = "H2:I464")
View(stringency_index)

##Cargar hitos legislación y transporte en Argentinas y Buenos Aires

##Cambio de nombres de columnas por practicidad
colnames(WDC)=c("Semana","Casos")
colnames(stringency_index)=c("Dia","Index")

#Realizar el Gráfico

##Crear el mapa base
base_line <- geom_line(data=WDC,aes(Semana,Casos,alpha=0.8))

##Integrar Stringency Index de Oxford
stringency <- geom_bar(data=stringency_index,stat="identity",aes(Dia,max(WDC$Casos),fill=Index))

##Integrando el gráfico
ggplot() + stringency + base_line + theme_classic() + scale_fill_viridis_c("Stringency Index
      Washington D.C.",begin=0.4,end=1,option="magma",direction=-1,aesthetics = "fill") + # escala del mapa de stringency
  labs(x="Semana",y="Casos semanales promedio",
       title="Curva de contagios por COVID-19",
       subtitle="Casos promedio semanales en Washington D.C., EE.UU.",
       caption=("Fuente de datos: coronavirus.dc.gov")) + ###labels
  guides(alpha=FALSE)
