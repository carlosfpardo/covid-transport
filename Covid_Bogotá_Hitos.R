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
                                 range = "R2:S428")
View(stringency_index)

##Cargar hitos legislación y transporte en Colombia
Hitos <- read_excel("Bogota_daily_cases.xlsx", 
                                 range = "H2:K24")
View(Hitos)

##Cambio de nombres de columnas por practicidad
colnames(Bog)=c("Semana","Casos")
colnames(stringency_index)=c("Dia","Index")
colnames(Hitos)=c("Fecha","Clase","Entidad","Hito")

max(Bog$Casos)

#Crear hitos
##Hitos internacionales
###OMS_pandemia <- as.Date("2020-03-11")

##Hitos Nacionales
###Estado_emergencia <- as.Date("2020-03-12")
###Ais_prev_obl <- as.Date("2020-03-22")
###COVID_friday <- as.Date("2020-06-19")
###Vacuna_fase1 <- as.Date("2021-02-17")

##Hitos Bogotá
###Calamidad_pública <- as.Date("2020-03-17")
###Simulacro_Vital <- as.Date("2020-03-19")
###Bog_solidaria <- as.Date("2020-03-25")
###Ais_prev_obl_bog <- as.Date("2020-07-01")
###Cuarentena_18loc <- as.Date("2020-07-13")
###Lev_medidas_estrictas <- as.Date("2020-08-26")
###Ais_sel_reapertura <- as.Date("2020-09-21")
###Med_fin_año <- as.Date("2020-12-29")
###Alerta_roja <- as.Date("2021-01-07")

##Hitos Transporte
###Ciclovia_22km <- as.Date("2020-03-16")
###Ciclovia_117km <- as.Date("2020-03-17")
###Ciclovia_76km <- as.Date("2020-03-18")
###Protocolos_TP <- as.Date("2020-04-23")
###Ciclovia_80km <- as.Date("2020-04-27")
###Ocup_TP50 <- as.Date("2020-04-30")
###Ocup_TP35 <- as.Date("2020-05-22")
###Ocup_TP50_rec <- as.Date("2020-08-25")

#Creación de la Base de datos de Hitos
###Hitos <- data.frame(Date=c(OMS_pandemia,###Internacionales (recientes al final)
###Estado_emergencia,Ais_prev_obl,COVID_friday,Vacuna_fase1,###Nacionales (recientes al final)
###Calamidad_pública,Simulacro_Vital,Bog_solidaria,Ais_prev_obl_bog,Cuarentena_18loc,Lev_medidas_estrictas,Ais_sel_reapertura,Med_fin_año,Alerta_roja,###Bogotá (recientes al final)
###Ciclovia_22km,Ciclovia_117km,Ciclovia_76km,Protocolos_TP,Ciclovia_80km,Ocup_TP50,Ocup_TP35,Ocup_TP50_rec), ###transporte (recientes al final)
###Event=c("OMS declara pandemia",
###"Estado de emergencia Nacional","Aislamiento Preventivo Obligatorio Nacional","'COVID Friday'","Inicio Fase 1 de Vacunación",
###"Bogotá declara calamidad pública","Simulacro Vital Bogotá","'Bogotá Solidaria en Casa'","Aislamiento preventivo Obligatorio para Bogotá","Cuarentena Estricta en 18 localidades de Bogotá","Levantamiento de medidas estrictas de aislamiento en Bogotá","Aislamiento selectivo y medidas de reapertura económica en Bogotá","Medidas para fin de año en Bogotá","Alerta Roja en Bogotá",
###"22 Kilómetros de ciclovía temporal","117 Kilómetros de ciclovía temporal","76 Kilómetros de ciclovía temporal","Protocolos para Transporte Público","80 Kilómetros de ciclovía temporal","Disminución de porcentaje de ocupación en Transporte público a 50%","Disminución de porcentaje de ocupación en Transporte público a 35%","Disminución de porcentaje de ocupación en Transporte público a 50%"))

#Ajustes de de base de hitos dataframe-gráfica
baseline = min(Bog$Casos)
delta = 0.05 * diff(range(Bog$Casos))
Hitos$ymin = baseline
Hitos$timelapse = c(diff(Hitos$Fecha),2020-03-01)
Hitos$bump = Hitos$timelapse < 2*370 #~4 años
offsets <- rle(Hitos$bump)
Hitos$offset <- unlist(mapply(function(l,v) {if(v){(l:1)+1}else{rep(1,l)}}, l=offsets$lengths, v=offsets$values, USE.NAMES=FALSE))
Hitos$ymax <- Hitos$ymin + Hitos$offset * delta

#Realizar el Gráfico


##Crear el mapa base
base_line <- geom_line(data=Bog,aes(Semana,Casos,alpha=0.8))

##Integrar Stringency Index de Oxford
stringency <- geom_bar(data=stringency_index,stat="identity",width=1,aes(Dia,Index,fill=Index))

##Integrando el gráfico
ggplot()+stringency+base_line+
  theme_classic()+ 
  scale_fill_viridis_c("Stringency Index",begin=0.4,end=1,option="magma")+ # escala del mapa de stringency
  labs(x="Semana",y="Casos semanales promedio",
       title="Curva de contagios por COVID-19",
       subtitle="Casos promedio semanales en Bogotá, Colombia",
       caption=("Fuente de datos: saludata.saludcapital.gov.co"))+ ###labels
  scale_x_date(date_breaks = "1 month",date_labels="%m %y",limits=c(as.Date("2020-02-01"),as.Date("2021-04-01")))+ ###Cambiar escala del eje x
  scale_y_continuous(limits=c(0,5500))+ ###Cambiar escala de eje y
  geom_segment(data = Hitos, mapping=aes(x=Fecha, y=ymin, xend=Fecha, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = Hitos, mapping=aes(x=Fecha,y=ymax), size=1) + ###Agregar puntos de hitos
  geom_text(data = Hitos, mapping=aes(x=Fecha, y=ymax, label=Hito), hjust=-0.1, vjust=0.1, size=3)+ ###Agregar leyendas de hitos
  guides(alpha=FALSE)

rlang::last_error()
rlang::last_trace()
##Grafica preliminar de casos semanales
###Solo borrador de curva general
ggplot(Bog, aes(x=Semana)) + 
  geom_line(aes(y=Casos)) + 
  labs(title="Curva de contagios por COVID-19", 
       subtitle="Casos promedio semanales en Bogotá, Colombia", 
       caption="Fuente de datos: saludata.saludcapital.gov.co", 
       y="Casos semanales promedio", 
       color=NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8), 
        panel.grid.minor = element_blank())
