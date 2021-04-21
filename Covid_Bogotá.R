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
##Cargar casos promedio semanales y Stringency Index
Bog <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                  range = "E2:G60", col_types=c("date","numeric","numeric"))
View(Bog)

##Cargar hitos legislación y transporte en Colombia
Hitos <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                    range = "J2:N24", col_types=c("date","text","text","text","numeric"))
View(Hitos)

##Cambio de nombres de columnas por practicidad
colnames(Bog)=c("Dia","Casos","Index")
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
##Crear curva de casos con Stringency Index
base_line <- geom_line(data=Bog,aes(Dia,Casos,colour=Index), size = 2)

##Integrando el gráfico
ggplot() + base_line + theme_minimal() + # escala del mapa de stringency
  labs(x="Semana",y="Casos semanales promedio",
       title="Curva de contagios por COVID-19",
       subtitle="Casos promedio semanales en Bogotá, Colombia",
       color = "Stringency Index
      Colombia",
       caption=("Fuente de datos: saludata.saludcapital.gov.co")) + ###labels
  #scale_x_date(date_labels="%b %y",date_breaks = "1 month",limits=c(as.Date("2020-03-01"),as.Date("2021-05-01")))+
  #scale_color_gradient(low = "green", high = "red")+
  scale_color_gradientn(colors = c("#ADF84E", "#CCEE18", "#E2DF00", "#EECB00", "#F2B300", "#EE9700", "#E37830", "#D15544", "#B9264E", "#A00052"))+
  scale_y_continuous(limits=c(-4300,5400)) + # change y axis scale
  geom_segment(data = Hitos, mapping=aes(x=Fecha, y=ymin, xend=Fecha, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = Hitos, mapping=aes(x=Fecha,y=ymax), size=1.2) + ###Agregar puntos de hitos
  geom_text(data = Hitos, mapping=aes(x=Fecha, y=ymax, label=Hito), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  guides(alpha=FALSE)

##Ahora con Google Mobility
###Cargando la base
Google_mob <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                  range = "E2:H60", col_types=c("date","numeric","numeric","numeric"))
View(Google_mob)

##Cambio de nombres de columnas por practicidad
colnames(Google_mob)=c("Dia","Casos","Index","Google")

###Integrar el indice de restricción de movilidad de Google para el caso de tiendas
base_line <- geom_line(data=Google_mob,aes(Dia,Casos,colour=Google), size = 2)

###Integrando el gráfico
ggplot() + base_line + theme_minimal() +
  labs(x="Semana",y="Casos semanales promedio",
       title="Curva de contagios por COVID-19",
       subtitle="Casos promedio semanales en Bogotá, Colombia",
       color = "Google mobility Index 
            Bogotá",
       caption=("Fuente de datos: saludata.saludcapital.gov.co")) + ###labels
  #scale_x_date(date_labels="%b %y",date_breaks = "1 month",limits=c(as.Date("2020-03-01"),as.Date("2021-05-01")))+
  scale_color_gradientn(colors = c("#A00052", "#B9264E", "#D15544", "#E37830", "#EE9700", "#F2B300", "#EECB00", "#E2DF00", "#CCEE18", "#ADF84E"))+
  scale_y_continuous(limits=c(0,5400)) + # change y axis scale
  #geom_segment(data = Hitos, mapping=aes(x=Fecha, y=ymin, xend=Fecha, yend=ymax)) + ###Agregar lineas de Hitos
  #geom_point(data = Hitos, mapping=aes(x=Fecha,y=ymax), size=1.2) + ###Agregar puntos de hitos
  #geom_text(data = Hitos, mapping=aes(x=Fecha, y=ymax, label=Hito), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  guides(alpha=FALSE)

###Configuración de escala cromática
pal <- choose_palette()
pal(10)