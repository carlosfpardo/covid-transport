#limpiar RAM y caché
gc()
rm(list=ls())

#Instalar Librerias
library(ggplot2)
library(dplyr)
library(readxl)
library(viridis)
library("colorspace")
install.packages("ggnewscale")
library(ggnewscale)

#Para el caso de calidad del aire en París, Francia
#Se cargan las bases de excel desde GitHub Desktop
##Cargar calidad del aire para Paris
ICA_Paris <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                  sheet = "air_quality",
                  range = "A2:D2600",
                  col_types=c("date","numeric","numeric","numeric"))
View(ICA_Paris)

##Cargar hitos legislación y transporte en Paris
Hitos_Paris <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                    sheet = "air_quality",
                    range = "H2:P23",
                    col_types=c("date","text","text","text","skip","text","text","numeric","numeric"))
View(Hitos_Paris)

##Cambio de nombres de columnas por practicidad
colnames(ICA_Paris)=c("Dia","Paris_25","Paris_10","moving_average_week_25")
colnames(Hitos_Paris)=c("Fecha","Ciudad","Clase","Fase","Hito","Link","Inicio","Altura")

#Ajustes de de base de hitos dataframe-gráfica
baseline = min(ICA_Paris$Paris_25)
delta = 0.05 * diff(range(ICA_Paris$Paris_25))
Hitos_Paris$ymin = baseline
Hitos_Paris$timelapse = c(diff(Hitos_Paris$Fecha),2014-01-01)
Hitos_Paris$bump = Hitos_Paris$timelapse < 9*366 #~9 años
offsets <- rle(Hitos_Paris$bump)
Hitos_Paris$offset <- unlist(mapply(function(l,v) {if(v){(l:1)+1}else{rep(1,l)}}, l=offsets$lengths, v=offsets$values, USE.NAMES=FALSE))
Hitos_Paris$ymax <- Hitos_Paris$Altura
Hitos_Paris$ymin + Hitos_Paris$offset * delta

#Crear el label de los hitos con fecha
hitos_lab <- paste(Hitos_Paris$Hito,"\n",Hitos_Paris$Fecha,sep="")

#Realizar el Gráfico para Paris
##Crear el mapa base
base_line_Paris <- geom_line(data=ICA_Paris,aes(Dia,moving_average_week_25,colour=Paris_25), size = 1.8)

##Integrando el gráfico
ggplot() + base_line_Paris + theme_minimal() + # escala del mapa de stringency
  labs(x="Fecha",y="ICA PM 2.5",
       title="Índice de Calidad del aire en Paris, Francia",
       subtitle="ICA para material particulado menor a 2.5 micras por metro cúbico",
       color = "Índice de 
Calidad del Aire",
       caption=("aqicn.org")) + ###labels
  scale_color_gradientn(colours = c("#00FF00", "#FFFF00", "#FFFF00", "#FFA07A", "#FF0000", "#800080", "#800080", "#A0522D", "#A0522D", "#A0522D", "#A0522D", "#A0522D"),limits = c(0,500))+
  scale_y_continuous(limits=c(0,200)) + # change y axis scale
  geom_segment(data = Hitos_Paris, mapping=aes(x=Fecha, y=Inicio, xend=Fecha, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = Hitos_Paris, mapping=aes(x=Fecha,y=ymax,shape=Clase), size=2) + ###Agregar puntos de hitos
  new_scale_color() +
  geom_text(data = Hitos_Paris, mapping=aes(x=Fecha, y=ymax, label=hitos_lab, color = Fase), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  scale_colour_manual(breaks=c("A-","B-","C-","D","C+","B+","A+"),
                      values=c('A-'="#FFD8A2", 'B-'="#FAB875", 'C-'="#E19438", 'D'="#C56F00",
                               'C+'="#A4C6FF", 'B+'="#40A2FF", 'A+'="#006BFF")) +
  guides(alpha=FALSE)

#Para el caso de calidad del aire en Delhi, India
#Se cargan las bases de excel desde GitHub Desktop
##Cargar calidad del aire para Delhi
ICA_Delhi <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                        sheet = "air_quality",
                        range = "R2:U1300",
                        col_types=c("date","numeric","numeric","numeric"))
View(ICA_Delhi)

##Cargar hitos legislación y transporte en Delhi
Hitos_Delhi <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                          sheet = "air_quality",
                          range = "Y2:AG35",
                          col_types=c("date","text","text","text","skip","text","text","numeric","numeric"))
View(Hitos_Delhi)

##Cambio de nombres de columnas por practicidad
colnames(ICA_Delhi)=c("Dia","Delhi_25","Delhi_10","moving_average_week_25")
colnames(Hitos_Delhi)=c("Fecha","Ciudad","Clase","Fase","Hito","Link","Inicio","Altura")

#Ajustes de de base de hitos dataframe-gráfica
baseline = min(ICA_Delhi$Delhi_25)
delta = 0.05 * diff(range(ICA_Delhi$Delhi_25))
Hitos_Delhi$ymin = baseline
Hitos_Delhi$timelapse = c(diff(Hitos_Delhi$Fecha),2018-01-01)
Hitos_Delhi$bump = Hitos_Delhi$timelapse < 6*366 #~5 años
offsets <- rle(Hitos_Delhi$bump)
Hitos_Delhi$offset <- unlist(mapply(function(l,v) {if(v){(l:1)+1}else{rep(1,l)}}, l=offsets$lengths, v=offsets$values, USE.NAMES=FALSE))
Hitos_Delhi$ymax <- Hitos_Delhi$Altura
Hitos_Delhi$ymin + Hitos_Delhi$offset * delta

#Crear el label de los hitos con fecha
hitos_lab <- paste(Hitos_Delhi$Hito,"\n",Hitos_Delhi$Fecha,sep="")

#Realizar el Gráfico para Paris
##Crear el mapa base
base_line_Delhi <- geom_line(data=ICA_Delhi,aes(Dia,moving_average_week_25,colour=Delhi_25), size = 1.8)

##Integrando el gráfico
ggplot() + base_line_Delhi + theme_minimal() + # escala del mapa de stringency
  labs(x="Fecha",y="ICA PM 2.5",
       title="Índice de Calidad del aire en Delhi, India",
       subtitle="ICA para material particulado menor a 2.5 micras por metro cúbico",
       color = "Índice de 
Calidad del Aire",
       caption=("aqicn.org")) + ###labels
  scale_color_gradientn(colours = c("#00FF00", "#FFFF00", "#FFFF00", "#FFA07A", "#FF0000", "#800080", "#800080", "#A0522D", "#A0522D", "#A0522D", "#A0522D", "#A0522D"),limits = c(0,500))+
  scale_y_continuous(limits=c(0,600)) + # change y axis scale
  geom_segment(data = Hitos_Delhi, mapping=aes(x=Fecha, y=Inicio, xend=Fecha, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = Hitos_Delhi, mapping=aes(x=Fecha,y=ymax,shape=Clase), size=2) + ###Agregar puntos de hitos
  new_scale_color() +
  geom_text(data = Hitos_Delhi, mapping=aes(x=Fecha, y=ymax, label=hitos_lab, color = Fase), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  scale_colour_manual(breaks=c("A-","B-","C-","D","C+","B+","A+"),
                      values=c('A-'="#FFD8A2", 'B-'="#FAB875", 'C-'="#E19438", 'D'="#C56F00",
                               'C+'="#A4C6FF", 'B+'="#40A2FF", 'A+'="#006BFF")) +
  guides(alpha=FALSE)


leyenda <- c('0-50'="#00FF00", '51-100'="#FFFF00",'101-150'="#FFA07A",'151-200'="#FF0000",'201-300'="#800080",'301-500'="#A0522D")
levels(ICA$Delhi) <- leyenda
scale_colour_manual(breaks=c("0-50","51-100","101-200","201-300","300+"),
                    values=c('0-50'="#00FF00", '51-100'="#FFFF00",'101-150'="#FFA07A",'151-200'="#FF0000",'201-300'="#800080",'301-900'="#A0522D")) +
###Configuración de escala cromática
pal <- choose_palette()
pal(6)
