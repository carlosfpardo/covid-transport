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

#Se cargan las bases de excel desde GitHub Desktop
##Cargar casos promedio semanales en Buenos Aires
WDC <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                 sheet = "Washington_DC_cases", range = "E2:H60", col_types=c("date","numeric","numeric","numeric"))
View(WDC)

##Cargar hitos legislación y transporte en Washington DC y Estados Unidos
Hitos <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                    sheet = "Washington_DC_cases",range = "J2:N19", col_types=c("date","text","text","text","numeric"))
View(Hitos)

##Cambio de nombres de columnas por practicidad
colnames(WDC)=c("Dia","Casos","Index")
colnames(Hitos)=c("Fecha","Clase","Entidad","Hito","Altura")

#Ajustes de de base de hitos dataframe-gráfica
baseline = min(WDC$Casos)
delta = 0.05 * diff(range(WDC$Casos))
Hitos$ymin = baseline
Hitos$timelapse = c(diff(Hitos$Fecha),2020-03-01)
Hitos$bump = Hitos$timelapse < 2*370 #~2 años
offsets <- rle(Hitos$bump)
Hitos$offset <- unlist(mapply(function(l,v) {if(v){(l:1)+1}else{rep(1,l)}}, l=offsets$lengths, v=offsets$values, USE.NAMES=FALSE))
Hitos$ymax <- Hitos$Altura
Hitos$ymin + Hitos$offset * delta

#Realizar el Gráfico
##Crear el mapa base
base_line <- geom_line(data=WDC,aes(Dia,Casos,colour=Index), size = 2)

##Integrando el gráfico
ggplot() + base_line + theme_minimal() + # escala del mapa de stringency
  labs(x="Semana",y="Casos semanales promedio",
       title="Curva de contagios por COVID-19",
       subtitle="Casos promedio semanales en Washington D.C., EE.UU.",
       color = "Stringency Index
      Washington DC",
       caption=("Fuente de datos: coronavirus.dc.gov")) + ###labels
  scale_color_gradientn(colors = c("#ADF84E", "#CCEE18", "#E2DF00", "#EECB00", "#F2B300", "#EE9700", "#E37830", "#D15544", "#B9264E", "#A00052"))+
  scale_y_continuous(limits=c(-200,310)) + # change y axis scale
  geom_segment(data = Hitos, mapping=aes(x=Fecha, y=ymin, xend=Fecha, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = Hitos, mapping=aes(x=Fecha,y=ymax), size=1.2) + ###Agregar puntos de hitos
  geom_text(data = Hitos, mapping=aes(x=Fecha, y=ymax, label=Hito), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  guides(alpha=FALSE)

##Para el caso de muertes promedio semanales
#Se cargan las bases de excel desde GitHub Desktop
##Cargar casos promedio semanales en Buenos Aires
WDC <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                  sheet = "Washington_DC_cases",
                  range = "H2:K63",
                  col_types=c("date","numeric","numeric","numeric"))
View(WDC)

##Cargar hitos legislación y transporte en Washington DC y Estados Unidos
Hitos <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                    sheet = "Washington_DC_cases",
                    range = "N2:U20",
                    col_types=c("date","text","text","text","text","skip","numeric","numeric"))
View(Hitos)

##Cambio de nombres de columnas por practicidad
colnames(WDC)=c("Dia","Casos","Muertes","Index")
colnames(Hitos)=c("Fecha","Clase","Fase","Entidad","Hito","Inicio","Altura")
str(WDC)
str(Hitos)

#Ajustes de de base de hitos dataframe-gráfica
baseline = min(WDC$Muertes)
delta = 0.05 * diff(range(WDC$Muertes))
Hitos$ymin = baseline
Hitos$timelapse = c(diff(Hitos$Fecha),2020-03-01)
Hitos$bump = Hitos$timelapse < 2*370 #~2 años
offsets <- rle(Hitos$bump)
Hitos$offset <- unlist(mapply(function(l,v) {if(v){(l:1)+1}else{rep(1,l)}}, l=offsets$lengths, v=offsets$values, USE.NAMES=FALSE))
Hitos$ymax <- Hitos$Altura
Hitos$ymin + Hitos$offset * delta

#Realizar el Gráfico
##Crear el mapa base
base_line <- geom_line(data=WDC,aes(Dia,Muertes,colour=Index), size = 2)

###Crear el label de los hitos con fecha
hitos_lab <- paste(Hitos$Hito,"\n",Hitos$Fecha,sep="")

##Integrando el gráfico
ggplot() + base_line + theme_minimal() + # escala del mapa de stringency
  labs(x="Semana",y="Muertes semanales promedio",
       title="Curva de fallecidos por COVID-19",
       subtitle="Muertes promedio semanales en Washington D.C., EE.UU.",
       color = "Stringency Index
      Washington DC",
       caption=("Fuente de datos: coronavirus.dc.gov")) + ###labels
  scale_color_gradientn(colors = c("#ADF84E", "#CCEE18", "#E2DF00", "#EECB00", "#F2B300", "#EE9700", "#E37830", "#D15544", "#B9264E", "#A00052"))+
  scale_y_continuous(limits=c(-9,12)) + # change y axis scale
  geom_segment(data = Hitos, mapping=aes(x=Fecha, y=Inicio, xend=Fecha, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = Hitos, mapping=aes(x=Fecha,y=ymax,shape=Clase), size=2) + ###Agregar puntos de hitos
  new_scale_color() +
  geom_text(data = Hitos, mapping=aes(x=Fecha, y=ymax, label=hitos_lab, color = Fase), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  scale_colour_manual(breaks=c("A-","B-","C-","D","C+","B+","A+"),
                      values=c('A-'="#FFD8A2", 'B-'="#FAB875", 'C-'="#E19438", 'D'="#C56F00",
                               'C+'="#A4C6FF", 'B+'="#40A2FF", 'A+'="#006BFF")) +
  guides(alpha=FALSE)

##English version
###Cambio idioma
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")

#Se cargan las bases de excel desde GitHub Desktop
##Cargar casos promedio semanales en Buenos Aires
WDC <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                  sheet = "Washington_DC_cases",
                  range = "H2:K63",
                  col_types=c("date","numeric","numeric","numeric"))
View(WDC)

##Cargar hitos legislación y transporte en Washington DC y Estados Unidos
Hitos <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                    sheet = "Washington_DC_cases",
                    range = "X2:AE20",
                    col_types=c("date","text","text","text","text","skip","numeric","numeric"))
View(Hitos)

##Cambio de nombres de columnas por practicidad
colnames(WDC)=c("Dia","Casos","Muertes","Index")
colnames(Hitos)=c("Fecha","Clase","Fase","Entidad","Hito","Inicio","Altura")
str(WDC)
str(Hitos)

#Ajustes de de base de hitos dataframe-gráfica
baseline = min(WDC$Muertes)
delta = 0.05 * diff(range(WDC$Muertes))
Hitos$ymin = baseline
Hitos$timelapse = c(diff(Hitos$Fecha),2020-03-01)
Hitos$bump = Hitos$timelapse < 2*370 #~2 años
offsets <- rle(Hitos$bump)
Hitos$offset <- unlist(mapply(function(l,v) {if(v){(l:1)+1}else{rep(1,l)}}, l=offsets$lengths, v=offsets$values, USE.NAMES=FALSE))
Hitos$ymax <- Hitos$Altura
Hitos$ymin + Hitos$offset * delta

#Realizar el Gráfico
##Crear el mapa base
base_line <- geom_line(data=WDC,aes(Dia,Muertes,colour=Index), size = 2)

###Crear el label de los hitos con fecha
hitos_lab <- paste(Hitos$Hito,"\n",Hitos$Fecha,sep="")

##Integrando el gráfico
ggplot() + base_line + theme_minimal() + # escala del mapa de stringency
  labs(x="", y="Average weekly deaths reported",
       title="COVID-19 deaths, lockdown stringency and events(transport or general) in DC",
       subtitle="Weekly average deaths reported in Washington D.C., EE.UU.",
       color = "Stringency",
       caption=("Data source
                COVID-19 deaths: DC government
                Stringency: Oxford Stringency Index
                Events: several sources")) + ###labels
  scale_color_gradientn(colors = c("#ADF84E", "#CCEE18", "#E2DF00", "#EECB00", "#F2B300", "#EE9700", "#E37830", "#D15544", "#B9264E", "#A00052"))+
  scale_y_continuous(limits=c(-9,12)) + # change y axis scale
  geom_segment(data = Hitos, mapping=aes(x=Fecha, y=Inicio, xend=Fecha, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = Hitos, mapping=aes(x=Fecha,y=ymax,shape=Clase), size=2) + ###Agregar puntos de hitos
  new_scale_color() +
  geom_text(data = Hitos, mapping=aes(x=Fecha, y=ymax, label=hitos_lab, color = Fase), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  scale_colour_manual(breaks=c("A-","B-","C-","D","C+","B+","A+"),
                      values=c('A-'="#FFD8A2", 'B-'="#FAB875", 'C-'="#E19438", 'D'="#C56F00",
                               'C+'="#A4C6FF", 'B+'="#40A2FF", 'A+'="#006BFF")) +
  labs(shape = "Event type") +
  guides(alpha=FALSE)

##Ahora con google mobility
###Cambio idioma
Sys.setenv("LANGUAGE"="Es")
Sys.setlocale("LC_ALL", "Spanish")

###Cargando la base
Google_mob <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                         sheet = "Washington_DC_cases", range = "E2:H60", col_types=c("date","numeric","numeric","numeric"))
View(Google_mob)

###Cambio de nombres de columnas por practicidad
colnames(Google_mob)=c("Dia","Casos","Index","Google")

###Integrar el indice de restricción de movilidad de Google para el caso de tiendas
base_line <- geom_line(data=Google_mob,aes(Dia,Casos,colour=Google), size = 2)

###Integrando el gráfico
ggplot() + base_line + theme_classic() + 
  labs(x="Semana",y="Casos semanales promedio",
       title="Curva de contagios por COVID-19",
       subtitle="Casos promedio semanales en Washington D.C., EE.UU.",
       color = "Google mobility Index 
            Bogotá",
       caption=("Fuente de datos: coronavirus.dc.gov")) + ###labels
  scale_color_gradientn(colors = c("#A00052", "#B9264E", "#D15544", "#E37830", "#EE9700", "#F2B300", "#EECB00", "#E2DF00", "#CCEE18", "#ADF84E"))+
  scale_y_continuous(limits=c(0,310)) + # change y axis scale
  #geom_segment(data = Hitos, mapping=aes(x=Fecha, y=ymin, xend=Fecha, yend=ymax)) + ###Agregar lineas de Hitos
  #geom_point(data = Hitos, mapping=aes(x=Fecha,y=ymax), size=1.2) + ###Agregar puntos de hitos
  #geom_text(data = Hitos, mapping=aes(x=Fecha, y=ymax, label=Hito), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  guides(alpha=FALSE)

