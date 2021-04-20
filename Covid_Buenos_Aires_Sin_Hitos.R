#limpiar RAM y caché
gc()
rm(list=ls())

#Instalar Librerias
library(ggplot2)
library(dplyr)
library(readxl)

#Se cargan las bases de excel desde GitHub Desktop
##Cargar casos promedio semanales en Buenos Aires
BA <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                 sheet = "BA_cases", range = "E2:F60")
View(BA)

##Cargar stringency index para Argentina
stringency_index <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                     sheet = "BA_cases", range = "S2:T443")
View(stringency_index)

##Cargar hitos legislación y transporte en Argentinas y Buenos Aires
Hitos <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                    sheet = "BA_cases",range = "H2:L19")
View(Hitos)

##Cambio de nombres de columnas por practicidad
colnames(BA)=c("Semana","Casos")
colnames(stringency_index)=c("Dia","Index")
colnames(Hitos)=c("Fecha","Clase","Entidad","Hito","Altura")

#Ajustes de de base de hitos dataframe-gráfica
baseline = min(BA$Casos)
delta = 0.05 * diff(range(BA$Casos))
Hitos$ymin = baseline
Hitos$timelapse = c(diff(Hitos$Fecha),2020-03-01)
Hitos$bump = Hitos$timelapse < 2*370 #~2 años
offsets <- rle(Hitos$bump)
Hitos$offset <- unlist(mapply(function(l,v) {if(v){(l:1)+1}else{rep(1,l)}}, l=offsets$lengths, v=offsets$values, USE.NAMES=FALSE))
Hitos$ymax <- Hitos$Altura
Hitos$ymin + Hitos$offset * delta

#Realizar el Gráfico
##Crear el mapa base
base_line <- geom_line(data=BA,aes(Semana,Casos,alpha=0.8))

##Integrar Stringency Index de Oxford
stringency <- geom_bar(data=stringency_index,stat="identity",aes(Dia,max(BA$Casos),fill=Index))

##Integrando el gráfico
ggplot() + stringency + base_line + theme_classic() + scale_fill_viridis_c("Stringency Index
      Argentina",begin=0.4,end=1,option="magma",direction=-1,aesthetics = "fill") + # escala del mapa de stringency
  labs(x="Semana",y="Casos semanales promedio",
       title="Curva de contagios por COVID-19",
       subtitle="Casos promedio semanales en Buenos Aires, Argentina",
       caption=("Fuente de datos: buenosaires.gob.ar")) + ###labels
  scale_y_continuous(limits=c(-3150,4250)) + # change y axis scale
  geom_segment(data = Hitos, mapping=aes(x=Fecha, y=ymin, xend=Fecha, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = Hitos, mapping=aes(x=Fecha,y=ymax), size=1.2) + ###Agregar puntos de hitos
  geom_text(data = Hitos, mapping=aes(x=Fecha, y=ymax, label=Hito), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  guides(alpha=FALSE)

##Ahora con google mobility
###Cargando la base
Google_mob <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                         sheet = "BA_cases", range = "S2:U454", col_types=c("date","skip","numeric"))
View(Google_mob)

###Integrar el indice de restricción de movilidad de Google para el caso de tiendas
mobility_index <- geom_bar(data=Google_mob,stat="identity",aes(Dia,max(BA$Casos),fill=Google_index))

###Integrando el gráfico
ggplot() + mobility_index + base_line + theme_classic() + scale_fill_viridis_c("Google mobility Index
        Buenos Aires",begin=0.4,end=1,option="magma",aesthetics = "fill") + # escala del mapa de stringency
  labs(x="Semana",y="Casos semanales promedio",
       title="Curva de contagios por COVID-19",
       subtitle="Casos promedio semanales en Buenos Aires, Argentina",
       caption=("Fuente de datos: buenosaires.gob.ar")) + ###labels
  guides(alpha=FALSE)

