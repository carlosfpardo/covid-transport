#limpiar RAM y caché
gc()
rm(list=ls())

install.packages("ggnewscale")
install.packages('ggthemes', dependencies = TRUE)

#Instalar Librerias
library(ggplot2)
library(dplyr)
library(readxl)
library(viridis)
library(lubridate)
library("colorspace")
library(ggthemes)
library(ggnewscale)


#Para el caso de calidad del aire en París, Francia
#Se cargan las bases de excel desde GitHub Desktop
##Cargar calidad del aire para Paris
ICA_Paris <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                        sheet = "air_quality",
                        range = "A2:N2687",
                        col_types=c("date","numeric","numeric","numeric","skip","skip","text","text","text","skip","text","text","numeric","numeric"))
View(ICA_Paris)

##Cambio de nombres de columnas por practicidad
colnames(ICA_Paris)=c("Dia","Paris_25","Paris_10","moving_average_week_25","Ciudad","Clase","Fase","Hito","Link","Inicio","Altura")

#Ajustes de de base de hitos dataframe-gráfica
baseline = min(ICA_Paris$Paris_25)
delta = 0.05 * diff(range(ICA_Paris$Paris_25))
ICA_Paris$ymin = baseline
ICA_Paris$timelapse = c(diff(ICA_Paris$Dia),2014-01-01)
ICA_Paris$bump = ICA_Paris$timelapse < 9*366 #~9 años
offsets <- rle(ICA_Paris$bump)
ICA_Paris$offset <- unlist(mapply(function(l,v) {if(v){(l:1)+1}else{rep(1,l)}}, l=offsets$lengths, v=offsets$values, USE.NAMES=FALSE))
ICA_Paris$ymax <- ICA_Paris$Altura
ICA_Paris$ymin + ICA_Paris$offset * delta

str(ICA_Paris)
ICA_Paris$Dia <- as.Date(ICA_Paris$Dia)
ymd(ICA_Paris$Dia)
str(ICA_Paris$Dia)


#Crear el label de los hitos con fecha
hitos_lab <- paste(ICA_Paris$Hito,"\n",ICA_Paris$Dia,sep="")
my_caption <-expression(paste(italic(bold("Fuente de datos:")), italic("    ICA: "), "aqicn.org.", italic("    Eventos: "), "Varias fuentes."), size = 2)

#Realizar el Gráfico para Paris
##Crear el mapa base
base_line_Paris <- geom_line(data=ICA_Paris,aes(Dia,moving_average_week_25,colour=Paris_25), size = 1.8)

##Integrando el gráfico
ggplot() + base_line_Paris + theme_minimal () + # escala del mapa de stringency
  theme(axis.title.x = element_blank(),
        plot.title = element_text(family='',hjust = 0.5, size=26),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_text(face = "bold"),
        plot.margin = unit(c(1, 5, 1, 1), "lines")) +
  coord_cartesian(clip = "off") +
  scale_x_date(date_labels='\n%Y',
               date_breaks="year") +
  labs(y="ICA PM 2.5",
       title="Índice de Calidad del aire en Paris, Francia",
       subtitle="ICA para material particulado menor a 2.5 micras por metro cúbico",
       color = "     Índice de \nCalidad del Aire*",
       caption=(my_caption)) + ###labels
  annotate(geom="text", x=as.Date("2013-11-01"), y=85, size = 3.5,
           label="Umbral \nsegún UE", color = "red") +
  geom_text(mapping=aes(x=max(ICA_Paris$Dia), y=0, label="       *La altura de la curva está
representada por el promedio de 7 días,
  mientras que el color hace referencia
          al valor diario del ICA.",face = "italic"), hjust=-0.5, vjust=-3.2, size=3)+  
  scale_color_gradientn(colours = c("#00FF00", "#FFFF00", "#FFFF00", "#FFA07A", "#FF0000", "#800080", "#800080", "#A0522D", "#A0522D", "#A0522D", "#A0522D", "#A0522D"),limits = c(0,500))+
  scale_y_continuous(expand  = c(0,0),limits=c(0,200)) + # change y axis scale
  geom_segment(data = ICA_Paris, mapping=aes(x=Dia, y=Inicio, xend=Dia, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = ICA_Paris, mapping=aes(x=Dia,y=ymax,shape=Clase), size=2) + ###Agregar puntos de hitos
  geom_hline(yintercept=78.12875536, color="red", size=.9) +
  scale_shape_discrete(name = "Tipo de evento", labels = c("General", "Transporte", "")) +
  new_scale_color() +
  geom_text(data = ICA_Paris, mapping=aes(x=Dia, y=ymax, label=hitos_lab, color = Fase), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  scale_colour_manual(breaks=c("A-","B-","C-","D","C+","B+","A+"),
                      values=c('A-'="#FFD8A2", 'B-'="#FAB875", 'C-'="#E19438", 'D'="#C56F00",
                               'C+'="#A4C6FF", 'B+'="#40A2FF", 'A+'="#006BFF")) +
  guides(alpha=FALSE, color=FALSE)

##Gráfico Closeup 2016-2018
ggplot() + base_line_Paris + theme_minimal () + # escala del mapa de stringency
  theme(axis.title.x = element_blank(),
        plot.title = element_text(family='',hjust = 0.5, size=26),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_text(face = "bold"),
        plot.margin = unit(c(1, 5, 1, 1), "lines")) +
  coord_cartesian(clip = "off") +
  scale_x_date(date_labels='\n%Y',
               date_breaks="year",
               limits = c(as.Date("2016-01-01"),as.Date("2019-01-01"))) +
  labs(y="ICA PM 2.5",
       title="Índice de Calidad del aire en Paris, Francia\nClose-up años 2016-2018",
       subtitle="ICA para material particulado menor a 2.5 micras por metro cúbico",
       color = "     Índice de \nCalidad del Aire*",
       caption=(my_caption)) + ###labels
  annotate(geom="text", x=as.Date("2016-01-01"), y=85, size = 3.5,
           label="Umbral \nsegún UE", color = "red") +
  geom_text(mapping=aes(x=as.Date("2019-01-01"), y=0, label="       *La altura de la curva está
representada por el promedio de 7 días,
  mientras que el color hace referencia
          al valor diario del ICA.",face = "italic"), hjust=-0.5, vjust=-2.7, size=3.2)+  
  scale_color_gradientn(colours = c("#00FF00", "#FFFF00", "#FFFF00", "#FFA07A", "#FF0000", "#800080", "#800080", "#A0522D", "#A0522D", "#A0522D", "#A0522D", "#A0522D"),limits = c(0,500))+
  scale_y_continuous(expand  = c(0,0),limits=c(0,200)) + # change y axis scale
  geom_segment(data = ICA_Paris, mapping=aes(x=Dia, y=Inicio, xend=Dia, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = ICA_Paris, mapping=aes(x=Dia,y=ymax,shape=Clase), size=2) + ###Agregar puntos de hitos
  geom_hline(yintercept=78.12875536, color="red", size=.9) +
  scale_shape_discrete(name = "Tipo de evento", labels = c("General", "Transporte", "")) +
  new_scale_color() +
  geom_text(data = ICA_Paris, mapping=aes(x=Dia, y=ymax, label=hitos_lab, color = Fase), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  scale_colour_manual(breaks=c("A-","B-","C-","D","C+","B+","A+"),
                      values=c('A-'="#FFD8A2", 'B-'="#FAB875", 'C-'="#E19438", 'D'="#C56F00",
                               'C+'="#A4C6FF", 'B+'="#40A2FF", 'A+'="#006BFF")) +
  guides(alpha=FALSE, color=FALSE)

#Para la gráfica con una sola variable
#Se cargan las bases de excel desde GitHub Desktop
##Cargar calidad del aire para Paris
ICA_Paris <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                        sheet = "air_quality",
                        range = "A2:N2687",
                        col_types=c("date","numeric","numeric","numeric","skip","skip","text","text","text","skip","text","text","numeric","numeric"))
View(ICA_Paris)

##Cambio de nombres de columnas por practicidad
colnames(ICA_Paris)=c("Dia","Paris_25","Paris_10","moving_average_week_25","Ciudad","Clase","Fase","Hito","Link","Inicio","Altura")

#Ajustes de de base de hitos dataframe-gráfica
baseline = min(ICA_Paris$Paris_25)
delta = 0.05 * diff(range(ICA_Paris$Paris_25))
ICA_Paris$ymin = baseline
ICA_Paris$timelapse = c(diff(ICA_Paris$Dia),2014-01-01)
ICA_Paris$bump = ICA_Paris$timelapse < 9*366 #~9 años
offsets <- rle(ICA_Paris$bump)
ICA_Paris$offset <- unlist(mapply(function(l,v) {if(v){(l:1)+1}else{rep(1,l)}}, l=offsets$lengths, v=offsets$values, USE.NAMES=FALSE))
ICA_Paris$ymax <- ICA_Paris$Altura
ICA_Paris$ymin + ICA_Paris$offset * delta

str(ICA_Paris)
ICA_Paris$Dia <- as.Date(ICA_Paris$Dia)
ymd(ICA_Paris$Dia)
str(ICA_Paris$Dia)

#Crear el label de los hitos con fecha
hitos_lab <- paste(ICA_Paris$Hito,"\n",ICA_Paris$Dia,sep="")
my_caption <-expression(paste(italic(bold("Fuente de datos:")), italic("    ICA: "), "aqicn.org.", italic("    Eventos: "), "Varias fuentes."), size = 2)

#Realizar el Gráfico para Paris
##Crear el mapa base
base_line_Paris <- geom_line(data=ICA_Paris,aes(Dia,moving_average_week_25,colour=moving_average_week_25), size = 1.8)

##Integrando el gráfico
ggplot() + base_line_Paris + theme_minimal () + # escala del mapa de stringency
  theme(axis.title.x = element_blank(),
        plot.title = element_text(family='',hjust = 0.5, size=26),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_text(face = "bold"),
        plot.margin = unit(c(1, 5, 1, 1), "lines")) +
  coord_cartesian(clip = "off") +
  scale_x_date(date_labels='\n%Y',
               date_breaks="year") +
  labs(y="ICA PM 2.5",
       title="Índice de Calidad del aire en Paris, Francia",
       subtitle="ICA para material particulado menor a 2.5 micras por metro cúbico",
       color = "     Índice de \nCalidad del Aire*",
       caption=(my_caption)) + ###labels
  annotate(geom="text", x=as.Date("2013-11-01"), y=85, size = 3.5,
           label="Umbral \nsegún UE", color = "red") +
  geom_text(mapping=aes(x=max(ICA_Paris$Dia), y=0, label="       *La altura de la curva está
representada por el promedio de 7 días,
  mientras que el color hace referencia
          al valor diario del ICA.",face = "italic"), hjust=-0.5, vjust=-3.2, size=3)+  
  scale_color_gradientn(colours = c("#00FF00", "#FFFF00", "#FFFF00", "#FFA07A", "#FF0000", "#800080", "#800080", "#A0522D", "#A0522D", "#A0522D", "#A0522D", "#A0522D"),limits = c(0,500))+
  scale_y_continuous(expand  = c(0,0),limits=c(0,200)) + # change y axis scale
  geom_segment(data = ICA_Paris, mapping=aes(x=Dia, y=Inicio, xend=Dia, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = ICA_Paris, mapping=aes(x=Dia,y=ymax,shape=Clase), size=2) + ###Agregar puntos de hitos
  geom_hline(yintercept=78.12875536, color="red", size=.9) +
  scale_shape_discrete(name = "Tipo de evento", labels = c("General", "Transporte", "")) +
  new_scale_color() +
  geom_text(data = ICA_Paris, mapping=aes(x=Dia, y=ymax, label=hitos_lab, color = Fase), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  scale_colour_manual(breaks=c("A-","B-","C-","D","C+","B+","A+"),
                      values=c('A-'="#FFD8A2", 'B-'="#FAB875", 'C-'="#E19438", 'D'="#C56F00",
                               'C+'="#A4C6FF", 'B+'="#40A2FF", 'A+'="#006BFF")) +
  guides(alpha=FALSE, color=FALSE)





#Para el caso de calidad del aire en Delhi, India
#Se cargan las bases de excel desde GitHub Desktop
##Cargar calidad del aire para Paris
ICA_Delhi <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                        sheet = "air_quality",
                        range = "P2:AC1385",
                        col_types=c("date","numeric","numeric","numeric","skip","skip","text","text","text","skip","text","text","numeric","numeric"))
View(ICA_Delhi)

##Cambio de nombres de columnas por practicidad
colnames(ICA_Delhi)=c("Dia","Delhi_25","Delhi_10","moving_average_week_25","Ciudad","Clase","Fase","Hito","Link","Inicio","Altura")

#Ajustes de de base de hitos dataframe-gráfica
baseline = min(ICA_Delhi$Delhi_25)
delta = 0.05 * diff(range(ICA_Delhi$Delhi_25))
ICA_Delhi$ymin = baseline
ICA_Delhi$timelapse = c(diff(ICA_Delhi$Dia),2014-01-01)
ICA_Delhi$bump = ICA_Delhi$timelapse < 9*366 #~9 años
offsets <- rle(ICA_Delhi$bump)
ICA_Delhi$offset <- unlist(mapply(function(l,v) {if(v){(l:1)+1}else{rep(1,l)}}, l=offsets$lengths, v=offsets$values, USE.NAMES=FALSE))
ICA_Delhi$ymax <- ICA_Delhi$Altura
ICA_Delhi$ymin + ICA_Delhi$offset * delta

str(ICA_Delhi)
ICA_Delhi$Dia <- as.Date(ICA_Delhi$Dia)
ymd(ICA_Delhi$Dia)
str(ICA_Delhi$Dia)

#Crear el label de los hitos con fecha
hitos_lab <- paste(ICA_Delhi$Hito,"\n",ICA_Delhi$Dia,sep="")
my_caption <-expression(paste(italic(bold("Fuente de datos:")), italic("    ICA: "), "aqicn.org.", italic("    Eventos: "), "Varias fuentes."), size = 2)

#Realizar el Gráfico para Delhi
##Crear el mapa base
base_line_Delhi <- geom_line(data=ICA_Delhi,aes(Dia,moving_average_week_25,colour=Delhi_25), size = 1.8)

##Integrando el gráfico
ggplot() + base_line_Delhi + theme_minimal () + # escala del mapa de stringency
  theme(axis.title.x = element_blank(),
        plot.title = element_text(family='',hjust = 0.5, size=26),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_text(face = "bold"),
        plot.margin = unit(c(1, 5, 1, 1), "lines")) +
  coord_cartesian(clip = "off") +
  scale_x_date(date_labels='\n%Y',
               date_breaks="year") +
  labs(y="ICA PM 2.5",
        title="Índice de Calidad del aire en Delhi, India",
        subtitle="ICA para material particulado menor a 2.5 micras por metro cúbico",
        color = "    Índice de\nCalidad del Aire*",
        caption=(my_caption)) + ###labels
  annotate(geom="text", x=as.Date("2021-10-01"), y=275, size = 3.5,
           label="Umbral según\nGobierno de India", color = "red") +
  geom_text(mapping=aes(x=max(ICA_Delhi$Dia), y=0, label="         *La altura de la curva está
representada por el promedio de 7 días,
  mientras que el color hace referencia
            al valor diario del ICA.",face = "italic"), hjust=-0.7, vjust=-3.8, size=3)+  
  scale_color_gradientn(colours = c("#00FF00", "#FFFF00", "#FFFF00", "#FFA07A", "#FF0000", "#800080", "#800080", "#A0522D", "#A0522D", "#A0522D", "#A0522D", "#A0522D"),limits = c(0,500))+
  scale_y_continuous(expand  = c(0,0),limits=c(0,600)) + # change y axis scale
  geom_segment(data = ICA_Delhi, mapping=aes(x=Dia, y=Inicio, xend=Dia, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = ICA_Delhi, mapping=aes(x=Dia,y=ymax,shape = Clase), size=2) + ###Agregar puntos de hitos
  geom_hline(yintercept=250, color="red", size=.9) +
  scale_shape_discrete(name = "Tipo de evento", labels = c("General", "Transporte", "")) +
  new_scale_color() +
  geom_text(data = ICA_Delhi, mapping=aes(x=Dia, y=ymax, label=hitos_lab, color = Fase), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  scale_colour_manual(breaks=c("A-","B-","C-","D","C+","B+","A+"),
                      values=c('A-'="#FFD8A2", 'B-'="#FAB875", 'C-'="#E19438", 'D'="#C56F00",
                               'C+'="#A4C6FF", 'B+'="#40A2FF", 'A+'="#006BFF")) +
  guides(alpha=FALSE, color=FALSE)

#Gráfico close up 2018-2019
ggplot() + base_line_Delhi + theme_minimal () + # escala del mapa de stringency
  theme(axis.title.x = element_blank(),
        plot.title = element_text(family='',hjust = 0.5, size=26),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_text(face = "bold"),
        plot.margin = unit(c(1, 5, 1, 1), "lines"),
        legend.box.background = element_rect(color="transparent", size=0.1),
        legend.box.margin = margin(40, 6, 6, 6)) +
  coord_cartesian(clip = "off") +
  scale_x_date(date_labels='\n%Y',
               date_breaks="year",
               limits = c(as.Date("2018-01-01"),as.Date("2020-01-01"))) +
  labs(y="ICA PM 2.5",
       title="Índice de Calidad del aire en Delhi, India",
       subtitle="ICA para material particulado menor a 2.5 micras por metro cúbico",
       color = "    Índice de\nCalidad del Aire*",
       caption=(my_caption)) + ###labels
  annotate(geom="text", x=as.Date("2018-01-01"), y=275, size = 3.5,
           label="Umbral según\nGobierno de India", color = "red") +
  geom_text(mapping=aes(x=as.Date("2020-01-01"), y=0, label="         *La altura de la curva está
   representada por el promedio de 7 días,
    mientras que el color hace referencia
            al valor diario del ICA.",face = "italic"), hjust=-0.4, vjust=-2.5, size=3)+  
  scale_color_gradientn(colours = c("#00FF00", "#FFFF00", "#FFFF00", "#FFA07A", "#FF0000", "#800080", "#800080", "#A0522D", "#A0522D", "#A0522D", "#A0522D", "#A0522D"),limits = c(0,500))+
  scale_y_continuous(expand  = c(0,0),limits=c(0,600)) + # change y axis scale
  geom_segment(data = ICA_Delhi, mapping=aes(x=Dia, y=Inicio, xend=Dia, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = ICA_Delhi, mapping=aes(x=Dia,y=ymax,shape = Clase), size=2) + ###Agregar puntos de hitos
  geom_hline(yintercept=250, color="red", size=.9) +
  scale_shape_discrete(name = "Tipo de evento", labels = c("General", "Transporte", "")) +
  new_scale_color() +
  geom_text(data = ICA_Delhi, mapping=aes(x=Dia, y=ymax, label=hitos_lab, color = Fase), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  scale_colour_manual(breaks=c("A-","B-","C-","D","C+","B+","A+"),
                      values=c('A-'="#FFD8A2", 'B-'="#FAB875", 'C-'="#E19438", 'D'="#C56F00",
                               'C+'="#A4C6FF", 'B+'="#40A2FF", 'A+'="#006BFF")) +
  guides(alpha=FALSE, color=FALSE)


#Para el caso de gráfica con una sola variable
#Se cargan las bases de excel desde GitHub Desktop
##Cargar calidad del aire para Paris
ICA_Delhi <- read_excel("~/GitHub/covid-transport/Bogota_daily_cases.xlsx", 
                        sheet = "air_quality",
                        range = "P2:AC1385",
                        col_types=c("date","numeric","numeric","numeric","skip","skip","text","text","text","skip","text","text","numeric","numeric"))
View(ICA_Delhi)

##Cambio de nombres de columnas por practicidad
colnames(ICA_Delhi)=c("Dia","Delhi_25","Delhi_10","moving_average_week_25","Ciudad","Clase","Fase","Hito","Link","Inicio","Altura")

#Ajustes de de base de hitos dataframe-gráfica
baseline = min(ICA_Delhi$Delhi_25)
delta = 0.05 * diff(range(ICA_Delhi$Delhi_25))
ICA_Delhi$ymin = baseline
ICA_Delhi$timelapse = c(diff(ICA_Delhi$Dia),2014-01-01)
ICA_Delhi$bump = ICA_Delhi$timelapse < 9*366 #~9 años
offsets <- rle(ICA_Delhi$bump)
ICA_Delhi$offset <- unlist(mapply(function(l,v) {if(v){(l:1)+1}else{rep(1,l)}}, l=offsets$lengths, v=offsets$values, USE.NAMES=FALSE))
ICA_Delhi$ymax <- ICA_Delhi$Altura
ICA_Delhi$ymin + ICA_Delhi$offset * delta

str(ICA_Delhi)
ICA_Delhi$Dia <- as.Date(ICA_Delhi$Dia)
ymd(ICA_Delhi$Dia)
str(ICA_Delhi$Dia)

#Crear el label de los hitos con fecha
hitos_lab <- paste(ICA_Delhi$Hito,"\n",ICA_Delhi$Dia,sep="")
my_caption <-expression(paste(italic(bold("Fuente de datos:")), italic("    ICA: "), "aqicn.org.", italic("    Eventos: "), "Varias fuentes."), size = 2)

#Realizar el Gráfico para Delhi
##Crear el mapa base
base_line_Delhi <- geom_line(data=ICA_Delhi,aes(Dia,moving_average_week_25,colour=moving_average_week_25), size = 1.8)

##Integrando el gráfico
ggplot() + base_line_Delhi + theme_minimal () + # escala del mapa de stringency
  theme(axis.title.x = element_blank(),
        plot.title = element_text(family='',hjust = 0.5, size=26),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_text(face = "bold"),
        plot.margin = unit(c(1, 5, 1, 1), "lines")) +
  coord_cartesian(clip = "off") +
  scale_x_date(date_labels='\n%Y',
               date_breaks="year") +
  labs(y="ICA PM 2.5",
       title="Índice de Calidad del aire en Delhi, India",
       subtitle="ICA para material particulado menor a 2.5 micras por metro cúbico",
       color = "    Índice de\nCalidad del Aire*",
       caption=(my_caption)) + ###labels
  annotate(geom="text", x=as.Date("2021-10-01"), y=275, size = 3.5,
           label="Umbral según\nGobierno de India", color = "red") +
  geom_text(mapping=aes(x=max(ICA_Delhi$Dia), y=0, label="         *La altura de la curva está
representada por el promedio de 7 días,
  mientras que el color hace referencia
            al valor diario del ICA.",face = "italic"), hjust=-0.7, vjust=-3.8, size=3)+  
  scale_color_gradientn(colours = c("#00FF00", "#FFFF00", "#FFFF00", "#FFA07A", "#FF0000", "#800080", "#800080", "#A0522D", "#A0522D", "#A0522D", "#A0522D", "#A0522D"),limits = c(0,500))+
  scale_y_continuous(expand  = c(0,0),limits=c(0,600)) + # change y axis scale
  geom_segment(data = ICA_Delhi, mapping=aes(x=Dia, y=Inicio, xend=Dia, yend=ymax)) + ###Agregar lineas de Hitos
  geom_point(data = ICA_Delhi, mapping=aes(x=Dia,y=ymax,shape = Clase), size=2) + ###Agregar puntos de hitos
  geom_hline(yintercept=250, color="red", size=.9) +
  scale_shape_discrete(name = "Tipo de evento", labels = c("General", "Transporte", "")) +
  new_scale_color() +
  geom_text(data = ICA_Delhi, mapping=aes(x=Dia, y=ymax, label=hitos_lab, color = Fase), hjust=-0.01, vjust=0.5, size=2.8)+ ###Agregar leyendas de hitos
  scale_colour_manual(breaks=c("A-","B-","C-","D","C+","B+","A+"),
                      values=c('A-'="#FFD8A2", 'B-'="#FAB875", 'C-'="#E19438", 'D'="#C56F00",
                               'C+'="#A4C6FF", 'B+'="#40A2FF", 'A+'="#006BFF")) +
  guides(alpha=FALSE, color=FALSE)

#Auxiliares
leyenda <- c('0-50'="#00FF00", '51-100'="#FFFF00",'101-150'="#FFA07A",'151-200'="#FF0000",'201-300'="#800080",'301-500'="#A0522D")
levels(ICA$Delhi) <- leyenda
scale_colour_manual(breaks=c("0-50","51-100","101-200","201-300","300+"),
                    values=c('0-50'="#00FF00", '51-100'="#FFFF00",'101-150'="#FFA07A",'151-200'="#FF0000",'201-300'="#800080",'301-900'="#A0522D")) +
###Configuración de escala cromática
pal <- choose_palette()
pal(6)
