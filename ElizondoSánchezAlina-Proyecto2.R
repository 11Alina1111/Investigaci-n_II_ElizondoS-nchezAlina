#**************************************************************
# Universidad Estatal a Distancia
# Carrera de Sistemas de Información en Salud
# 03576 Investigación en Sistemas de Información en Salud II
#Proyecto 2: Análisis cuantitativo de datos e interpretación 
#de resultados en sistemas de información en salud 

#**************************************************************
##*********Estudiante y programadora: Alina Elizondo Sánchez***
##*************************************************************

#######################################################################
#######################################################################
#Objetivo 1. Identificar los principales logros que se han obtenido
#con la implementación de la telemedicina en las regiones socioeconómicas 
#Chorotega, Brunca, Pacífico Central y Huetar Norte de Costa Rica
#######################################################################
#######################################################################
####################################################################
####################################################################
####################################################################
#Gráfico 1. Porcentaje de teleconsultas por Direcciones de Red 
# Integrada de prestación de Servicios de Salud Año 2020
# Teleconsulta (teleconsultado),por Direcciones de Red Integrada de 
# Prestación de Servicios de Salud Año 2020

#Indicador:Acceso geográfico

##**************Instalación(si hace falta) y carga de paquetes*****************
#- tydyverse: manipulación y visualización de datos (ggplot2, dplyr, tidyr, etc.)
# -scales: formateo de etiquetas (porcentajes. moneda, escalas)
packages<- c("tidyverse", "scales", "RColorBrewer")

to_install <-packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

library (tidyverse)
library(scales)
library(RColorBrewer)

##************** Porcentaje de teleconsultas**************
teleconsultas <- c(381,  1307 ,784, 1899, 1312)
regiones <- c("Huetar Norte ", "Chorotega","Pacífico Central", "Brunca", "Otras regiones")

#Porcentajes 
porcentajes <- teleconsultas /sum(teleconsultas)

# Tabla base + cálculos
datos_salud <- tibble(
  categoria = regiones,
  porcentaje = porcentajes
)

##************ Estilo de colores y tamaño del agujero***********
radio_interno <- 0.40

#**********Posiciones para etiquetas en el anillo************

datos_salud_lab <- datos_salud |>
  mutate (
    ymax= cumsum(porcentaje),
    ymin = lag(ymax, default= 0),
    ymid= (ymin + ymax) / 2
  )


##******Gráfico de dona con etiquetas externas********

etiquetas_externas <- ggplot(datos_salud, aes(x=2, y=porcentaje, fill=categoria)) +
  geom_col(width=1, color=NA) + coord_polar(theta="y") + xlim(0.5, 2.5) +
  theme_void() +scale_fill_brewer(palette="GnBu") +
  ggtitle ("Porcentaje de teleconsulta por Direcciones de Red Integrada de 
prestación de Servicios de Salud en el 2020") +
  labs (caption = "Fuente:CCSS(2021)") +
  annotate ("rect",
            xmin= 0.5, xmax= 2 - radio_interno,
            ymin= -Inf, ymax= Inf,
            fill = "white", color = NA )+
  annotate("text", x= 1.5, y=0, label= "\n\nTotal\n100%", hjust= 0.5, color= "black", 
           fontface= "bold")+
  geom_text(
    aes(label= percent(porcentaje, accuracy= 0.1)), 
    position = position_stack(vjust= 0.5),
    size= 5, color = "gray10", fontface= "bold")+
  guides(fill= guide_legend(title = NULL, ncol=1))+
  theme(
    plot.title = element_text(hjust= 0.5,face= "bold", size= 20),
    plot.caption = element_text(hjust= 0.5, size= 16),
    plot.background = element_rect(fill= "white", color= NA),
    legend.text = element_text(size= 15)
  )

#Muestra la versión con etiquetas
etiquetas_externas


#*********Exporta los gráficos a archivos PNG************
ggsave("grafico2s.png", etiquetas_externas,width =8, height= 6, dpi=300 )


#*********Exporta los gráficos a archivos PNG************
ggsave("grafico2s.png", etiquetas_externas,width =8, height= 6, dpi=300 )
########################################################################
################################################################################
################################################################################
################################################################################
################################################################################

#Objetivo 2.Identificar las brechas y limitantes que impiden la expansión de la 
#telemedicina en las regiones socioeconómicas Chorotega, Brunca, Pacífico 
#Central y Huetar Norte de Costa Rica.

##############################################################################################
#Gráfico 2. Porcentaje de hogares sin internet según región socioeconómica
#en estudio en el 2024
#Indicador: Conectividad
#Datos
Regiones <- c("Chorotega", "Huetar Norte", "Pacífico Central", "Brunca")
porcentajes <- c(23.3, 22.4, 19.1, 16.3)

df <- data.frame(Regiones, porcentajes)


ggplot(df, aes(x=Regiones, y=porcentajes, fill=Regiones)) +
  geom_bar(stat="identity") +
  geom_text (aes(label = paste0(porcentajes ,"%")), vjust= -0.2, size= 8, fontface= "bold")+
  labs(title="Porcentaje de hogares sin internet según región sociecómica en estudio en el 2024",
       x="Regiones en estudio", y="Porcentaje (%)") +
  scale_fill_brewer(palette= "GnBu" )+
  theme_classic() +
  theme(
    plot.title = element_text(face="bold", size=16),
    axis.title.x = element_text(face="bold", size=18),
    axis.title.y = element_text(face="bold", size=18),
    axis.text = element_text(size=18),
    legend.text = element_text(size=18),
    legend.title = element_text(face= "bold",size=18)
  )

####################################################################
####################################################################
############################################################################
#Gráfico 3. Evolución de la conexión a Internet en Costa Rica
#Indicador: Conectividad
#Datos
años <- c(2000, 2021, 2024)
porcentajes <- c(5.8, 82.8, 85.4)

#Gráfico de líneas
library(ggplot2)

df <- data.frame(años, porcentajes)

ggplot(df, aes(x=años, y=porcentajes)) +
  geom_line(color="blue") +
  geom_point(shape=20, color="red") +
  geom_text(aes(label=paste0(porcentajes, "%")), vjust= 0.5, size= 7, fontface= "bold") +
  scale_x_continuous(breaks=años) +
  labs(title="Evolución de la conexión a Internet en Costa Rica",
       x="Año", y="Porcentaje de población (%)") +
  ylim(0,100)+
  theme_classic()+
  theme(
    plot.title = element_text(face="bold",size=18), 
    axis.title.x = element_text(face="bold",size=18), 
    axis.title.y = element_text(face="bold",size=18), 
    axis.text.x =  element_text(face="bold",size=18), 
    axis.text.y =  element_text(face="bold",size=18)
  )
################################################################################
################################################################################
#Objetivo 3. Determinar la percepción de los usuarios que habitan las regiones 
#socioeconómicas Chorotega, Brunca, Pacífico Central y Huetar Norte de 
#Costa Rica con respecto a la accesibilidad y satisfacción brindada por el 
#servicio de telemedicina.
################################################################################
################################################################################
#Gráfico 4.
library(dplyr)
#Desactivar notación cinetífica
options(scipen =999)
#Datos
datos <- data.frame(
  Categoria = rep(c("Chorotega", "Pacífico Central", "Brunca", "Huetar Norte"), each = 3),
  Tipo_Internet  = rep(c("Por dispositivos móviles", "Fibra óptica", "Por cable coaxial"), times = 4),
  Valor = c(55393, 49836,46782,48813,30689, 51695, 78055, 470333, 31864,111202, 57975, 37381)
)

#Porcentajes
datos <- datos %>%
  group_by(Categoria) %>%
  mutate(Porcentaje = Valor/ sum(Valor) * 100)

#Gráfico
ggplot(datos, aes(x = Categoria, y = Valor, fill = Tipo_Internet)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Porcentaje,1), "%")), 
            position = position_stack(vjust = 0.5), size = 6, color = "black") +
  scale_fill_brewer(palette = "GnBu") +
  labs(
    title ="Acceso a internet según región y tipo de conexión",
    x = "Región",
    y = "Cantidad"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),   
    axis.title.x = element_text(size = 18, face = "bold"), 
    axis.title.y = element_text(size = 18, face = "bold"), 
    axis.text.x  = element_text(size = 18, angle = 45, hjust=1),
    axis.text.y  = element_text(size = 18),                
    legend.text  = element_text(size = 18),     
    legend.title = element_text(size = 18, face = "bold") 
)















#Graficos extras del Objetivo #1
#Distribución de frecuencias de consultas virtuales en la atención de pacientes
#crónicos del servicio de salud del Instituto Costarricense de Electricidad (ICE)

#Cantidad de consultas
cantidad <- c(0,1,2,3,4,5,6,7,8,9,10,12,15,20,"Más de 2","No sabe/NR")

#Frecuencia absoluta
frec_absoluta <- c(10,85,97,67,32,15,9,5,2,7,4,2,2,1,8,4)

#Frecuencia porcentual
frec_porcentual <- c(2.8,24.2,27.7,19.1,9.1,4.2,2.5,1.4,0.5,2.0,1.1,0.5,0.5,0.2,2.2,1.14)

#Dataframe
df <- data.frame(Cantidad = cantidad, Frecuencia = frec_absoluta, Porcentaje = frec_porcentual)

#Grafico de barras
grafico <- barplot(df$Frecuencia, names.arg = df$Cantidad, col = "lightblue",
                   #        main = "Distribución de frecuencias de consultas virtuales en la atención de pacientes crónicos del servicio de salud del Instituto Costarricense de Electricidad (ICE)",
                   xlab = "Cantidad de consultas virtuales",
                   ylab = "Frecuencia absoluta",
                   las = 2)

#Etiquetas con los porcentajes 
text( grafico,  frec_porcentual + 8, labels = paste0(frec_porcentual, "%"), cex = 0.8)

###############################################################################
#Porcentaje de teleconsultas por año en pacientes de Talamanca, Puriscal y
# Ciudad Neily recibieron atención en el Centro Nacional de Rehabilitación 
# (Cenare) a través de la telemedicina

# Datos
porcentajes <- c(356, 7911)
categorias <- c("Telemedicina", "Horario vespertino")

#Etiquetas
etiquetas <- paste(categorias, paste0(porcentajes, "%"))

#Grafico
pie(porcentajes, labels = etiquetas, col=c("orange","blue"),
    main="Distribución de pacientes atendidos en Talamanca, Puriscal y Ciudad Neily en el CENARE en el 2020")

###############################################################################

#Porcentaje de citas realizadas mediante teleconsulta en la CCSS durante el 2018-2019

#Datos
años <- c(2018, 2019)
valores <- c(14.664, 13.418)

#Gráfico
plot(años, valores, type="o", col="red",
     main="Porcentaje de citas realizadas mediante teleconsulta en la CCSS durante el 2018-2019 en Costa Rica",
     xlab="Año", ylab="Cantidad de citas",
     ylim=c(0,100), pch=16)

# Añadir etiquetas
text(años, valores + 5, labels = valores)


# Gráfico 2.  Distribuciónde de las Áreas de Salud con mayor representatividad de teleconsultas
#Puriscal-Turrubares en el 2017
#Indicador: Cobertura

library(ggplot2)

# Datos 
Áreas <- c("Talamanca", "Siquirres", "Puriscal-Turrubares")
teleconsultas <- c(1246, 1194,972)

df <- data.frame(Áreas, teleconsultas)

ggplot(df, aes(x=Áreas, y=teleconsultas, fill=Áreas)) +
  geom_bar(stat="identity") +
  geom_text (aes(label = teleconsultas), vjust= 0.5, size= 8, fontface= "bold")+
  labs(title="Distribuciónde de las Áreas de Salud con mayor representatividad de teleconsultas") +
  scale_fill_brewer(palette= "GnBu" )+
  theme_classic() +
  theme(
    plot.title = element_text(face="bold", size=16),
    axis.title.x = element_text(face="bold", size=16),
    axis.title.y = element_text(face="bold", size=16),
    axis.text = element_text(size=16),
    legend.text = element_text(size=16),
    legend.title = element_text(face= "bold",size=16)
  )