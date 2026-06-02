## Gráfico de dona o Donut Chart**
  
##*********Programador: Alina Elizondo Sánchez************



##**************Instalación(si hace falta) y carga de paquetes****************************
#- tydyverse: manipulación y visualización de datos (ggplot2, dplyr, tidyr, etc.)
# -scales: formateo de etiquetas (porcentajes. moneda, escalas)
packages<- c("tidyverse", "scales")

to_install <-packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

library (tidyverse)
library(scales)


##************** Datos reales de conexión a internet en hogares**************



categorias <-c(
  "Chorotega",
  "Huetar Norte",
  "Pacífico Central",
  "Brunca"
)

#Porcentajes reales
porcentajes <- c(83.9, 91.7, 88.2, 82.6)/ 100

# Tabla base + cálculos
datos_salud <- tibble(
  categoria = categorias,
  porcentaje = porcentajes
  )


##************ Estilo de colores y tamaño del agujero***********

paleta <- c("#7FFF00","#2E86AB" ,"#FF7F24", "#9932CC")
radio_interno <- 0.40


##**********Construcción de la base de la dona************

base_dona <- ggplot(datos_salud, aes(x= 2, y = porcentaje, fill= categoria))+
  geom_col(width = 1, color = NA, show.legend = TRUE)+
  coord_polar(theta = "y")+
  xlim(0.5, 2.5)+
  theme_minimal(base_size= 12)+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.title= element_blank(),
    plot.title = element_text(face= "bold", size = 20, hjust= 0.5),
    plot.caption = element_text(color = "gray60", face = "bold", size= 16, hjust= 0.5),
    legend.position = "center"
  ) +
  scale_fill_manual(values = paleta)+
  ggtitle("Tenencia de internet en los hogares") +
  labs(caption = "Fuente: INEC") +
  guides(fill = guide_legend(ncol = 1))


##*********Creación del agujero y etiqueta central**********

base_dona <- base_dona +
  annotate ("rect",
            xmin= 0.5, xmax= 2 - radio_interno,
            ymin= -Inf, ymax= Inf,
            fill = "white", color = NA) +
  annotate ("text",
            x = 0.5 + (2 - 0.5)/2, y =0,
            label ="Total\n\100%", big.mark = ",", hjust= 0.5,
            color = "black", size = 6, fontface = "bold")

#Renderiza la dona base
base_dona


#**********Posiciones para etiquetas en el anillo************

datos_salud_lab <- datos_salud |>
  mutate (
    ymax= cumsum(porcentaje),
    ymin = lag(ymax, default= 0),
    ymid= (ymin= + ymax) / 2
  )


##******Gráfico de dona con etiquetas externas********

etiquetas_externas <- ggplot(datos_salud_lab, aes(x= 2, y = porcentaje, fill = categoria))+
  geom_col(width =1, color = NA)+
  coord_polar(theta = "y")+
  xlim (0.5, 2.5)+
  theme_void()+
  scale_fill_manual(values = paleta)+
  ggtitle ("Porcentaje de hogares con internet según región") +
           labs (caption = "Fuente:INEC") +
  annotate ("rect",
            xmin= 0.5, xmax= 2 - radio_interno,
            ymin= -Inf, ymax= Inf,
            fill = "white", color = NA )+
  geom_text(
    aes(label= percent(porcentaje, accuracy= 0.1)),position = position_stack(vjust= 0.5),
    size= 5, color = "gray10", fontface= "bold")+
  guides(fill= guide_legend(title = NULL, ncol=1))+
  theme(
    plot.title = element_text(hjust= 0,face= "bold", size= 20),
    plot.caption = element_text(hjust= 0.5, size= 16),
    plot.background = element_rect(fill= "white", color= NA),
    legend.text = element_text(size= 15)
  )

#Muestra la versión con etiquetas
etiquetas_externas


#*********Exporta los gráficos a archivos PNG************

ggsave("proyecto2.png", base_dona, width =8, height= 6, dpi=300)
ggsave("proyecto2_con_etiquetas.png", etiquetas_externas,width =8, height= 6, dpi=300 )
