#Creación de mapa jerárquico

#Instalar librerías si no las tienes
#install.packages("tgraph")
#install.packages("ggraph")
#install.packages("ggplot2")

library(igraph)
library(ggraph)
library(ggplot2)

# Definir nodos (conceptos principales y secundartos)
nodos <- data.frame(
  name = c("Marco Teórico",
           "Teoría Aday y Anderson", "Disponibilidad de servicios", "Características de la población","Organización del sistema de salud",
           "Telesalud", "Teleconsulta", "Telecirugía","Telemonitoreo","Teleorientación",
           "Transformación digital", "TIC", "Interoperabilidad", "Salud digital","Teleeducación",
           "Atención al usuario",  "Brechas digitales", "Acesso","Experiencia del usuario")
)

# Definir relaciones jerárquicas (aristas)
aristas <- data.frame(
  from=c("Marco Teórico", "Marco Teórico", "Marco Teórico", "Marco Teórico",
         "Teoría Aday y Anderson", "Teoría Aday y Anderson","Teoría Aday y Anderson",
         "Telesalud", "Telesalud","Telesalud","Telesalud",
         "Transformación digital", "Transformación digital", "Transformación digital","Transformación digital",
         "Atención al usuario","Atención al usuario","Atención al usuario"),
  
  to = c("Teoría Aday y Anderson", "Telesalud", "Transformación digital", "Atención al usuario",
         "Disponibilidad de servicios", "Características de la población","Organización del sistema de salud",
          "Teleconsulta", "Telecirugía","Telemonitoreo","Teleorientación",
          "TIC", "Interoperabilidad", "Salud digital","Teleeducación",
          "Brechas digitales", "Acesso","Experiencia del usuario")
)

# Crear grafo
g<- graph_from_data_frame(aristas, vertices= nodos)

#Construir el gráfico
p <- ggraph (g, layout = "tree")+
  geom_edge_link(color= "orange")+
  geom_node_point (color= "green", size=14)+
  geom_node_text(aes(label=name), repel = TRUE, size = 6, shape = 20, fill = "green", stroke = 2)+
  theme_minimal()+
  ggtitle("Mapa jerárquico del Marco Téorico:Telemedicina en regiones fuera del GAM en Costa Rica") +
  theme(
    plot.title = element_text(size = 20, face= "bold")
  )

#Visualización
print(p)
      


