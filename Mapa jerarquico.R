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
           "IA Salud", "Aplicaciones clínicas", "Aplicaciones administrativas",
           "Aprendizaje Automático", "Modelos predictivos", "Procesamiento de lenguaje natural",
           "Ética y Gobernanza", "Transparencia", "Regulaciones", "Sesgos algoritmicos",
           "Impacto en SIS", "Optimización de procesos", "Calidad asistencial", "Innovación en salud digital")
)

# Definir relaciones jerárquicas (aristas)
aristas <- data.frame(
from=c("Marco Teórico", "Marco Teórico", "Marco Teórico", "Marco Teórico",
       "IA Salud", "IA Salud",
       "Aprendizaje Automático", "Aprendizaje Automático",
       "Ética y Gobernanza", "Ética y Gobernanza", "Ética y Gobernanza",
       "Impacto en SIS", "Impacto en SIS", "Impacto en SIS"),

to = c("IA Salud", "Aprendizaje Automático", "Ética y Gobernanza", "Impacto en SIS",
       "Aplicaciones clínicas", "Aplicaciones administrativas",
       "Modelos predictivos", "Procesamiento de lenguaje natural",
       "Transparencia", "Regulaciones", "Sesgos algoritmicos",
       "Optimización de procesos", "Calidad asistencial", "Innovación en salud digital")
)
# Crear grafo
g<- graph_from_data_frame(aristas, vertices= nodos)

#Construir el gráfico
p <- ggraph (g, layout = "tree")+
  geom_edge_link(color= "orange")+
  geom_node_point (color= "green", size=8)+
  geom_node_text(aes(label=name), repel = TRUE, size = 6, shape = 20, fill = "green", stroke = 2)+
  theme_minimal()+
  ggtitle("Mapa jerárquico del Marco Téorico:Telemedicina") +
  theme(
    plot.title = element_text(size = 20, face = "bold")
    )

#Fortalecer visualización en VS Code
print(p)

