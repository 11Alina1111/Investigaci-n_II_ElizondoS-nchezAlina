################################################################################
# Universidad Estatal a Distancia
# Carrera de Sistemas de Información en Salud
# Investigación en Sistemas de Información en Salud II(03576)
# TALLER OBLIGATORIO #4: ANÁLISIS DE DATOS CUANTITATIVOS Y SU PROCESAMIENTO
# Estudiante: Alina Elizondo Sánchez
################################################################################

# 1. Simulación de datos
set.seed(123) # fija una semilla para reproducibilidad

#==================================================================#
# 2. Análisis estadístico
n <- 300      # número de observaciones
mu <- 150      # media
sigma <- 100   # desviación estándar

datos <- rnorm(n, mean = mu, sd = sigma)

#==================================================================#
# 3. Visualización
library(ggplot2)
df <- data.frame()
ggplot(df, aes(x = datos)) +
  geom_density(fill = "lightgreen", alpha = 0.5  ) +
  labs(
    title = "Datos simulados",
    x = "Distribución",
    y = "Densidad"
  ) +
  theme_minimal()

#==============================================================================================#
cat('Interpretación: La forma de la distribución es una curva en forma de 
    campana, es decir, una distribución normal. Asimismo, es simétrica, ya que
    está centrada a la media de 150 y ambas colas son similares.  La relación
    entre la media de 150 y la mediana de n/2 (300/2) =150, indica que ambas son
    iguales, por lo que se trata de una distribución normal. Seguidamente, al 
    ser la desviación estándar de 100 y el promedio de 150, se demuestra que, 
    la desviación estándar en menor que el promedio, por lo que la concentración
    de los datos está cerca del promedio y el gráfico así lo demuestra.')

