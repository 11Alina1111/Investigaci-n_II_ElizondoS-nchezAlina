# Universidad Estatal a Distancia
# Carrera de Sistemas de Información en Salud
# 03576-3-2026 Investigación en Sistemas de Información en Salud II
# Práctica Formativa U3. Muestreo aleatorio simple y estratificado
# Programador: Lic. Iván M. Rodríguez Soriano
# Versión: 1.0 (2026/03/14)

#Estudiante: Alina Elizondo Sánchez

# Objetivos:
# 1) Simular una población con variables relevantes
# 2) Extraer una muestra aleatoria simple con sample()
# 3) Extraer una muestra estratificada con dplyr
# 4) Comparar composición de población y muestras

# 0) Preparación del entorno

# Instala dplyr si no lo tienes (ejecutar UNA sola vez)
# install.packages("dplyr")

library(dplyr)

# Función auxiliar para mostrar proporciones de forma cómoda
prop_tab <- function(x) round(prop.table(table(x)), 4)


# 1) Simulación de la población
# Simular una población de 1000 personas con las siguientes variables:
# id (único), edad (18-85), sexo (Femenino/Masculino), region (Urbana/Rural)

set.seed(123)  # Reproducibilidad

poblacion <- data.frame(
  id     = 1:1000,
  edad   = sample(18:85, 1000, replace = TRUE),
  sexo   = sample(c("Femenino", "Masculino"), 1000, replace = TRUE),
  region = sample(c("Urbana", "Rural"), 1000, replace = TRUE, prob = c(0.65, 0.35))
)

cat('Población de estudio')
str(poblacion)

cat("\nPoblación total")
print(prop_tab(poblacion$region))
print(prop_tab(poblacion$sexo))
print(mean(poblacion$edad))

################################################################################
# 2) Muestreo aleatorio simple (MAS)
# Extraer una muestra aleatoria simple de n = 100 usando sample()

set.seed(456)

muestra_simple <- poblacion[
  sample(nrow(poblacion), size = 100),
]

cat("Edad promedio población")
print(mean(poblacion$edad))

###############################################################################
cat("\na. Verifique el tamaño de muestra. \n")
nrow(muestra_simple)

"\nb. Calcule la distribución de sexo y región."
cat("\na.Distribución (muestra simple) - región:\n")
print(prop_tab(muestra_simple$region))

cat("\nb.Distribución (muestra simple) - sexo:\n")
print(prop_tab(muestra_simple$sexo))

cat("\nc.Edad promedio (muestra simple):\n")
print(mean(muestra_simple$edad))

cat("\nc. Compare estas distribuciones con la población total. \n")
cat("\na.Distribución población) - región:\n")
print(prop_tab(poblacion$region))

cat("\nb.Distribución población - sexo:\n")
print(prop_tab(poblacion$sexo))

###############################################################################

# 3) Muestreo estratificado por 'region'
# Extraer una muestra estratificada de n = 100,
# manteniendo proporciones similares por región (Urbana/Rural).
# Usar dplyr: group_by() + sample_n() + ungroup()

set.seed(789)

n_total <- 100

muestra_estratificada <- poblacion %>%
  group_by(region) %>%
  # tamaño por estrato proporcional al estrato en la población:
  sample_n(size = round(n() / nrow(poblacion) * n_total)) %>%
  ungroup()

nrow(muestra_estratificada)

# Nota:
# Por el uso de round(), el tamaño final puede no ser EXACTAMENTE 100.
# Esto se puede discutir metodológicamente y/o ajustar como extensión.

#a. Distribución (muestra estratificada) - región:
print(prop_tab(muestra_estratificada$region))

#b. Distribución (muestra estratificada) - sexo:
print(prop_tab(muestra_estratificada$sexo))

#c. Edad promedio (muestra estratificada):
print(mean(muestra_estratificada$edad))

##############################################################################
# 4) Comparación de resultados
# Compare población vs. muestras en:
# proporciones por región
# proporciones por sexo
# edad promedio

comparacion <- data.frame(
  conjunto = c("Población", "Muestra simple", "Muestra estratificada"),
  n        = c(nrow(poblacion), nrow(muestra_simple), nrow(muestra_estratificada)),
  edad_promedio = c(mean(poblacion$edad), mean(muestra_simple$edad), mean(muestra_estratificada$edad)),
  prop_urbana = c(
    prop.table(table(poblacion$region))["Urbana"],
    prop.table(table(muestra_simple$region))["Urbana"],
    prop.table(table(muestra_estratificada$region))["Urbana"]
  ),
  prop_rural = c(
    prop.table(table(poblacion$region))["Rural"],
    prop.table(table(muestra_simple$region))["Rural"],
    prop.table(table(muestra_estratificada$region))["Rural"]
  )
)

# Redondeo para imprimir más claro
comparacion$edad_promedio <- round(comparacion$edad_promedio, 2)
comparacion$prop_urbana   <- round(comparacion$prop_urbana, 4)
comparacion$prop_rural    <- round(comparacion$prop_rural, 4)

print(comparacion, row.names = FALSE)


# 5) Preguntas de análisis
# ¿Qué diferencias observa entre la muestra aleatoria simple y la estratificada?
cat("La distribución  según región en la población es de 36% en zonas rurales 
    y 64% en zonas rurales. A su vez, existe un  un 52% de femeninas y un 48% 
    de masculinos.
    
    Mientras que, en el muestreo simple, las zonas rurales representa un 34% y 
    las zonas urbanas un 66%. Y existe existe un 62% de femeninas y un 38% de 
    masculinos.
    
    Por otra parte, el muestreo estratificado muestra una distribución por 
    región, donde un 36% es rural y un 64% es urbana. A su vez, presenta una 
    muestra por sexo de un 43% femeninas y un 57% masculinos.
    
    Teniendo en cuenta lo anterior, la diferencia entre ambas muestras es 
    el muestreo simple altera el porcentaje según región y el sexo con respecto
    a la población en general. Mientras, que el muestreo estratificado logra 
    representar muy bien la distribución por regiones y por sexo crea un 
    porcentaje similar. 
    
    Asimismo, la edad promedio en la muestra simple es de 52 años, mientras que
    en el muestreo estratificado es de 51 años. Y en la población general el 
    promedio de edad es 51. Ambas muestras, andan bastante parecidas.
    
    ")

# ¿Cuál método representa mejor la estructura de la población por región?
("Por región la muestra más representativa es el muestreo estratificado. Dado 
que, se obtienen porcentajes iguales a la población original.
  
  ")



# ¿En qué escenarios de SIS sería preferible usar muestreo estratificado?
("Cuando se realizan investigaciones en las cuales de se deben encuestar, 
estudiar o evaluar  poblaciones de diferentes regiones o de diferentes grupos
etarios.
  ")


# ¿Qué riesgos metodológicos existen al usar solo MAS en poblaciones heterogéneas?
(" Uno de los mayores riesgos es que se pierdan la representatividad en las 
poblaciones más pequeñas. Por eso, en esos casos es mejor utilizar el muestreo
estrateficado, que aunque grupos pequeños se evaluén con grupos pequeños, todos
puedan ser representados de forma equitativa
  
  ")
