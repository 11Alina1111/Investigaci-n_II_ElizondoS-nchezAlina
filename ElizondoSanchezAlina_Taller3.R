################################################
# TALLER OBLIGATORIO #3: MARCO METODOLÓGICO
# EN INVESTIGACIÓN EN SISTEMAS EN SALUD
# Estudiante: Alina Elizondo Sánchez
# Carrera de Sistemas de Información en Salud
###############################################
#Importar Librerías(si no hay sino instaladas antes)
#install.packages('tidyverse')
#install.packages('janitor')

#1. Cargar librerías
library(tidyverse)
library(janitor)

#2. Carga y limpieza datos
df <- read.csv('aha.csv') |>
  clean_names()

glimpse

#3. Definición de variables
# Opción 1: Poco significativa
#var_x <- 'pct_small_rural_hospitals_cehrt'
#var_y <- 'pct_hospitals_integrate_any_clinical_info'

# Opción 2: Más relevante
var_x <- "pct_hospitals_cehrt"
var_y <- "pct_hospitals_send_clinical_info"

#4. Comprobación de las variables
if(!all(c(var_x, var_y)%in% names(df))){
  stop(
    'Una o ambas variables no existen en el dataset.\n',
    'Revisa los nombres con names(df) o glimpse(df)'
  )
}
#5. Preparación de los datos
datos <- df |>
  select (all_of(c(var_x, var_y))) |>
  drop_na() |>
  mutate(across(where(is.character), readr::parse_number))

# 6. Correlación
resultado <- cor.test(
  datos[[var_x]],
  datos[[var_y]],
  method = "pearson"
)

# 7. Tabla resumen
tabla_resumen <- tibble(
  Variable_X = var_x,
  Variable_Y = var_y,
  Metodo = "Pearson",
  Coeficiente_r = unname(resultado$estimate),
  IC_95_inferior = resultado$conf.int[1],
  IC_95_superior = resultado$conf.int[2],
  Valor_p = resultado$p.value,
  N = nrow(datos)
)

print(tabla_resumen, width = Inf)


cat('Interpretación:Opción 1: Utilizando las variables
Porcentaje de hospitales rurales pequeños con tecnología CERHT
(pct_small_rural_hospitals_cehrt) y Porcentaje de hospitales que integran 
cualquier información clínica (pct_hospitals_integrate_any_clinical_info) se 
obtiene un coeficiente una correlación positiva muy débil de 0.168.

Por otro lado, se obtienen los límites inferiores y superiores (-0.113 y 0.424), 
al obtener cero no se puede descartar la ausencia de correlación.

Con un valor de p= 0.240, se valora que no es estadísticamente significativo:
0.240 > 0.05. Y N= 51, indica el tamaño de la muestra.

No hay evidencia estadística significativa de la existencia de una relación lineal,
dado que el intervalo de confianza y valor p indican lo contrario ')




cat('Interpretación: Opción 2: Usando las variables Porcentaje de hospitales
que usan CERHT (pct_hospitals_cehrt) y porcentaje de hospitales que envían
información clínica (pct_hospitals_send_clinical_info),  se obtiene una
correlación positiva moderada de 0.437.

Sus límites inferiores y superiores se encuentran entre 0.333 y 0.530, es
positiva y estadísticamnete positiva.

El valor de p es de 1.71e-13 > 0.05, lo cual es estadísticamente significativo.
Por último la muestra es de 259.

Por ende, se concluye que si existe una relación moderada positiva en donde los
hospitales que más usan CERHT comparten más información clínica.')
