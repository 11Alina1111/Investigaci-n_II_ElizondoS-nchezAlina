# Universidad Estatal a Distancia
# Carrera de Sistemas de Información en Salud
# 03576-3-2026 Investigación en Sistemas de Información en Salud II
# Práctica Formativa U2. Gráfico tipo mapa de calor o Heatmap
# Programador: Lic. Iván M. Rodríguez Soriano
# Versión: 1.0 (2026/02/28)
###################################################################
#Modificaciones por
#Estudiante: Alina Elizondo Sánchez
###################################################################

# Instalación (si hace falta) y carga de paquetes
# tidyverse: manipulación y visualización de datos (ggplot2, dplyr, tidyr, etc.)
# scales: formato de etiquetas (porcentajes, moneda, escalas)
# lubridate: manejo de fechas y horas
# viridis: paleta de colores

packages <- c("tidyverse", "lubridate", "scales", "viridis")

to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(scales)
library(lubridate)

# Generación de datos simulados
set.seed(123) # reproducibilidad

# Parámetros para el mapa de calor (cruce de región por semana)
n_regiones <- 10
n_semanas  <- 14
edad_levels <- c("0-14", "15-39", "40-64", "65+")

regiones <- paste0("Region_", str_pad(1:n_regiones, 2, pad = "0"))

# Generar semanas (usamos lunes como inicio para agrupar)
fecha_inicio <- as.Date("2025-01-06") # lunes
semanas <- seq.Date(from = fecha_inicio, by = "week", length.out = n_semanas)

# Definimos la población por región (este dato es fijo por región)
poblacion_region <- tibble(
  region = regiones,
  poblacion = round(runif(n_regiones, min = 50000, max = 350000))
)

# Efecto regional (riesgo relativo): algunas regiones más "calientes"
efecto_region <- tibble(
  region = regiones,
  rr_region = exp(rnorm(n_regiones, mean = 0, sd = 0.35))
)

# Efecto por edad (mayor riesgo en mayores, por ejemplo)
efecto_edad <- tibble(
  edad = factor(edad_levels, levels = edad_levels),
  rr_edad = c(0.7, 1.0, 1.2, 1.8)
)

# Estacionalidad: pico hacia el centro del periodo
# (curva tipo campana sobre semanas)
idx <- 1:n_semanas
season <- tibble(
  semana = semanas,
  rr_season = 0.6 + 1.6 * dnorm(idx, mean = (n_semanas+1)/2, sd = n_semanas/5)
  / max(dnorm(idx, mean=(n_semanas+1)/2, sd=n_semanas/5))
)

# Base de simulación por región-semana-edad
base <- expand_grid(
  region = regiones,
  semana = semanas,
  edad = factor(edad_levels, levels = edad_levels)
) %>%
  left_join(poblacion_region, by = "region") %>%
  left_join(efecto_region, by = "region") %>%
  left_join(efecto_edad, by = "edad") %>%
  left_join(season, by = "semana")

# Tasa base semanal (por persona) ~ ajustable
tasa_base <- 8 / 100000  # 8 por 100k por semana como baseline

# Media esperada de casos (lambda Poisson)
sim <- base %>%
  mutate(
    lambda = poblacion * tasa_base * rr_region * rr_edad * rr_season,
    casos  = rpois(n(), lambda = lambda)
  )

# Agregar por región-semana (sumando edades)
agg <- sim %>%
  group_by(region, semana) %>%
  summarise(
    casos = sum(casos),
    poblacion = first(poblacion),
    .groups = "drop"
  ) %>%
  mutate(
    incidencia_100k = (casos / poblacion) * 100000,
    semana_label = paste0("W", isoweek(semana))
  )

# Ordenar regiones por incidencia total (para un heatmap más informativo)
orden_regiones <- agg %>%
  group_by(region) %>%
  summarise(total = sum(incidencia_100k), .groups = "drop") %>%
  arrange(total) %>%
  pull(region)

agg <- agg %>%
  mutate(region = factor(region, levels = orden_regiones))

# Construimos el gráfico de mapa de calor o heatmap
ggplot(agg, aes(x = semana, y = region, fill = incidencia_100k)) +
  geom_tile(color = "gray", linewidth = 0.5) +
  scale_fill_viridis_c(
    option = "H",# Cambio a Virilis H
    end = 1,
    labels = label_number(accuracy = 0.1),
    name = "Incidencia\n(por 100,000)"
  ) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 5))+ #Ancho y alto de la leyenda
  scale_x_date(date_breaks = "1 week", date_labels = "W%V") +
  labs(
    title = "Heatmap de incidencia semanal por región",
    subtitle = "Datos epidemiológicos simulados (vigilancia sindrómica)",
    x = "Semana epidemiológica",
    y = "Región",
    caption = "Fuente: datos simulados"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust= 0.5, face= "bold", size= 16), # Cambio de formato del titulo
    plot.subtitle = element_text(hjust= 0.5, face= "italic", size= 12), # y subtitulo
    axis.text.x = element_text(angle = 45, hjust = 0.5, size= 11),
    panel.grid = element_blank()
  )
