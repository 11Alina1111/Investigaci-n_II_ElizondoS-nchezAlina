# Universidad Estatal a Distancia
# Carrera de Sistemas de Información en Salud
# 03576-3-2026 Investigación en Sistemas de Información en Salud II
# Práctica Formativa U4. Muestreo aleatorio simple y estratificado
# Programador: Lic. Iván M. Rodríguez Soriano
# Versión: 1.0 (2026/03/14)
#Modificaciones: Estudiante Alina Elizondo Sánchez

# Objetivos:
# 1) Generar un conjunto de datos sobre una enfermedad "X"
# 2) Visualizar la distribución de una variable cuantitativa mediante un histograma
# 3) Generar una animación con base en el histograma
# 4) Exportar el resultado a un archivo .GIF

# 0. Preparación del entorno
# Recomendaciónes: 
# Definir la carpeta de trabajo (opcional):
# setwd("C:\Users\alina\OneDrive - Universidad Estatal a Distancia\A_Asignaturas SIS\A_Investigación 2\ActividadFormativa4")
# Limpiar el entorno
rm(list = ls())

# 1. Instalar / cargar paquetes
# install.packages(c("ggplot2","dplyr","tibble","tidyr","gganimate","gifski"))

library(ggplot2)
library(dplyr)
library(tibble)
library(tidyr)
library(gganimate)
library(gifski)

#*****************************************************************
# 2. Definir parámetros del “experimento”
set.seed(2026)

# N_total suavisa la distribución (más registros = más suave)
N_total    <- 2000
mu         <- 50     # media (valor central de “Carga viral”)
sigma      <- 10     # desviación estándar

# “Periodo” = número de columnas del histograma
n_bins     <- 25

# Controla el efecto de “crecimiento” del histograma
grow_steps <- 4

# Parámetros de exportación
fps        <- 12
out_gif    <- "histograma_enfermedadX.gif"
out_csv    <- "datos_enfermedadX_simulados.csv"

#*****************************************************************
# 3. Definición de los conteos por bin (distribución)
# Se calcula la probabilidad por bin usando dnorm() (Gauss)
# Se asignan conteos enteros por bin con rmultinom() (siempre >=0 y suma N_total)

# Captura típica de una normal: ±3σ alrededor de la media
x_min <- mu - 3*sigma
x_max <- mu + 3*sigma

bin_edges   <- seq(x_min, x_max, length.out = n_bins + 1)
bin_centers <- (bin_edges[-1] + bin_edges[-length(bin_edges)]) / 2
bin_width   <- diff(bin_edges)[1]

# Probabilidad por bin (aprox densidad * ancho)
w <- dnorm(bin_centers, mean = mu, sd = sigma)
p_bin <- w * bin_width
p_bin <- p_bin / sum(p_bin)

# Conteos por bin (enteros, no negativos, suman exactamente N_total)
counts <- as.vector(rmultinom(1, size = N_total, prob = p_bin))

bins <- tibble(
  bin_id      = 1:n_bins,
  x_center    = bin_centers,
  count_final = counts
)

#*****************************************************************
# 4. Simular dataset de pacientes y guardarlo en CSV
# Nota: Este dataset es el “registro simulado” (SIS) para el ejercicio.
# La variable cuantitativa se llama: carga_viral (etiquetada como "Carga viral" en el gráfico)
# Generamos valores de carga_viral dentro de cada bin según count_final
carga_viral <- bins %>%
  rowwise() %>%
  summarise(
    valores = list(runif(count_final, min = bin_edges[bin_id], max = bin_edges[bin_id + 1]))
  ) %>%
  pull(valores) %>%
  unlist()

# Dataset simulado (campos típicos del enunciado)
datos <- tibble(
  id = 1:N_total,
  edad = pmin(pmax(round(rnorm(N_total, mean = 45, sd = 18)), 0), 95),
  sexo = sample(c("F", "M"), size = N_total, replace = TRUE, prob = c(0.52, 0.48)),
  riesgo = sample(c("Bajo", "Medio", "Alto"), size = N_total, replace = TRUE, prob = c(0.45, 0.40, 0.15)),
  dias_desde_inicio = pmax(round(rgamma(N_total, shape = 3, scale = 2)), 0),
  carga_viral = carga_viral
) %>%
  mutate(
    # Hospitalización simple (ejemplo): depende de riesgo y carga_viral
    p_hosp = plogis(-6 + 0.8*(riesgo == "Medio") + 1.4*(riesgo == "Alto") + 0.02*carga_viral),
    hospitalizado = ifelse(runif(N_total) < p_hosp, "Sí", "No")
  ) %>%
  select(-p_hosp)

# Guardar CSV
write.csv(datos, out_csv, row.names = FALSE)
message("✅ CSV generado: ", normalizePath(out_csv))

# PASO 5. Preparar la animación (barras que crecen por “periodo”)
# Construimos frames:
# Recorremos bins de izquierda a derecha
# En cada bin, usamos grow_steps subframes para que la barra crezca gradual

T_frames <- n_bins * grow_steps
frame_id <- 1:T_frames

bin_revelado <- ((frame_id - 1) %/% grow_steps) + 1
paso         <- ((frame_id - 1) %% grow_steps) + 1
progreso     <- paso / grow_steps

frames <- tibble(
  frame_id     = frame_id,
  bin_revelado = bin_revelado,
  progreso     = progreso
)

# Expandimos bins x frames y calculamos la altura en cada frame
bars_anim <- tidyr::crossing(bins, frames) %>%
  mutate(
    height = case_when(
      bin_id < bin_revelado ~ count_final,
      bin_id == bin_revelado ~ count_final * progreso,
      TRUE ~ 0
    )
  )

#*****************************************************************
# 6. Agregamos la curva de la distribución (último frame)
# Para que la línea roja aparezca únicamente en el frame final:
# Creamos un dataset separado con frame_id = max(frame_id)

x_curve <- seq(x_min, x_max, length.out = 400)

curve_df <- tibble(
  x_center = x_curve,
  y = dnorm(x_curve, mean = mu, sd = sigma) * N_total * bin_width,
  frame_id = max(frame_id)
)

#*****************************************************************
# 7. Detalles finales y renderizado
p <- ggplot(bars_anim, aes(x = x_center, y = height)) +
  geom_col(
    width = bin_width * 0.95,
    fill = "#00FF00",
    color = "black",
    alpha = 0.9
  ) +
  geom_line(
    data = curve_df,
    aes(x = x_center, y = y),
    inherit.aes = FALSE,
    color = "red",
    linewidth = 1.5,
    linetype = "twodash"
  ) +
  labs(
    title = "Enfermedad X",
    x = "Carga viral",
    y = "Frecuencia"
  ) +
  theme_light(base_size = 15)+ 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size= 20),
    axis.title.x= element_text( face = "bold", size= 16),
    axis.title.y= element_text( face = "bold", size= 16)
  )

# Animación por frames discretos
anim_plot <- p + transition_manual(frame_id)

# Render a GIF con gifski
gif <- animate(
  anim_plot,
  fps = fps,
  width = 900,
  height = 550,
  end_pause = 3 * fps,   # ← pausa final de ~3 segundos
  renderer = gifski_renderer()
)

anim_save(out_gif, animation = gif)
message("✅ GIF generado: ", normalizePath(out_gif))