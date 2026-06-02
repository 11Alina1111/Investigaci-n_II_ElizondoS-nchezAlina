install.packages('leaflet')

library (leaflet)

#Mapa interactivo
leaflet ()%>%
  addTiles()%>%
  setView (lng = -3.7, lat = 40.4, zoom = 5)%>%
  addMarkers(lng = -3.70256, lat = 40.4167678, popup = "Hospital Central de Madrid") %>%
  addCircleMarkers (lng = -0.37628, lat = 39.46990, radius = 8, color= "red",
                   popup = "Brote de enfermedad de Valencia")

###############################################33333
##################################################
library(leaflet)

# Crear mapa base
leaflet() %>% 
  addTiles() %>%
  
  # Región Central
  addPolygons(
    lng = c(-84.5, -83.5, -83.5, -84.5),
    lat = c(10.2, 10.2, 9.5, 9.5),
    fillColor = "red",
    fillOpacity = 0.5,
    color = "black",
    popup = "Central: 2,688,664 hab"
  ) %>%
  
  # Chorotega
  addPolygons(
    lng = c(-86.0, -85.0, -85.0, -86.0),
    lat = c(11.2, 11.2, 10.2, 10.2),
    fillColor = "orange",
    fillOpacity = 0.5,
    color = "black",
    popup = "Chorotega: 370,906 hab"
  ) %>%
  
  # Pacífico Central
  addPolygons(
    lng = c(-85.5, -84.5, -84.5, -85.5),
    lat = c(10.2, 10.2, 9.5, 9.5),
    fillColor = "yellow",
    fillOpacity = 0.5,
    color = "black",
    popup = "Pacífico Central: 243,295 hab"
  ) %>%
  
  # Brunca
  addPolygons(
    lng = c(-84.5, -83.0, -83.0, -84.5),
    lat = c(9.5, 9.5, 8.0, 8.0),
    fillColor = "green",
    fillOpacity = 0.5,
    color = "black",
    popup = "Brunca: 379,000 hab"
  ) %>%
  
  # Huetar Norte
  addPolygons(
    lng = c(-85.0, -83.5, -83.5, -85.0),
    lat = c(11.0, 11.0, 10.2, 10.2),
    fillColor = "blue",
    fillOpacity = 0.5,
    color = "black",
    popup = "Huetar Norte: 430,000 hab"
  ) %>%
  
  # Huetar Caribe
  addPolygons(
    lng = c(-83.5, -82.5, -82.5, -83.5),
    lat = c(10.2, 10.2, 9.0, 9.0),
    fillColor = "purple",
    fillOpacity = 0.5,
    color = "black",
    popup = "Huetar Caribe: 495,000 hab"
  ) %>%
  
  # Vista inicial
  setView(lng = -84, lat = 9.9, zoom = 7)