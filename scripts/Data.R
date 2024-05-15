#Organizar base de datos

#setwd("C:/Users/USUARIO/Documents/GitHub/problem_set_3/scripts")

train<-read.csv("C:/Users/USUARIO/Documents/GitHub/problem_set_3/scripts/train.csv")
test<-read.csv("C:/Users/USUARIO/Documents/GitHub/problem_set_3/scripts/test.csv")

# Cargar pacman (contiene la función p_load)
library(pacman) 

# Cargar las librerías 
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Get OSM's data 
       tidymodels,#para modelos de ML
       magritt) 

#revisión de la información disponible 
table(train$operation_type)

train %>%
  count(property_type)

train %>%
  count(rooms)

train %>%
  count(bedrooms)

train  %>%
  count(bathrooms)

mediana_sup_cubierta <- median(train$surface_covered, na.rm = TRUE)
mediana_sup_total<- median(train$surface_total, na.rm = TRUE)

p_load(stargazer)
stargazer(train,type="text")

# Calculamos valor del metro cuadrado 
train <- train %>%
  mutate(precio_por_mt2 = round(price / surface_total, 0))%>%
  mutate(precio_por_mt2  =precio_por_mt2/1000000 )  ## precio x Mt2 en millones. 
stargazer(train["precio_por_mt2"],type="text")

#Filtramos outlier de valor por metro cuadrado
train <- train %>%
  filter(between(precio_por_mt2, 0.10,  20))

# Visualicemos la distribución de nuestra variable de interés
pr <- ggplot(train, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.4) +
  labs(x = "Valor de venta (log-scale)", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(pr)

# Observamos la distribución de los inmuebles en el mapa de Bogotá
leaflet() %>%
  addTiles() %>%
  addCircles(lng = test$lon, 
             lat = test$lat)
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon, 
             lat = train$lat)

#revisar variables disponibles en openmaps 
available_tags("leisure") 
available_tags("amenity")
available_tags ("highway")

#revisión de missings
p_load( visdat)
vis_dat(train)
#hay missings en rooms, bathrooms, y en mayor medida en surface total y surface covered

#####reemplazar missings (solo en caso de que ya hayan avanzado con modelos y estemos con texto aún)
pre_process_propiedades<-  function(data, ...) {
  
  data <- data %>%
    mutate(rooms = replace_na(rooms, 3),
           bedrooms = replace_na(bedrooms, 3),
           bathrooms = replace_na(bathrooms, 2),
           surface_covered = replace_na(surface_covered, mediana_sup_cubierta),
           surface_total = replace_na(surface_total, mediana_sup_total),
    )
}

####Variables adicionales
###Parques
# Extraemos la info de todos los parques 
parques <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 

# Cambiamos el formato para que sea un objeto sf (simple features)
parques_sf <- osmdata_sf(parques)

# De las features del parque nos interesa su geomoetría y donde estan ubicados 
parques_geometria <- parques_sf$osm_polygons %>% 
  dplyr::select(osm_id, name) 

# Guardemos los poligonos de los parques 
parques_geometria <- st_as_sf(parques_sf$osm_polygons)

# Calculamos el centroide de cada parque para aproximar su ubciacion como un solo punto 
centroides <- st_centroid(parques_geometria, byid = T)
centroides <- centroides %>%
  mutate(x=st_coordinates(centroides)[, "X"]) %>%
  mutate(y=st_coordinates(centroides)[, "Y"]) 

#Mapa de los parques
latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = parques_geometria, col = "red",weight = 10,
              opacity = 0.8, popup = parques_geometria$name) %>%
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "darkblue", opacity = 0.5, radius = 1)

# proyección de los centroides y de los datos de propiedades 
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)

#distancias de cada propiedad al parque más cercano
dist_matrix <- st_distance(x = sf_train, y = centroides_sf)
dist_min <- apply(dist_matrix, 1, min)
dist_matrix_test <- st_distance(x = sf_test, y = centroides_sf)
dist_min_test <- apply(dist_matrix_test, 1, min)

#añadir variable a la base general
train <- train %>% mutate (distancia_parque = dist_min)
test <- test %>% mutate (distancia_parque = dist_min_test)

#Añadir variables predictoras
#Eliminamos las observaciones que no tienen información de longitud ni latitud
train <- train %>%
  filter(!is.na(lat) & !is.na(lon))
test <- test %>%
  filter(!is.na(lat) & !is.na(lon))

#Distribucion de esta variable

p <- ggplot(train, aes(x = distancia_parque)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
       title = "Distribución de la distancia a los parques") +
  theme_bw()
ggplotly(p)

#Relacion con la variable de interés

p <- ggplot(train%>%sample_n(1000), aes(x = distancia_parque, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros (log-scale)", 
       y = "Valor de venta  (log-scale)",
       title = "Relación entre la proximidad a un parque y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)

###Ver otras variables disponibles en osm

library(osmdata)
available_tags("building")
available_tags("leisure")
available_tags("natural")
available_tags("landuse")
available_tags("amenity")
available_tags("tourism")
available_tags("public_transport")
available_tags("highway")

###De acuerdo con las variables usadas en el paper de Zhaoyang et.,al tomamos algunas similares

#Educacion
escuela <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "building", value = "school")

#Cambiamos el formato para que se un objeto sf 
escuela_sf <- osmdata_sf(escuela)

#De las features de escuela nos interesa su geometria y donde estan ubicadas
escuela_geometria <- escuela_sf$osm_polygons %>%
  dplyr::select(osm_id, name)

#Guardemos los poligonos de las escuelas
escuela_geometria <- st_as_sf(escuela_sf$osm_polygons)

#Calculamos el centroide de cada escuela para aproximar su ubicacion como un solo punto
centroides_e <- st_centroid(escuela_geometria, byid = T)

centroides <- centroides %>%
  mutate(x = st_coordinates(centroides)[,"X"]) %>%
  mutate(y = st_coordinates(centroides)[,"Y"])

#Creamos mapa de las escuelas

latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = escuela_geometria, col = "red",weight = 10,
              opacity = 0.8, popup = parques_geometria$name) %>%
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "darkblue", opacity = 0.5, radius = 1)

#Proyeccion de los centroides y de los datos de propiedad
centroides_sf_escuela <- st_as_sf(centroides, coords = c("x", "y"), crs = 4326) 
sf_train <- st_as_sf(train,coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

#Calcular la distancia de cada propiedad a la escuela mas cercana
dist_matrix_escuela <- st_distance( x = sf_train, y = centroides_sf)
dist_min_escuela <- apply(dist_matrix_escuela, 1, min)
dist_matrix_escuelat <- st_distance(x = sf_test, y = centroides_sf)
dist_min_test_escuelat <- apply(dist_matrix_escuelat, 1, min)

#añadir variable a la base general
train <- train %>% mutate (distancia_escuela = dist_min_escuela)
test <- test %>% mutate (distancia_escuela = dist_min_test_escuelat)

#Añadir variables predictoras
#Eliminamos las observaciones que no tienen información de longitud ni latitud
train <- train %>%
  filter(!is.na(lat) & !is.na(lon))
test <- test %>%
  filter(!is.na(lat) & !is.na(lon))

#Distribucion de la variable escuela
e <- ggplot(train, aes(x = distancia_escuela)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
       title = "Distribución de la distancia a los parques") +
  theme_bw()
ggplotly(e)

es <- ggplot(train%>%sample_n(1000), aes(x = distancia_escuela, y = price)) +
  geom_point(col = "blue", alpha = 0.4) +
  labs(x = "Distancia mínima a una escuela en metros (log-scale)", 
       y = "Valor de venta  (log-scale)",
       title = "Relación entre la proximidad a una escuela y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(es)

##Agregar variable estacion
estacion <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "public_transport", value = "station")

#Cambiamos el formato para que se un objeto sf
estacion_sf <- osmdata_sf(estacion)

#De las features nos interesa su geometria y donde estan ubicados
estacion_geometria <- estacion_sf$osm_polygons %>%
  dplyr::select(osm_id, name)

#Guardemos los poligonos de las estaciones
estacion_geometria <- st_as_sf(estacion_sf$osm_polygons)

#Calculamos el centroide de cada estacion para aproximar su ubicacion como un solo punto
centroides_et <- st_centroid(escuela_geometria, byid = T)

centroides_et <- centroides_et %>%
  mutate(x = st_coordinates(centroides_et)[,"X"]) %>%
  mutate(y = st_coordinates(centroides_et)[,"Y"])

#Creamos mapa de las etaciones

latitud_central_et <- mean(train$lat)
longitud_central_et <- mean(train$lon)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = estacion_geometria, col = "red",weight = 10,
              opacity = 0.8, popup = parques_geometria$name) %>%
  addCircles(lng = centroides_et$x, 
             lat = centroides_et$y, 
             col = "darkblue", opacity = 0.5, radius = 1)

#Proyeccion de los centroides y de los datos de propiedad
centroides_sf_estacion <- st_as_sf(centroides_et, coords = c("x", "y"), crs = 4326) 
sf_train <- st_as_sf(train,coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

#Calcular la distancia de cada propiedad a la escuela mas cercana
dist_matrix_et <- st_distance( x = sf_train, y = centroides_sf_estacion)
dist_min_estacion <- apply(dist_matrix_et, 1, min)
dist_matrix_est <- st_distance(x = sf_test, y = centroides_sf_estacion)
dist_min_est <- apply(dist_matrix_est, 1, min)

#añadir variable a la base general
train <- train %>% mutate (distancia_estacion = dist_min_estacion)
test <- test %>% mutate (distancia_estacion = dist_min_est)

#Añadir variables predictoras
#Eliminamos las observaciones que no tienen información de longitud ni latitud
train <- train %>%
  filter(!is.na(lat) & !is.na(lon))
test <- test %>%
  filter(!is.na(lat) & !is.na(lon))

#Distribucion de la variable escuela
est <- ggplot(train, aes(x = distancia_estacion)) +
  geom_histogram(bins = 50, fill = "red", alpha = 0.4) +
  labs(x = "Distancia mínima a una estacion en metros", y = "Cantidad",
       title = "Distribución de la distancia a las estaciones") +
  theme_bw()
ggplotly(est)

est <- ggplot(train%>%sample_n(1000), aes(x = distancia_estacion, y = price)) +
  geom_point(col = "red", alpha = 0.4) +
  labs(x = "Distancia mínima a una estacion en metros (log-scale)", 
       y = "Valor de venta  (log-scale)",
       title = "Relación entre la proximidad a una estacion y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(est)
