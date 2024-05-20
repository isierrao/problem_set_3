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

#Relacion entre los parques y los precios del inmueble

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

centroides_e <- centroides_e %>%
  mutate(x = st_coordinates(centroides_e)[,"X"]) %>%
  mutate(y = st_coordinates(centroides_e)[,"Y"])

#Creamos mapa de las escuelas

latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = escuela_geometria, col = "red",weight = 10,
              opacity = 0.8, popup = escuela_geometria$name) %>%
  addCircles(lng = centroides_e$x, 
             lat = centroides_e$y, 
             col = "darkblue", opacity = 0.5, radius = 1)

#Proyeccion de los centroides y de los datos de propiedad
centroides_sf_e <- st_as_sf(centroides_e, coords = c("x", "y"), crs = 4326) 
sf_train <- st_as_sf(train,coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

#Calcular la distancia de cada propiedad a la escuela mas cercana
dist_matrix_e <- st_distance( x = sf_train, y = centroides_sf_e)
dist_min_e <- apply(dist_matrix_e, 1, min)
dist_matrix_es <- st_distance(x = sf_test, y = centroides_sf_e)
dist_min_es <- apply(dist_matrix_es, 1, min)

#añadir variable a la base general
train <- train %>% mutate (distancia_escuela = dist_min_e)
test <- test %>% mutate (distancia_escuela = dist_min_es)

#Añadir variables predictoras
#Eliminamos las observaciones que no tienen información de longitud ni latitud
train <- train %>%
  filter(!is.na(lat) & !is.na(lon))
test <- test %>%
  filter(!is.na(lat) & !is.na(lon))

#Relacion de la variable escuela y el precio del inmueble

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
centroides_et <- st_centroid(estacion_geometria, byid = T)

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
              opacity = 0.8, popup = estacion_geometria$name) %>%
  addCircles(lng = centroides_et$x, 
             lat = centroides_et$y, 
             col = "darkblue", opacity = 0.5, radius = 1)

#Proyeccion de los centroides y de los datos de propiedad
centroides_sf_estacion <- st_as_sf(centroides_et, coords = c("x", "y"), crs = 4326) 
sf_train <- st_as_sf(train,coords = c("lon", "lat"), crs = 4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

#Calcular la distancia de cada propiedad a la escuela mas cercana
dist_matrix_et <- st_distance( x = sf_train, y = centroides_sf_estacion)
dist_min_et <- apply(dist_matrix_et, 1, min)
dist_matrix_est <- st_distance(x = sf_test, y = centroides_sf_estacion)
dist_min_est <- apply(dist_matrix_est, 1, min)

#añadir variable a la base general
train <- train %>% mutate (distancia_estacion = dist_min_et)
test <- test %>% mutate (distancia_estacion = dist_min_est)

#Añadir variables predictoras
#Eliminamos las observaciones que no tienen información de longitud ni latitud
train <- train %>%
  filter(!is.na(lat) & !is.na(lon))
test <- test %>%
  filter(!is.na(lat) & !is.na(lon))

#Relacion entre la estacion y el precio del inmueble

est <- ggplot(train%>%sample_n(1000), aes(x = distancia_estacion, y = price)) +
  geom_point(col = "red", alpha = 0.4) +
  labs(x = "Distancia mínima a una estacion en metros (log-scale)", 
       y = "Valor de venta  (log-scale)",
       title = "Relación entre la proximidad a una estacion y el precio del immueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(est)

#Agregar variable comercial como aproximación a los centros comerciales o zonas comerciales
comercial <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "landuse", value = "commercial")

#Cambiamos el formato para que sea un objeto
comercial_sf <- osmdata_sf(comercial)

#De las features de comercial nos interesa su geometria y donde estan ubicados
comercial_geometria <- comercial_sf$osm_polygons  %>%
  dplyr::select(osm_id, name)

#Guardamos los poligonos de comercial
comercial_geometria <- st_as_sf(comercial_sf$osm_polygons)

#Calculamos el centroide de cada comercial para aproximar su ubicacion como un solo punto
centroides_c  <- st_centroid(comercial_geometria, byid = T)

centroides_c <- centroides_c %>%
  mutate(x=st_coordinates(centroides_c)[, "X"])  %>%
  mutate(y=st_coordinates(centroides_c)[, "Y"])

#Mapa de Bogotá con estos terrenos comerciales
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = comercial_geometria, col = "purple", weight = 10,
              opacity = 0.8, popup = comercial_geometria$name)  %>%
  addCircles(lng = centroides_c$x,
             lat = centroides_c$y,
             col = "darkblue", opacity = 0.5, radius = 1)

centroides_sf_c <- st_as_sf(centroides_c, coords = c("x", "y"), crs=4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs=4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs=4326)

#Calcular la distancia de cada propiedad a la variable comercial
dist_matrix_c <- st_distance(x = sf_train, y = centroides_sf_c)
dist_min_c <- apply(dist_matrix_c, 1, min)
dist_matrix_co <- st_distance(x = sf_test, y = centroides_sf_c)
dist_min_co <- apply(dist_matrix_co, 1, min)

#Agregar la variable a la base de datos 
train <- train %>% mutate(distancia_comercial = dist_min_c)
test <- test %>% mutate (distancia_comercial = dist_min_co)

#Añadir variables predictoras
#Eliminamos las observaciones que no tienen información de longitud ni latitud
train <- train %>%
  filter(!is.na(lat) & !is.na(lon))
test <- test %>%
  filter(!is.na(lat) & !is.na(lon))

##Agregamos la variable banco para aproximar a sector financiero
banco <- opq(bbox = getbb("Bogotá Colombia"))  %>%
  add_osm_feature(key = "amenity", value = "bank")

#Cambiamos el formato para que sea un objeto
banco_sf <- osmdata_sf(banco)

#De las features de la variable banco nos interesa su geometria y donde están ubicados
banco_geometria <- banco_sf$osm_polygons %>%
  dplyr::select(osm_id, name)

#Guardar poligonos de los bancos
banco_geometria <- st_as_sf(banco_sf$osm_polygons)

#Calculamos el centroide de cada parque para aprox su ubicación a un solo punto
centroides_ba <- st_centroid(banco_geometria, byid = T)

centroides_ba <- centroides_ba %>%
  mutate(x=st_coordinates(centroides_ba)[, "X"])  %>%
  mutate(y=st_coordinates(centroides_ba)[, "Y"])

#Creamos el mapa de Bogotá con los bancos
leaflet()  %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = banco_geometria, col = "purple", weight = 10,
              opacity = 0.8, popup = banco_geometria$name)  %>%
  addCircles(lng = centroides_ba$x,
             lat = centroides_ba$y,
             col = "darkblue", opacity = 0.5, radius = 1)

centroides_sf_ba <- st_as_sf(centroides_ba, coords = c("x", "y"), crs=4326)
sf_train <- st_as_sf(train,coords = c("lon", "lat"), crs=4326)
sf_test <- st_as_sf(test, coords = c("lon","lat"), crs=4326)

#Calculamos la distancia de cada propiedad al banco mas cercano
dist_matrix_ba <- st_distance(x = sf_train, y = centroides_sf_ba)
dist_min_ba <- apply(dist_matrix_ba, 1, min)
dist_matrix_bc <- st_distance(x = sf_test, y = centroides_sf_ba)
dist_min_bc <- apply(dist_matrix_bc, 1 , min)

#Agregar como variables
train <- train %>% mutate (distancia_banco = dist_min_ba)
test <- test %>% mutate(distancia_banco = dist_min_bc)

#Añadir variables predictoras
#Eliminamos las observaciones que no tienen información de longitud ni latitud
train <- train %>%
  filter(!is.na(lat) & !is.na(lon))
test <- test %>%
  filter(!is.na(lat) & !is.na(lon))

##Agregamos la variable parada de autobus
parada_bus <- opq(bbox = getbb("Bogotá Colombia"))  %>%
  add_osm_feature(key = "highway", value = "bus_stop")

#Cambiamos el formato para que sea un objeto
parada_bus_sf <- osmdata_sf(parada_bus)

#De las features de parada de autobus nos interesa su geometria y donde estan ubicados
parada_bus_geometria <- parada_bus_sf$osm_polygons  %>%
  dplyr::select(osm_id, name)

#Guardamos los poligonos de parada de autobus
parada_bus_geometria <- st_as_sf(parada_bus_sf$osm_polygons)

#Calculamos el centroide de cada paradero para aproximar su ubicacion a un solo punto
centroides_pb <- st_centroid(parada_bus_geometria, byid = T)
centroides_pb <- centroides_pb %>%
  mutate(x=st_coordinates(centroides_pb)[,"X"])  %>%
  mutate(y=st_coordinates(centroides_pb)[,"Y"])

#Mapa de Bogotá con los paraderos
leaflet()   %>%
  addTiles()  %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12)  %>%
  addPolygons(data = parada_bus_geometria, col = "purple", weight = 10,
              opacity = 0.8, popup = parada_bus_geometria$name) %>%
  addCircles(lng = centroides_pb$x,
             lat = centroides_pb$y,
             col = "darkblue", opacity = 0.5, radius = 1)

centroides_sf_pb <- st_as_sf(centroides_pb, coords = c("x", "y"), crs=4326)            
sf_train <- st_as_sf(train,coords = c("lon", "lat"), crs=4326)
sf_test <- st_as_sf(test,coords = c("lon", "lat"), crs=4326)

#Calcular la distancia de cada propiedad al paradero mas cercano
dist_matrix_pb <- st_distance (x = sf_train, y = centroides_sf_pb)
dist_min_pb <- apply(dist_matrix_pb, 1, min)
dist_matrix_b <- st_distance(x = sf_test, y = centroides_sf_pb)
dist_min_b <- apply(dist_matrix_b, 1, min)

#Agregar como variable a la base de datos
train <- train %>% mutate (distancia_bus = dist_min_pb)
test <- test %>% mutate (distancia_bus = dist_min_b)

#Añadir variables predictoras
#Eliminamos las observaciones que no tienen información de longitud ni latitud
train <- train %>%
  filter(!is.na(lat) & !is.na(lon))
test <- test %>%
  filter(!is.na(lat) & !is.na(lon))

####Delimitar los datos a Chapinero
#Eliminar las observaciones que no tienen información de latitud y longitud
train <- train %>%
  filter(!is.na(lat) & !is.na(lon))

#Visualizamos los datos para Bogotá
leaflet()  %>%
  addTiles()  %>%
  addCircles(lng = train$lon,
             lat = train$lat)

#Ahora nos quedaremos con las observaciones que pertenecen a Chapinero
localidad <- "Chapinero"
ciudad <- "Bogotá"

#Consultar los datos en OMS para los límites de la localidad
query <- paste0(localidad, ",",ciudad)
Chapinero <- opq(query)  %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  osmdata_sf()

#Extraer limites
Chapinero_bbox <- st_bbox(limites_Chapinero)
print(Chapinero_bbox)

train_filtrados <- train %>%
  filter(
    between(lon, Chapinero_bbox["xmin"], Chapinero_bbox["xmax"]) &
      between(lat, Chapinero_bbox["ymin"], Chapinero_bbox["ymax"])
  )

test_filtrados <- test %>%
  filter(
    between(lon, Chapinero_bbox["xmin"], Chapinero_bbox["xmax"]) &
      between(lat, Chapinero_bbox["ymin"], Chapinero_bbox["ymax"])
  )


train_filtrados_sf <- st_as_sf(train_filtrados, coords = c("lon", "lat"), crs=4326)
test_filtrados_sf <- st_as_sf(test_filtrados, coords = c("lon", "lat"), crs = 4326)

Chapinero$osm_multipolygons

limites_Chapinero <- Chapinero$osm_multipolygons
train_Chapinero <- st_intersection(train_filtrados_sf, limites_Chapinero)
test_Chapinero <- st_intersection(test_filtrados_sf, limites_Chapinero)

str(limites_Chapinero)
print(limites_Chapinero)
print(head(train_filtrados))
print(dim(train_filtrados))

#Graficar limites de Chapinero
ggplot() +
  geom_sf(data = limites_Chapinero, fill = "purple", color = "darkblue", alpha = 0.3) +
  theme_minimal() +
  labs(title = "Límites de Chapinero, Bogotá",
       x = "Longitud", y = "Latitud")


#Revisar proyeccion de los datos
st_crs(limites_Chapinero)

#Graficar los datos filtrados que corresponden a Chapinero
library(ggplot2)

ggplot() +
  geom_sf(data = limites_Chapinero, fill = "purple", color = "darkblue", alpha = 0.3) +
  geom_sf(data = train_Chapinero, color = "blue", size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Datos filtrados dentro de Chapinero, Bogotá",
       x = "Longitud", y = "Latitud")

ggplot() +
  geom_sf(data = limites_Chapinero, fill = "purple", color = "darkblue", alpha = 0.3) +
  geom_sf(data = test_Chapinero, color = "blue", size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Datos filtrados dentro de Chapinero, Bogotá",
       x = "Longitud", y = "Latitud")
