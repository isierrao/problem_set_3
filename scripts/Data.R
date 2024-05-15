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

#distancias de cada propiedad al parque más cercano
dist_matrix <- st_distance(x = sf_train, y = centroides_sf)
dist_min <- apply(dist_matrix, 1, min) 

#añadir variable a la base general
train <- train %>% mutate

#Añadir variables predictoras
#Eliminamos las observaciones que no tienen información de longitud ni latitud
train <- train %>%
  filter(!is.na(lat) & !is.na(lon))

##Añadir variables adicionales de Open street maps
#Ver variables disponibles

library(osmdata)
available_tags("building")
available_tags("leisure")
available_tags("natural")
available_tags("landuse")
available_tags("amenity")
available_tags("tourism")
available_tags("public_transport")

##De acuerdo con las variables usadas en el paper de Zhaoyang et.,al tomamos algunas similares

#Educacion
escuela <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "school") 

# Cambiamos el formato para que sea un objeto sf (simple features)
escuela_sf <- osmdata_sf(escuela)

# De las features del parque nos interesa su geomoetría y donde estan ubicados 
escuela_geometria <- escuela_sf$osm_polygons %>% 
  dplyr::select(osm_id, name) 

# Guardemos los poligonos de las escuelas 
escuela_geometria <- st_as_sf(escuela_sf$osm_polygons)

# Calculamos el centroide de cada escuela para aproximar su ubciacion como un solo punto 
centroides <- st_centroid(escuela_geometria, byid = T)
centroides <- centroides %>%
  mutate(x=st_coordinates(centroides)[, "X"]) %>%
  mutate(y=st_coordinates(centroides)[, "Y"]) 

#Mapa de las escuelas
latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = escuela_geometria, col = "blue",weight = 10,
              opacity = 0.8, popup = parques_geometria$name) %>%
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "darkblue", opacity = 0.5, radius = 1)

# proyección de los centroides y de los datos de propiedades 
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)

#distancias de cada propiedad a la escuela más cercana
dist_matrix <- st_distance(x = sf_train, y = centroides_sf)
dist_min <- apply(dist_matrix, 1, min) 

#añadir variable a la base general
train <- train %>% mutate

#Añadir variables predictoras
#Eliminamos las observaciones que no tienen información de longitud ni latitud
train <- train %>%
  filter(!is.na(lat) & !is.na(lon))











