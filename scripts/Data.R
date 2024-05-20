#Organizar base de datos

rm(list=ls())

setwd("C:/Users/Paula Osorio/OneDrive - Universidad de los andes/2022-2023-2024/MEcA/Big Data/Problem set 3")

train<-read.csv("train.csv")
test<-read.csv("test.csv")


#1. Cargar las librerías 
library(pacman) 
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Get OSM's data 
       tidymodels,#para modelos de ML
       magritt,
       tm,   # para Text Mining
       tidytext, #Para tokenización
       wordcloud, # Nube de palabras 
       stopwords  # consultar stopwords
       ) 

#2.revisión de la información disponible 
table(train$operation_type)

train %>%
  count(property_type)

train %>%
  count(rooms)

train %>%
  count(bedrooms)

train  %>%
  count(bathrooms)

mediana_sup_cubierta <- median(data$surface_covered, na.rm = TRUE)
mediana_sup_total<- median(data$surface_total, na.rm = TRUE)

p_load(stargazer)
stargazer(train,type="text")

#2.1 Calculamos valor del metro cuadrado 
train <- train %>%
  mutate(precio_por_mt2 = round(price / surface_total, 0))%>%
  mutate(precio_por_mt2  =precio_por_mt2/1000000 )  ## precio x Mt2 en millones. 
stargazer(train["precio_por_mt2"],type="text")

#2.2 Filtramos outlier de valor por metro cuadrado
train <- train %>%
  filter(between(precio_por_mt2, 0.10,  20))

#2.3 Visualicemos la distribución de la variable del precio del inmueble
pr <- ggplot(train, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.4) +
  labs(x = "Valor de venta (log-scale)", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)

#2.4 Observamos la distribución de los inmuebles en el mapa de Bogotá
leaflet() %>%
  addTiles() %>%
  addCircles(lng = test$lon, 
             lat = test$lat)
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon, 
             lat = train$lat)


#2.5 revisión de missings
p_load( visdat)
vis_dat(train)
#hay missings en rooms, bathrooms, y en mayor medida en surface total y surface covered

#2.6 reemplazar missings 
pre_process_propiedades<-  function(data, ...) {
  
  data <- data %>%
    mutate(rooms = replace_na(rooms, 3),
           bedrooms = replace_na(bedrooms, 3),
           bathrooms = replace_na(bathrooms, 2),
           surface_covered = replace_na(surface_covered, mediana_sup_cubierta),
           surface_total = replace_na(surface_total, mediana_sup_total),
    )
}

##3.Análisis de la información de la descripción

##3.1 Revisión base train
#estandarizar datos
texto_train <- train$description
texto_train <- removeNumbers(texto_train)
texto_train <- removePunctuation(texto_train)
texto_train <- tolower(texto_train)
texto_train <- stripWhitespace(texto_train)

#tokenizar
texto_tidy_train <- as.data.frame(texto_train) %>% unnest_tokens( "word", texto_train)

#ver palabras más frecuentes
texto_tidy_train  %>% 
  count(word, sort = TRUE)   %>% 
  head()

#eliminar stopwords (conjunciones, artículos, etc.)
texto_tidy_train <- texto_tidy_train  %>% 
  anti_join(tibble(word =stopwords("spanish")))

#visualizar palabras más frecuentes
wordcloud(texto_tidy_train$word, min.freq = 100, 
          colors= c(rgb(72/255, 191/255, 169/255),rgb(249/255, 220/255, 92/255), rgb(229/255, 249/255, 147/255))) 

##3.2 Cambios a para base train


#función para reemplazar palabras por numeros
reemplazar_numeros <- function(texto) {
  # Diccionario de números en texto y sus correspondientes dígitos
  numeros <- c(
    " una " = " 1 ", " un "=" 1 "," primer "=" 1 ", " dos " = " 2 ", " segundo " = " 2 ", " tres " = " 3 ", " tercer " = " 3 ",
    " cuatro " = " 4 "," 4to "="4"," cinco" = " 5 "," quinto " = " 5 "," seis " = " 6 ", " sexto " = " 6 ", 
    " siete " = " 7 "," septimo " = " 7 ", " ocho " = " 8 "," octavo " = " 8 ", " nueve " = " 9 ", " noveno " = " 9 ",
    " diez " = " 10 "," decimo " = " 10 "
  )
  
  # Reemplazar números en texto por dígitos
  for (word in names(numeros)) {
    texto <- gsub(word, numeros[[word]], texto, ignore.case = TRUE)
  }
  
  return(texto)
}

#aplicar función de limpieza a base train
train$description_num<-reemplazar_numeros(train$description)

#extraer metraje
a1 <- "[:space:]+[:digit:]+metros" 
a2 <- "[:space:]+[:digit:]+mts"
a3 <- "[:space:]+[:digit:]+mts2"
a4 <- "[:space:]+[:digit:]+mt"
a5 <- "[:space:]+[:digit:]+m2" 
a6 <- "[:space:]+[:digit:]+mt2"
b1 <- "[:space:]+[:digit:]+[:space:]+metros" 
b2 <- "[:space:]+[:digit:]+[:space:]+mts"
b3 <- "[:space:]+[:digit:]+[:space:]+mts2"
b4 <- "[:space:]+[:digit:]+[:space:]+mt" 
b5 <- "[:space:]+[:digit:]+[:space:]+m2"
b6 <- "[:space:]+[:digit:]+[:space:]+mt2"
c1 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+metros" 
c2 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mts"
c3 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mts2"
c4 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mt" 
c5 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m2"
c6 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mt2"

train<-train %>% mutate(metraje = str_extract(string = train$description_num,
                                                pattern =  paste0(a1,"|",a2,"|",a3,"|",a4,"|",a5,"|",a6,"|",
                                                                  b1,"|",b2,"|",b3,"|",b4,"|",b5,"|",b6,"|",
                                                                  c1,"|",c2,"|",c3,"|",c4,"|",c5,"|",c6)))
train$metraje<- as.numeric(gsub("[^0-9]", "", train$metraje))
#qué hacer con los datos que son muy pequeños?

#habitaciones/alcobas/dormitorios
d1 <-"[:digit:]+alcobas"
d2 <-"[:digit:]+habitaciones"
d3 <-"[:digit:]+dormitorios"
d4 <-"[:digit:]+habitacion"
d5 <-"[:digit:]+alcoba"
d6 <-"[:digit:]+dormitorio"
d7 <-"[:digit:]+habitacin"
d8 <-"[:digit:]+cuartos"
e1 <-"[:space:]+[:digit:]+alcobas"
e2 <-"[:space:]+[:digit:]+habitaciones"
e3 <-"[:space:]+[:digit:]+dormitorios"
e4 <-"[:space:]+[:digit:]+habitacion"
e5 <-"[:space:]+[:digit:]+alcoba"
e6 <-"[:space:]+[:digit:]+dormitorio"
e7 <-"[:space:]+[:digit:]+habitacin"
e8 <-"[:space:]+[:digit:]+cuartos"
f1 <-"[:space:]+[:digit:]+[:space:]+alcobas"
f2 <-"[:space:]+[:digit:]+[:space:]+habitaciones"
f3 <-"[:space:]+[:digit:]+[:space:]+dormitorios"
f4 <-"[:space:]+[:digit:]+[:space:]+habitacion"
f5 <-"[:space:]+[:digit:]+[:space:]+alcoba"
f6 <-"[:space:]+[:digit:]+[:space:]+dormitorio"
f7 <-"[:space:]+[:digit:]+[:space:]+habitacin"
f8 <-"[:space:]+[:digit:]+[:space:]+cuartos"

train<-train %>% mutate(alcobas = str_extract(string = train$description_num,
                                              pattern =  paste0(d1,"|",d2,"|",d3,"|",d4,"|",d5,"|",d6,"|",d7,"|",d8,"|",
                                                                e1,"|",e2,"|",e3,"|",e4,"|",e5,"|",e6,"|",e7,"|",e8,"|",
                                                                f1,"|",f2,"|",f3,"|",f4,"|",f5,"|",f6,"|",f7,"|",f8)))
train$alcobas_num<- as.numeric(gsub("[^0-9]", "", train$alcobas))

#baños
g1 <-"[:digit:]+bano"
g2 <-"[:digit:]+banos"
g3 <-"[:digit:]+baos"
g4 <-"[:digit:]+bao"
h1 <-"[:space:]+[:digit:]+bano"
h2 <-"[:space:]+[:digit:]+banos"
h3 <-"[:space:]+[:digit:]+baos"
h4 <-"[:space:]+[:digit:]+bao"
i1 <-"[:space:]+[:digit:]+[:space:]+bano"
i2 <-"[:space:]+[:digit:]+[:space:]+banos"
i3 <-"[:space:]+[:digit:]+[:space:]+baos"
i4 <-"[:space:]+[:digit:]+[:space:]+bao"
train<-train %>% mutate(bano = str_extract(string = train$description_num,
                                              pattern =  paste0(g1,"|",g2,"|",g3,"|",g4,"|",
                                                                h1,"|",h2,"|",h3,"|",h4,"|",
                                                                i1,"|",i2,"|",i3,"|",i4)))
train$bano_num<- as.numeric(gsub("[^0-9]", "", train$bano))

#altura/piso
j1 <- "[:digit:]+[:space:]+piso"
j2 <- "[:space:]+[:digit:]+[:space:]+piso"
j3 <- "piso+[:space:]+[:digit:]"
j4 <- "[:space:]+piso+[:space:]+[:digit:]"

train<-train %>% mutate(piso = str_extract(string = train$description_num,
                                           pattern =  paste0(j1,"|",j2,"|",j3,"|",j4)))
train$piso_num<- as.numeric(gsub("[^0-9]", "", train$piso))

#estrato
k1 <- "estrato+[:space:]+[:digit:]" 
k2 <- "[:space:]+estrato+[:digit:]"
k3 <- "[:space:]+estrato+[:space:]+[:digit:]"

train<-train %>% mutate(estrato = str_extract(string = train$description_num,
                                           pattern =  paste0(k1,"|",k2,"|",k3)))
train$estrato_num<- as.numeric(gsub("[^0-9]", "", train$estrato))

#años de construido / remodelado
# l1 <- "anos+[:space:]+[:digit:]" 
# l2 <- "[:space:]+anos+[:digit:]"
# l3 <- "[:space:]+anos+[:space:]+[:digit:]"
# train<-train %>% mutate(antiguedad = str_extract(string = train$description_num,
#                                               pattern =  paste0(l1,"|",l2,"|",l3)))
# train$antiguedad_num<- as.numeric(gsub("[^0-9]", "", train$antiguedad))
l1<-str_detect( train$description,"remodelado") 
train$remodel<-ifelse(l1==TRUE, 1,0 )

#tiene ascensor
m1<-str_detect( train$description,"ascensor") 
m2<-str_detect( train$description,"acensor") 
m3<-str_detect( train$description,"asensor") 
m4<-str_detect( train$description,"elevador") 
m5<-str_detect( train$description,"ascensores") 
m6<-str_detect( train$description,"acensores") 
m7<-str_detect( train$description,"asensores") 
m8<-str_detect( train$description,"elevadores") 
train$ascensor<-ifelse(m1==TRUE|m2==TRUE| m3==TRUE|m4==TRUE|m5==TRUE|m6==TRUE|m7==TRUE|m8==TRUE, 1,0 )


#Es iluminado
n1<-str_detect( train$description,"iluminado") 
n2<-str_detect( train$description,"luz")
train$iluminado<-ifelse(n1==TRUE|n2==TRUE, 1,0 )

#tiene parqueadero/garaje
o1<-str_detect( train$description,"parqueadero") 
o2<-str_detect( train$description,"garaje")
train$parqueo<-ifelse(o1==TRUE|o2==TRUE, 1,0 )

#tiene patio/jardín/terraza
p1<-str_detect( train$description,"patio") 
p2<-str_detect( train$description,"jardin")
p3<-str_detect( train$description,"terraza")
p4<-str_detect( train$description,"balcon")
train$patio<-ifelse(p1==TRUE|p2==TRUE|p3==TRUE|p4==TRUE, 1,0 )

### 3. Cambios a para base test

#aplicar función de limpieza a base test
test$descr_nueva <-limpieza(test$description)

#añadir vector a la base train
#test$NewColumn <- clean_description_test

#### 4. Variables  espaciales

#4.1 revisar variables disponibles en openmaps 
available_tags("leisure") 
available_tags("amenity")
available_tags ("highway")

#4.2 Parques
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
train <- train %>% mutate(distancia_parque = dist_min)










