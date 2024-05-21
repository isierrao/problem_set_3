#Organizar base de datos
rm(list=ls())

#setwd("C:/Users/USUARIO/Documents/GitHub/problem_set_3/scripts")

train<-read.csv("C:/Users/USUARIO/Documents/GitHub/problem_set_3/scripts/train.csv")
test<-read.csv("C:/Users/USUARIO/Documents/GitHub/problem_set_3/scripts/test.csv")

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

#2.1 revisión de missings
p_load( visdat)
vis_dat(train)
#Nota: hay missings en rooms, bathrooms, y en mayor medida en surface total y surface covered

#Contabilizar total de observaciones sin missings
sum(table(train$surface_total))#7854
sum(table(train$surface_covered))#8565
sum(table(train$rooms))#20384
sum(table(train$bedrooms))#38644
sum(table(train$bathrooms))#28573

#2.2 Visualicemos la distribución de la variable del precio del inmueble
pr <- ggplot(train, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.4) +
  labs(x = "Valor de venta (log-scale)", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)

#2.3 Observamos la distribución de los inmuebles en el mapa de Bogotá
leaflet() %>%
  addTiles() %>%
  addCircles(lng = test$lon, 
             lat = test$lat)
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon, 
             lat = train$lat)

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

###############################    Cambios a base train    #############################################

train<-read.csv("train.csv")

## 4 Utilizar información descripción para cubrir missings

## 4.1función para reemplazar palabras por numeros
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

## 4.2 extraer metraje
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


## 4.3 habitaciones/alcobas/dormitorios
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
train$alcobas<- as.numeric(gsub("[^0-9]", "", train$alcobas))

## 4.4 baños
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
train$bano<- as.numeric(gsub("[^0-9]", "", train$bano))

## 4.5 Reemplazar missings en variables surface_total, rooms y bathrooms, y ver en cuanto incrementa la muestra
library(dplyr)
train <- train %>%
  mutate(surface_total = ifelse(is.na(surface_total), metraje, surface_total))
sum(table(train$surface_total))#20376

train <- train %>%
  mutate(rooms = ifelse(is.na(rooms), alcobas, rooms))
sum(table(train$rooms))#33745

train <- train %>%
  mutate(bathrooms = ifelse(is.na(bathrooms), bano, bathrooms))
sum(table(train$bathrooms))#32900

## 5. Añadir variables adicionales a partir de la descripción

## 5.1 Está remodelado
l1<-str_detect( train$description,"remodelado") 
train$remodel<-ifelse(l1==TRUE, 1,0 )
train %>%
  count(remodel)

## 5.2 tiene ascensor
m1<-str_detect( train$description,"ascensor") 
m2<-str_detect( train$description,"acensor") 
m3<-str_detect( train$description,"asensor") 
m4<-str_detect( train$description,"elevador") 
m5<-str_detect( train$description,"ascensores") 
m6<-str_detect( train$description,"acensores") 
m7<-str_detect( train$description,"asensores") 
m8<-str_detect( train$description,"elevadores") 
train$ascensor<-ifelse(m1==TRUE|m2==TRUE| m3==TRUE|m4==TRUE|m5==TRUE|m6==TRUE|m7==TRUE|m8==TRUE, 1,0 )
train %>%
  count(ascensor)

## 5.3 Es iluminado
n1<-str_detect( train$description,"iluminado") 
n2<-str_detect( train$description,"luz")
train$iluminado<-ifelse(n1==TRUE|n2==TRUE, 1,0 )
train %>%
  count(iluminado)

## 5.4 Tiene parqueadero/garaje
o1<-str_detect( train$description,"parqueadero") 
o2<-str_detect( train$description,"garaje")
train$parqueo<-ifelse(o1==TRUE|o2==TRUE, 1,0 )
train %>%
  count(parqueo)

## 5.5 Tiene patio/jardín/terraza
p1<-str_detect( train$description,"patio") 
p2<-str_detect( train$description,"jardin")
p3<-str_detect( train$description,"terraza")
p4<-str_detect( train$description,"balcon")
train$patio<-ifelse(p1==TRUE|p2==TRUE|p3==TRUE|p4==TRUE, 1,0 )
train %>%
  count(patio)

## 5.6 Tiene depósito
q1<-str_detect( train$description,"deposito")
train$deposito<-ifelse(q1==TRUE, 1,0 )
train %>%
  count(deposito)

## 5.7 altura/piso
j1 <- "[:digit:]+[:space:]+piso"
j2 <- "[:space:]+[:digit:]+[:space:]+piso"
j3 <- "piso+[:space:]+[:digit:]"
j4 <- "[:space:]+piso+[:space:]+[:digit:]"

train<-train %>% mutate(piso = str_extract(string = train$description_num,
                                           pattern =  paste0(j1,"|",j2,"|",j3,"|",j4)))
train$piso<- as.numeric(gsub("[^0-9]", "", train$piso))

#ver distribución de datos de piso dependiendo de si es casa o apartamento
train %>%
  count(property_type,piso)

#reemplazar missings dependiendo de la media de pisos de cada tipo de propiedad
train <- train %>%
  mutate(piso = ifelse(is.na(piso) & property_type == "Apartamento", 4, piso))
train <- train %>%
  mutate(piso = ifelse(is.na(piso) & property_type == "Casa", 2, piso))

#nota: para estimar las medias, esto se corrío antes de quitar todos los missing
train %>%
  group_by(property_type) %>%
  summarize(
    count = n(),
    mean = mean(piso, na.rm = TRUE),
    median = median(piso, na.rm = TRUE),
    sd = sd(piso, na.rm = TRUE),
    min = min(piso, na.rm = TRUE),
    max = max(piso, na.rm = TRUE)
  )


## 5.8 estrato
#nota: tiene muchos missing, por lo que puede ser mejor incluir esa variable con datos espaciales
# k1 <- "estrato+[:space:]+[:digit:]" 
# k2 <- "[:space:]+estrato+[:digit:]"
# k3 <- "[:space:]+estrato+[:space:]+[:digit:]"
# 
# train<-train %>% mutate(estrato = str_extract(string = train$description_num,
#                                            pattern =  paste0(k1,"|",k2,"|",k3)))
# train$estrato<- as.numeric(gsub("[^0-9]", "", train$estrato))


## 5.9 años de construido 
#Nota: hay muchos missing, por lo que no se utiliza
# l1 <- "anos+[:space:]+[:digit:]" 
# l2 <- "[:space:]+anos+[:digit:]"
# l3 <- "[:space:]+anos+[:space:]+[:digit:]"
# train<-train %>% mutate(antiguedad = str_extract(string = train$description_num,
#                                               pattern =  paste0(l1,"|",l2,"|",l3)))
# train$antiguedad_num<- as.numeric(gsub("[^0-9]", "", train$antiguedad))


## 6. Limpiar base de missings restantes 

## 6.1 Reemplazar missings de surface_total, rooms, bedrooms, bathrooms

#identificar nuevas medias
#mediana_sup_cubierta <- median(train$surface_covered, na.rm = TRUE)
mediana_sup_total<- median(train$surface_total, na.rm = TRUE)
stargazer(train,type="text")

#reemplazar medias en missings
process_missings<-  function(data, ...) {
  
  data <- data %>%
    mutate(rooms = replace_na(rooms, 3),
           bedrooms = replace_na(bedrooms, 3),
           bathrooms = replace_na(bathrooms, 2),
           #surface_covered = replace_na(surface_covered, mediana_sup_cubierta),
           surface_total = replace_na(surface_total, mediana_sup_total),
    )
}

train <- process_missings(train)

#Nota: hasta aqui la base mantiene sus 38644 obs "originales"

## 7. Limpiar outliers restantes

## 7.1 outliers de piso
train <- train %>%
  filter(property_type== "Apartamento" & piso >= 0 & piso <= 25|
           property_type== "Casa" & piso >= 0 & piso <= 6)
#Nota: aquí la base va en 38503 obs

## 7.2 outliers de habitaciones
#nota:aquí la base cae a 38425
p_load( visdat)
vis_dat(train)

train %>%
  count(rooms)
train <- train %>%
  filter(between(rooms, 0,  20))

train %>%
  count(bedrooms)

## 7.3 outliers de baños
#nota:aquí la base cae a 38401
train  %>%
  count(bathrooms)
train <- train %>%
  filter(between(bathrooms, 0,  23))

vis_dat(train, warn_large_data = FALSE)

## 7.4  outlier de valor por metro cuadrado
#Calculamos valor del metro cuadrado 
train <- train %>%
  mutate(precio_por_mt2 = round(price / surface_total, 0))%>%
  mutate(precio_por_mt2  =precio_por_mt2/1000000 )  ## precio x Mt2 en millones. 
stargazer(train["precio_por_mt2"],type="text")

#quitar outliers
#nota: aquí la base cae a 35910, no hay missings
train <- train %>%
  filter(between(precio_por_mt2, 0.10,  20))

train <- train %>%
  select(-surface_covered, -metraje, -alcobas,-bano)
vis_dat(train, warn_large_data = FALSE)

######################## Cambios a para base test ########################################################
test<-read.csv("test.csv")

## 8 Utilizar información descripción para cubrir missings
#Nota: la base inicia con 10286 obs

## 8.1 función para reemplazar palabras por numeros
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
test$description_num<-reemplazar_numeros(test$description)

## 8.2 extraer metraje
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

test<-test %>% mutate(metraje = str_extract(string = test$description_num,
                                            pattern =  paste0(a1,"|",a2,"|",a3,"|",a4,"|",a5,"|",a6,"|",
                                                              b1,"|",b2,"|",b3,"|",b4,"|",b5,"|",b6,"|",
                                                              c1,"|",c2,"|",c3,"|",c4,"|",c5,"|",c6)))
test$metraje<- as.numeric(gsub("[^0-9]", "", test$metraje))


## 8.3 habitaciones/alcobas/dormitorios
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

test<-test %>% mutate(alcobas = str_extract(string = test$description_num,
                                            pattern =  paste0(d1,"|",d2,"|",d3,"|",d4,"|",d5,"|",d6,"|",d7,"|",d8,"|",
                                                              e1,"|",e2,"|",e3,"|",e4,"|",e5,"|",e6,"|",e7,"|",e8,"|",
                                                              f1,"|",f2,"|",f3,"|",f4,"|",f5,"|",f6,"|",f7,"|",f8)))
test$alcobas<- as.numeric(gsub("[^0-9]", "", test$alcobas))

## 8.4 baños
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
test<-test %>% mutate(bano = str_extract(string = test$description_num,
                                         pattern =  paste0(g1,"|",g2,"|",g3,"|",g4,"|",
                                                           h1,"|",h2,"|",h3,"|",h4,"|",
                                                           i1,"|",i2,"|",i3,"|",i4)))
test$bano<- as.numeric(gsub("[^0-9]", "", test$bano))

## 8.5 Reemplazar missings en variables surface_total, rooms y bathrooms, y ver en cuanto incrementa la muestra
library(dplyr)
test <- test %>%
  mutate(surface_total = ifelse(is.na(surface_total), metraje, surface_total))
sum(table(test$surface_total))#5418

test <- test %>%
  mutate(rooms = ifelse(is.na(rooms), alcobas, rooms))
sum(table(test$rooms))#8887

test <- test %>%
  mutate(bathrooms = ifelse(is.na(bathrooms), bano, bathrooms))
sum(table(test$bathrooms))#8897

## 9. Añadir variables adicionales a partir de la descripción

## 9.1 Está remodelado
l1<-str_detect( test$description,"remodelado") 
test$remodel<-ifelse(l1==TRUE, 1,0 )
test %>%
  count(remodel)

## 9.2 tiene ascensor
m1<-str_detect( test$description,"ascensor") 
m2<-str_detect( test$description,"acensor") 
m3<-str_detect( test$description,"asensor") 
m4<-str_detect( test$description,"elevador") 
m5<-str_detect( test$description,"ascensores") 
m6<-str_detect( test$description,"acensores") 
m7<-str_detect( test$description,"asensores") 
m8<-str_detect( test$description,"elevadores") 
test$ascensor<-ifelse(m1==TRUE|m2==TRUE| m3==TRUE|m4==TRUE|m5==TRUE|m6==TRUE|m7==TRUE|m8==TRUE, 1,0 )
test %>%
  count(ascensor)

## 9.3 Es iluminado
n1<-str_detect( test$description,"iluminado") 
n2<-str_detect( test$description,"luz")
test$iluminado<-ifelse(n1==TRUE|n2==TRUE, 1,0 )
test %>%
  count(iluminado)

## 9.4 Tiene parqueadero/garaje
o1<-str_detect( test$description,"parqueadero") 
o2<-str_detect( test$description,"garaje")
test$parqueo<-ifelse(o1==TRUE|o2==TRUE, 1,0 )
test %>%
  count(parqueo)

## 9.5 Tiene patio/jardín/terraza
p1<-str_detect( test$description,"patio") 
p2<-str_detect( test$description,"jardin")
p3<-str_detect( test$description,"terraza")
p4<-str_detect( test$description,"balcon")
test$patio<-ifelse(p1==TRUE|p2==TRUE|p3==TRUE|p4==TRUE, 1,0 )
test %>%
  count(patio)

## 9.6 Tiene depósito
q1<-str_detect( test$description,"deposito")
test$deposito<-ifelse(q1==TRUE, 1,0 )
test %>%
  count(deposito)

## 9.7 altura/piso
j1 <- "[:digit:]+[:space:]+piso"
j2 <- "[:space:]+[:digit:]+[:space:]+piso"
j3 <- "piso+[:space:]+[:digit:]"
j4 <- "[:space:]+piso+[:space:]+[:digit:]"

test<-test %>% mutate(piso = str_extract(string = test$description_num,
                                         pattern =  paste0(j1,"|",j2,"|",j3,"|",j4)))
test$piso<- as.numeric(gsub("[^0-9]", "", test$piso))

#ver distribución de datos de piso dependiendo de si es casa o apartamento
test %>%
  count(property_type,piso)

#reemplazar missings dependiendo de la media de pisos de cada tipo de propiedad
test <- test %>%
  mutate(piso = ifelse(is.na(piso) & property_type == "Apartamento", 6, piso))
test <- test %>%
  mutate(piso = ifelse(is.na(piso) & property_type == "Casa", 3, piso))

#nota: para estimar las medias, esto se corrío antes de quitar todos los missing
test %>%
  group_by(property_type) %>%
  summarize(
    count = n(),
    mean = mean(piso, na.rm = TRUE),
    median = median(piso, na.rm = TRUE),
    sd = sd(piso, na.rm = TRUE),
    min = min(piso, na.rm = TRUE),
    max = max(piso, na.rm = TRUE)
  )

## 10. Limpiar base de missings restantes 

## 10.1 Reemplazar missings de surface_total, rooms, bedrooms, bathrooms

#identificar nuevas medias
#mediana_sup_cubierta <- median(test$surface_covered, na.rm = TRUE)
mediana_sup_total<- median(test$surface_total, na.rm = TRUE)
stargazer(test,type="text")

test %>%
  group_by(property_type) %>%
  summarize(
    count = n(),
    mean = mean(rooms, na.rm = TRUE),
    median = median(rooms, na.rm = TRUE),
    sd = sd(rooms, na.rm = TRUE),
    min = min(rooms, na.rm = TRUE),
    max = max(rooms, na.rm = TRUE)
  )
test %>%
  count(property_type,rooms)

#reemplazar medias en missings
process_missings<-  function(data, ...) {
  
  data <- data %>%
    mutate(rooms = replace_na(rooms, 3),
           bedrooms = replace_na(bedrooms, 2),
           bathrooms = replace_na(bathrooms, 3),
           #surface_covered = replace_na(surface_covered, mediana_sup_cubierta),
           surface_total = replace_na(surface_total, mediana_sup_total),
    )
}

test <- process_missings(test)

#Nota: hasta aqui la base mantiene sus 38644 obs "originales"

## 11. Limpiar outliers restantes

## 11.1 outliers de piso
test <- test %>%
  filter(property_type== "Apartamento" & piso >= 0 & piso <= 25|
           property_type== "Casa" & piso >= 0 & piso <= 6)
#Nota: aquí la base va en 10258 obs

## 11.2 outliers de habitaciones
#nota:aquí la base cae a 10235
p_load( visdat)
vis_dat(test)

test %>%
  count(rooms)
test <- test %>%
  filter(between(rooms, 0,  20))

test %>%
  count(bedrooms)

## 11.3 outliers de baños
#nota:aquí la base cae a 10223
test  %>%
  count(bathrooms)
test <- test %>%
  filter(between(bathrooms, 0,  23))

vis_dat(test, warn_large_data = FALSE)

test <- test %>%
  select(-surface_covered, -metraje, -alcobas,-bano)


################################## Variables Espaciales ###################################

## 12. Parques
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
#Nota: De acuerdo con las variables usadas en el paper de Zhaoyang et.,al tomamos algunas similares

## 13. Educacion
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

## 14. Estación de transporte público
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

## 15. Centros Comerciales
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

## 16. Bancos
#Agregamos la variable banco para aproximar a sector financiero
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

## 17. Parada de Autobus

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

##18. Delimitar los datos a Chapinero
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
