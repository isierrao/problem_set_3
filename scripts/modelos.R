p_load(caret, neuralnet)



# Elegir si se prueba con particiones o sobre los datos completos
opt <- 1 # 1 se hacen particiones para medir F1 de antemano, 0 se deja completo los datos


if (opt==1){
  #subdivisión de la base de datos para ir revisando el desempeño del modelo
  inTrain <- createDataPartition(
    y = train$price,## La variable dependiente u objetivo 
    p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
    list = FALSE)
  train_1 <- train[ inTrain,]
  test_1 <- train[-inTrain,]
}


reg_lineal <- lm(price ~ surface_total + bedrooms +
    rooms + property_type + lat + lon + 
     ascensor  + parqueo + patio +
     deposito  + distancia_escuela +
     distancia_estacion + distancia_comercial +
     distancia_bus , data = train)

predict_lm <- test %>% mutate(price = predict(reg_lineal, 
                                newdata = test)) %>%
    select(property_id, price)

  
mae(predict_lm$price, as.numeric(predict_lm$price_lm))


###########################

train_control <- trainControl(method = "cv", number = 10)
hyper_grid <- expand.grid(alpha = seq(0, 1, length = 10),
                          lambda = seq(0.01, 0.1, by = 0.01))


model <- train(price ~ surface_total + bedrooms +
                 property_type + lat + lon + 
                 ascensor + parqueo + patio +
                 deposito + precio_por_mt2 + distancia_escuela +
                 distancia_estacion + distancia_comercial +
                 distancia_bus, data = train_1, 
               method = "glmboost", 
               trControl = train_control)


predict <- test_1 %>% mutate(price_lm = predict(model, 
                                                   newdata = test_1)) %>%
  select(price_lm, price)

mae(predict$price, as.numeric(predict$price_lm))



model_pcr <- train(price ~ surface_total + bedrooms +
                     property_type + lat + lon + 
                     ascensor + parqueo +
                     deposito + precio_por_mt2 + distancia_escuela +
                     distancia_estacion + distancia_comercial +
                     distancia_bus, 
                   data = train_1, 
                   method = "pcr", 
                   trControl = train_control)


predict_pcr <- test_1 %>% mutate(price_lm = predict(model_pcr, 
                                                newdata = test_1)) %>%
  select(price_lm, price)

mae(predict_pcr$price, as.numeric(predict_pcr$price_lm))


grid <- expand.grid(
  n.trees = c(100, 200, 300),      # Número de árboles
  interaction.depth = c(1, 3, 5),  # Profundidad de interacción
  shrinkage = c(0.01, 0.1),        # Tasa de aprendizaje
  n.minobsinnode = c(10, 20)       # Número mínimo de observaciones en nodos terminales
)

# Entrenar el modelo GBM
model_gbm <- train(price ~ surface_total + bedrooms + 
                     property_type + lat + lon + ascensor + parqueo + patio +
                     deposito + precio_por_mt2 + distancia_escuela +
                     distancia_estacion + distancia_comercial + distancia_bus, 
                   data = train_1, 
                   method = "gbm", 
                   trControl = train_control, 
                   tuneGrid = grid,
                   verbose = FALSE)

predict_pcr <- test_1 %>% mutate(price_lm = predict(model_gbm, 
                                                    newdata = test_1)) %>%
  select(price_lm, price)

mae(predict_pcr$price, as.numeric(predict_pcr$price_lm))

#red neuronal

model_neuralnet <- train(price ~ surface_total + bedrooms + 
                     property_type + lat + lon + ascensor + parqueo + patio +
                     deposito + precio_por_mt2 + distancia_escuela +
                     distancia_estacion + distancia_comercial + distancia_bus, 
                   data = train_1, 
                   method = "neuralnet", 
                   trControl = train_control)
# Chapinero ---------------------------------------------------------------

limites <- getbb("Chapinero Bogota Colombia")

test_chapinero <- test %>%
  filter(
    between(lon, limites[1, "min"], limites[1, "max"]) & 
      between(lat, limites[2, "min"], limites[2, "max"])
  )


train_chapinero <- train %>%
  filter(
    between(lon, limites[1, "min"], limites[1, "max"]) & 
      between(lat, limites[2, "min"], limites[2, "max"])
  )
