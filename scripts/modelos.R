p_load(caret)



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
     distancia_bus , data = train_1)

predict_lm <- test_1 %>% mutate(price_lm = predict(reg_lineal, 
                                newdata = test_1)) %>%
    select(price_lm, price)
  
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
               method = "glmnet", 
               trControl = train_control, 
               tuneGrid = hyper_grid)


predict <- test_1 %>% mutate(price_lm = predict(model, 
                                                   newdata = test_1)) %>%
  select(price_lm, price)

mae(predict$price, as.numeric(predict$price_lm))
