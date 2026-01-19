# 1 Descripción de la tarea ----

# Toma de contacto con los datos, limpieza, transformación y análisis exploratorio inicial

# Dos modelos de clasificación que permitan predecir el abandono de un cliente 
# (Abandono) basado en la experiencia pasada.  Con el fin de responder a la 
# pregunta, podrá usar un modelo logit y de árbol de decisión y usando las 
# siguientes variables: Contrato, Factura digital, Servicio Internet, Soporte 
# técnico, Copia de Seguridad Online, Televisión, Meses de alta en el servicio 

# ¿Qué modelo de clasificación tiene una mayor precisión? Razone su respuesta

# ¿Cómo se podría obtener una clasificación de la importancia de las variables 
# en nuestro modelo?

# Particionar el conjunto de datos en entrenamiento y en 
# test con la proporción de 80/20 (entrenamiento/test). 
# Lo usaremos al final de la actividad como forma de VALIDACIÓN DEL MODELO


# 2 Carga y transformación de datos ----
## 2.1 Librerias necesarias ----
library(tidyverse)    # Para tratamiento y visualización de datos
library(lubridate)    # Manejo de fechas
library(ggplot2)      # Visualización
library(psych)        # Estadística descriptiva
library(corrplot)     # Matriz de correlación
library(forecast)     # Predicciones de series temporales
library(broom)        # Para tidy summaries
library(margins)      # Para odds ratios y efectos marginales
library(performance)  # Para diagnósticos de modelo
library(yardstick)    # Para métricas precisas de clasificación
library(pROC)         # Para curva ROC y AUC
library(caret)        # Para clasificación y entrenamiento de datos
library(rpart)        # Para implementar árboles de decisión
library(rpart.plot)   # Para visualizar árboles de desición
library(caTools)      # Para divivir la muestra con sample.split
library(ggplot2)      #


## 2.2 Carga de datos ----
data <- read.csv("datos_teleco_Act2_ADMN.csv")

## 2.3 Conocer y tratar los datos ----

# Ver el set de datos
str(data)
summary(data)
head(data)

# A) Detección de filas duplicadas
num_duplicados <- sum(duplicated(data))  # No hay

# B) Detección de Valores Ausentes (NAs)
colSums(is.na(data))  # No hay

# C) Categorizar dummies como factores
data2 <- data |> 
  mutate(
    Jubilado = factor(Jubilado, levels = c(0, 1), labels = c("No", "Yes")),
    Sexo = as.factor(Sexo),
    Socio = as.factor(Socio),
    Empleado = as.factor(Empleado),
    Servicio_telefonico = as.factor(Servicio_telefonico),
    Lineas_multiples = as.factor(Lineas_multiples),
    Servicio_Internet = as.factor(Servicio_Internet),
    Seguridad_Online = as.factor(Seguridad_Online),
    CopiaSeguridad_Online = as.factor(CopiaSeguridad_Online),
    Proteccion_dispositivo = as.factor(Proteccion_dispositivo),
    Soporte_tecnico = as.factor(Soporte_tecnico),
    Television_carta = as.factor(Television_carta),
    Peliculas_carta = as.factor(Peliculas_carta),
    Contrato = as.factor(Contrato),
    Factura_digital = as.factor(Factura_digital),
    Metodo_pago = as.factor(Metodo_pago),
    Abandono = as.factor(Abandono),
    meses_alta_cut = as.factor(meses_alta_cut)
  ) |> 
# Selección de las variables requeridas para el modelo
  select(Abandono, Contrato, Factura_digital, Servicio_Internet, Soporte_tecnico,
         CopiaSeguridad_Online, Television_carta, Meses_alta)

# D) Verificación del dataset limpio 'data2'
str(data2)
glimpse(data2)
summary(data2)

## 2.4 Análisis descriptivo inicial de la tasa de abandono ----
cat("\n--- DISTRIBUCIÓN DE LA VARIABLE OBJETIVO (ABANDONO) ---\n")
conteo <- table(data2$Abandono)
porcentajes <- prop.table(conteo) * 100
print(conteo)
print(round(porcentajes, 2))

## 2.5 Análisis Bivariante (Factores Clave) ----
# Cruzamos variables para ver tendencias a simple vista.

# A) Abandono vs Tipo de Contrato
cat("\n--- TASA DE ABANDONO POR TIPO DE CONTRATO ---\n")
tabla_contrato <- table(data2$Contrato, data2$Abandono)
print(prop.table(tabla_contrato, margin = 1) * 100)
# Interpretación: Observa cómo el contrato mensual tiene mucha más fuga.

# B) Abandono vs Servicio de Internet
cat("\n--- TASA DE ABANDONO POR TIPO DE INTERNET ---\n")
tabla_internet <- table(data2$Servicio_Internet, data2$Abandono)
print(prop.table(tabla_internet, margin = 1) * 100)

## 2.6 Análisis de Antigüedad ----
# ¿Se van más los clientes nuevos o los antiguos?
cat("\n--- MEDIA DE MESES DE ALTA (ANTIGÜEDAD) ---\n")
# Calculamos la media de meses para los que se quedan (No) vs los que se van (Yes)
medias_antiguedad <- tapply(data2$Meses_alta, data2$Abandono, mean, na.rm=TRUE)
print(medias_antiguedad)

# Visualización (Boxplot) 
boxplot(Meses_alta ~ Abandono, data = data2,
        main = "Antigüedad (Meses) vs Abandono",
        xlab = "¿Abandona?", ylab = "Meses de Antigüedad",
        col = c("forestgreen", "red"))


# 3 Modelización ----

## 3.1 Modelo Logit ----
m1 <- glm(Abandono ~ Contrato + Factura_digital + Servicio_Internet +
            Soporte_tecnico + CopiaSeguridad_Online + Television_carta +
            Meses_alta, 
          family = "binomial",
          data = data2)

summary(m1)  ## ¿Cómo se podría obtener una clasificación de la importancia de las variables en nuestro modelo? 
## Analizamos en el informe los Signif. codes de cada variable

# A) Odd Ratios: Para poder interpretar estos β (Coeficientes estimados)
OR <- exp(cbind(coef(m1), confint(m1)))
print("Odds Ratios (IC 95%):")
print(OR)

# B) Efecto Marginal Promedio (AME): Cómo cambia la probabilidad de Abandono,
# cuando otra variable cambia, en el caso de las Dummies, se evalua con la 
# categoría de referencia.
margins(m1)

# C) Predicción del modelo LOGIT
m1pred <- predict(m1, type = "response")

# Se puede observar que hemos hecho una predicción para cada individuo,
# pero, nuestras predicciones se encuentran entre 0 y 1 -> Hay que codificarlas
View(m1pred)

# D) Predicción del modelo LOGIT haciendo la codificación a "Yes" / "No",
# Además fijamos el umbral de probabilidad en 0.5
m1predcod <- ifelse(m1pred > 0.5, "Yes", "No") |> 
  factor(levels = c("No", "Yes"))

# E) Matriz de confusión
confusionMatrix(m1predcod, data2$Abandono, positive = "Yes")
### El modelo ha acertado un 79.22% de las veces ###

# F) Probaremos a cambiar el umbral a 0.65, 
# para comprobar si esto mejora o empeora el modelo
m1predcod1 <- ifelse(m1pred > 0.65, "Yes", "No") |> 
  factor(levels = c("No", "Yes"))

# Matriz de confusión
confusionMatrix(m1predcod1, data2$Abandono, positive = "Yes")

# Ahora, el modelo acertó un 78.28% de las veces, por lo que EMPEORÓ el modelo

### NOS QUEDAREMOS CON EL UMBRAL DE PROBABILIDAD FIJADO EN 0.5 ### 
## QUE ACIERTA EN UN 79.22% DE LAS VECES ###


## 3.2 Modelo Arbol de decisión ---- 

# Estimación del modelo
m2 <- rpart(Abandono ~ Contrato + Factura_digital + Servicio_Internet +
            Soporte_tecnico + CopiaSeguridad_Online + Television_carta +
            Meses_alta,
            method = "class",
            data = data2)

# Resumen del módelo árbol de decisión
summary(m2)

# Visualización del árbol de decisión
rpart.plot(m2, tweak=1, branch.col = "grey40", branch.lwd = 2,
           compress = TRUE, uniform = TRUE)

# Predicción de la probabilidad de abandono según el modelo árbol de decisión
# asignando directamente a la clase mayoritaria en cada hoja del árbol
m2pred <- predict(m2, type = "class")

# Matriz de confusión
confusionMatrix(m2pred, data2$Abandono, positive = "Yes")

## 4 ¿Qué modelo de clasificación tiene una mayor precisión? ----

# a. Predicciones con el Modelo Logit (m1)
# Calculamos la probabilidad y convertimos a clase "Yes/No" usando el umbral 0.5
prob_logit <- predict(m1, type = "response")
pred_logit <- factor(ifelse(prob_logit > 0.5, "Yes", "No"), levels = c("No", "Yes"))

# b. Predicciones con el Modelo de Árbol (m2)
# El árbol permite obtener directamente la clase predicha
pred_tree <- predict(m2, type = "class")

# c. Matrices de Confusión
# Comparamos las predicciones contra la columna real 'Abandono' de data2
cm_logit <- confusionMatrix(pred_logit, data2$Abandono, positive = "Yes")
cm_tree  <- confusionMatrix(pred_tree, data2$Abandono, positive = "Yes")

# d. Cálculo de AUC (Área bajo la curva)
roc_logit <- roc(data2$Abandono, prob_logit)
# Para el árbol extraemos la probabilidad de la columna "Yes"
prob_tree <- predict(m2, type = "prob")[, "Yes"]
roc_tree  <- roc(data2$Abandono, prob_tree)

# --- RESULTADOS ---
cat("--- PRECISIÓN (ACCURACY) ---\n")
cat("Logit:", round(cm_logit$overall["Accuracy"], 4), "\n")
cat("Árbol:", round(cm_tree$overall["Accuracy"], 4), "\n\n")

cat("--- ÁREA BAJO LA CURVA (AUC) ---\n")
cat("Logit:", round(auc(roc_logit), 4), "\n")
cat("Árbol:", round(auc(roc_tree), 4), "\n")

# 1. Crear el objeto de la gráfica
plot_roc <- ggroc(list(Logit = roc_logit, Arbol = roc_tree), size = 1) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "grey") + # Línea de referencia
  labs(title = "Comparación de Curvas ROC",
       subtitle = "Modelo Logit vs. Árbol de Decisión",
       x = "Especificidad (1 - Falsos Positivos)",
       y = "Sensibilidad (Verdaderos Positivos)",
       color = "Modelo") +
  theme_minimal()

# 2. Mostrar la gráfica
print(plot_roc)

## 5 Partición del conjunto de datos: validación del modelo ----

##### DEJO ESTO POR AQUÍ POR SI FUERA ÚTIL PARA LA PARTICIÓN CON LA QUE SE VALIDA EL MODELO:
# (Era lo que inicialmente estaba en el Modelo Logit de Joel, que incluía una partición)

## Modelo Logit ----

m1 <- glm(Abandono ~ Contrato + Factura_digital + Servicio_Internet +
          Soporte_tecnico + CopiaSeguridad_Online + Television_carta +
          Meses_alta, 
          family = "binomial",
          data = data2)

summary(m1)

# Efecto Marginal Promedio (AME): Cómo cambia la probabilidad de Abandono,
# cuando otra variable cambia, en el caso de las Dummies, se evalua con la 
# categoría de referencia.
margins(m1)

# Establecer datos y matriz de confusión

set.seed(123) # Para garantizar la reproducibilidad de los resultados.

# Partición 80% train, 20% test
train_index <- createDataPartition(data2$Abandono, p = 0.8, list = FALSE)

data_train <- data2[train_index, ]
data_test  <- data2[-train_index, ]
prob_test <- predict(m1, newdata = data_test, type = "response")

pred_test <- ifelse(prob_test > 0.5, "Yes", "No") |> 
  factor(levels = c("No", "Yes"))

confusionMatrix(pred_test, data_test$Abandono)

# 79.29% de las observaciones se clasificaron correctamente
