
# 1. Descripción de la tarea ----

# Con el fin de evitar el sobreajuste, y como paso previo a la aplicación de 
# clasificadores, deberá particionar el conjunto de datos en entrenamiento y en 
# test con la proporción de 80/20 (entrenamiento/test).

# Dos modelos de clasificación que permita predecir el abandono de un cliente 
# (Abandono) basado en la experiencia pasada.  Con el fin de responder a la 
# pregunta, podrá usar un modelo logit y de árbol de decisión y usando las 
# siguientes variables: Contrato, Factura digital, Servicio Internet, Soporte 
# técnico, Copia de Seguridad Online, Televisión, Meses de alta en el servicio, 

# ¿Qué modelo de clasificación tiene una mayor precisión? Razone su respuesta
# ¿Cómo se podría obtener una clasificación de la importancia de las variables 
# en nuestro modelo?



# 2. Pasos previos a la modelización ----
# Librerias necesarios
library(tidyverse)    # Para tratamiento y visualización de datos
library(broom)        # Para tidy summaries
library(margins)      # Para odds ratios y efectos marginales
library(performance)  # Para diagnósticos de modelo
library(yardstick)    # Para métricas precisas de clasificación
library(pROC)         # Para curva ROC y AUC

# Carga de datos
data <- read.csv("datos_teleco_Act2_ADMN.csv")

# Ver los datos
str(data)
summary(data)
colSums(is.na(data))

# Análisis descriptivo de la tasa de abandono
table(data$Abandono)
prop.table(table(data$Abandono))
