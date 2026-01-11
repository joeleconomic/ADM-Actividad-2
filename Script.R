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

