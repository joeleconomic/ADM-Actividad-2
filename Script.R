
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



# 2. Carga y transformación de datos ----
## 2.1 Librerias necesarias ----
library(tidyverse)    # Para tratamiento y visualización de datos
library(broom)        # Para tidy summaries
library(margins)      # Para odds ratios y efectos marginales
library(performance)  # Para diagnósticos de modelo
library(yardstick)    # Para métricas precisas de clasificación
library(pROC)         # Para curva ROC y AUC

## 2.2 Carga de datos ----
data <- read.csv("datos_teleco_Act2_ADMN.csv")

## 2.3 Conocer y tratar los datos ----

# Ver el set de datos
str(data)
summary(data)
colSums(is.na(data))

# Categorizar dummies como factores
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
  )

summary(data2)

# Análisis descriptivo de la tasa de abandono
table(data2$Abandono)
prop.table(table(data2$Abandono))

# 3 Modelo Logit ----

## 3.1 Modelo 1 ----

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


