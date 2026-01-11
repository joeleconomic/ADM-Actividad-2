# Actividad 2: Modelos de Clasificación – Predicción de Abandono de Clientes TELCO SL

## Descripción del Proyecto
El objetivo de esta actividad es desarrollar modelos de clasificación para predecir el **abandono de clientes** de la empresa TELCO SL, utilizando datos históricos del comportamiento de los clientes. Esto permitirá identificar qué clientes tienen mayor probabilidad de abandonar el servicio y entender qué variables influyen más en su decisión.

La base de datos original puede descargarse en [Kaggle](https://www.kaggle.com/blastchar/telco-customer-churn). Para la actividad se utilizará la versión procesada: `datos_teleco.csv`.

## Objetivos
1. **Partición de los datos**  
   Dividir el conjunto de datos en **entrenamiento (80%)** y **test (20%)** para evitar sobreajuste.

2. **Modelos de clasificación**  
   - **Modelo logit** (regresión logística) para predecir la variable `Abandono`.  
   - **Modelo de árbol de decisión** para la misma predicción.  
   - Variables utilizadas:  
     - `Contrato`  
     - `Factura digital`  
     - `Servicio Internet`  
     - `Soporte técnico`  
     - `Copia de Seguridad Online`  
     - `Televisión`  
     - `Meses de alta en el servicio`

3. **Evaluación de modelos**  
   - Comparar la **precisión** de ambos modelos y justificar cuál es más adecuado.

4. **Importancia de variables**  
   - Proponer un método para obtener un **ranking de importancia de las variables** en el modelo.

## Estructura de Archivos
- `datos_teleco.csv` – Conjunto de datos limpio y preparado para la actividad.  
- `actividad2_logit.R` – Script con la implementación del modelo logit.  
- `actividad2_arbol.R` – Script con la implementación del árbol de decisión.  
- `README.md` – Este archivo con la descripción y objetivos de la actividad.

## Rúbrica de Evaluación
| Criterio | Nivel 2 – Excelente | Nivel 1 – Adecuado | Nivel 0,5 – Insuficiente | Nivel 0 – No logrado |
|-----------|------------------|------------------|------------------------|-------------------|
| **Partición de datos** | Partición correcta y justificada (1.5 ptos) | Partición correcta sin justificar (0.9 ptos) | Partición incorrecta o sin sentido (0.45 ptos) | No realiza partición (0 ptos) |
| **Modelo logit** | Modelo correcto, interpretado y justificado (2.5 ptos) | Modelo correcto, interpretación parcial (1.5 ptos) | Modelo con errores o sin interpretación (0.75 ptos) | No desarrolla el modelo (0 ptos) |
| **Árbol de decisión y exactitud** | Árbol implementado, exactitud calculada y comparada (2.5 ptos) | Cálculo correcto, sin comparación o interpretación (1.5 ptos) | Cálculo erróneo o sin interpretación (0.75 ptos) | No implementa ni calcula (0 ptos) |
| **Importancia de variables** | Implementación o propuesta sólida e interpretada (3.5 ptos) | Propuesta básica o interpretación parcial (2.1 ptos) | Sin base técnica o interpretación débil (1.05 ptos) | No aporta nada (0 ptos) |

## Notas
- El proyecto se realizará en **R**, utilizando librerías como `caret`, `rpart` y `glm`.  
- Todos los pasos deben estar justificados y los resultados interpretados en términos del negocio.  
- La finalidad es que la empresa pueda aplicar estos modelos para anticipar la pérdida de clientes en el próximo otoño.
