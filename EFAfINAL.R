---
title: "Soto"
author: "Alfredo Aro Terleira"
date: "2025-06-16"
output: html_document
---
  
  # Análisis Factorial
  
  ## Importamos la data
  
  ```{r}
link = "https://raw.githubusercontent.com/alfredoaroterleira/UNMSM_Soto/refs/heads/main/data.csv"
data = read.csv(link)
data
```

El análisis factorial exploratorio cuenta con algunos problemas creo yo, ya que las variables resultan categóricas, y muchas de ellas no son ordinales, por lo que asignarles un valor resulta complicado o tratarlas como variables numéricas. 

Por ejemplo, la p2, p3, p5, p7, p15 y p17 no tienen un orden intrínseco. Convertirlas a numéricas asume un orden artificial ("Respeto derecho" = 1 vs "Libertad de expresión"=2, no implica que 2 > 1)

La solución más inmediata resulta excluirlas del EFA

Por otro lado, tenemos variables condicionales (p11 y p12) que tienen missing no aleatorios, por lo que creo que también deberíamos excluirlos en una prima instancia. La p11 solo aplica a quienes respondieron que No en p10 y la p12 a quienes respondieron que SÍ en la p10. Resultan variables mutuamente excluyenetes. 
Cada observación o solo tendrá marcada p11 o solo p12, pero nunca, en teoría, ambas. 

Por otro lado, la p16 cuenta con el 0 (No sé = 0), pero el 0 no es parte de la escala ordinal. Podemos recodificarlo.

Creo que las variables apropiadas (aunque las que excluimos creo que le puedes dar un tratamiento futuro) son las siguiente: p1, p4, p6, p8, p9, p10, p13, p14, p16(recodificada) y p18


## Preparación de datos

```{r}
install.packages("polycor")
install.packages("stringi")
install.packages("tidyverse")
library (dplyr)
library(tidyverse)

data = data %>%
select(p8, p9, p10, p13, p14, p18)
str(data)
```

```{r}
# Verificamos la 

glimpse(data)
```
```{r}
data <- data.frame(lapply(data, as.numeric))
str(data)
```


## Análisis Factorial Exploratorio (EFA)

Calculamos las correlaciones entre todas las variables
```{r}
#creamos la matriz de correlaciones (en nuestro caso policóricas, ya que tenemos variables ordinales)
install.packages("psych")
library(polycor)
library(psych)

cor_matrix = polychoric(data, correct = 0)$rho
```

El objeto cor_matrix guarda las correlaciones entre todas las variables

```{r}
round(cor_matrix,2)
```
```{r}
install.packages("ggcorrplot")
library(ggcorrplot)

ggcorrplot(cor_matrix)
```

### Verificar si los datos permiten factorizar

```{r}
library(psych)
psych::KMO(cor_matrix)  #mayor a 0.6 es aceptable
```
### Verificar si la matriz de correlación es adecuada

- Matriz identidad

```{r}
cortest.bartlett(cor_matrix,n=nrow(data))$p.value>0.05
```
- Matriz singular

```{r}
install.packages("matrixcalc")
library(matrixcalc)

is.singular.matrix(cor_matrix)
```
Ambas FALSE, así que todo bien. Seguimos.


### Determinar en cuantos factores o variables latentes podríamos redimensionar la data.

```{r}
eigen_values <- eigen(cor_matrix)$values
print(eigen_values)
```
```{r}
#Gráfico de sedimentación
library(psych)

psych::scree(cor_matrix, factors = TRUE, main = "Scree Plot")
```


```{r}
#Análisis paralelo
library(psych)

psych::fa.parallel(cor_matrix, 
                   n.obs = nrow(data), 
                   fa = "fa", 
                   main = "Análisis Paralelo")
```

Nos señala como mejor opción optar por emplear 4 variables

## Ejecución

```{r}
library(GPArotation)

afe <- psych::fa(
  r = cor_matrix,          # Matriz de correlaciones
  nfactors = 4,            # Número de factores 
  n.obs = nrow(data), # Número de observaciones
  rotate = "promax",       # Rotación oblicua (factores correlacionados)
  fm = "ml",               # Método: Máxima Verosimilitud
  scores = "regression",   # Método para calcular puntuaciones
  missing = TRUE
)
```



### Interpretación

```{r}
# A. Cargas factoriales
print("Cargas Factoriales:")
print(afe$loadings, cutoff = 0.40, sort = TRUE)
```

```{r}
# B. Comunalidades
print("Comunalidades:")
print(afe$communalities)
```


```{r}
# C. Varianza explicada
print("Varianza Explicada:")
print(afe$Vaccounted)
```
```{r}
# D. ¿Qué variables contribuyen a la construcción de más de un factor?

sort(afe$complexity)
```
```{r}
# E. Tucker Lewis

afe$TLI
```

```{r}
# F. RMS cerca a cero
afe$rms
```

```{r}
# G. RMSEA cerca a cero
afe$RMSEA
```
```{r}
# H. BIC
afe$BIC
```


```{r}
# I. Diagrama de factores
library(psych)

psych::fa.diagram(afe, main = "Diagrama de Factores")
```

## Obtención de índices

```{r}
library(dplyr)
library(psych)

data <- data %>%
  mutate(
    indice_1 = rowMeans(select(., p13, p10, p14, p18), na.rm = TRUE),
    indice_1_z = scale(indice_compromiso)
  )

data <- data %>% 
  mutate(
    indice_2 = rowMeans(select(., p9, p8), na.rm = TRUE),
    indice_2_z = scale(indice_participacion)
  )

#Verificamos
psych::describe(data %>% select(starts_with("indice")))

```