
library(tidyverse)
library(FactoMineR)
library(NbClust)

df10 %>% colnames()

# Iteración ----
clustering <- NbClust(
  data = df10 %>% select(all_of(dimensiones)),
  distance = "euclidean",
  method = "ward.D2",
  index = "dunn"
)

clustering

plot(c(0, clustering$All.index))

# Creando 4 segmentos ----
clustering_4 <- NbClust(
  data = df10 %>% select(all_of(dimensiones)),
  distance = "euclidean",
  method = "ward.D2",
  index = "dunn",
  min.nc = 4,
  max.nc = 4
)

clustering_4

# Agregando las clasificaciones al df
df11 <- df10 %>% 
  mutate(segmento = as.factor(clustering_4$Best.partition))

# Análisis descriptivo ----

## Peso de los segmentos ----
table(clustering_4$Best.partition)
prop.table(table(clustering_4$Best.partition))


## Cruce de variables ----

df11 %>% 
  group_by(segmento) %>% 
  summarise(
    extroversion = mean(extroversion),
    aventura     = mean(aventura),
    tradicional  = mean(tradicional),
    empatia      = mean(empatia),
    introversion = mean(introversion)
  )


## Análisis de correpondencias ----

### Estandarizando las variables (a positivas)
library(scales)

df12 <- df11 %>% 
  mutate_at(.vars = dimensiones,
            .funs = rescale)

df13 <- df12 %>% 
  group_by(segmento) %>% 
  summarise(
    extroversion = mean(extroversion),
    aventura     = mean(aventura),
    tradicional  = mean(tradicional),
    empatia      = mean(empatia),
    introversion = mean(introversion)
  ) %>% 
  column_to_rownames("segmento")


### Corriendo en AnaCor
FactoMineR::CA(df13)





















