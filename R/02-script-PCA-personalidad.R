
load(file = "R/data-personalidad.RData")

# Librerías
library(tidyverse)
library(FactoMineR)
library(ggcorrplot)
library(plotly)

# Matriz de correlaciones ----
frases2

r <- cor(
  x = df6 %>% select(all_of(frases2)),
  method = "pearson"
)

# Gráfico de correlaciones ----

ggplotly(
  ggcorrplot(
    corr = r,
    colors = c("red", "white", "blue"),
    show.legend = F,
    tl.cex = 8
  ) +
  theme(axis.text.x = element_blank(),
        panel.grid = element_blank())
)

# PCA: Principal Component Analysis ----

## Dimensión 01: Extroversión ----

### Definiendo el vector con las frases
extroversion <- frases2[c(8,20,23)]

### Creando la dimensión
PCA.extroversion <- FactoMineR::PCA(
  X = df6 %>% select(all_of(extroversion)),
  ncp = 1,
  scale.unit = TRUE
)

### Eigenvalue y varianza explicada
PCA.extroversion$eig

### Correlación entre la CP1 y las variables originales
PCA.extroversion$var$cor

### Valores de la nueva variable (CP1)
PCA.extroversion$ind$coord %>% head()

### Tibble para comparar CP1 con variables originales
data.frame(
  extroversion = PCA.extroversion$ind$coord,
  df6 %>% select(all_of(extroversion))
) %>% View()


## Dimensión 02: Aventura (*-1) ----

### Definiendo el vector con las frases
aventura <- frases2[c(7,19)]

### Creando la dimensión
PCA.aventura <- FactoMineR::PCA(
  X = df6 %>% select(all_of(aventura)),
  ncp = 1
)

### Eigenvalues y varianza explicada
PCA.aventura$eig

### Correlación entre la CP1 y las variables originales
PCA.aventura$var$cor

### Comparar los valores originales con la CP1
data.frame(
  CP1 = PCA.aventura$ind$coord,
  df6 %>% select(all_of(aventura))
) %>% View()



## Dimensión 03: Tradicional (*-1) ----

### Definiendo el vector con las frases
tradicional <- frases2[c(9,12,14)]

### Creando la dimensión
PCA.tradicional <- FactoMineR::PCA(
  X = df6 %>% select(all_of(tradicional)),
  ncp = 1
)

### Eigenvalues y varianza explicada
PCA.tradicional$eig

### Correlación entre la CP1 y las variables originales
PCA.tradicional$var$cor



## Dimensión 04: Empatía ----

### Definiendo el vector con las frases
empatia <- frases2[c(1,18,3)]

### Creando la dimensión
PCA.empatia <- FactoMineR::PCA(
  X = df6 %>% select(all_of(empatia)),
  ncp = 1
)

### Eigenvalues y varianza explicada
PCA.empatia$eig

### Correlación entre la CP1 y las variables originales
PCA.empatia$var$cor



## Dimensión 05: introversion (*-1) ----

### Definiendo el vector con las frases
introversion <- frases2[c(21,22)]

### Creando la dimensión
PCA.introversion <- FactoMineR::PCA(
  X = df6 %>% select(all_of(introversion)),
  ncp = 1
)

### Eigenvalues y varianza explicada
PCA.introversion$eig

### Correlación entre la CP1 y las variables originales
PCA.introversion$var$cor


# Adjuntando las dimensiones al df ----
df10 <- df6 %>% 
  mutate(
    extroversion = PCA.extroversion$ind$coord,
    aventura = PCA.aventura$ind$coord *-1,
    tradicional = PCA.tradicional$ind$coord *-1,
    empatia = PCA.empatia$ind$coord,
    introversion = PCA.introversion$ind$coord *-1
  )

# Crear un vector para las dimensiones
dimensiones <- c(
  "extroversion",
  "aventura",
  "tradicional",
  "empatia",
  "introversion"
)

























