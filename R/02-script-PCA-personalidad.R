
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





























