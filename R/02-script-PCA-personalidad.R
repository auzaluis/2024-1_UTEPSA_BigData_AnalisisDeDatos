
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
