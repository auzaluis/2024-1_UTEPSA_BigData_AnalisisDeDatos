
# Librerías
library(tidyverse)
library(car)
library(plotly)

set.seed(2024)

df = data.frame(
  metrica = c(
    rnorm(500, mean = 50, sd = 10),
    rnorm(500, mean = 55, sd = 10),
    rnorm(500, mean = 60, sd = 10)
  ),
  factor = factor(
    rep(c("A", "B", "C"), each = 500)
  )
)

df %>% head()
df %>% tail()

# Boxplot
ggplotly(
  ggplot(data = df,
         aes(x = factor, y = metrica, fill = factor)) +
    geom_boxplot()
)


# ANOVA
anova <- aov(metrica ~ factor, data = df)
summary(anova)

# Supuestos
## Normalidad
shapiro.test(anova$residuals)

## Homocedasticidad
leveneTest(df$metrica,
           group = df$factor,
           center = mean)

# En caso no se cumplan los supuestos (kruskal)
kruskal.test(metrica ~ factor, data = df)




