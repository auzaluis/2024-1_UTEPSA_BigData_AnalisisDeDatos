
# Tema 1: Carga de datos ----

## Carga local ----
df <- read.csv("R/Personalidad y uso de apps (respuestas) - Respuestas de formulario 1.csv",
               check.names = F)

colnames(df)


## Carga desde la API de Google ----
# install.packages("gsheet")
library(gsheet)

url_google <- "https://docs.google.com/spreadsheets/d/1IQ_RxxTSmBKHTExlxboIRNlMov_F6RyqdcOPrflCv_w/edit?usp=sharing"

# antes de esta ejecución el df tiene 216 obs
df <- read.csv(text = gsheet2text(url_google),
               check.names = F)



## Estructura el df ----
class(df)
class(df$Sexo)
class(df$`Escribe tu edad exacta`)
nrow(df) # Número de filas
ncol(df) # Número de columnas



# Tema 2: Transformación de datos ----

## Valores perdidos ----

# Los NA pueden ser tratados de 2 maneras:
# 1. Imputarlos/reemplazarlos
# 2. Eliminarlos/ignorarlos

df$`Escribe tu edad exacta`
summary(is.na(df$`Escribe tu edad exacta`))

### Reemplazo por el promedio
# install.packages("tidyverse")
library(tidyverse)

mean(df$`Escribe tu edad exacta`, na.rm = T)

df2 <- df %>% 
  mutate(edad2 = ifelse(test = is.na(`Escribe tu edad exacta`),
                        yes  = mean(df$`Escribe tu edad exacta`, na.rm = T),
                        no   = `Escribe tu edad exacta`)) %>% 
  relocate(edad2, .after = `Escribe tu edad exacta`)


### Eliminar la fila completa
df2 <- na.omit(df2)



## Estandarización de variables ----

### Normalización ----
scale(df2$`Escribe tu edad exacta`)

df3 <- df2 %>% 
  mutate(edadZ = scale(`Escribe tu edad exacta`)) %>% 
  relocate(edadZ, .after = edad2)


### Rango ----
library(scales)
rescale(df3$`Escribe tu edad exacta`)

data.frame(
  original = df3$`Escribe tu edad exacta`,
  rango = rescale(df3$`Escribe tu edad exacta`)
)



## Agrupaciones ----

### Numéricas ----
df4 <- df3 %>% 
  mutate(edadGR = cut(`Escribe tu edad exacta`,
                      breaks = c(-Inf, 18, 21, Inf),
                      labels = c("18 o menos",
                                 "19 a 21",
                                 "22 o más"))) %>% 
  relocate(edadGR, .after = `Escribe tu edad exacta`)

summary(df4$edadGR)


### Categóricas ----

unique(df4$`Según tu forma de ser ¿Cuál de las siguientes frases te describe mejor: [No discrimino y trato a todos por igual]`)
colnames(df4)
unique(df4[,8])

ifelse(test = df4[,8] == "Un poco verdadero" | df4[,8] == "Totalmente verdadero",
       yes = 1,
       no = 0)


# Bucles
df5 <- df4

## Paso 1: Crear un vector que contenga los nombres de las columnas

frases <- df5 %>% 
  select(starts_with("Según tu ")) %>%
  colnames()

## Crear el bucle

for (frase in frases) {
  df5[,frase] <- ifelse(
    test = df5[,frase] == "Un poco verdadero" | df5[,frase] == "Totalmente verdadero",
    yes  = 1,
    no   = 0
  )
}


# Tema 3: Manipulación de datos ----

df5 <- as_tibble(df5)

## Función select(): columnas ----
df5 %>% select(`Marca temporal`)
df5 %>% select(`Marca temporal`, `Escribe tu edad exacta`)
df5 %>% select(-c(`Marca temporal`, `¿Estás estudiando en algún colegio, universidad o instituto?`))
df5 %>% select(-c(`Marca temporal`:edadZ))
df5 %>% select(starts_with("edad"))
df5 %>% select(contains("edad"))
df5 %>% select(ends_with("00:00"))


## Función filter(): filas ----
df5 %>% filter(Sexo == "Hombre")
df5 %>% filter(Sexo != "Mujer")
df5 %>% filter(edadGR == "18 o menos" | edadGR == "19 a 21")
df5 %>% filter(edadGR %in% c("18 o menos", "19 a 21"))
df5 %>% filter(!edadGR %in% c("18 o menos", "19 a 21"))
df5 %>% filter(edad2 >= 21)
df5 %>% filter(edad2 < 21)
df5 %>% filter(between(edad2, 18, 21))
df5 %>% filter(edad2 >= 18, edad2 <= 21)
df5 %>% filter(edad2 >= 18 & edad2 <= 21)

## Combinando select y filter
df5 %>% 
  select(edad2, edadGR) %>% 
  filter(edad2 < 18)


## Nombre de columnas ----

df6 <- df5

### Apps

#### Paso 1: Crear un vector con los nuevos nombres
apps <- c("TikTok", "Instagram", "Facebook", "YouTube")

#### Paso 2: Reemplazo
colnames(df6)[33:36] <- apps


### Frases

#### Paso 1: Crear un vector con los nuevos nombres
frases2 <- frases %>%
  as_tibble() %>% 
  separate(col = "value",
           into = c("basura", "frase"),
           sep = "\\[") %>% 
  select(-basura) %>% 
  separate(col = "frase",
           into = c("frase", "basura"),
           sep = "\\]") %>% 
  pull(frase)

#### Paso 2: Reemplazo
colnames(df6)[8:31] <- frases2


## Pivot ----

### Pivot Longer

df7 <- df6 %>% 
  pivot_longer(cols = apps,
               names_to = "app",
               values_to = "time")


### Pivot Wider

df8 <- df7 %>% 
  pivot_wider(names_from = app,
              values_from = time)


df7 <- df7 %>% 
  select(`Marca temporal`, Sexo, edadGR, app, time)


# Tema 4: Detección de ouliers ----

strsplit(df7$time, split = ":") %>% head()

df7$time <- sapply(
  strsplit(df7$time, split = ":"),
  function(x) {
    x <- as.numeric(x)
    x[1] + x[2]/60 + x[3]/60^2
  }
)


## Detección gráfica ----

boxplot(df7$time)

library(plotly)

ggplotly(
  df7 %>% 
    # Dibujando el lienzo
    ggplot(aes(x = app, y = time, fill = app)) +
    # Especificando el tipo gráfico
    geom_boxplot() +
    # Escogiendo tema minimalista
    theme_minimal() +
    # Quitando leyenda
    theme(legend.position = "none")
)


ggplotly(
  df7 %>% 
    ggplot(aes(x = Sexo, y = time, fill = app)) +
    geom_boxplot() +
    facet_wrap("app", nrow = 1) +
    theme_minimal() +
    theme(legend.position = "none")
)


## Tratamiento ----

df9 <- df7 %>% 
  mutate(
    outlier = case_when(
      app == "Facebook"  & time > 10    ~ "outlier",
      app == "Instagram" & time > 12    ~ "outlier",
      app == "TikTok"    & time > 16.39 ~ "outlier",
      app == "YouTube"   & time > 9     ~ "outlier",
      .default = "No outlier"
    )
  ) %>% 
  group_by(app) %>% 
  mutate(time2 = ifelse(test = outlier == "outlier",
                        yes = mean(time, na.rm = T),
                        no = time)) %>% 
  ungroup()


# Tema 5: Aggregations ----

df9 %>% 
  group_by(app, Sexo) %>% 
  summarise(conteo = n(),
            promedio  = mean(time),
            desv      = sd(time),
            promedio2 = mean(time2),
            desv2     = sd(time2))


df9 %>% 
  count(edadGR) %>%
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n),
         porcentaje = scales::percent(prop))


df9 %>% 
  count(Sexo, edadGR) %>% 
  group_by(Sexo) %>% 
  mutate(prop = n/sum(n),
         porcentaje = scales::percent(prop)) %>% 
  ungroup()



# Guardando RDS
saveRDS(object = df6, file = "R/data_frame.RDS")







































