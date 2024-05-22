
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

# Convertir el data frame en tibble
df5 <- as_tibble(df5)

## Función select(): columnas ----

## Función filter(): filas ----









