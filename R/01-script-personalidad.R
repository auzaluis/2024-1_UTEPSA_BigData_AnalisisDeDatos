
# Tema 1: Carga de datos ----

## Carga local ----
df <- read.csv("R/Personalidad y uso de apps (respuestas) - Respuestas de formulario 1.csv",
               check.names = F)

colnames(df)


## Carga desde la API de Google ----



## Analizando el df ----
class(df)
class(df$Sexo)
class(df$`Escribe tu edad exacta`)
nrow(df) # Número de filas
ncol(df) # Número de columnas
