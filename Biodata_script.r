
install.packages("tidyverse") # Paquete recopilatorio de herramientas de manejo de bases de datos.
install.packages("esquisse") # Generador de gráficos x ggplot.
install.packages("ggThemeAssist") # Customización de gráficos.
install.packages("plotly") # Visualización interactiva de datos.


library(tidyverse)
library(esquisse)
library(ggThemeAssist)
library(plotly)

# Cargar la base de datos 
data <- read.csv("Data.csv")

# Cambiar nombres
colnames(data) <- c("Zona", "Individuo", "t1", "t2", "t3", "t4", "t5", "t6") # c -> Vector de datos

# Revisar datos

head(data) # Visualiza las primeras filas de la tabla.
str(data) # Visualiza la estructura de nuestros datos | Tipos de datos.
glimpse(data) # Lo mismo.
summary(data) # Más detalle

# ¿Cuantas zonas son? - Signo "$"
unique(data$Zona) # Entrega un vector de los datos, sin duplicados.

# ¿Cuantos individuos hay?
nrow(data) # Número de filas.


# ¿Qué zona tiene más individuos?

## Forma 1 - Generando objetos
recuento_por_zona <- table(data$Zona) # Encontrar la zona con el mayor número de registros
recuento_por_zona
zona_max_registros <- names(recuento_por_zona)[which.max(recuento_por_zona)] # Seleccionar la zona con el mayor número de registros
zona_max_registros

### Extrae los datos que necesito; De esos datos diferencia la zona que tenga el mayor recuento; Muestrame el resultado.

## Forma 2 - Pipes %>% 
data %>% count(Zona)  %>% filter(n == max(n))
data  %>% count(Zona)

### En "data" mientras cuenta los registros por zona, filtra el que tenga mayor n.

# Parte III

# a. GAVIA (Global Avian Invasion Atlas) - Estado de invasión de aves 
# b. AnimalTraits - Rasgos de animales terrestres: body mass, metabolic rate n brain size 

# Visualizando bases de datos:
invasion <- read.csv("GAVIA.csv")
rasgos <- read.csv("AnimalTraits.csv")

# Analizando y preguntando a los datos
## Animal Traits
rasgos %>% summary(class %in% c("Aves"))
# En la base de datos rasgos, entregame un resumen de la clase de las aves

# ¿Tienen mayor tamaño cerebral las aves exitosamente invasoras? 

# A) Trabajamos AnimalTraits | Rasgos. Necesitamos los datos de:
# - class
# - family
# - genus
# - species
# - brain.size

rasgos2 <- rasgos %>% 
  dplyr::filter(class %in% c("Aves")) %>% 
  dplyr::select(family, genus, species, brain.size) %>%
  na.omit()

# Voy a crear rasgos2 a partir de rasgos mientras que filtro por las pertenecientes a la clase de las aves y seleccionando las clumnas familia, genero, especie y tamaño cerebral.

# B) Trabajamos GAVIA | Invasión. Necesitamos los datos de:
# - Family - family.
# - Genus - genus.
# - Binomial - species.
# - StatusCat - Nueva Variable.

invasion2 <- invasion %>% 
  dplyr::select("family" = Family, "genus" = Genus, "species" = Binomial, "statuscat" = StatusCat) %>% 
  dplyr::filter(statuscat %in% c("Established", "Unsuccessful"))  %>% 
  distinct_all()

# C) Unión 

# En nuestro caso buscamos que datos de aves tengan anexo un tamaño cerebral.
datos <- left_join(invasion2, rasgos2) %>% na.omit()

#Transformación de datos.
datos <- transform(datos, family = as.factor(family),
                   genus = as.factor(genus),
                   species = as.factor(species),
                   statuscat = as.factor(statuscat),
                   brain.size = as.numeric(brain.size))

# Gráfico.
datos %>%
  filter(brain.size >= 0 & brain.size <= 0.024) %>%
  ggplot() +
  aes(x = statuscat, y = brain.size, fill = statuscat) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

#END

