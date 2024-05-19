# Sesión Biodata n°5 - Integrando datos para investigación en Ecología - 11/06/2024 - Alonso Jara R.

################################################################################

#### 1 - R Basics.

# Setear el directorio: 
setwd("C:/Users/black/OneDrive/Escritorio/Me/Universidad/Biodata")
# Paquetes necesarios:
install.packages("tidyverse")
library(tidyverse)

# Abrir nuestra base de datos:
data <- read.csv("Data.csv")

# A) Nombres
colnames(data) <- c("Zona", "Individuo", "t1", "t2", "t3", "t4", "t5", "t6") # c -> Vector de datos

# B) Analizando nuestra database.
head(data) # Visualiza las primeras filas de la tabla.
str(data) # Visualiza la estructura de nuestros datos | Tipos de datos.

glimpse(data) # Lo mismo.
summary(data) # Más detalle

# C) Preguntarle a los datos.
# ¿Cuantas zonas son?
unique(data$Zona)

# ¿Cuantos individuos hay?
nrow(data)

# ¿Qué zona tiene más individuos?
## Forma 1 - Generando objetos
recuento_por_zona <- table(data$Zona)
# Encontrar la zona con el mayor número de registros
zona_max_registros <- names(recuento_por_zona)[which.max(recuento_por_zona)]
# Imprimir la zona con el mayor número de registros
zona_max_registros
### Extrae los datos que necesito; De esos datos diferencia la zona que tenga el mayor recuento; Muestrame el resultado.

## Forma 2 - Pipes %>% 
data %>% count(Zona)  %>% filter(n == max(n))
data  %>% count(Zona)
### En "data", cuenta los registros por zona, filtra el q tenga mayor n.

################################################################################

#### 2 - Obtención de bases de datos

# a. GAVIA (Global Avian Invasion Atlas) - Estado de invasión de aves | https://figshare.com/articles/dataset/Data_from_The_Global_Avian_Invasions_Atlas_-_A_database_of_alien_bird_distributions_worldwide/4234850
# b. AnimalTraits - Rasgos de animales terrestres: body mass, metabolic rate n brain size | https://animaltraits.org/ 

# Visualizando bases de datos:
invasion <- read.csv("GAVIA.csv")
rasgos <- read.csv("AnimalTraits.csv")

# Analizando y preguntando a los datos
## Animal Traits
rasgos %>% summary(class %in% c("Aves"))
# Hablando de los rasgos de los animales terrestres, ¿que se puede encontrar de aves?

## OJO! 
# Estructura de los datos - No coinciden.
# ¿Realmente necesito todos estos parámetros? - Depende de la pregunta.

# Para mejores conclusiones, mejor organizar y reducir.
################################################################################

#### 3 - Trabajando grandes bases de datos.

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

# Voy a crear rasgos2 a partir de rasgos MIENTRAS QUE selecciono las columas [...]
# y MIENTRAS QUE filtro los pertenecientes a la clase aves. 

ggplot(rasgos2) +
 aes(x = brain.size) +
 geom_histogram(bins = 30L, fill = "#112446") +
 theme_minimal()

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

datos <- left_join(invasion2, rasgos2) %>% na.omit()

#Try Gráfico - Esquisse

#Transformación de datos.
datos <- transform(datos, family = as.factor(family),
               genus = as.factor(genus),
               species = as.factor(species),
               statuscat = as.factor(statuscat),
               brain.size = as.numeric(brain.size))

# Try 2 Gráfico - Esquisse


################################################################################
library(plotly)
library(ggThemeAssist)

