# ////////////////////////////////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////////////////////////////////
# Library load ----
# ////////////////////////////////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////////////////////////////////

# Tidyverse es para cargar todo un conjunto de librerías para la programación funcional
library(tidyverse)
# haven habilita la conversión de datos de SPSS, además de que permite pasar facilmente de 
# una variable double labelled a un factor.
library(haven)
# Habilita la función tidy y otras herramientas cómodas para el manejo de modelos
library(broom)

library(lubridate)




# ////////////////////////////////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////////////////////////////////
# Data load ----
# ////////////////////////////////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////////////////////////////////

# Los datos son los correspondientes al PEPs del experimento
PEP <- read_sav("Data/Definitive_PEPs.sav") %>%
  mutate_if(is.labelled,as_factor)



# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////
# Diccionario de variables ----
# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////

# En esta pequeña seccion se crean una lista en el que cada elemento es un vector de carácteres con
# los nombres de todas las variables para esa categoría

# identificadores: variables que identifican al paciente de forma general, así como sus metadatos
# trastornos: datos de trastornos reconocidos en el paciente
# mediciones_Basal : variables reocgidas en la primera medida del experimento
# mediciones_2M: datos reocgidos en a los 2 meses del experimento
# mediciones_6M: datos reocgidos en a los 2 meses del experimento
# mediciones_1A: datos reocgidos en a los 2 meses del experimento
# mediciones_2A: datos reocgidos en a los 2 meses del experimento
# historia_familiar: datos recogidos en la historia familia (incluyendo padre, madre, hermanos y parientes)
# encefalo: datos del encefalo del paciente (pruebas y estado) neuroimagen


diccionario_variables_original <- list(
  'identificadores' = c(
    names(PEP)[1:27], 
    'disponibilidad',
    'fecha_entrevista',
    'antecedentes_psiquiatricos',
    'antecedentes_psiquiatricos_espe',
    'antecedentes_psicoticos',
    'antecedentes_psicoticos_espe',
    'fecha_primer_diagnostico'),
  'trastornos' = names(PEP)[30:379],
  'mediciones_Basal'  = str_subset(names(PEP), 'VB|BASAL'),
  'mediciones_2M'     = str_subset(names(PEP),'V2M'),
  'mediciones_6M'     = str_subset(names(PEP), 'V6M'),
  'mediciones_1A'     = str_subset(names(PEP), 'V1A|V12M '),
  'mediciones_2A'     = str_subset(names(PEP), 'V2A|V24M'),
  'historia_familiar' = str_subset(names(PEP),'[pP]adre|[Mm]adre|[Hh]ermano|[Hh]ijo|[Pp]ariente'),
  'encefalo' = names(PEP)[2421:2548])

diccionario_variables <- list(
  'identificadores' = c(
    names(PEP)[1:27], 
    'disponibilidad',
    'fecha_entrevista',
    'antecedentes_psiquiatricos',
    'antecedentes_psiquiatricos_espe',
    'antecedentes_psicoticos',
    'antecedentes_psicoticos_espe',
    'fecha_primer_diagnostico'),
  'mediciones_Basal'  = str_subset(names(PEP), 'VB|BASAL'),
  'mediciones_2M'     = str_subset(names(PEP),'V2M'),
  'mediciones_6M'     = str_subset(names(PEP), 'V6M'),
  'mediciones_1A'     = str_subset(names(PEP), 'V1A|V12M '),
  'mediciones_2A'     = str_subset(names(PEP), 'V2A|V24M'),
  'encefalo' = names(PEP)[2421:2548])


# /////////////////////////////////////////////
## Numero de variables perdidas no classificadas de las 2548 iniciales -----
# /////////////////////////////////////////////

# Aun creando todo el diccionario de variables, no todas las variables son clasificadas adecuadamente
# estas son las que quedarían por clasificar de alguna forma u otra.

p - diccionario_variables %>% map_dbl(~length(.x)) %>% sum()


# ////////////////////////////////////////////////////////////////////////////////////////////////
# Analitica descriptiva ----
# ////////////////////////////////////////////////////////////////////////////////////////////////

# /////////////////////////////////////////////
## Dimensiones del dataframe -----
# /////////////////////////////////////////////

n<- dim(PEP)[1] # Numero de individuos
p<- dim(PEP)[2] # Numero de variables


# /////////////////////////////////////////////
## Total de NA presentes por variable  ----
# /////////////////////////////////////////////

NA_variables <- tibble( 
  'Nombre_variable' = names(PEP),
  'Recuento_NA' = map_dbl(PEP, ~sum(is.na(.x))),
  'NA_ratio' = map_dbl(PEP, ~sum(is.na(.x))/n )) %>% 
  arrange(desc(NA_ratio))

quantile(NA_variables$NA_ratio, seq(0,1,0.1) )

NA_variables %>%
  filter(NA_ratio == 1)

NA_variables %>%
  filter(NA_ratio > 0.9)

NA_variables %>%
  filter(NA_ratio > 0.8)

NA_variables %>% 
  ggplot(aes(NA_ratio)) +
  geom_density()

# /////////////////////////////////////////////
## Total de NA presentes por Individuo  ----
# /////////////////////////////////////////////
### Necesario para hacer un seguimiento de los casos que mas dropean el estudio por ejemplo

NA_ind <- data.frame(
  'id'= 1:n,
  'NA_count'  = unlist(apply(PEP, MARGIN = 1, function(x) sum(is.na(x)))),
  'NA_percent'= unlist(apply(PEP, MARGIN = 1, function(x) sum(is.na(x))))/p)

quantile(NA_ind$NA_count, seq(0,1,0.1) )

# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////
## Sumario por categorias del diccioanrio de variables ----
# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////


Sumario_variables_diccionario <- diccionario_variables %>%
  map(
    ~ list(
        'resumen_variables'= PEP %>%
          select(any_of(.x)) %>%
          map(~summary(.)),
        'resumen_NA' = PEP %>%
          select(any_of(.x)) %>%
          imap_dfr(
            ~ data.frame(
              'variable'= .y,
              'NA_n'= sum(is.na(.x)))) %>%
          mutate(
            'NA_perct_variable'= NA_n / n )
        )
    )


Sumario_variables_diccionario$identificadores$resumen_variables

# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////
# Arrelgo en variables indentificadoras ----
# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////



PEP %>%
  select(any_of(diccionario_variables$identificadores)) %>%
  mutate(across(c('LewisA':'LewisT'), as.factor)) %>%
  # Creamos las variables de edad correspondientes
  mutate(
    'edad_estudio'         = year(as.period(interval(fecha_nacimiento, fecha_entrevista))),
    'edad_primer_episodio' = year(as.period(interval(fecha_nacimiento, fecha_primer_diagnostico ))) ) %>%
  select(-c(fecha_nacimiento ,primera_entrevista, fecha_entrevista, fecha_primer_diagnostico)) %>%
  # modificamos la etnia para ternr una sola dicotomica
  mutate(
    etnia = case_when(
      etnia == 'caucasian' ~ 'caucasian',
      TRUE ~ 'others'),
    inmigrante= case_when(
      lugar_naci_pais !='ESP' & !is.na(lugar_naci_pais)  ~ 'internacional',
      as.character(provincia_naci) != as.character(provincia_resid) & 
        lugar_naci_pais != 'Extranjero' &
        (!is.na(provincia_naci) | !is.na(provincia_resid)) ~ 'Nacional',
      TRUE~'No')) %>%
  select(-c(lugar_naci_pais:nivel_ocupacional_progenitor, disponibilidad:antecedentes_psicoticos_espe )) %>%
  view()
  

