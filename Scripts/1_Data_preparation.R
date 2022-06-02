# ////////////////////////////////////////////////////////////////////////////////////////////////
# Library load ----
# ////////////////////////////////////////////////////////////////////////////////////////////////

library(tidyverse)
library(haven)

# ////////////////////////////////////////////////////////////////////////////////////////////////
# Data load ----
# ////////////////////////////////////////////////////////////////////////////////////////////////

PEP <- read_sav("Data/Definitive_PEPs.sav")

# ////////////////////////////////////////////////////////////////////////////////////////////////
# Data Basics ----
# ////////////////////////////////////////////////////////////////////////////////////////////////

## Dimensiones del dataframe
n<- dim(PEP)[1] # Numero de individuos
p<- dim(PEP)[2] # Numero de variables

## Calculo de NA

tibble( 
  'Nombre_variable' = names(PEP),
  'Recuento_NA' = map_dbl(PEP, ~sum(is.na(.x))),
  'NA_ratio' = map_dbl(PEP, ~sum(is.na(.x))/n )) %>% 
  arrange(desc(NA_ratio)) %>%
  view()

## Diccionario de variables

identificadores <- c(
  names(PEP)[1:27], 
  'disponibilidad',
  'fecha_entrevista',
  'antecedentes_psiquiatricos',
  'antecedentes_psiquiatricos_espe',
  'antecedentes_psicoticos',
  'antecedentes_psicoticos_espe')

trastornos <- names(PEP)[80:379]

mediciones_Basal  <- str_subset(names(PEP), 'VB|BASAL')

mediciones_2M     <- str_subset(names(PEP),'V2M')

mediciones_6M     <- str_subset(names(PEP), 'V6M')

mediciones_1A     <- str_subset(names(PEP), 'V1A|V12M ')

mediciones_2A     <- str_subset(names(PEP), 'V2A|V24M')

historia_familiar <- str_subset(names(PEP),'[pP]adre|[Mm]adre|[Hh]ermano|[Hh]ijo|[Pp]ariente')

encefalo <- names(PEP)[2421:2548]



p - sum(
  map_dbl( 
    ls()[ls() %in% c( 
      "encefalo","historia_familiar","identificadores",'trastornos',
      "mediciones_Basal","mediciones_2M","mediciones_6M","mediciones_1A","mediciones_2A")], 
    ~ length(get(.x)) ))



