
Farmacos_mal_etiquetados <- read_xlsx(path = 'Data/farmacos_mal_etiquetados_SM.xlsx') %>%
  rename('P_actiu'= `Ppi actiu`)

PEP_farma <- PEP %>% select(any_of(diccionario_variables$farma))

PEP_farma_nombres <- PEP_farma %>% select(matches('^([fF])arma')) 


limpiar_farmacos_maletiquetados <- function(columna_PEP_farma){
  columna_PEP_farma<- as.data.frame(columna_PEP_farma)
  save_name <- names(columna_PEP_farma)
  names(columna_PEP_farma) <- 'Farmaco_X'
  
  columna_farmacos_limpios <- columna_PEP_farma %>%
    left_join(., Farmacos_mal_etiquetados, by = c('Farmaco_X' = 'farmacos_mal_etiquetados')) %>%
    mutate(Farmaco_X = case_when(
      str_detect(Farmaco_X, pattern="^[[:digit:]]") == T ~ P_actiu, 
      str_detect(Farmaco_X, pattern="^[[:digit:]]") == F ~ Farmaco_X)) %>%
    select(Farmaco_X) %>%
    unlist()
  
  names(columna_farmacos_limpios) <- save_name
  columna_farmacos_limpios<- as.character(columna_farmacos_limpios)
  return(columna_farmacos_limpios)
}

PEP_farma_nombres_arreglados <- map(
  PEP_farma_nombres, 
  limpiar_farmacos_maletiquetados) %>%
  as_tibble() %>%
  mutate_all(str_to_title) %>%
  mutate_all(., list(~na_if(.,"")))

PEP_farma[names(PEP_farma_nombres_arreglados)] <- PEP_farma_nombres_arreglados 

# creamos una lista con los farmacos arreglado por momento en el experimento
# Empezamos con la lista basal por que no tiene codigo de identificacion 
# Luego añadimos las variables que contienen _A _B y _C (mediante expresion regular)
# para poder limpiar tener todos los farmacos listos
PEP_farma_times <- list(PEP_farma %>% select(farmaco1:CPZ5)) %>%
  append(map(
    list('_[:A:]','_[:B:]','_[:C:]' ), 
    ~ PEP_farma %>% 
      select(matches(.x)) %>%
      select(-c(matches('^m'))))) %>%
  set_names( c('PEP_farma_Basal','PEP_farma_A', 'PEP_farma_B', 'PEP_farma_C')) %>%
  map2(list(
    PEP_farma %>% select(CPZBASAL),
    PEP_farma %>% select(CPZ2meses),
    PEP_farma %>% select(CPZ6MESES),
    PEP_farma %>% select(CPZ12MESES)),
    ~ bind_cols(.x, .y)) 


# ///////////////////////////////////////////////////
# ///////////////////////////////////////////////////
# ///////////////////////////////////////////////////

pegado_categoria <- function(columna_PEP_farmaco){
  columna_PEP_farma_nombres<- as.data.frame(columna_PEP_farmaco)
  save_name <- names(columna_PEP_farma_nombres)
  names(columna_PEP_farma_nombres) <- 'Farmaco_X'
  
  grupo_farmacos_limpios <- columna_PEP_farma_nombres %>%
    full_join(., Farmacos_mal_etiquetados, by = c('Farmaco_X' = 'P_actiu')) %>%
    select(Farmaco_X,Grupo) %>%
    rename(as.character(save_name) == 'Farmaco_X') %>%
    as_tibble()
  
  return(grupo_farmacos_limpios)
}

columna
Farmacos_mal_etiquetados$P_actiu %>% unique()
columna_PEP_farma_nombres<- as.data.frame(columna)
save_name <- names(columna_PEP_farma_nombres)
names(columna_PEP_farma_nombres) <- 'Farmaco_X'
  
columna_PEP_farma_nombres %>%
    full_join(., Farmacos_mal_etiquetados, by = c('Farmaco_X' = 'P_actiu')) %>%
    select(Farmaco_X,Grupo) %>%
    set_names(c(save_name), 'Grupo') %>%
    as_tibble()

columna<-PEP_farma_times$PEP_farma_Basal %>% 
  select(matches('farma')) %>% 
  .[,1]

pegado_categoria(a)
PEP_farma_times$PEP_farma_Basal %>%
  select(matches('farma')) %>%
  map(~as_tibble(pegado_categoria(.x))) 
  

Farmacos_mal_etiquetados




