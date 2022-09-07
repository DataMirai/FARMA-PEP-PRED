
Farmacos_mal_etiquetados <- read_xlsx(path = 'Data/farmacos_mal_etiquetados_SM.xlsx') %>%
  rename('P_actiu'= `Ppi actiu`)

PEP_farma <- PEP %>%
  select(any_of(diccionario_variables$farma))

PEP_farma_nombres <- PEP_farma %>% select(matches('^([fF])arma')) 





limpiar_farmacos_maletiquetados <- function(columna_PEP_farma_nombres){
  columna_PEP_farma_nombres<- as.data.frame(columna_PEP_farma_nombres)
  save_name <- names(columna_PEP_farma_nombres)
  names(columna_PEP_farma_nombres) <- 'Farmaco_X'
  
  columna_farmacos_limpios <- columna_PEP_farma_nombres %>%
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

PEP_farma_nombres_arreglados <- map(PEP_farma_nombres, limpiar_farmacos_maletiquetados) %>%
  as_tibble() %>%
  mutate_all(str_to_title) %>%
  mutate_all(., list(~na_if(.,"")))

PEP_farma[names(PEP_farma_nombres_arreglados)] <- PEP_farma_nombres_arreglados 




