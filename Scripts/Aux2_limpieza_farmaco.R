rm(list = setdiff(ls(), c('PEP','diccionario_variables')))
# ///////////////////////////////////////////////////////////////////////////////////
# Carga de bases de datos necesarias ----
# ///////////////////////////////////////////////////////////////////////////////////

anti_psicoticos <- read_xlsx(path = 'Data/CodisAP.xlsx') %>%
  mutate( AP = case_when(AP == 'ARIPIRAZOL' ~  'ARIPIPRAZOL' , TRUE ~ AP))
  
Farmacos_mal_etiquetados <- read_xlsx(path = 'Data/farmacos_mal_etiquetados_SM.xlsx') %>%
  rename('P_actiu'= `Ppi actiu`)

PEP_farma <- PEP %>% select(any_of(diccionario_variables$farma))

# ///////////////////////////////////////////////////////////////////////////////////
# Primera separacion de farmacos por periodo del experimento  ----
# ///////////////////////////////////////////////////////////////////////////////////

# Añadir la CPZ total por periodo de entrevista
PEP_farma_separada <- list(
  'Farmacos_VB' = PEP_farma %>% select(farmaco1:CPZ5    ,antipsicoticoVB :metabolito_activo3VB),
  'Farmacos_2M' = PEP_farma %>% select(farmaco1_A:CPZ5_A,antipsicoticoV2M:metabolito_activo3V2M),
  'Farmacos_6M' = PEP_farma %>% select(farmaco1_B:CPZ5_B,antipsicóticoV6M:metabolito_activo3V6M),
  'Farmacos_12M'= PEP_farma %>% select(farmaco1_C:CPZ5_C,antipsicóticoV1A:metabolito_activo3V1A)) %>%
  map(~ map_dfc(., ~ na_if(.,'')))
# ///////////////////////////////////////////////////////////////////////////////////
# Farmacos aislados ----
# ///////////////////////////////////////////////////////////////////////////////////

PEP_farma_farmacos <- crossing(
  PEP_farma_separada,
  list(c(1:3),c(4:6),c(7:9),c(10:12),c(13:15))) %>%
  set_names(c('dataframes','posiciones')) 


PEP_farma_farmacos <- map2(
  PEP_farma_farmacos$dataframes,
  PEP_farma_farmacos$posiciones,
  ~ select(.x, .y))

nombres_farmacos <- PEP_farma_farmacos %>%
  map(names) %>% 
  map_chr(1)

PEP_farma_farmacos <- PEP_farma_farmacos %>%
  set_names(nombres_farmacos)

# ///////////////////////////////////////////////////////////////////////////////////
# Información de antisipcóticos bien separadas -----
# ///////////////////////////////////////////////////////////////////////////////////

PEP_farma_antipsicoticos <- crossing(
  PEP_farma_separada,
  list( c(16:18),c(19:21),c(22:24))) %>%
  set_names(c('dataframes','posiciones')) 

PEP_farma_antipsicoticos <- map2(
  PEP_farma_antipsicoticos$dataframes,
  PEP_farma_antipsicoticos$posiciones,
  ~ select(.x, .y))

nombres_antipsicoticos <- PEP_farma_antipsicoticos %>%
  map(names) %>% 
  map_chr(1)

PEP_farma_antipsicoticos <- PEP_farma_antipsicoticos %>%
  set_names(nombres_antipsicoticos) %>%
  map(~ rename_at(., 1, ~ 'CODI')) 

# ////////////////////////////////////////////////////////////////////////////////////
# Limpieza de farmacos -----
# ////////////////////////////////////////////////////////////////////////////////////

arreglo_basico_farmacos <- function(data_frame_farmaco){

  saved_farmaco <- names(data_frame_farmaco)[1]
  code_farmaco  <- str_extract(saved_farmaco,'(?<=co).{1,}')

  final_df_farmaco_arreglado <- left_join(
    rename_at(data_frame_farmaco,1, ~ 'farmacoX'),
    rename_at(Farmacos_mal_etiquetados, 1, ~ 'farmacoX'), 
    by = 'farmacoX') %>%
  left_join(.,anti_psicoticos, by=c('farmacoX'= 'AP' )) %>%
    mutate(farmacoX = case_when(
      str_detect(farmacoX, pattern="^[[:digit:]]") == T ~ P_actiu,
      str_detect(farmacoX, pattern="^[[:digit:]]") == F ~ farmacoX)) %>%
    select(-c(P_actiu)) %>%
    rename_at(., 1, ~ saved_farmaco) 
  # %>% rename_at(vars(matches('Grupo')), ~ str_glue('Grupo', code_farmaco))
  
  return(final_df_farmaco_arreglado)
}
PEP_farma_farmacos <- PEP_farma_farmacos %>%
  map( arreglo_basico_farmacos ) 

PEP_farma_farmacos_VB <- PEP_farma_farmacos[1:5]
PEP_farma_farmacos_2M <- PEP_farma_farmacos[6:10]
PEP_farma_farmacos_6M <- PEP_farma_farmacos[11:15]
PEP_farma_farmacos_12M <- PEP_farma_farmacos[16:20]

PEP_farma_antipsicoticos_VB <- PEP_farma_antipsicoticos[1:3]
PEP_farma_antipsicoticos_2M <- PEP_farma_antipsicoticos[4:6]
PEP_farma_antipsicoticos_6M <- PEP_farma_antipsicoticos[7:9]
PEP_farma_antipsicoticos_12M <- PEP_farma_antipsicoticos[10:12]



arreglo_farmaco_con_bioquimica_VB <- function( data_frame_farmaco_basico, lista_antipsicoticos  ){
  
  saved_farmaco <- names(data_frame_farmaco_basico)[1]
  code_farmaco  <- str_extract(saved_farmaco,'(?<=co).{1,}')
  
  final <- append(list( data_frame_farmaco_basico ), lista_antipsicoticos) %>%
    reduce(bind_cols)  %>%
    rename_at(.,1, ~ 'farmacoX') %>%
    mutate(
      nivel_antipsicotico = case_when(
        CODI...5 == CODI...6  ~ antipsicotico_nivelVB,
        CODI...5 == CODI...9  ~ antipsicotico_nivel2VB,
        CODI...5 == CODI...12 ~ antipsicotico_nivel3VB),
      metabolito_activo= case_when(
        CODI...5 == CODI...6  ~ metabolito_activoVB,
        CODI...5 == CODI...9  ~ metabolito_activo2VB,
        CODI...5 == CODI...12 ~ metabolito_activo3VB)) %>%
    select_at(vars(1:4,15,16)) %>%
    mutate(Grupo = case_when(
      farmacoX %in% c(
        "ARIPIPRAZOL","CLOZAPINA","HALOPERIDOL",
        "OLANZAPINA","PALIPERIDONA","QUETIAPINA","RISPERIDONA") ~ 'Antipsicotico',
      TRUE ~ Grupo)) %>%
    rename_at(vars(matches('Grupo')), ~ str_glue('Grupo_', code_farmaco)) %>% 
    rename_at(vars(matches('nivel')), ~ str_glue('nivel_antipsicotico_', code_farmaco)) %>%
    rename_at(vars(matches('metabolito')), ~ str_glue('metabolito_activo_', code_farmaco)) %>%
    rename_at(vars(1), ~ str_glue('farmaco_', code_farmaco))
  
  return(final)
}


PEP_farma_farmacos_VB <- PEP_farma_farmacos_VB %>%
  map( ~ arreglo_farmaco_con_bioquimica(., PEP_farma_antipsicoticos_VB))

PEP_farma_farmacos_2M <- PEP_farma_farmacos_2M %>%
  map( ~ arreglo_farmaco_con_bioquimica(., PEP_farma_antipsicoticos_2M))

PEP_farma_farmacos_6M <- PEP_farma_farmacos_6M %>%
  map( ~ arreglo_farmaco_con_bioquimica(., PEP_farma_antipsicoticos_6M))

PEP_farma_farmacos_12M <- PEP_farma_farmacos_12M %>%
  map( ~ arreglo_farmaco_con_bioquimica(., PEP_farma_antipsicoticos_12M))





# test %>%
#   bind_cols() %>% 
#   select(matches('Grupo')) %>%
#   map(unique) %>%
#   map(na.omit) %>%
#   reduce( append) %>%
#   unique()


# test %>%
#   bind_cols() %>%
#   select(matches('Grupo')) %>%
#   summarize(
#     Toma_antipsicoticos = rowSums(. =='Antipsicotico',na.rm=T),
#     Toma_Estabilizador_Animo  = rowSums(. =='Estabilizador Animo',na.rm=T),
#     Toma_Antidepresivo        = rowSums(. =='Antidepresivo',na.rm=T),
#     Toma_Antiepiléptico       = rowSums(. =='Antiepiléptico',na.rm=T),
#     Toma_No_Psicofarmacos     = rowSums(. =='No Psicofarmacos',na.rm=T),
#     Toma_Benzodiazepina       = rowSums(. =='Benzodiazepina',na.rm=T),
#     Toma_Anticolinérgico      = rowSums(. =='Anticolinérgico',na.rm=T),
#     Toma_Psicoestimulante     = rowSums(. =='Antidepresivo+benzodiazepina',na.rm=T),
#     Toma_Antidepresivo_benzodiazepina  = rowSums(. =='Psicoestimulante',na.rm=T))


test <- PEP_farma_farmacos_VB[[1]]
aux_test <- PEP_farma_antipsicoticos_2M


saved_farmaco <- names(test)[1]
code_farmaco  <- str_extract(saved_farmaco,'(?<=co).{1,}')

append(list( test ), aux_test) %>%
  reduce(bind_cols)  %>%
  rename_at(.,1, ~ 'farmacoX') %>%
  rename_at(.,vars(matches('antipsicótico_nivel')), ~ str_remove(., '(?!=(el))V.{1,}')) %>%
  rename_at(.,vars(matches(  'metabolito_activo')), ~ str_remove(., '(?!=(vo))V.{1,}')) %>%
  mutate(
    nivel_antipsicotico_limpio = case_when(
      CODI...5 == CODI...6  ~ antipsicótico_nivel,
      CODI...5 == CODI...9  ~ antipsicótico_nivel2,
      CODI...5 == CODI...12 ~ antipsicótico_nivel3),
    metabolito_activo_limpio= case_when(
      CODI...5 == CODI...6  ~ metabolito_activo,
      CODI...5 == CODI...9  ~ metabolito_activo2,
      CODI...5 == CODI...12 ~ metabolito_activo3)) %>%
  select_at(vars(1:4,15,16)) %>%
  mutate(Grupo = case_when(
    farmacoX %in% c(
      "ARIPIPRAZOL","CLOZAPINA","HALOPERIDOL",
      "OLANZAPINA","PALIPERIDONA","QUETIAPINA","RISPERIDONA") ~ 'Antipsicotico',
    TRUE ~ Grupo)) %>%
  rename_at(vars(matches('Grupo')), ~ str_glue('Grupo_', code_farmaco)) %>% 
  rename_at(vars(matches('nivel')), ~ str_glue('nivel_antipsicotico_', code_farmaco)) %>%
  rename_at(vars(matches('metabolito')), ~ str_glue('metabolito_activo_', code_farmaco)) %>%
  rename_at(vars(1), ~ str_glue('farmaco_', code_farmaco))





