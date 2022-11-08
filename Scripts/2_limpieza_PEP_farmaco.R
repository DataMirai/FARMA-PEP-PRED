
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

  left_join(
    rename_at(data_frame_farmaco,1, ~ 'farmacoX'),
    rename_at(Farmacos_mal_etiquetados, 1, ~ 'farmacoX'), 
    by = 'farmacoX') %>%
    left_join(.,anti_psicoticos, by=c('farmacoX'= 'AP' )) %>%
    mutate(farmacoX = case_when(
      str_detect(farmacoX, pattern="^[[:digit:]]") == T ~ P_actiu,
      str_detect(farmacoX, pattern="^[[:digit:]]") == F ~ farmacoX)) %>%
    select(-c(P_actiu)) %>%
    rename_at(., 1, ~ saved_farmaco) %>%
    # rename_at(vars(matches('Grupo')), ~ str_glue('Grupo', code_farmaco)) %>%
    return()
}

PEP_farma_farmacos <- PEP_farma_farmacos %>% map( arreglo_basico_farmacos ) 

# ////////////////////////////////////////////////////////////////////////////////////
# Split de listas de farmacos y antipsicoticos  -----
# ////////////////////////////////////////////////////////////////////////////////////

PEP_farma_farmacos_VB <- PEP_farma_farmacos[1:5]
PEP_farma_farmacos_2M <- PEP_farma_farmacos[6:10]
PEP_farma_farmacos_6M <- PEP_farma_farmacos[11:15]
PEP_farma_farmacos_12M <- PEP_farma_farmacos[16:20]

PEP_farma_antipsicoticos_VB <- PEP_farma_antipsicoticos[1:3]
PEP_farma_antipsicoticos_2M <- PEP_farma_antipsicoticos[4:6]
PEP_farma_antipsicoticos_6M <- PEP_farma_antipsicoticos[7:9]
PEP_farma_antipsicoticos_12M <- PEP_farma_antipsicoticos[10:12]


# ////////////////////////////////////////////////////////////////////////////////////
# Arreglos de farmaco de pacientes en el estado Basal  del experimento-----
# ////////////////////////////////////////////////////////////////////////////////////

arreglo_farmaco_con_bioquimica_VB <- function( data_frame_farmaco_basico, lista_antipsicoticos  ){

  saved_farmaco <- names(data_frame_farmaco_basico)[1]
  code_farmaco  <- str_extract(saved_farmaco,'(?<=co).{1,}')

  append(list( data_frame_farmaco_basico ), lista_antipsicoticos) %>%
    reduce(bind_cols)  %>%
    rename_at(.,1, ~ 'farmacoX') %>%
    rename_at(.,vars(matches('antipsicotico_nivel')), ~ str_remove(., '(?!=(el))V.{1,}')) %>%
    rename_at(.,vars(matches('metabolito_activo'))  , ~ str_remove(., '(?!=(vo))V.{1,}')) %>%
    mutate(
      nivel_antipsicotico = case_when(
        CODI...5 == CODI...6  ~ antipsicotico_nivel,
        CODI...5 == CODI...9  ~ antipsicotico_nivel2,
        CODI...5 == CODI...12 ~ antipsicotico_nivel3),
      metabolito_activo= case_when(
        CODI...5 == CODI...6  ~ metabolito_activo,
        CODI...5 == CODI...9  ~ metabolito_activo2,
        CODI...5 == CODI...12 ~ metabolito_activo3)) %>%
    select_at(vars(1:4,14,15)) %>%
    mutate(Grupo = case_when(
      farmacoX %in% c(
        "ARIPIPRAZOL","CLOZAPINA","HALOPERIDOL",
        "OLANZAPINA","PALIPERIDONA","QUETIAPINA","RISPERIDONA") ~ 'Antipsicotico',
      TRUE ~ Grupo)) %>%
    rename_at(vars(matches('Grupo')), ~ str_glue('Grupo_', code_farmaco)) %>%
    rename_at(vars(matches('nivel')), ~ str_glue('nivel_antipsicotico_', code_farmaco)) %>%
    rename_at(vars(matches('metabolito')), ~ str_glue('metabolito_activo_', code_farmaco)) %>%
    rename_at(vars(1), ~ str_glue('farmaco_', code_farmaco)) %>%
    return()
}
# 

PEP_farma_farmacos_VB <- PEP_farma_farmacos_VB %>%
  map( ~ arreglo_farmaco_con_bioquimica_VB(., PEP_farma_antipsicoticos_VB)) %>%
  map(~ rename_at( .,vars(matches('posologia')), ~ str_replace(.,'posologia', 'posologia_'))) %>%
  map(~ rename_at( .,vars(matches('CPZ')), ~ str_replace(.,'CPZ', 'CPZ_'))) %>%
  map(~ rename_all(. , ~ paste0(., '_VB')))



# ////////////////////////////////////////////////////////////////////////////////////
# Arreglos de farmaco de pacientes en estados avanzados del experimento-----
# ////////////////////////////////////////////////////////////////////////////////////


arreglo_farmaco_con_bioquimica_no_VB <- function( data_frame_farmaco_basico, lista_antipsicoticos ){

  saved_farmaco <- names(data_frame_farmaco_basico)[1]
  code_farmaco  <- str_extract(saved_farmaco,'(?<=co).{1,}')
  
  final <- append(list( data_frame_farmaco_basico ), lista_antipsicoticos) %>%
    reduce(bind_cols) %>% 
    rename_at(.,1, ~ 'farmacoX') %>%
    rename_at(. ,vars( matches('antipsicótico_nivel')), ~ str_remove(., '(?!=(el))V.{1,}')) %>%
    rename_at(. ,vars( matches('metabolito_activo'  )), ~ str_remove(., '(?!=(vo))V.{1,}')) %>%
    rename_at(. ,vars( matches('antipsicótico_nivel')), ~ str_replace(., 'ó','o')) %>%
    mutate(
      nivel_antipsicotico_limpio = case_when(
        CODI...5 == CODI...6  ~ antipsicotico_nivel,
        CODI...5 == CODI...9  ~ antipsicotico_nivel2,
        CODI...5 == CODI...12 ~ antipsicotico_nivel3),
      metabolito_activo_limpio= case_when(
        CODI...5 == CODI...6  ~ metabolito_activo,
        CODI...5 == CODI...9  ~ metabolito_activo2,
        CODI...5 == CODI...12 ~ metabolito_activo3)) %>%
    select_at(vars(1:4,15,16)) %>%
    mutate(
      Grupo = case_when(
        farmacoX %in% c("ARIPIPRAZOL","CLOZAPINA","HALOPERIDOL","OLANZAPINA","PALIPERIDONA","QUETIAPINA","RISPERIDONA") ~ 'Antipsicotico',
        TRUE ~ Grupo)) %>% 
    rename_at(vars(matches('Grupo'))     , ~ str_glue('Grupo_', code_farmaco))  %>%
    rename_at(vars(matches('nivel'))     , ~ str_glue('nivel_antipsicotico_', code_farmaco))  %>%
    rename_at(vars(matches('farmacoX'))  , ~ str_glue('farmaco_', code_farmaco))  %>%
    rename_at(vars(matches('metabolito')), ~ str_glue('metabolito_activo_', code_farmaco)) 
  
  return(final)
  
}


pos <- map(list('_2M','_6M','_12M'), ~ paste0('posologia_',1:5,.)) %>%  map(as.list)
cpz <- map(list('_2M','_6M','_12M'), ~ paste0('CPZ_'      ,1:5,.)) %>%  map(as.list)

PEP_farma_farmacos_2M <- PEP_farma_farmacos_2M %>%
  map( ~ arreglo_farmaco_con_bioquimica_no_VB(., PEP_farma_antipsicoticos_2M)) %>%
  map2(.x = . , .y = pos[[1]], ~ rename_at(.x, 2, ~ !!.y) ) %>%
  map2(.x = . , .y = cpz[[1]], ~ rename_at(.x, 3, ~ !!.y) ) %>%
  map(~ rename_all(. , ~ str_replace(., '_A','_2M')))

PEP_farma_farmacos_6M <- PEP_farma_farmacos_6M %>%
  map( ~ arreglo_farmaco_con_bioquimica_no_VB(., PEP_farma_antipsicoticos_6M)) %>%
  map2(.x = . , .y = pos[[2]], ~ rename_at(.x, 2, ~ !!.y) ) %>%
  map2(.x = . , .y = cpz[[2]], ~ rename_at(.x, 3, ~ !!.y) ) %>%
  map(~ rename_all(. , ~ str_replace(., '_B','_6M')))

PEP_farma_farmacos_12M <- PEP_farma_farmacos_12M %>%
  map( ~ arreglo_farmaco_con_bioquimica_no_VB(., PEP_farma_antipsicoticos_12M)) %>%
  map2(.x = . , .y = pos[[3]], ~ rename_at(.x, 2, ~ !!.y) ) %>%
  map2(.x = . , .y = cpz[[3]], ~ rename_at(.x, 3, ~ !!.y) ) %>%
  map(~rename_all(. , ~ str_replace(., '_C','_12M')))



# ////////////////////////////////////////////////////////////////////////////////////
# Creación de vriables de conteo de farmacos por pacientes -----
# ////////////////////////////////////////////////////////////////////////////////////

aux_limpieza_categorias <- PEP_farma_farmacos_VB %>%
  bind_cols() %>%
  select(matches('Grupo')) %>% 
  map(unique) %>%
  reduce(append) %>%
  unique() %>%
  na.omit() %>%
  .[ str_detect(.,'(Antidepresivo\\+benzodiazepina)', negate = T) ] %>%
  str_replace(.,' ','_') 

# ////////////////////////////////////////////////////////////////////////////////////
# FINAL VB -----
# ////////////////////////////////////////////////////////////////////////////////////

aux_VB <- PEP_farma_farmacos_VB %>%     # <--- Cambio de nombre !
  bind_cols() %>%
  select(matches('Grupo')) %>%
  list() %>%
  crossing(aux_limpieza_categorias) %>%
  set_names('dataframes', 'tipo_farmaco')

PEP_VB_farma <- map2_dfc(
  aux_VB$dataframes, 
  aux_VB$tipo_farmaco, 
    ~ .x %>% modify_at(1:5,function(x) str_count(.y,x) )%>% rowSums(., na.rm=T)  ) %>%
  set_names(aux_VB$tipo_farmaco) %>% # <--- aux cambio !
  rename_all( ~ paste0('N_',.,'_VB')) %>%
  mutate(N_total_farmacos_VB = rowSums(. )) %>%
  # ///////////
  bind_cols(PEP_farma_farmacos_VB) %>%   # <--- Cambio de nombre !
  mutate(
    CPZ_total_VB = rowSums(across(matches('CPZ') ), na.rm = T),
    posologia_total_VB = rowSums(across(matches('posologia') ), na.rm = T)) %>%
  select(
    starts_with('farmaco_1'):starts_with('nivel_antipsicotico_5'), 
    starts_with('N_'),
    posologia_total_VB,
    CPZ_total_VB) %>%
  mutate_if(is.character, str_to_title ) 


# ////////////////////////////////////////////////////////////////////////////////////
# FINAL 2M -----
# ////////////////////////////////////////////////////////////////////////////////////

aux_2M <- PEP_farma_farmacos_2M %>%     # <--- Cambio de nombre !
  bind_cols() %>%
  select(matches('Grupo')) %>%
  list() %>%
  crossing(aux_limpieza_categorias) %>%
  set_names('dataframes', 'tipo_farmaco')

PEP_2M_farma <- map2_dfc(
  aux_2M$dataframes, 
  aux_2M$tipo_farmaco, 
  ~ .x %>% modify_at(1:5,function(x) str_count(.y,x) )%>% rowSums(., na.rm=T)  ) %>%
  set_names(aux_2M$tipo_farmaco) %>% # <--- aux cambio !
  rename_all( ~ paste0('N_',.,'_2M')) %>%
  mutate(N_total_farmacos_2M = rowSums(. )) %>%
  # ///////////
  bind_cols(PEP_farma_farmacos_2M) %>%   # <--- Cambio de nombre !
  mutate(
    CPZ_total_2M = rowSums(across(matches('CPZ') ), na.rm = T),
    posologia_total_2M = rowSums(across(matches('posologia') ), na.rm = T)) %>%
  select(
    starts_with('farmaco_1'):starts_with('nivel_antipsicotico_5'), 
    starts_with('N_'),
    posologia_total_2M, # <--- Cambio de nombre !
    CPZ_total_2M) %>%   # <--- Cambio de nombre !
  mutate_if(is.character, str_to_title ) 


# ////////////////////////////////////////////////////////////////////////////////////
# FINAL 6M -----
# ////////////////////////////////////////////////////////////////////////////////////

aux_6M <- PEP_farma_farmacos_6M %>%     # <--- Cambio de nombre !
  bind_cols() %>%
  select(matches('Grupo')) %>%
  list() %>%
  crossing(aux_limpieza_categorias) %>%
  set_names('dataframes', 'tipo_farmaco')

PEP_6M_farma <- map2_dfc(
  aux_6M$dataframes,     # <---  Cambio de auxiliar !
  aux_6M$tipo_farmaco,   # <---  Cambio de auxiliar !
  ~ .x %>% modify_at(1:5,function(x) str_count(.y,x) )%>% rowSums(., na.rm=T)  ) %>%
  set_names(aux_6M$tipo_farmaco) %>%     # <---  Cambio de auxiliar !
  rename_all( ~ paste0('N_',.,'6M')) %>%
  mutate(N_total_farmacos_6M = rowSums(. )) %>%
  # ///////////
  bind_cols(PEP_farma_farmacos_6M) %>%   # <--- Cambio de nombre !
  mutate(
    CPZ_total_6M = rowSums(across(matches('CPZ') ), na.rm = T),
    posologia_total_6M = rowSums(across(matches('posologia') ), na.rm = T)) %>%
  select(
    starts_with('farmaco_1'):starts_with('nivel_antipsicotico_5'), 
    starts_with('N_'),
    posologia_total_6M, # <--- Cambio de nombre !
    CPZ_total_6M) %>%   # <--- Cambio de nombre !
  mutate_if(is.character, str_to_title ) 


# ////////////////////////////////////////////////////////////////////////////////////
# FINAL 12M -----
# ////////////////////////////////////////////////////////////////////////////////////


aux_12M <- PEP_farma_farmacos_12M %>%     # <--- Cambio de nombre !
  bind_cols() %>%
  select(matches('Grupo')) %>%
  list() %>%
  crossing(aux_limpieza_categorias) %>%
  set_names('dataframes', 'tipo_farmaco')

PEP_12M_farma <- map2_dfc(
  aux_12M$dataframes,     # <---  Cambio de auxiliar !
  aux_12M$tipo_farmaco,   # <---  Cambio de auxiliar !
  ~ .x %>% modify_at(1:5,function(x) str_count(.y,x) )%>% rowSums(., na.rm=T)  ) %>%
  set_names(aux_12M$tipo_farmaco) %>%     # <---  Cambio de auxiliar !
  rename_all( ~ paste0('N_',.,'12M')) %>%
  mutate(N_total_farmacos_12M = rowSums(. )) %>%
  # ///////////
  bind_cols(PEP_farma_farmacos_12M) %>%   # <--- Cambio de nombre !
  mutate(
    CPZ_total_12M = rowSums(across(matches('CPZ') ), na.rm = T),                 # <--- Cambio de nombre !
    posologia_total_12M = rowSums(across(matches('posologia') ), na.rm = T)) %>% # <--- Cambio de nombre !
  select(
    starts_with('farmaco_1'):starts_with('nivel_antipsicotico_5'), 
    starts_with('N_'),
    posologia_total_12M, # <--- Cambio de nombre !
    CPZ_total_12M) %>%   # <--- Cambio de nombre !
  mutate_if(is.character, str_to_title ) 





