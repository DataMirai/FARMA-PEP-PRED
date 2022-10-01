rm(list = setdiff(ls(), c('PEP','diccionario_variables')))

anti_psicoticos <- read_xlsx(path = 'Data/CodisAP.xlsx')

Farmacos_mal_etiquetados <- read_xlsx(path = 'Data/farmacos_mal_etiquetados_SM.xlsx') %>%
  rename('P_actiu'= `Ppi actiu`)

PEP_farma <- PEP %>% select(any_of(diccionario_variables$farma))

# Añadir la CPZ total por periodo de entrevista
PEP_farma <- list(
  'Farmacos_VB' = PEP_farma %>% select(farmaco1:CPZ5    ,antipsicoticoVB :metabolito_activo3VB),
  'Farmacos_2M' = PEP_farma %>% select(farmaco1_A:CPZ5_A,antipsicoticoV2M:metabolito_activo3V2M),
  'Farmacos_6M' = PEP_farma %>% select(farmaco1_B:CPZ5_B,antipsicóticoV6M:metabolito_activo3V6M),
  'Farmacos_12M'= PEP_farma %>% select(farmaco1_C:CPZ5_C,antipsicóticoV1A:metabolito_activo3V1A)) %>%
  map(~ map_dfc(., ~ na_if(.,'')))

test <- PEP_farma$Farmacos_VB
test

test


PEP_farma <- crossing(
  PEP_farma,
  list(
    c(1:3,16:18),
    c(4:6,19:21),
    c(7:9,22:24),
    c(10:12),
    c(13:15))) %>%
  set_names(c('dataframes','posiciones')) 

PEP_farma <- map2(
  PEP_farma$dataframes,
  PEP_farma$posiciones,
  ~ select(.x, .y))

PEP_farma





df1 <- data.frame(A = c(1, 2, 3), B = c(0,0,3), C = c(3,2,1)) 
df2 <- data.frame(A = c(0, 2, 4), B = c(1,0,3), C = c(0,1,4))


map2_dfr(
  .x = df1,
  .y = df2,
  ~ case_when(
    .x == 0 & .y > 0 ~ "colonised",
    .x > 0 & .y == 0 ~ "extinct",
    .x < .y  ~ "increased",
    .x == .y ~ "stable",
    .x > .y  ~ "decreased"))
