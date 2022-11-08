
# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////
# Armamento del dataframe final ----
# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////

PEP_final <- pmap(
  list(
    list(PEP_identificadores[,-1],
         PEP_VB_entrevista,
         PEP_2M_entrevista,
         PEP_6M_entrevista,
         PEP_12M_entrevista,
         PEP_toxicos,
         PEP_VB_farma,
         PEP_2M_farma,
         PEP_6M_farma,
         PEP_12M_farma,
         PEP_genetica[,-1]),
    list('Identificadores',
         'Entrevista_basal',
         'Entrevista_2M',
         'Entrevista_6M',
         'Entrevista_12M',
         'Toxicos',
         'Farmacos_VB',
         'Farmacos_2M',
         'Farmacos_6M',
         'Farmacos_12M',
         'Genetica')),
  ~ tibble( ident_caso = PEP_identificadores[,1]) %>%
    bind_cols(..1) %>%
    nest( -c(ident_caso),.key = ..2)) %>%
  reduce(inner_join, by = "ident_caso")



PEP_final_unnested <- PEP_final %>% unnest(
  cols = c(ident_caso, Identificadores, Entrevista_basal, Entrevista_2M, 
  Entrevista_6M, Entrevista_12M, Toxicos, Farmacos_VB, Farmacos_2M, 
  Farmacos_6M, Farmacos_12M, Genetica))  %>%
  select(-c(fecha_1ªhospitalizacion_sint_psic_VB))



rm(list=setdiff(ls(), c('PEP_final', 'PEP_final_unnested')))

aux_renombre <- read_xlsx('data/Nombres_PEPS_New.xlsx')

nombres_arreglados <- aux_renombre %>%
  mutate(final = case_when( 
    is.na(New_Nombres_PEP)  ~ Nombres_PEP,
    !is.na(New_Nombres_PEP) ~ New_Nombres_PEP)) %>%
  select(final) %>% 
  pull() %>%
  .[-c(22)]

names(PEP_final_unnested)[1:30];
nombres_arreglados[1:30]


PEP_final_unnested %>%
  select(-c(fecha_1ªhospitalizacion_sint_psic_VB))
  

# no controles para la reticiencia.

#  amyor puntuación en genetica PRS_SZ
# PRS_BD
# PRS_MDD
# PRS_AD
# PRS_AN
# PRS_PTSD
# PRS_OCD
# PRS_ADHD
# PRS_Neuroticism

# PRS_IQ
# PRS_CIQ
# PRS_EA 
# PRS_CP

