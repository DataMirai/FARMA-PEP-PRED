
# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////
# Armamento del dataframe final ----
# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////

PEP_reconstruida <- pmap(
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
         PEP_12M_farma),
    list('Identificadores',
         'Entrevista_basal',
         'Entrevista_2M',
         'Entrevista_6M',
         'Entrevista_12M',
         'Toxicos',
         'Farmacos_VB',
         'Farmacos_2M',
         'Farmacos_6M',
         'Farmacos_12M')),
  ~ tibble( ident_caso = PEP_identificadores[,1]) %>%
    bind_cols(..1) %>%
    nest( -c(ident_caso),.key = ..2)) %>%
  reduce(inner_join, by = "ident_caso")

PEP_reconstruida_unnested <- PEP_reconstruida %>% unnest(
  cols = c(
    ident_caso, Identificadores, 
    Entrevista_basal, Entrevista_2M, Entrevista_6M, Entrevista_12M, 
    Toxicos, 
    Farmacos_VB,Farmacos_2M, Farmacos_6M, Farmacos_12M)) 

rm(list=setdiff(ls(), c('PEP_reconstruida', 'PEP_reconstruida_unnested')))

renombres <- read_xlsx('data/PEP/Nombres_PEPS_New.xlsx') %>% 
  mutate(nombres_final = case_when(
    is.na(New_Nombres_PEP) ~ Nombres_PEP,
    TRUE ~ New_Nombres_PEP)) %>% 
  pull(nombres_final)

PEP_reconstruida_unnested <- PEP_reconstruida_unnested %>% 
  set_names(renombres) 


extras <- read_xlsx('data/PEP/BBDD_PEPs_Extras.xlsx')

PEP_genetica <- read_xlsx('data/PEP/PEP_PRS.xlsx')

PEP_reconstruida_unnested <- PEP_reconstruida_unnested %>%  
  left_join(.,PEP_genetica,by= c('ident_caso'= 'EXCEL')) %>%
  inner_join(.,extras,by= c('ident_caso' = 'Case_identification'))

rm(list=setdiff(ls(), c('PEP_reconstruida_unnested')))



PEP_esquizofrenia <- PEP_reconstruida_unnested %>%
  select(
    # Identificadores
    ident_caso:DTP,ESTIMATION_CI:COGNITIVE_RESERVE,PNS_DEFINITIVA,
    # Genetica,
    PRS_SZ:PRS_UC,
    # Toxicos
    Tabaco_VB:Inhalantes_V12M,
    # Basal
    CGI_Gravedad_VB:  GEOPTE_Total_VB,PSFS_VB,  NSFS_VB ,   PNS_VB,   MAP_VB,    EXP_VB,  peso_VB:qt_VB,
    # 2 meses
    CGI_Gravedad_V2M:SAS_Total_V2M,   PSFS_V2M, NSFS_V2M,                                 peso_V2M:qt_V2M,
    # 6 meses
    CGI_Gravedad_V6M:UKU_Total_V6M   ,PSFS_V6M, NSFS_V6M,   PNS_V6M,                      peso_V6M:qt_V6M,
    # 12 meses
    CGI_Gravedad_V12M:UKU_Total_V12M, PSFS_V12M, NSFS_V12M, PNS_V12M, MAP_V12M, EXP_V12M, peso_V12M:qt_V12M,
    # Farmacos Basal
    farmaco_1_VB:CEDD_Total_VB,
    # Farmacos 2 meses
    farmaco_1_2M:CEDD_Total_V2M,
    # Farmacos 5 meses
    farmaco_1_6M: CEDD_Total_V6M,
    # Farmacos 12 meses
    farmaco_1_12M:CEDD_Total_V12M)


write_xlsx(PEP_esquizofrenia,path= 'Data/PEP/PEP_proyecto_esquizofrenia/PEP_esquizofrenia.xlsx')


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

