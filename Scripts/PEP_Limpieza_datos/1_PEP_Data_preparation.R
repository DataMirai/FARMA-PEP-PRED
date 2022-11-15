# ////////////////////////////////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////////////////////////////////
# Library load ----
# ////////////////////////////////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////////////////////////////////
if(!require('pacman')){install.packages('pacman')}
pacman::p_load(tidyverse,haven, broom, lubridate, readxl,writexl)


# ////////////////////////////////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////////////////////////////////
# Data load ----
# ////////////////////////////////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////////////////////////////////

# Los datos son los correspondientes al PEPs del experimento
PEP <- read_sav("Data/Definitive_PEPs.sav") %>%
  mutate_if(is.labelled,as_factor) %>%
  # añadimos la variable identificadora DTP, que está en el PEP DTP
  inner_join(read_excel("Data/PEPs.xlsx"), by=c('ident_caso' = 'ID')) 


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
# mediciones_2M: datos reocgidos en a los 2  meses del experimento
# mediciones_6M: datos reocgidos en a los 6  meses del experimento
# mediciones_1A: datos reocgidos en a los 12 meses del experimento
# mediciones_2A: datos reocgidos en a los 24 meses del experimento
# historia_familiar: datos recogidos en la historia familia (incluyendo padre, madre, hermanos y parientes)
# encefalo: datos del encefalo del paciente (pruebas y estado) neuroimagen


# diccionario_variables_original <- list(
#   'identificadores' = c(
#     names(PEP)[1:27], 
#     'disponibilidad',
#     'fecha_entrevista',
#     'antecedentes_psiquiatricos',
#     'antecedentes_psiquiatricos_espe',
#     'antecedentes_psicoticos',
#     'antecedentes_psicoticos_espe',
#     'fecha_primer_diagnostico'),
#   'trastornos' = names(PEP)[30:379],
#   'mediciones_Basal'  = str_subset(names(PEP), 'VB|BASAL'),
#   'mediciones_2M'     = str_subset(names(PEP),'V2M'),
#   'mediciones_6M'     = str_subset(names(PEP), 'V6M'),
#   'mediciones_1A'     = str_subset(names(PEP), 'V1A|V12M '),
#   'mediciones_2A'     = str_subset(names(PEP), 'V2A|V24M'),
#   'historia_familiar' = str_subset(names(PEP),'[pP]adre|[Mm]adre|[Hh]ermano|[Hh]ijo|[Pp]ariente'),
#   'encefalo' = names(PEP)[2421:2548])

diccionario_variables <- list(
  
  'identificadores' = c(
    names(PEP)[1:27],
    names(PEP)[c(1285,1291,1297,1303)],
    'disponibilidad',
    'fecha_entrevista',
    'antecedentes_psiquiatricos','antecedentes_psiquiatricos_espe','antecedentes_psicoticos','antecedentes_psicoticos_espe',
    'fecha_primer_diagnostico','fecha_1ªhospitalizacion_sint_psic_VB','Iniciosíntomaspsicóticos_Fecha_estimacion_entrevistador',
    'infanciaVB', 'adolescenciatempranaVB', 'adolescenciatardíaVB',' adultoVB','generalVB', 'puntuacionTotal_PAS_VB',
    'DUP','DTP'),
  
  'mediciones_Basal'  = str_subset(names(PEP),'VB|BASAL'),
  'mediciones_2M'     = str_subset(names(PEP),'V2M'),
  'mediciones_6M'     = str_subset(names(PEP),'V6M'),
  'mediciones_1A'     = str_subset(names(PEP),'V1A|V12M '),
  'mediciones_2A'     = str_subset(names(PEP),'V2A|V24M'),
  
  'encefalo' = names(PEP)[2421:2548],
  
  'toxicos' = c(
    "ConsumoTabaco_VB",           
    "ConsumoAlcohol_VB",     
    "Consumocannabis_VB",      
    "ConsumoCocaina_VB",     
    "SedantesConsumo_VB",      
    "Estimulantesconsumo_VB",
    "Alucinogenosconsumo_VB" ,  
    "Heroínaconsumo_VB",     
    "Metadonaconsumo_VB",    
    "Otrosopiaceosconsumo_VB", 
    "Inhalantesconsumo_VB",    
    "ConsumoTabaco_V2M",
    "ConsumoAlcohol_V2M",
    "Consumocannabis_V2M", 
    "ConsumoCocaina_V2M",
    "SedantesConsumo_V2M",
    "Estimulantesconsumo_V2M",
    "Alucinogenosconsumo_V2M"  , 
    "Heroínaconsumo_V2M",
    "Metadonaconsumo_V2M",
    "Otrosopiaceosconsumo_V2M",
    "Inhalantesconsumo_V2M",    
    "ConsumoTabaco_V6M",
    "ConsumoAlcohol_V6M",
    "Consumocannabis_V6M",
    "ConsumoCocaina_V6M",
    "SedantesConsumo_V6M",
    "Estimulantesconsumo_V6M",
    "Alucinogenosconsumo_V6M",
    "Heroínaconsumo_V6M",
    "Metadonaconsumo_V6M",
    "Otrosopiaceosconsumo_V6M",
    "Inhalantesconsumo_V6M",
    "ConsumoTabaco_V1AÑO",
    "ConsumoAlcohol_V1AÑO",
    "Consumocannabis_V1AÑO",
    "ConsumoCocaina_V1AÑO",
    "SedantesConsumo_V1AÑO"   ,
    "Estimulantesconsumo_V1AÑO",
    "Alucinogenosconsumo_V1AÑO",
    "Heroínaconsumo_V1AÑO",
    "Metadonaconsumo_V1AÑO",
    "Otrosopiaceosconsumo_V1AÑO",
    "Inhalantesconsumo_V1AÑO"),
  
  'farma' = c(names(PEP)[1689:1772], names(PEP)[2073:2129]))

# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////
## Arreglo en variables indentificadoras ----
# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////

PEP_identificadores <- PEP %>%
  select(any_of(diccionario_variables$identificadores)) %>%
  # Arreglamos las variables de complicaciones obstetricas a factor, no a numerica
  mutate(across(c('LewisA':'LewisT'), as.factor)) %>%
  # Creamos las variables de edad correspondientes
  mutate(
    'edad_primer_episodio'         = year(as.period(interval(fecha_nacimiento, Iniciosíntomaspsicóticos_Fecha_estimacion_entrevistador))),
    'edad_primera_hospitalizacion' = year(as.period(interval(fecha_nacimiento, fecha_1ªhospitalizacion_sint_psic_VB))),
    'edad_primer_diagnostico'      = year(as.period(interval(fecha_nacimiento, fecha_primer_diagnostico ))),
    'edad_estudio'                 = year(as.period(interval(fecha_nacimiento, fecha_entrevista))),) %>%
  # Modificamos la etnia para tener una sola dicotomica
  mutate(
    etnia = case_when(etnia == 'caucasian' ~ 'caucasian', TRUE ~ 'others'),
    inmigrante= factor(case_when(
      lugar_naci_pais !='ESP' & !is.na(lugar_naci_pais)  ~ 'internacional',
      as.character(provincia_naci) != as.character(provincia_resid) &  
        lugar_naci_pais != 'Extranjero' & (!is.na(provincia_naci) | !is.na(provincia_resid)) ~ 'Nacional',
      TRUE ~ 'No'))) %>%
  select(-c(
    fecha_1ªhospitalizacion_sint_psic_VB,
    fecha_nacimiento ,primera_entrevista, fecha_entrevista, fecha_primer_diagnostico,
    Iniciosíntomaspsicóticos_Fecha_estimacion_entrevistador,
    lugar_naci_pais:nivel_ocupacional_progenitor, disponibilidad:antecedentes_psicoticos_espe )) %>%
  # Reordernar variables
  select(ident_caso, tipo_sujeto, sexo, etnia, inmigrante,
         edad_estudio, edad_primer_episodio,
         edad_primer_diagnostico, edad_primera_hospitalizacion, everything()) 

# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////
## Arreglo en diccionario variables toxicologicas ----
# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////

PEP_toxicos <- PEP %>% select(any_of(diccionario_variables$toxicos))

# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////
## Arreglo en diccionario variables Simtomatologicas de estado BASAL ----
# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////

# PEP basal, conservar variables en el diccioanrio de variables.
PEP_VB_entrevista <- PEP %>%
  select(
    'GravedaddelaenfermedadVB','EEAGpuntuacionTotalVB',
    'total_positivosVB','total_negativosVB','total_generalesVB','total_panssVB',
    'YOUNGpuntuacionTotalVB','MADRSpuntuacionTotalVB','PuntuaciónTotalFASTVB',
    'puntuacionTotalVB', #escala de tios de Valencia
    'peso_VB','imc_VB', 'perimetro_VB',
    # Bioquimica
    "BASAL_Hematocrito","BASAL_Hemoglobina",'BASAL_Hemoglobina_glicosilada',"BASAL_Hematies","BASAL_VCM",
    "BASAL_HCM","BASAL_CHCM","BASAL_Plaquetas","BASAL_VPM","BASAL_IDP","BASAL_Leucocitos_totales",
    "BASAL_Eosinófilos_totales","BASAL_Basófilos_totales","BASAL_Linfocitos_totales",
    "BASAL_Monocitos_totales","BASAL_Neutrofilos_totales","BASAL_Eritrosedimentación",
    "BASAL_Glucosa_suero","BASAL_Creatinina_suero","BASAL_Sodio","BASAL_Potasio","BASAL_Calcio",
    "BASAL_Fósforo","BASAL_Cloro","BASAL_Hemoglobina_glicosilada","BASAL_Trigliceridos",
    "BASAL_Colesterol_total","BASAL_Colesterol_HDL","BASAL_Colesterol_LDL","BASAL_TSH",
    "BASAL_T4_libre","BASAL_Prolactina","BASAL_Estradiol","BASAL_FSH",
    "BASAL_Progesterona","BASAL_LH","BASAL_Testosterona",
    # Cardio
    "electroVB","Presión_sistolica_VB", "Presión_diastolica_VB","anormalidadVB",
    "Taquicardia_sinusalVB","Bradicardia_sinusalVB","Hipertrofia_auricular_izquierdaVB",
    "Latido_supraventricular_prematuroVB","Latido_ventricular_prematuroVB",
    "Taquicardia_supraventricularVB","Taquicardia_ventricularVB",
    "Inversiones_simetricas_onda_TVB","Pobre_progresion_onda_RVB",
    "Bloqueo_incompleto_rama_derechaVB"  ,"Bloqueo_completo_ramaderechaVB",
    "Bloqueo_incompleto_rama_izquierdaVB","Bloqueo_completo_izquierdaVB",
    "lpmVB","qrsVB","prVB","qtVB")


# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////
## Arreglo en diccionario variables Simtomatologicas de estado dos meses (2M) ----
# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////

PEP_2M_entrevista <- PEP %>%
  select(contains('V2M')) %>%
  select(matches( str_remove_all(names(PEP_VB_entrevista),'_VB|BASAL_|VB'))) %>%
  mutate(PuntuaciónTotalV2M_UKU = PEP$PuntuaciónTotalV2M_UKU) %>%
  select(-c(
    prueba_embarazo_V2M,
    prueba_embarazo_resultado_V2M,
    SGpreocupacionessomaticasV2M:Ociopracticardeporte23V2M,
    aprenderaaprender_PDV2M:aprenderaaprender_PCV2M,
    Sindrome_pre_excitacionV2M)) %>%
  select(gravedaddelaenfermedadV2M:PuntuaciónTotalFASTV2M, 
         PuntuaciónTotalV2M_UKU, 
         everything()) 

# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////
## Arreglo en diccionario variables Simtomatologicas de estado seis meses (6M) ----
# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////

PEP_6M_entrevista <- PEP %>%
  select(contains('V6M')) %>%
  select(matches( str_remove_all(names(PEP_VB_entrevista),'_VB|BASAL_|VB'))) %>%
  mutate(PuntuaciónTotalV6M_UKU = PEP$PuntuaciónTotalV6M_UKU) %>%
  select( -c(
    prueba_embarazo_V6M:prueba_embarazo_resultado_V6M,
    SGpreocupacionessomaticasV6M:Sindrome_pre_excitacionV6M)) %>%
  select(
    gravedaddelaenfermedadV6M:PuntuaciónTotalFASTV6M, PuntuaciónTotalV6M_UKU, 
    peso_V6M: V6M_Testosterona,
    Presion_sistolica_V6M,Presion_diastolica_V6M,
    everything())

# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////
## Arreglo en diccionario variables Simtomatologicas de estado doce meses (12M) ----
# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////

PEP_12M_entrevista <- PEP %>%
  select(matches('1A|12M') ) %>%
  select(matches( str_remove_all(names(PEP_VB_entrevista),'_VB|BASAL_|VB'))) %>%
  mutate(PuntuaciónTotalV1A_UKU  = PEP$PuntuaciónTotalV1A_UKU) %>%
  select(-c(
    prueba_embarazo_V1AÑO:prueba_embarazo_resultado_V1AÑO,
    SGpreocupacionessomaticasV1A:Ociopracticardeporte23V1A,
    Sindrome_pre_excitacionV1A)) %>%
  select(
    gravedaddelaenfermedadV1A:PuntuaciónTotalFASTV1A, PuntuaciónTotalV1A_UKU, 
    peso_V1AÑO: V12M_Testosterona,
    Presion_sistolica_V1AÑO,Presion_diastolica_V1AÑO,
    everything())




