identificadores_paciente <- c(
  'fecha_nacimiento',                                                
  "tipo_sujeto",
  "sexo",
  "primera_entrevista",
  "etnia",
  "lugar_naci_pais",
  "provincia_naci",
  "provincia_resid",
  "adoptado",
  "estado_civil",
  "convivencia",
  "nivel_educacion",
  "educacion_especificar",
  "situacion_laboral",
  "nivel_ocupacional",
  "nivel_educaci_progenitor",
  "situacion_laboral_progenitor",
  "nivel_ocupacional_progenitor",
  "nivel_socioeconomico",
  "nivel_urbanicidad",
  "antecedentespsiquiátricos",
  "antecedentes_generales_psiquiátricos_descripcion",
  "fecha_primer_diagnostico" 
)


mediciones_Basal  <- str_subset(names(PEP), 'VB|BASAL')

mediciones_2M     <- str_subset(names(PEP),'V2M')

mediciones_6M     <- str_subset(names(PEP), 'V6M')

mediciones_1A     <- str_subset(names(PEP), 'V1A|V12M ')

mediciones_2A     <- str_subset(names(PEP), 'V2A|V24M')

historia_familiar <- str_subset(names(PEP), ) 

