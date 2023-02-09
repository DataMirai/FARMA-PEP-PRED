if(!require('pacman')){install.packages('pacman')}
pacman::p_load(
  # para cargar todas als librereias requeridas directamente 
  tidyverse, tidymodels, # Conjunto de librerías de progtamación de buena sintaxis y funcional
  themis, tidyposterior,baguette,corrr,readr,magrittr,forcats,skimr,patchwork,GGally,
  doParallel,DALEXtra,broom,xgboost,patchwork,probably,vip,
  readxl, # importación y exportaciónd e datos)
  fastshap,gt,kernelshap, shapviz,withr,ggfittext
)

Model_best_results <- readRDS('Scripts/PEP_modelos/Reincidencia_esquizofrenia/Scripts/Models_testing/best_result.rds')

PNS_data <- read_xlsx('Data/PEP/PEP_proyecto_esquizofrenia/PEP_Aitor.xlsx') %>% 
  select( 
    "Ident_caso","PNS_DEFINITIVA","PNS_Liemburg_Yes_No_V12M",
    "Sexo","Etnia","Inmigrante","Nivel_socioeconomico","Nivel_urbanicidad",
    "Antecedentes_psiquiátricos","Edad_primer_episodio",'Edad_estudio',
    "Lewis_AB","Lewis_Total",
    "Tabaco_VB","Alcohol_VB","cannabis_VB","Cocaina_VB",
    "PAS_Total","DUP","DTP","PANSS_positivos_VB","PANSS_negativos_VB",
    "PANSS_generales_VB","PANSS_total_VB","YOUNG_Total_VB",
    "MADRS_Total_VB","FAST_Total_VB",                   
    "PRS_SZ","PRS_BD","PRS_MDD","PRS_AD","PRS_AN","PRS_PTSD","PRS_OCD",
    "PRS_ADHD","PRS_Neuroticism","PRS_IQ","PRS_CIQ","PRS_EA","PRS_CP",
    "Estimation_ci","Atention","Working_memory","Verbal_memor",
    "Executive_function","Cognitive_reserve", 
    "FES_SSD_NoAffective_NoToxicPsych","TraumaticExperiences",
    "WinterBirth" ) %>% 
  # filtros requeridos para tener la base de datos correcta
  filter(
    !str_detect(.$Ident_caso, '[:alpha:]') &
      !is.na(PNS_DEFINITIVA) &
      Edad_estudio >=16 &
      FES_SSD_NoAffective_NoToxicPsych ==1) %>% 
  select(-c(Edad_estudio,FES_SSD_NoAffective_NoToxicPsych)) %>% 
  # Arreglo de Nivel_urbanicidad
  mutate(Nivel_urbanicidad = case_when(
    Nivel_urbanicidad == "Ciudad de provincia con mas de 10.000 hab" ~ "Provincia",
    Nivel_urbanicidad == "Ciudad de provincia con mas de 100.000 hab" ~ "Provincia",
    Nivel_urbanicidad == "Desconocido" ~ NA_character_ ,
    Nivel_urbanicidad == "Suburbio de capital" ~  "Capital",
    TRUE ~as.character(Nivel_urbanicidad))) %>% 
  # Arreglo Antecedentes_psiquiátricos
  mutate(Antecedentes_psiquiátricos= case_when(
    Antecedentes_psiquiátricos=="No valorable"~ NA_character_,
    TRUE ~ as.character(Antecedentes_psiquiátricos)))  %>%  
  # Arreglo de toxicos (reemplazo de No evaluado por NA)
  mutate_at(vars(Tabaco_VB, Alcohol_VB, cannabis_VB,Cocaina_VB), as.character) %>% 
  mutate_at(
    vars(Tabaco_VB, Alcohol_VB, cannabis_VB,Cocaina_VB), 
    ~ case_when(. == "No evaluado" ~ NA_character_ , TRUE ~ .)  ) %>% 
  # Arreglo Nivel_socioeconomico
  mutate(Nivel_socioeconomico = case_when(
    Nivel_socioeconomico %in% c("Medium","Medium-High","Medium-Low") ~ "Medium",
    Nivel_socioeconomico == "Unknown" ~ NA_character_ ,
    TRUE ~as.character(Nivel_urbanicidad)) ) %>%
  mutate(
    PNS_reincidencia = case_when(
      PNS_DEFINITIVA == 1~ 'Si',
      PNS_DEFINITIVA == 0 ~ 'No',
      TRUE ~as.character(PNS_DEFINITIVA)),
    Antecedentes_psiquiátricos = case_when(
      Antecedentes_psiquiátricos == 'Sí' ~ 'Si',
      Antecedentes_psiquiátricos == 'No' ~ 'No',
      TRUE ~as.character(Antecedentes_psiquiátricos))) %>% 
  select(
    PNS_reincidencia,
    #Numericas
    # Identificadoras
    Edad_primer_episodio,
    # Escalas
    PANSS_negativos_VB, PANSS_total_VB, PAS_Total, FAST_Total_VB, FAST_Total_VB,MADRS_Total_VB,DUP,
    # Cognitiva
    Verbal_memor,Cognitive_reserve,Working_memory,
    # geneticas
    PRS_CP,PRS_EA,PRS_OCD,PRS_SZ,
    #Categoricas
    Antecedentes_psiquiátricos, Cocaina_VB, Alcohol_VB,cannabis_VB, Sexo) %>% 
  mutate_if(is.character,as.factor )



# ////////////////////////////////////////////////////////////
# Setting seed
# ////////////////////////////////////////////////////////////

set.seed(246)

# ////////////////////////////////////////////////////////////
# Setting seed
# ////////////////////////////////////////////////////////////

PNS_split <- initial_split(PNS_data, prop = 0.8, strata = PNS_reincidencia )

PNS_train <- training(PNS_split)
PNS_test  <- testing(PNS_split)

# //////////////////////////////////////////////////////////////////////
# - Models definition ---- 
# //////////////////////////////////////////////////////////////////////

Model_RandomForest <- 
  rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

# //////////////////////////////////////////////////////////////////////
# - Recipe and Formulas specification ---- 
# //////////////////////////////////////////////////////////////////////

Recipe_NEARMISS <- # Remove Majority Class Instances by Undersampling
  recipe(PNS_reincidencia ~ . , data = training(PNS_split)) %>% 
  step_impute_knn(all_nominal(),-all_outcomes()) %>%
  step_impute_bag(all_numeric(),-all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>% 
  step_nearmiss(PNS_reincidencia)

# //////////////////////////////////////////////////////////////////////
# - Model execution ---- 
# //////////////////////////////////////////////////////////////////////

Model_fit <- workflow() %>% 
  add_recipe( Recipe_NEARMISS ) %>% 
  add_model( Model_RandomForest ) %>% 
  finalize_workflow(Model_best_results) %>%
  fit(PNS_train) 



# //////////////////////////////////////////////////////////////////////
# - Model explanation  ---- 
# //////////////////////////////////////////////////////////////////////

vip_features <- names(PNS_train)[-1]

vip_train <- 
  PNS_train %>% 
  select(all_of(vip_features))



Model_explainer <- 
  explain_tidymodels(
    Model_fit, 
    data = vip_train, 
    y = PNS_train$PNS_reincidencia,
    label = "random forest",
    verbose = FALSE
  )



lista_pacientes  <- c(1, 174, 20, 54, 75, 123) 

lista_pacientes <- map(lista_pacientes, ~ vip_train[.,]) %>%
  set_names(paste0('paciente_',lista_pacientes) )
  

lista_pacientes

SHAP_pacientes <- map(
  lista_pacientes, 
  ~ predict_parts(
    explainer = Model_explainer, 
    new_observation = .x, 
    type = "shap",
    B = 20
  )
)

SHAP_pacientes %>% 
  map(~ .x %>% select(mean) %>% rename('mean'= 'SHAP') )

SHAP_pacientes[[1]]


?predict_parts()

predict_parts(
  explainer = Model_explainer, 
  new_observation = lista_pacientes[[2]], 
  type = "break_down"
)
