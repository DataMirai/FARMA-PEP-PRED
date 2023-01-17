if(!require('pacman')){install.packages('pacman')}
pacman::p_load(
  # para cargar todas als librereias requeridas directamente 
  tidyverse, tidymodels, # Conjunto de librerías de progtamación de buena sintaxis y funcional
  parsnip, ranger, #Modelizacion con tidymodels, mice,
  naniar,mice,randomForest,glmnet,doParallel,DALEXtra,broom,
  readxl,writexl # importación y exportaciónd e datos)
)

reincidencia_data_PNS <- read_xlsx('Data/PEP/PEP_proyecto_esquizofrenia/PEP_Aitor.xlsx') %>% 
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
    "Executive_function","Composite_score","Cognitive_reserve", 
    "FES_SSD_NoAffective_NoToxicPsych","TraumaticExperiences",
    "WinterBirth" ) %>% 
  # filtros requeridos para tener la base de datos correcta
  filter(
    !str_detect(.$Ident_caso, '[:alpha:]') &
      !is.na(PNS_DEFINITIVA) &
      Edad_estudio >=16 &
      FES_SSD_NoAffective_NoToxicPsych ==1) %>% 
  select(-c(Edad_estudio,FES_SSD_NoAffective_NoToxicPsych)) %>% 
  # Convertir variables a factor
  mutate_at(vars(c(
    PNS_DEFINITIVA,
    PNS_Liemburg_Yes_No_V12M,
    WinterBirth,
    TraumaticExperiences)),as.factor  ) %>% 
  mutate_if(is.character,as.factor ) %>% 
  # Arreglo de Nivel_urbanicidad
  mutate(Nivel_urbanicidad = case_when(
    Nivel_urbanicidad == "Ciudad de provincia con mas de 10.000 hab" ~ "Provincia",
    Nivel_urbanicidad == "Ciudad de provincia con mas de 100.000 hab" ~ "Provincia",
    Nivel_urbanicidad == "Desconocido" ~ NA_character_ ,
    Nivel_urbanicidad == "Suburbio de capital" ~  "Capital",
    TRUE ~as.character(Nivel_urbanicidad)) %>% as.factor() ) %>% 
  mutate(Nivel_urbanicidad = factor(
    Nivel_urbanicidad, levels= c('Capital','Provincia','Area rural'))) %>% 
  # Arreglo Antecedentes_psiquiátricos
  mutate(Antecedentes_psiquiátricos= case_when(
    Antecedentes_psiquiátricos=="No valorable"~ NA_character_,
    TRUE ~ as.character(Antecedentes_psiquiátricos)))  %>%  
  # Arreglo de toxicos (reemplazo de No evaluado por NA)
  mutate_at(vars(Tabaco_VB, Alcohol_VB, cannabis_VB,Cocaina_VB), as.character) %>% 
  mutate_at(vars(Tabaco_VB, Alcohol_VB, cannabis_VB,Cocaina_VB), 
            ~ case_when(. == "No evaluado" ~ NA_character_ , TRUE ~ .)  ) %>% 
  mutate_at(vars(Tabaco_VB, Alcohol_VB, cannabis_VB,Cocaina_VB), as.factor ) %>% 
  # Arreglo Nivel_socioeconomico
  mutate(Nivel_socioeconomico = case_when(
    Nivel_socioeconomico %in% c("Medium","Medium-High","Medium-Low") ~ "Medium",
    Nivel_socioeconomico == "Unknown" ~ NA_character_ ,
    TRUE ~as.character(Nivel_urbanicidad)) %>% as.factor() ) %>%
  mutate_if(is.character,as.factor ) %>%
  select(
    PNS_DEFINITIVA,
    #Numericas
    # Identificadoras
    Edad_primer_episodio,
    # Escalas
    PANSS_negativos_VB, PANSS_total_VB, PAS_Total, FAST_Total_VB, FAST_Total_VB,MADRS_Total_VB,DUP,
    # Cognitiva
    Verbal_memor,Composite_score,Cognitive_reserve,Working_memory,
    # geneticas
    PRS_CP,PRS_Neuroticism,PRS_EA,PRS_OCD,PRS_SZ,
    #Categoricas
    Antecedentes_psiquiátricos, Cocaina_VB, Alcohol_VB, 
    Sexo, Nivel_socioeconomico,Nivel_urbanicidad)

skimr::skim( reincidencia_data_PNS)

# reincidencia_data_PNS %>% 
#   map_int(~sum(is.na(.)) )

# reincidencia_data_PNS %>%
#   gg_miss_var()
# 
# reincidencia_data_PNS %>% gg_miss_upset(. , nsets = 30, nintersects =10)


# //////////////////////////////////////////////////////
# //////////////////////////////////////////////////////
# //////////////////////////////////////////////////////

# Imputation

# //////////////////////////////////////////////////////
# //////////////////////////////////////////////////////
# //////////////////////////////////////////////////////

reincidencia_split <- initial_split( reincidencia_data_PNS )
reincidencia_train <- training( reincidencia_split ) 
reincidencia_test  <- testing( reincidencia_split )



# saveRDS(reincidencia_split, 'Scripts/PEP_modelos/Reincidencia_esquizofrenia/Data/split_data.rds' )
# saveRDS(reincidencia_train, 'Scripts/PEP_modelos/Reincidencia_esquizofrenia/Data/split_training.rds' )
# saveRDS(reincidencia_test,  'Scripts/PEP_modelos/Reincidencia_esquizofrenia/Data/split_test.rds' )

# //////////////////////////////////////////////////////
# 
# imputed_train_data <- mice(data = reincidencia_train, m = 3 , maxit = 20)
# saveRDS(imputed_train_data, 'Scripts/PEP_modelos/Reincidencia_esquizofrenia/Data/imputacion_training.rds')


imputated_train <- readRDS('Scripts/PEP_modelos/Reincidencia_esquizofrenia/Data/imputacion_training.rds' ) %>% 
  complete(., include= F,action= "long") %>% 
  as_tibble() %>% 
  select(- c(.id )) %>% 
  group_split(.imp)


test1 <- imputated_train[[1]] %>% 
  select(-.imp)
# //////////////////////////////////////////////////////
# //////////////////////////////////////////////////////
# //////////////////////////////////////////////////////

# Modeling

# //////////////////////////////////////////////////////
# //////////////////////////////////////////////////////
# //////////////////////////////////////////////////////

test1_1 <- test1 %>%  select(
  PNS_DEFINITIVA,
  #Numericas
  # Identificadoras
  Edad_primer_episodio,
  # Escalas
  PANSS_negativos_VB, PANSS_total_VB, PAS_Total, FAST_Total_VB, FAST_Total_VB,MADRS_Total_VB,DUP,
  # Cognitiva
  Verbal_memor,Composite_score,Cognitive_reserve,Working_memory,
  # geneticas
  PRS_CP,PRS_Neuroticism,PRS_EA,PRS_OCD,PRS_SZ,
  #Categoricas
  Antecedentes_psiquiátricos, Cocaina_VB, Alcohol_VB, Sexo, Nivel_socioeconomico
)

recipe_rf <- 
  #receta del modelo
  recipe(PNS_DEFINITIVA ~. , data = test1_1 ) %>% 
  # %>%
  # step_unknown(all_predictors(), new_level = "unknown")
  step_dummy(all_nominal(), -all_outcomes()) %>%
  # step_medianimpute(all_numerical() ) %>% 
  # step_normalize(all_numeric_predictors()) %>%
  # step_interact(terms = ~ smoker_yes:age) %>% 
  # step_log(all_outcomes()) %>% 
  # step_other(all_nominal(), threshold = 0.05)
  prep()

# Venos los resultados de las receta para estar seguros 🥰
juice(recipe_rf)
#  Funciona! 🚀🚀🚀


tune_spec_rf <- 
  # calibrado del modelo de random Forest
  rand_forest(
    trees = tune(), 
    #mtry = tune(),
    min_n = tune() ) %>%
  set_mode("classification") %>% 
  set_engine( "ranger", num.threads = 7, importance = "impurity")




tune_workflow <- workflow() %>%
  add_recipe(recipe_rf) %>%
  add_model(tune_spec_rf)

reincidencia_folds <- vfold_cv(test1,5)

random_forest_test_grid <- tune_grid(
  tune_workflow,
  resamples = reincidencia_folds,
  grid = 20
) 

random_forest_test_grid %>% collect_metrics()



test2 %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, trees) %>%
  pivot_longer(min_n:trees,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")



recipe_logReg <- 
  #receta del modelo
  recipe(PNS_DEFINITIVA ~ . , data = test1_1  ) %>% 
  step_dummy(all_nominal(), - all_outcomes()  ) %>% 
  step_corr(all_numeric() ,   threshold = 0.8 ) %>% 
  step_unknown(all_predictors())
  

# %>% 
#   update_role(Ident_caso , new_role = "ID") 
# %>%
# step_unknown(all_predictors(), new_level = "unknown")
# step_dummy(all_nominal(), -all_outcomes()) %>%
# step_medianimpute(all_numerical() ) %>% 
# step_normalize(all_numeric_predictors()) %>%
# step_interact(terms = ~ smoker_yes:age) %>% 
# step_log(all_outcomes()) %>% 
# step_other(all_nominal(), threshold = 0.05)

tune_spec_logisticReg <- 
  # calibrado del modelo de random Forest
  logistic_reg() %>%
  set_mode("classification") %>% 
  set_engine( "glm")

tune_workflow <- workflow() %>%
  add_recipe(recipe_rf) %>%
  add_model(tune_spec_logisticReg)

logistic_fit <-  fit(tune_workflow, test1_1) 


data_test <- readRDS('Scripts/PEP_modelos/Reincidencia_esquizofrenia/Data/split_test.rds' ) %>%  select(
  PNS_DEFINITIVA,
  #Numericas
  # Identificadoras
  Edad_primer_episodio,
  # Escalas
  PANSS_negativos_VB, PANSS_total_VB, PAS_Total, FAST_Total_VB, FAST_Total_VB,MADRS_Total_VB,DUP,
  # Cognitiva
  Verbal_memor,Composite_score,Cognitive_reserve,Working_memory,
  # geneticas
  PRS_CP,PRS_Neuroticism,PRS_EA,PRS_OCD,PRS_SZ,
  #Categoricas
  Antecedentes_psiquiátricos, Cocaina_VB, Alcohol_VB, Sexo, Nivel_socioeconomico
)






































