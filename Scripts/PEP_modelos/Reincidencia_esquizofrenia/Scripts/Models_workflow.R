
# //////////////////////////////////////////////////////////////////////
# - Libraries ---- 
# //////////////////////////////////////////////////////////////////////


if(!require('pacman')){install.packages('pacman')}
pacman::p_load(
  # para cargar todas als librereias requeridas directamente 
  tidyverse, tidymodels, # Conjunto de librerías de progtamación de buena sintaxis y funcional
  parsnip, ranger, #Modelizacion con tidymodels, mice,
  naniar,randomForest,glmnet,doParallel,DALEXtra,broom,
  readxl # importación y exportaciónd e datos)
)


# //////////////////////////////////////////////////////////////////////
# - Data splits ---- 
# //////////////////////////////////////////////////////////////////////

reincidencia_PNS_ML_RF_tibble <- tibble(
  'data_split'= rerun(
    .n = 10, 
    initial_split(data = reincidencia_data_PNS, prop = 0.75))) %>% 
  mutate('id'= paste0('Fold',1:length(data_split))) %>% 
  select(id, data_split ) %>% 
  mutate(
    data_training = map(data_split, ~ training(.x)),
    data_testing  = map(data_split, ~ testing(.x)))

reincidencia_PNS_ML_RF_tibble

# //////////////////////////////////////////////////////////////////////
# - Model definition ---- 
# //////////////////////////////////////////////////////////////////////

reincidencia_PNS_ML_RF_tibble <-  
  reincidencia_PNS_ML_RF_tibble %>% 
  mutate(
    # Creacion de la receta
    'recipe' = map(data_training, 
      ~ recipe(PNS_DEFINITIVA ~ . , data = .x ) %>%
        step_impute_knn(all_nominal(), -all_outcomes()) %>%
        step_impute_bag(all_numeric(), -all_outcomes()) %>%
        step_dummy(all_nominal(),-all_outcomes(), one_hot = T) %>%
        step_corr(all_numeric() ,   threshold = 0.9) %>%
        prep(.) ))

model_spec_randomForest_ranger <-
  # calibrado del modelo de random Forest
  rand_forest(trees = tune(),
              #mtry = tune(),
              min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger")
  

reincidencia_PNS_ML_RF_tibble <- 
  reincidencia_PNS_ML_RF_tibble %>% 
  mutate('model_spec'= list(model_spec_randomForest_ranger))

# //////////////////////////////////////////////////////////////////////
# - hyper parameter tunning ---- 
# //////////////////////////////////////////////////////////////////////
tune_best_list <- pmap(
  list(
    reincidencia_PNS_ML_RF_tibble$data_training , 
    reincidencia_PNS_ML_RF_tibble$recipe, 
    reincidencia_PNS_ML_RF_tibble$model_spec),
  ~ workflow() %>%
    add_recipe(..2) %>%-
    add_model(..3) %>% 
    tune_grid(
      .,
      resamples =  vfold_cv(data = ..1, v = 10, repeats = 1),
      grid = grid_latin_hypercube(trees(), min_n(), size = 10))
  )




reincidencia_PNS_ML_RF_tibble <- 
  reincidencia_PNS_ML_RF_tibble %>% 
  mutate(
    'best_tune'  =  map(tune_best_list, ~ .x %>% select_best(metric = "roc_auc")),
    'tune_trees' =  map_dbl(tune_best_list , ~ collect_metrics(.x) %>% pull(trees) %>% mean()),
    'tune_min_n' =  map_dbl(tune_best_list , ~ collect_metrics(.x) %>% pull(min_n) %>% mean())
)


# //////////////////////////////////////////////////////////////////////
# - Model definition ---- 
# //////////////////////////////////////////////////////////////////////














# //////////////////////////////////////////////////////////////////////
# - Model definition ---- 
# //////////////////////////////////////////////////////////////////////































































map(
  as.list(seq(0.4,0.85,0.05)), 
  ~ initial_split(data = reincidencia_data_PNS, prop = .x) )