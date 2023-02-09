
# //////////////////////////////////////////////////////////////////////
# - Libraries ---- 
# //////////////////////////////////////////////////////////////////////


if(!require('pacman')){install.packages('pacman')}
pacman::p_load(
  # para cargar todas als librereias requeridas directamente 
  tidyverse, tidymodels, # Conjunto de librerías de progtamación de buena sintaxis y funcional
  parsnip, #Modelizacion con tidymodels, mice,
  keras,tensorflow,
  readxl # importación y exportaciónd e datos)
)


# //////////////////////////////////////////////////////////////////////
# - Data splits ---- 
# //////////////////////////////////////////////////////////////////////

# Specify the number of splits in the previous argument N_splits_desirable

N_splits_desirable <- 2

PNS_ML_NN_tibble <- tibble(
  'data_split'= rerun(
    .n = N_splits_desirable, 
    initial_split(data = reincidencia_data_PNS, prop = 0.80))) %>% 
  # We create an ID for each data split, to allow tracking in diagnostics process.
  mutate('id'= paste0('Fold',1:length(data_split))) %>% 
  select(id, data_split) %>% 
  mutate(
    data_training = map(data_split, ~ training(.x)),
    data_testing  = map(data_split, ~ testing(.x)))

PNS_ML_NN_tibble

# //////////////////////////////////////////////////////////////////////
# - Recipe and Formulas specification ---- 
# //////////////////////////////////////////////////////////////////////

PNS_ML_NN_tibble <- PNS_ML_NN_tibble %>%
  mutate(
    # Creacion de la receta
    'recipe_1' = map(
      data_training,
      ~ recipe(PNS_DEFINITIVA ~ . , data = .x) %>%
        step_impute_knn(all_nominal(),-all_outcomes()) %>%
        step_impute_bag(all_numeric(),-all_outcomes()) %>%
        step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>%
        step_corr(all_numeric() ,   threshold = 0.9) %>%
        prep(.))
    # ,
    # 'recipe_2' = map(
    #   data_training,
    #   ~ recipe(PNS_DEFINITIVA ~ . , data = .x) %>%
    #     step_impute_knn(all_nominal(),-all_outcomes()) %>%
    #     step_impute_bag(all_numeric(),-all_outcomes()) %>%
    #     step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>%
    #     step_corr(all_numeric() ,   threshold = 0.9) %>%
    #     prep(.))
  )

PNS_ML_NN_tibble

# //////////////////////////////////////////////////////////////////////
# - Model definition ---- 
# //////////////////////////////////////////////////////////////////////


model_spec_Logreg <-
  # calibrado del modelo de regresion logistica 
  mlp() %>%
  set_mode("classification") %>% 
  set_engine("keras", verbose = 0) 

PNS_ML_NN_tibble <- PNS_ML_NN_tibble %>% 
  mutate(
    'model_spec_1'= list(model_spec_Logreg)
    # ,'model_spec_2'= list(model_spec_randomForest_ranger)
  )

# //////////////////////////////////////////////////////////////////////
# - Models fit ---- 
# //////////////////////////////////////////////////////////////////////

PNS_models_NN <- pmap(
  as.list(PNS_ML_NN_tibble),
  ~ workflow() %>%
    add_recipe(..5) %>%
    add_model(..6) %>% 
    last_fit(..2) ) %>%  
  set_names( PNS_ML_NN_tibble$id)

PNS_models_NN

# //////////////////////////////////////////////////////////////////////
# - Models evalutations ---- 
# //////////////////////////////////////////////////////////////////////


PNS_models_Logreg %>% 
  map(~ pull(.x,.predictions) %>% flatten_df() ) %>% 
  map(~
        conf_mat(
          data= .x,
          truth = PNS_DEFINITIVA,
          estimate = .pred_class) %>%  summary() )


PNS_models_Logreg %>% 
  map(~ pull(.x,.metrics) %>% flatten_df() ) %>%  
  bind_rows() %>% 
  filter(.metric=='roc_auc' ) 



PNS_models_Logreg %>% 
  map(~ pull(.x,.predictions) %>% flatten_df() ) %>% 
  map(~conf_mat(data= .x,
                truth = PNS_DEFINITIVA,
                estimate = .pred_class))



PNS_models_Logreg %>% 
  map(~ pull(.x,.predictions) %>% flatten_df() ) %>% 
  map(~
        conf_mat(
          data= .x,
          truth = PNS_DEFINITIVA,
          estimate = .pred_class) %>%   autoplot(type = 'heatmap') )




PNS_models_Logreg %>% 
  map(~ pull(.x,.predictions) %>% flatten_df() ) %>% 
  map(~
        conf_mat(
          data= .x,
          truth = PNS_DEFINITIVA,
          estimate = .pred_class) %>%  summary()) %>% 
  map(~ .x %>% 
        filter(.metric %in% c('accuracy','sens','spec')) %>%
        select( .metric, .estimate )) %>%  bind_rows() %>% 
  bind_rows(
    PNS_models_Logreg %>% 
      map(~ pull(.x,.metrics) %>% flatten_df() ) %>%
      bind_rows() %>% 
      select(.metric, .estimate) %>% 
      filter(.metric=='roc_auc') ) %>% 
  ggplot(.,aes(.metric ,.estimate, fill= .metric) ) +
  ggdist::stat_halfeye()


PNS_models_Logreg %>% 
  map(~ pull(.x,.predictions) %>% flatten_df() ) %>% 
  map(~
        conf_mat(
          data= .x,
          truth = PNS_DEFINITIVA,
          estimate = .pred_class) %>%  summary() ) %>% 
  map(~ .x %>%  select(-.estimator ) ) %>% 
  map_dfr(~.x) %>% 
  group_by(.metric) %>% 
  nest()  %>% 
  ungroup() %>% 
  mutate(
    'min' = map_dbl( map(data, ".estimate"), ~ min(.x, na.rm=T)),
    'avg' = map_dbl( map(data, ".estimate"), ~ mean(.x, na.rm=T)),
    'max' = map_dbl( map(data, ".estimate"), ~ max(.x, na.rm=T)),
    'sd'  = map_dbl( map(data, ".estimate"), ~ sd(.x, na.rm=T)))





















