
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

# Specify the number of splits in the previous argument N_splits_desirable

N_splits_desirable <- 20

reincidencia_PNS_ML_RF_tibble <- tibble(
  'data_split'= rerun(
    .n = N_splits_desirable, 
    initial_split(data = reincidencia_data_PNS, prop = 0.80))) %>% 
  # We create an ID for each data split, to allow tracking in diagnostics process.
  mutate('id'= paste0('Fold',1:length(data_split))) %>% 
  select(id, data_split) %>% 
  mutate(
    data_training = map(data_split, ~ training(.x)),
    data_testing  = map(data_split, ~ testing(.x)))

reincidencia_PNS_ML_RF_tibble

# //////////////////////////////////////////////////////////////////////
# - Recipe and Formulas specification ---- 
# //////////////////////////////////////////////////////////////////////

reincidencia_PNS_ML_RF_tibble <-
  reincidencia_PNS_ML_RF_tibble %>%
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

reincidencia_PNS_ML_RF_tibble
# //////////////////////////////////////////////////////////////////////
# - Model definition ---- 
# //////////////////////////////////////////////////////////////////////


model_spec_randomForest_ranger <-
  # calibrado del modelo de random Forest
  rand_forest(trees = tune(),
              #mtry = tune(),
              min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger")


reincidencia_PNS_ML_RF_tibble <- 
  reincidencia_PNS_ML_RF_tibble %>% 
  mutate(
    'model_spec_1'= list(model_spec_randomForest_ranger)
    # ,'model_spec_2'= list(model_spec_randomForest_ranger)
    )


# //////////////////////////////////////////////////////////////////////
# - hyper parameter tunning ---- 
# //////////////////////////////////////////////////////////////////////

tune_best_list <- pmap(
  reincidencia_PNS_ML_RF_tibble %>% 
    select(recipe_1 , model_spec_1,data_training) %>% 
    as.list(),
  ~ workflow() %>%
    add_recipe(..1) %>%
    add_model(..2) %>%
    tune_grid(
      .,
      resamples =  vfold_cv(data = ..3, v = 5, repeats = 1),
      grid = grid_latin_hypercube(trees(), min_n(), size = 10))
)

tune_best_list

reincidencia_PNS_ML_RF_tibble <- 
  reincidencia_PNS_ML_RF_tibble %>% 
  mutate(
    'best_tune'  =  map(tune_best_list, ~ .x %>% select_best(metric = "roc_auc")),
    'tune_trees' =  map_dbl(tune_best_list , ~ collect_metrics(.x) %>% pull(trees) %>% mean()),
    'tune_min_n' =  map_dbl(tune_best_list , ~ collect_metrics(.x) %>% pull(min_n) %>% mean())
  )

reincidencia_PNS_ML_RF_tibble

# //////////////////////////////////////////////////////////////////////
# - Models fit ---- 
# //////////////////////////////////////////////////////////////////////

PNS_models_RF <- pmap(
  as.list(reincidencia_PNS_ML_RF_tibble),
  ~ workflow() %>%
    add_recipe(..5) %>%
    add_model(..6) %>% 
    finalize_workflow(..7) %>% 
    last_fit(..2) ) %>%  set_names( reincidencia_PNS_ML_RF_tibble$id)

PNS_models_RF

# //////////////////////////////////////////////////////////////////////
# - Models evalutations ---- 
# //////////////////////////////////////////////////////////////////////


PNS_models_RF[[1]]$.predictions[[1]] %>% 
  filter( .pred_class != PNS_DEFINITIVA)

PNS_models_RF %>% 
  map(~ pull(.x,.predictions) %>% flatten_df() ) %>% 
  map(~
        conf_mat(
          data= .x,
          truth = PNS_DEFINITIVA,
          estimate = .pred_class) %>%  summary() )


PNS_models_RF %>% 
  map(~ pull(.x,.metrics) %>% flatten_df() ) %>%  
  bind_rows() %>% 
  filter(.metric=='roc_auc' ) 



PNS_models_RF %>% 
  map(~ pull(.x,.predictions) %>% flatten_df() ) %>% 
  map(~conf_mat(data= .x,
                truth = PNS_DEFINITIVA,
                estimate = .pred_class))



PNS_models_RF %>% 
  map(~ pull(.x,.predictions) %>% flatten_df() ) %>% 
  map(~
        conf_mat(
          data= .x,
          truth = PNS_DEFINITIVA,
          estimate = .pred_class) %>%   autoplot(type = 'heatmap') )




PNS_models_RF %>% 
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
    PNS_models_RF %>% 
      map(~ pull(.x,.metrics) %>% flatten_df() ) %>%
      bind_rows() %>% 
      select(.metric, .estimate) %>% 
      filter(.metric=='roc_auc') ) %>% 
  ggplot(.,aes(.metric ,.estimate, fill= .metric) ) +
  ggdist::stat_halfeye()
  # stat_halfeye(
  #   position = "dodge",
  #   point_interval = median_qi,
  #   aes(fill = after_stat(cut_cdf_qi(
  #     cdf, .width = c(0.66, 0.95, 1)
  #   ))),
  #   height = 0.75,
  #   slab_alpha = 0.7,
  #   interval_size = 3,
  #   interval_size_range = c(3, 5.5),
  #   interval_colour = "darkgoldenrod2",
  #   point_alpha = 1,
  #   point_colour = "black",
  #   shape = 18,
  #   fatten_point = 1
  # ) +






PNS_models_RF %>% 
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






