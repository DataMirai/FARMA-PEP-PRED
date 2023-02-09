
# //////////////////////////////////////////////////////////////////////
# - Libraries ---- 
# //////////////////////////////////////////////////////////////////////


if(!require('pacman')){install.packages('pacman')}
pacman::p_load(
  # para cargar todas als librereias requeridas directamente 
  tidyverse, tidymodels, # Conjunto de librerías de progtamación de buena sintaxis y funcional
  parsnip, ranger, #Modelizacion con tidymodels, mice,
  glmnet,probably,rsample, modeldata,
  readxl # importación y exportaciónd e datos)
)



# //////////////////////////////////////////////////////////////////////
# - Data splits ---- 
# //////////////////////////////////////////////////////////////////////

# Specify the number of splits in the previous argument N_splits_desirable

N_splits_desirable <- 10

PNS_ML_LR_tibble <- tibble(
  'data_split'= rerun(
    .n = N_splits_desirable, 
    initial_split(data = reincidencia_data_PNS, prop = 0.80))) %>% 
  # We create an ID for each data split, to allow tracking in diagnostics process.
  mutate('id'= paste0('Fold',1:length(data_split))) %>% 
  select(id, data_split) %>% 
  mutate(
    data_training = map(data_split, ~ training(.x)),
    data_testing  = map(data_split, ~ testing(.x)))

PNS_ML_LR_tibble

# //////////////////////////////////////////////////////////////////////
# - Recipe and Formulas specification ---- 
# //////////////////////////////////////////////////////////////////////

PNS_ML_LR_tibble <- PNS_ML_LR_tibble %>%
  mutate(
    # Creacion de la receta
    'recipe_1' = map(
      data_training,
      ~ recipe(PNS_DEFINITIVA ~ . , data = .x) %>%
        step_impute_knn(all_nominal(),-all_outcomes()) %>%
        step_impute_bag(all_numeric(),-all_outcomes()) %>%
        step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>%
        step_corr(all_numeric() ,   threshold = 0.9) %>%
        step_normalize(all_numeric()) %>%  # z-standardize all numeric variables
        step_zv(all_numeric_predictors()) %>%                     # Remove numeric variables with zero variance
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



PNS_ML_LR_tibble

# //////////////////////////////////////////////////////////////////////
# - Model definition ---- 
# //////////////////////////////////////////////////////////////////////

# calibrado del modelo de regresion logistica 

model_spec_Logreg <- logistic_reg(penalty = 0.01, mixture = 0.85 ) %>% 
  set_mode("classification") %>%
  set_engine("glmnet")



PNS_ML_LR_tibble <- PNS_ML_LR_tibble %>% 
  mutate('model_spec_1'= list(model_spec_Logreg))
    # ,'model_spec_2'= list(model_spec_randomForest_ranger)
  
# //////////////////////////////////////////////////////////////////////
# - Models fit ---- 
# //////////////////////////////////////////////////////////////////////

PNS_models_Logreg <- pmap(
  as.list(PNS_ML_LR_tibble),
  ~ workflow() %>%
    add_recipe(..5) %>%
    add_model(..6) %>% 
    last_fit(., split= ..2) ) %>%  
  set_names( PNS_ML_LR_tibble$id)

PNS_models_Logreg

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




best_accuracy_model <- PNS_models_Logreg %>% 
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
  filter(.metric=='accuracy' ) %>% 
  unnest(-.metric ) %>% 
  with(which(.estimate == max(.estimate) ))

best_accuracy_model

# //////////////////////////////////////////////////////////////////////
# - Models evalutations ---- 
# //////////////////////////////////////////////////////////////////////

model_PNS <- PNS_models_Logreg[[best_accuracy_model]] %>% flatten()

post_model_data <- reincidencia_data_PNS %>% 
  slice(model_PNS$.predictions$.row) %>% 
  mutate(model_case_id = model_PNS$.predictions$.row  )

PNS_test_predicted <- inner_join(
  model_PNS$.predictions,
  post_model_data,
  by= c('.row'= 'model_case_id' )
)

PNS_pred_0.5 <-  PNS_test_predicted %>%
  mutate(
    .pred = make_two_class_pred(
      estimate = .pred_1, 
      levels = levels(PNS_DEFINITIVA.x), 
      threshold = .5)) %>%
  select(PNS_DEFINITIVA.x, contains(".pred"))

PNS_pred_0.5 %>% 
  count(.truth = PNS_DEFINITIVA.x, .pred)

PNS_pred_0.75 <-  PNS_test_predicted %>%
  mutate(
    .pred = make_two_class_pred(
      estimate = .pred_1, 
      levels = levels(PNS_DEFINITIVA.x), 
      threshold = .65)) %>%
  select(PNS_DEFINITIVA.x, contains(".pred"))

PNS_pred_0.75 %>% 
  count(.truth = PNS_DEFINITIVA.x, .pred)


threshold_data <- PNS_test_predicted %>%
  threshold_perf(PNS_DEFINITIVA.x, .pred_1, thresholds = seq(0.5, 0.85, by = 0.0025)) 


threshold_data <- threshold_data %>%
  filter(.metric != "distance") %>%
  mutate(group = case_when(
    .metric == "sens" | .metric == "spec" ~ "1",
    TRUE ~ "2"
  ))

max_j_index_threshold <- threshold_data %>%
  filter(.metric == "j_index") %>%
  filter(.estimate == max(.estimate)) %>%
  pull(.threshold)

ggplot(threshold_data, aes(x = .threshold, y = .estimate, color = .metric, alpha = group)) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis_d(end = 0.9) +
  scale_alpha_manual(values = c(.4, 1), guide = "none") +
  geom_vline(xintercept = max_j_index_threshold, alpha = .6, color = "grey30") +
  labs(
    x = "'Good' Threshold\n(above this value is considered 'good')",
    y = "Metric Estimate",
    title = "Balancing performance by varying the threshold",
    subtitle = "Sensitivity or specificity alone might not be enough!\nVertical line = Max J-Index"
  )

