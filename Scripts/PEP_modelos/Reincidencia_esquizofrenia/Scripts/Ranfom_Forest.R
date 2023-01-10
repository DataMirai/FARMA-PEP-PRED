if(!require('pacman')){install.packages('pacman')}
pacman::p_load(
  # para cargar todas als librereias requeridas directamente 
  tidyverse, tidymodels, # Conjunto de librerías de progtamación de buena sintaxis y funcional
  parsnip, ranger, #Modelizacion con tidymodels, mice,
  naniar,mice,randomForest,glmnet,doParallel,DALEXtra,broom,yardstick,Metrics,
  readxl,writexl # importación y exportaciónd e datos)
)



particion_reincidencia <- initial_split(
  data = reincidencia_data_PNS, prop = 0.75)
train_c <- training(particion_reincidencia)
test_c <- testing(particion_reincidencia)

submuestras_kfold_c <- vfold_cv(data = train_c, v = 10, repeats = 1)

show_engines('rand_forest')

recipe_randomForest <-
  #receta del modelo
  recipe(PNS_DEFINITIVA ~ . , data = train_c) %>%
  step_impute_knn(all_nominal(), -all_outcomes()) %>%
  step_impute_bag(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(),-all_outcomes(), one_hot = T) %>%
  step_corr(all_numeric() ,   threshold = 0.9) %>%
  prep()

juice(recipe_logReg)

model_spec_randomForest_ranger <-
  # calibrado del modelo de random Forest
  rand_forest(trees = tune(),
              #mtry = tune(),
              min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger")
# %>% set_engine( "ranger", num.threads = 7, importance = "impurity")

workflow_randomForest <- workflow() %>%
  add_recipe(recipe_randomForest) %>%
  add_model(model_spec_randomForest_ranger)

grid_clasificacion <-
  grid_latin_hypercube(trees(), min_n(), size = 100)


registerDoParallel()
random_forest_grid <- tune_grid(
  workflow_randomForest,
  resamples = submuestras_kfold_c,
  grid = grid_clasificacion)
stopImplicitCluster()


random_forest_grid %>% collect_metrics()

random_forest_grid %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  ggplot(aes(x = trees, y = min_n , color = mean)) +
  geom_point(size = 3) +
  scale_x_log10() +
  scale_color_viridis_c()

mejor_clasificacion <-
  random_forest_grid %>% select_best(metric = "roc_auc")

final_wf_Randomforest <-  workflow_randomForest %>%
  finalize_workflow(mejor_clasificacion)

model_fit_RF <- final_wf_Randomforest %>%
  fit(train_c)

# Calculate evaluation metrics

reincidencia_results <- test_c %>% 
  select(PNS_DEFINITIVA) %>% 
  bind_cols(
    model_fit_RF %>% predict(new_data = test_c, type = 'prob'),
    model_fit_RF %>% predict(new_data = test_c, type = 'class'))


conf_mat(reincidencia_results,
         truth = PNS_DEFINITIVA,
         estimate = .pred_class)
metric_set( sens, spec, roc_auc)
conf_mat(
  data = reincidencia_results,
  truth = PNS_DEFINITIVA,
  estimate = .pred_class) %>%  summary()

conf_mat(
  data = reincidencia_results,
  truth = PNS_DEFINITIVA,
  estimate = .pred_class) %>%  autoplot(type = 'heatmap')

conf_mat(
  data = reincidencia_results,
  truth = PNS_DEFINITIVA,
  estimate = .pred_class) %>%  autoplot()















