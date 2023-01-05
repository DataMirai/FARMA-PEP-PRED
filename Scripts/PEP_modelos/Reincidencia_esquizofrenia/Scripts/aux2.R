
particion_inicial_reincidencia <- initial_split(data = reincidencia_data_PNS, prop = 0.75)
train_c <- training(particion_inicial_reincidencia)
test_c <- testing(particion_inicial_reincidencia)


submuestras_kfold_c <- vfold_cv(data = train_c, v = 10, repeats = 1)

recipe_logReg <- 
  #receta del modelo
  recipe(PNS_DEFINITIVA ~ . , data = train_c  ) %>% 
  step_impute_knn(all_nominal(),-all_outcomes()) %>% 
  step_impute_bag(all_numeric(),-all_outcomes()) %>% 
  step_dummy(all_nominal(), - all_outcomes(), one_hot = T  ) %>% 
  step_corr(all_numeric() ,   threshold = 0.9 ) %>% 
  prep()

# Vemos que se ha aplicado bien la receta y que no hay datos faltanes 
juice(recipe_logReg)

model_spec_logisticReg <- 
  # calibrado del modelo de random Forest
  logistic_reg(mixture = tune(), penalty = tune()) %>%
  set_mode("classification") %>%
  set_engine("glmnet")




grid_clasificacion <- grid_latin_hypercube(penalty(), mixture(), size = 20)

workflow_logReg <- workflow() %>%
  add_recipe(recipe_logReg) %>%
  add_model(model_spec_logisticReg)

registerDoParallel()
tune_param_logReg <- tune_grid(
  workflow_logReg,
  resamples = submuestras_kfold_c,
  grid = grid_clasificacion)
stopImplicitCluster()


tune_param_logReg %>% 
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>% 
  ggplot(aes(x = penalty, y = mixture, color = mean)) +
  geom_point(size = 3) +
  scale_x_log10() +
  scale_color_viridis_c()

mejor_clasificacion <- tune_param_logReg %>% select_best(metric = "roc_auc")
mejor_clasificacion[1,1] <- 0.08

final_wf_logReg <-  workflow_logReg %>% finalize_workflow(mejor_clasificacion)

model_fit <- final_wf_logReg %>% 
  fit(train_c) 

model_fit$fit$fit$fit$beta
model_fit$fit$fit$spec
model_fit$fit$fit$fit$dev.ratio
model_fit$fit$fit$fit$lambda
model_fit$fit$fit$fit$nobs
model_fit$fit$fit$fit

model_fit %>%
  extract_fit_parsnip() %>%
  tidy()









# ////////////////////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////////////////////
# Other grids
# ////////////////////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////////////////////

grid_clasificacion <- expand_grid(
  'penalty'= seq(0.1, 0.25, 0.01), 
  'mixture'= seq(0.6,0.75,0.01 ))

workflow_logReg <- workflow() %>%
  add_recipe(recipe_logReg) %>%
  add_model(model_spec_logisticReg)

registerDoParallel()

tune_param_logReg <- tune_grid(
  workflow_logReg,
  resamples = submuestras_kfold_c,
  grid = grid_clasificacion)

stopImplicitCluster()


tune_param_logReg %>% 
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>% 
  ggplot(aes(x = penalty, y = mixture, color = mean)) +
  geom_point(size = 3) +
  scale_x_log10() +
  scale_color_viridis_c()

mejor_clasificacion <- tune_param_logReg %>% select_best(metric = "roc_auc")
mejor_clasificacion[1,1] <- 0.01 

final_wf_logReg <-  workflow_logReg %>% finalize_workflow(mejor_clasificacion)

# ajuste_final_clasificacion <- last_fit(
#   final_wf_logReg,
#   particion_inicial_reincidencia)


model_fit <- final_wf_logReg %>% 
  fit(train_c) 


model_fit$fit %>%  coef()
  
  

?extract_fit_engine
  
# ////////////////////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////////////////////
# Other grids
# ////////////////////////////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////////////////////////////
  

recipe_logReg <- 
  #receta del modelo
  recipe(PNS_DEFINITIVA ~ . , data = train_c  ) %>% 
  step_impute_knn(all_nominal(),-all_outcomes()) %>% 
  step_impute_bag(all_numeric(),-all_outcomes()) %>% 
  step_dummy(all_nominal(), - all_outcomes(), one_hot = T  ) %>% 
  step_corr(all_numeric() ,   threshold = 0.9 ) %>% 
  prep()

# Vemos que se ha aplicado bien la receta y que no hay datos faltanes 
juice(recipe_logReg)

model_spec_logisticReg <- 
  # calibrado del modelo de random Forest
  logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")

workflow_logReg <- workflow() %>%
  add_recipe(recipe_logReg) %>%
  add_model(model_spec_logisticReg)


model_fit <- workflow_logReg %>% 
  last_fit ( particion_inicial_reincidencia)  

model_fit


collect_metrics(model_fit)





















