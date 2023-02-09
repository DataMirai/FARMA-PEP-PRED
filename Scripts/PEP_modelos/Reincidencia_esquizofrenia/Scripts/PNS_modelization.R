if(!require('pacman')){install.packages('pacman')}
pacman::p_load(
  # para cargar todas als librereias requeridas directamente 
  tidyverse, tidymodels, # Conjunto de librerías de progtamación de buena sintaxis y funcional
  themis, tidyposterior,baguette,corrr,readr,magrittr,forcats,skimr,patchwork,GGally,
  doParallel,DALEXtra,broom,xgboost,patchwork,probably,vip,
  readxl, # importación y exportaciónd e datos)
  fastshap,gt,kernelshap, shapviz,withr
)

options(yardstick.event_first = FALSE)


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
  
  
skimr::skim( reincidencia_data_PNS)


viz_by_dtype <- function (x,y) {
  title <- str_replace_all(y,"_"," ") %>% 
    str_to_title()
  if ("factor" %in% class(x)) {
    ggplot(reincidencia_data_PNS, aes(x, fill = x)) +
      geom_bar() +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 8)) +
      scale_fill_viridis_d()+
      labs(title = title, y = "", x = "")
  }
  else if ("numeric" %in% class(x)) {
    ggplot(reincidencia_data_PNS, aes(x)) +
      geom_histogram()  +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_viridis_d() +
      labs(title = title, y = "", x = "")
  } 
  else if ("integer" %in% class(x)) {
    ggplot(reincidencia_data_PNS, aes(x)) +
      geom_histogram() +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_viridis_d()+
      labs(title = title, y = "", x = "")
  }
  else if ("character" %in% class(x)) {
    ggplot(reincidencia_data_PNS, aes(x, fill = x)) +
      geom_bar() +
      theme_minimal() +
      scale_fill_viridis_d() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 8)) +
      labs(title = title, y  ="", x= "")
  }
}



variable_list <- colnames(reincidencia_data_PNS) %>% as.list()
variable_plot <- map2(reincidencia_data_PNS, variable_list, viz_by_dtype) %>%
  wrap_plots(ncol = 8, heights = 150, widths = 150)

PNS_describe_numerics <- ggpairs(
  reincidencia_data_PNS %>% 
    select(PNS_reincidencia, where(is.numeric)),
  ggplot2::aes(color = PNS_reincidencia, alpha = 0.3)) + 
  scale_fill_viridis_d(end = 0.8, aesthetics = c('color', 'fill')) + 
  theme_minimal() +
  labs(title = 'Analisis bivariado de PNS_reincidencia')


 
# reincidencia_data_PNS %>% 
#   select(PNS_reincidencia, where(is.factor)) %>% 
#   pivot_longer(2:6, names_to = "Variables", values_to = "Values") %>% 
#   group_by(Variables, Values) %>% 
#   summarise(mean = mean(PNS_reincidencia,na.rm() ),
#             conf_int = 1.96*sd(PNS_reincidencia)/sqrt(n())) %>% 
#   ggplot(aes(x=Values, y=mean, color=Values)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = mean - conf_int, ymax = mean + conf_int), width = 0.1) +
#   theme_minimal() +
#   theme(legend.position = "none",
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_color_viridis_d(aesthetics = c("color", "fill"), end = 0.8) +
#   facet_wrap(~Variables, scales = 'free') +
#   labs(title = 'Categorical Variable Analysis', subtitle = 'With 95% Confidence Intervals')



# //////////////////////////////////////////////////////////////////////
# - Data splits ---- 
# //////////////////////////////////////////////////////////////////////

set.seed(246)
reincidencia_split <- initial_split(
  reincidencia_data_PNS, 
  prop = 0.75, 
  strata = PNS_reincidencia)

saveRDS(reincidencia_split, 'Scripts/PEP_modelos/Reincidencia_esquizofrenia/Scripts/Models_testing/Split.rds' )

# //////////////////////////////////////////////////////////////////////
# - Models definition ---- 
# //////////////////////////////////////////////////////////////////////

Logistic_PNS_model <- 
  logistic_reg(penalty= tune(), mixture=tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

DecisionTree_PNS_model <- 
  decision_tree( cost_complexity = tune(), tree_depth = tune(), min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")

RandomForest_PNS_model <- 
  rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

xgboost_PNS_model <- 
  boost_tree(
    mtry = tune(), trees = tune(), min_n = tune(), 
    tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
    sample_size = tune())  %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

bagged_PNS_model <- 
  bag_tree(cost_complexity = tune(), tree_depth = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")



# //////////////////////////////////////////////////////////////////////
# - Recipe and Formulas specification ---- 
# //////////////////////////////////////////////////////////////////////

recipe_smote <- # SMOTE — Synthetic Minority Oversampling Technique
  recipe(PNS_reincidencia ~. , data = training(reincidencia_split)) %>% 
  step_impute_knn(all_nominal(),-all_outcomes()) %>%
  step_impute_bag(all_numeric(),-all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>% 
  step_scale(all_numeric_predictors(), -c(Edad_primer_episodio)) %>% 
  step_smote(PNS_reincidencia)

recipe_rose <- # Random Oversampling Technique
  recipe(PNS_reincidencia ~. , data = training(reincidencia_split)) %>% 
  step_impute_knn(all_nominal(),-all_outcomes()) %>%
  step_impute_bag(all_numeric(),-all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>% 
  step_scale(all_numeric_predictors(), -c(Edad_primer_episodio)) %>% 
  step_rose(PNS_reincidencia)

recipe_bsmote <- # Borderline Synthetic Minority Oversampling Technique
  recipe(PNS_reincidencia ~. , data = training(reincidencia_split)) %>% 
  step_impute_knn(all_nominal(),-all_outcomes()) %>%
  step_impute_bag(all_numeric(),-all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>% 
  step_scale(all_numeric_predictors(), -c(Edad_primer_episodio)) %>% 
  step_bsmote(PNS_reincidencia)

recipe_upsample <- # Add duplicate minority class data to specified ratio with majority class
  recipe(PNS_reincidencia ~. , data = training(reincidencia_split)) %>% 
  step_impute_knn(all_nominal(),-all_outcomes()) %>%
  step_impute_bag(all_numeric(),-all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>% 
  step_scale(all_numeric_predictors(), -c(Edad_primer_episodio)) %>% 
  step_upsample(PNS_reincidencia)

recipe_adasyn <- # Adaptive Synthetic Oversampling
  recipe(PNS_reincidencia ~. , data = training(reincidencia_split)) %>% 
  step_impute_knn(all_nominal(),-all_outcomes()) %>%
  step_impute_bag(all_numeric(),-all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>% 
  step_scale(all_numeric_predictors(), -c(Edad_primer_episodio)) %>% 
  step_adasyn(PNS_reincidencia)

recipe_tomek <- # Remove TOMEK links in Majority Class
  recipe(PNS_reincidencia ~. , data = training(reincidencia_split)) %>% 
  step_impute_knn(all_nominal(),-all_outcomes()) %>%
  step_impute_bag(all_numeric(),-all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>% 
  step_scale(all_numeric_predictors(), -c(Edad_primer_episodio)) %>% 
  step_tomek(PNS_reincidencia)

recipe_nearmiss <- # Remove Majority Class Instances by Undersampling
  recipe(PNS_reincidencia ~. , data = training(reincidencia_split)) %>% 
  step_impute_knn(all_nominal(),-all_outcomes()) %>%
  step_impute_bag(all_numeric(),-all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>% 
  step_scale(all_numeric_predictors(), -c(Edad_primer_episodio)) %>% 
  step_nearmiss(PNS_reincidencia)

recipe_nosampling <- # No sampling procedure
  recipe(PNS_reincidencia ~. , data = training(reincidencia_split)) %>% 
  step_impute_knn(all_nominal(),-all_outcomes()) %>%
  step_impute_bag(all_numeric(),-all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>% 
  step_scale(all_numeric_predictors(), -c(Edad_primer_episodio))




reincidencia_train <- recipe_adasyn %>% prep() %>% bake(new_data = NULL)
reincidencia_test <- recipe_adasyn  %>% prep() %>% bake(testing(reincidencia_split))

reincidencia_train %>% 
  bind_rows(reincidencia_test) %>% 
  mutate(PNS_reincidencia = as.numeric(PNS_reincidencia)) %>% 
  correlate() %>%
  rplot(print_cor = T, .order = "alphabet") +
  scale_color_gradient2(low = 'orange', high = 'light blue') + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Correlation Plot for Trained Dataset")



# //////////////////////////////////////////////////////////////////////
# -Workflow stablishment ---- 
# //////////////////////////////////////////////////////////////////////

model_list <- list(
  Logistic_regression = Logistic_PNS_model,
  Decision_Tree = DecisionTree_PNS_model, 
  Boosted_Trees = xgboost_PNS_model, 
  Random_Forest = RandomForest_PNS_model, 
  Bagged_Trees = bagged_PNS_model)

saveRDS(model_list,'Scripts/PEP_modelos/Reincidencia_esquizofrenia/Scripts/Models_testing/models_list.rds'  )

recipe_list <- list(
  SMOTE = recipe_smote, 
  ROSE = recipe_rose, 
  BSMOTE = recipe_bsmote, 
  UPSAMPLE = recipe_upsample, 
  ADASYN = recipe_adasyn, 
  TOMEK=recipe_tomek, 
  NEARMISS = recipe_nearmiss, 
  NOSAMPLING = recipe_nosampling)

saveRDS(model_list,'Scripts/PEP_modelos/Reincidencia_esquizofrenia/Scripts/Models_testing/Recipe_list'  )


wf_set <- workflow_set(
  preproc = recipe_list, 
  models = model_list, 
  cross = T)


set.seed(246)
train_resamples <-  vfold_cv(training(reincidencia_split), v = 5, strata = PNS_reincidencia)

class_metric <- metric_set(
  accuracy, f_meas, j_index, 
  kap, precision, 
  sensitivity, specificity, 
  roc_auc, mcc, pr_auc)



doParallel::registerDoParallel(cores = 8)
wf_sample_exp <- 
  wf_set %>% 
  workflow_map(
    resamples = train_resamples, 
    verbose = TRUE, 
    metrics = class_metric, 
    seed = 246)


wf_sample_exp

saveRDS(wf_sample_exp, 'Scripts/PEP_modelos/Reincidencia_esquizofrenia/Scripts/Models_testing/All_workflows.rds' )

# //////////////////////////////////////////////////////////////////////
# - Models evalutations ---- 
# //////////////////////////////////////////////////////////////////////

collect_metrics(wf_sample_exp) %>% 
  separate(wflow_id, into = c("Recipe", "Model_Type"), sep = "_", remove = F, extra = "merge") %>% 
  select(-.config) %>% 
  distinct() %>%
  group_by(.metric, wflow_id) %>% 
  filter(mean == max(mean)) %>% 
  group_by(.metric) %>% 
  mutate(
    Workflow_Rank =  row_number(-mean),
    .metric = str_to_upper(.metric)) %>% 
  arrange(Workflow_Rank) %>% 
  ggplot(aes(x=Workflow_Rank, y = mean, color = Model_Type)) +
  geom_point(aes(shape = Recipe)) +
  scale_shape_manual(values = 1:n_distinct(recipe_list)) +
  geom_errorbar(aes(ymin = mean-std_err, ymax = mean+std_err)) +
  theme_minimal() +
  scale_color_viridis_d() +
  labs(
    title = "Performance Comparison of Workflows", 
    x = "Workflow Rank", 
    y = "Error Metric", 
    color = "Model Types", 
    shape = "Recipes") +
  facet_wrap(~.metric,scales = 'free_y',ncol = 4)

# Given class imbalance, ROC AUC and Accuracy are not appropriate metrics!🚀 
# We consider Precision-Recall AUC, KAP, J-Index, Mathews Correlation Coefficient and Specificity.


# Focusing on the J-index, 
# We can compare the resampling posterior distributions using tidyposterior. 

# tidyposterior::perf_mod() takes the wf_sample_exp object that contains results from workflow_map,
# completes bayesian comparision of resamples on it and
# generates posterior distributions of the metric mean of interest. 
# N.B the workflow_set object MUST have the target metric calculated else this will not work! ☠


jindex_model_eval <- perf_mod(
  wf_sample_exp, 
  metric = "j_index", 
  iter = 5000)

saveRDS(jindex_model_eval,'Scripts/PEP_modelos/Reincidencia_esquizofrenia/Scripts/Models_testing/j_index_bayes.rds' )


jindex_model_eval %>% 
  tidy() %>% 
  mutate(model = fct_inorder(model)) %>% 
  separate(model, into = c("Recipe", "Model_Type"), sep = "_", remove = F, extra = "merge") %>% 
  ggplot(aes(x=posterior, fill = Model_Type)) +
  geom_density(aes(alpha = 0.7)) +
  theme_minimal() +
  scale_fill_viridis_d(end = 0.8) +
  facet_wrap(~Recipe, nrow = 10) +
  labs(title = "Comparison of Posterior Distributions of Model Recipe Combinations", 
       x = expression(paste("Posterior for Mean J Index")), 
       y = "")



class_metric <- metric_set(accuracy, f_meas, j_index, kap, precision, sensitivity, specificity, mcc)

best_result <- wf_sample_exp %>% 
  extract_workflow_set_result('ROSE_Random_Forest') %>% 
  select_best(metric = 'j_index')














