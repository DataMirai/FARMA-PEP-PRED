# resultado fijados para la semilla 246

J_index_model_eval <- readRDS('Scripts/PEP_modelos/Reincidencia_esquizofrenia/Scripts/Models_testing/j_index_bayes.rds')

All_workflows <- readRDS('Scripts/PEP_modelos/Reincidencia_esquizofrenia/Scripts/Models_testing/All_workflows.rds')

PNS_split <- readRDS('Scripts/PEP_modelos/Reincidencia_esquizofrenia/Scripts/Models_testing/Split.rds')


All_workflows


J_index_model_eval %>% 
  tidy() %>% 
  mutate(model = fct_inorder(model)) %>% 
  separate(model, into = c("Recipe", "Model_Type"), sep = "_", remove = F, extra = "merge") %>% 
  ggplot(aes(x=posterior, fill = Model_Type)) +
  geom_density(alpha = 0.7, ) +
  theme_minimal() +
  scale_fill_manual(values = rainbow(5) )+
  facet_wrap( ~ Recipe, nrow = 10) +
  labs(
    title = "Comparison of Posterior Distributions of Model Recipe Combinations", 
    x = expression(paste("Posterior for Mean J Index")), 
    y = "")



class_metric <- metric_set(accuracy, f_meas, j_index, kap, precision, sensitivity, specificity, mcc)

best_result <- All_workflows %>% 
  extract_workflow_set_result('NEARMISS_Random_Forest') %>% 
  select_best(metric = 'j_index')

saveRDS(best_result,'Scripts/PEP_modelos/Reincidencia_esquizofrenia/Scripts/Models_testing/best_result.rds')

Model_fit <- All_workflows %>% 
  extract_workflow('NEARMISS_Random_Forest') %>% 
  finalize_workflow(best_result) %>%
  fit(training(PNS_split))


Model_fit %>% 
  predict(new_data = testing(PNS_split), type = 'prob') %>% 
  bind_cols(testing(PNS_split)) %>% 
  ggplot(aes(x=.pred_Si, fill = PNS_reincidencia, color = PNS_reincidencia)) +
  geom_histogram(bins = 40, alpha = 0.5) +
  theme_minimal() +
  scale_fill_viridis_d(aesthetics = c('color', 'fill'), end = 0.8) +
  labs(title = 'Distribution of Prediction Probabilities by PNS_reincidencia Status', x = 'Probability Prediction', y = 'Count')


Model_fit_pred <- Model_fit %>% 
  predict(new_data = testing(PNS_split), type = 'prob') %>% 
  bind_cols(testing(PNS_split)) %>% 
  select(PNS_reincidencia, .pred_No, .pred_Si)


threshold_data <- Model_fit_pred %>% 
  threshold_perf(
    truth = PNS_reincidencia, estimate = .pred_Si, 
    thresholds = seq(0, 1, by = 0.01))


max_j_index <- threshold_data %>% 
  filter(.metric == 'j_index') %>% 
  filter(.estimate == max(.estimate)) %>% 
  select(.threshold) %>% 
  as_vector()


threshold_data %>% 
  filter(.metric != 'distance') %>% 
  ggplot(aes(x=.threshold, y=.estimate, color = .metric)) +
  geom_line(size = 2) +
  geom_vline(xintercept = max_j_index, lty = 5, alpha = .6) +
  theme_minimal() +
  scale_colour_viridis_d(end = 0.8) +
  labs(x='Threshold', 
       y='Estimate', 
       title = 'Balancing Performance by Varying Threshold',
       subtitle = 'Verticle Line = Max J-Index',
       color = 'Metric')


threshold_data %>% 
  filter(.metric %in% c('sens', 'spec')) %>% 
  pivot_wider(id_cols = .threshold, values_from = .estimate, names_from = .metric) %>%
  mutate(
    Vigilancia_FN = ((1-sens) * 0.6), 
    Vigilancia_FP = ((1-spec) * 0.4),
    Total_Vigilancia = Vigilancia_FN + Vigilancia_FP) %>% 
  select(.threshold, Vigilancia_FN, Vigilancia_FP, Total_Vigilancia) %>% 
  pivot_longer(2:4, names_to = 'Vigilancia_Function', values_to = 'Vigilancia') %>% 
  ggplot(aes(x = .threshold, y = Vigilancia, color = Vigilancia_Function)) +
  geom_line(size = 1.5) +
  theme_minimal() +
  scale_colour_viridis_d(end = 0.8) +
  labs(title = 'Threshold Vigilancia Function', x = 'Threshold')



threshold_data %>% 
  filter(.metric %in% c('sens', 'spec')) %>% 
  pivot_wider(id_cols = .threshold, values_from = .estimate, names_from = .metric) %>% 
  mutate(
    Vigilancia = ((1-sens) * 0.6) + ((1-spec) * 0.4),
    j_index = (sens+spec)-1) %>% 
  ggplot(aes( y = Vigilancia, x = .threshold)) +
  geom_line() +
  geom_point(aes(color = j_index)) +
  scale_x_continuous(breaks = seq(0,1,0.1))+
  geom_vline(xintercept = 0.59, lty = 2) +
  geom_vline(xintercept = 0.87, lty = 2) +
  theme_minimal() +
  scale_colour_viridis_c() +
  labs(
    title = 'Decision Threshold Attrition Cost Function', 
    x = 'Classification Threshold', 
    size = 'J-Index', 
    color = 'J-Index')



crossing(
  'data'= threshold_data,
  'thresholds' = seq(0.8,0.9,0.03))


pmap(
  crossing(
    'data'= list(Model_fit_pred),
    'thresholds' = seq(0.5,0.85,0.03)),
  ~ ..1 %>% 
    mutate(.pred = make_two_class_pred(.pred_No, levels(PNS_reincidencia), threshold = ..2)) %>%
    conf_mat(estimate = .pred, PNS_reincidencia) %>% 
    autoplot(type = 'heatmap') + 
    scale_fill_gradient2() +
    labs(title = paste('Default Decision Threshold = ',..2))
)


Model_fit$fit$fit$fit$variable.importance %>% 
  enframe(name = "predictor", value = "importancia") %>% 
  # Gráfico
  ggplot(
    data = .,
    aes(x    = reorder(predictor, importancia),
        y    = importancia,
        fill = importancia)
  ) +
  labs(x = "predictor", title = "Importancia predictores",
       subtitle='(método basado en impureza, acorde a los datos de entrenamiento)') +
  geom_col() +
  scale_fill_viridis_c() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")




Model_fit %>% extract_fit_parsnip() %>% vip()

tm_reg_pred <- function(x, y) {
  predict(x, new_data = y) %>% pull(.pred)
}


explain(
  Model_fit,
  feature_names = c('PAS_Total'),
  X = training(PNS_split))


chi_expl <-
  explain(
    Model_fit,
    data = training(PNS_split) %>% select(-PNS_reincidencia),
    X    = training(PNS_split),
    y    = training(PNS_split) %>% select( PNS_reincidencia),
    predict_function = tm_reg_pred
  )

?explain
