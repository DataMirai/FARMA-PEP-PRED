
# /////////////////////////////////////////////
## Total de NA presentes por variable  ----
# /////////////////////////////////////////////

NA_variables <- tibble( 
  'Nombre_variable' = names(PEP),
  'Recuento_NA' = map_dbl(PEP, ~sum(is.na(.x))),
  'NA_ratio' = map_dbl(PEP, ~sum(is.na(.x))/n )) %>% 
  arrange(desc(NA_ratio))

quantile(NA_variables$NA_ratio, seq(0,1,0.1) )

NA_variables %>%
  filter(NA_ratio == 1)

NA_variables %>%
  filter(NA_ratio > 0.9)

NA_variables %>%
  filter(NA_ratio > 0.8)

# NA_variables %>% 
#   ggplot(aes(NA_ratio)) +
#   geom_density()

# /////////////////////////////////////////////
## Total de NA presentes por Individuo  ----
# /////////////////////////////////////////////
### Necesario para hacer un seguimiento de los casos que mas dropean el estudio por ejemplo

NA_ind <- data.frame(
  'id'= 1:n,
  'NA_count'  = unlist(apply(PEP, MARGIN = 1, function(x) sum(is.na(x)))),
  'NA_percent'= unlist(apply(PEP, MARGIN = 1, function(x) sum(is.na(x))))/p)

quantile(NA_ind$NA_count, seq(0,1,0.1) )


# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////
## Sumario por categorias del diccioanrio de variables ----
# /////////////////////////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////////////////


Sumario_variables_diccionario <- diccionario_variables %>%
  map(
    ~ list(
      'resumen_variables'= PEP %>%
        select(any_of(.x)) %>%
        map(~summary(.)),
      'resumen_NA' = PEP %>%
        select(any_of(.x)) %>%
        imap_dfr(
          ~ data.frame(
            'variable'= .y,
            'NA_n'= sum(is.na(.x)))) %>%
        mutate(
          'NA_perct_variable'= NA_n / n )
    )
  )


Sumario_variables_diccionario$identificadores$resumen_variables








