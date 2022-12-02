
respuesta <- 
  ###
  reincidencia_data %>%
  select(PNS_DEFINITIVA ) %>%
  names()

explicatorias_categoricas <- 
  ###
  reincidencia_data %>%
  select(-c(PNS_DEFINITIVA)) %>%
  select_if(is.factor) %>%
  names() %>% 
  set_names(.)

cruce_categoricas <-
###
crossing( 
  data_frame=list(reincidencia_data),
  respuesta, explicatorias_categoricas )  %>% 
  mutate(tally_resumen= pmap(
    list(data_frame,respuesta,explicatorias_categoricas) ,
    ~ ..1 %>%
      select(any_of(c(..2,..3))) %>% 
      group_by(.dots= c(..2,..3)) %>% 
      tally() %>% 
      ungroup()  ))

cruce_categoricas <- cruce_categoricas$tally_resumen %>% 
  set_names(cruce_categoricas$explicatorias_categoricas)

cruce_categoricas_all <- map(
  cruce_categoricas,
  ~ ..1 %>% 
    group_by(.[[2]] ) %>% 
    summarize(n= sum(n)) %>% 
    set_names(c(names(..1)[2], 'n')) %>% 
    mutate(PNS_DEFINITIVA= 'All') %>% 
    bind_rows(..1) %>% 
    select(PNS_DEFINITIVA, everything(),n ))   


cruce_categoricas_all <- cruce_categoricas_all[1:2]
Nombres_cruce_categoricas <-  Nombres_cruce_categoricas[1:2]

# Nombres_cruce_categoricas <- Nombres_cruce_categoricas[1:2]
# cruce_categoricas <- cruce_categoricas[1:2]

map2(
  cruce_categoricas_all, Nombres_cruce_categoricas,
    ~ .x %>% 
      ggplot(aes_string(x = .y[1], y= .y[3] ,fill = .y[2] )) +
      geom_col( position = 'dodge') +
      facet_wrap(as.formula(paste('~',.y[1])),scales="free_x" ) 
      
)


as.factor(variable, levels = c("nombre 1", "nombre  2"))

pmap(
  cruce_categoricas %>% 
    as.list() ,
  ~ ..1 %>%
    ggplot(., aes_string(x = ..2, y = ..3 )) + 
    stat_halfeye(aes_string(fill = ..2), alpha=0.5,  width= 0.75)+
    geom_boxplot(aes_string(fill = ..2), alpha=0.35, width= 0.15 ) +
    scale_x_discrete(..2,labels = c("0" = "NO","1" = "SI","NA" = "NA")) +
    scale_y_continuous(n.breaks = 10) +
    stat_summary(fun=mean, colour="white", geom="point", shape=18, size=3, show.legend=F) + 
    geom_text(
      data= ..4,aes(
        x= PNS_DEFINITIVA, y= ..1 %>% select(any_of(..3)) %>% max(., na.rm=T ) * 1.15 ,
        label= paste0('n= ',V_n,'\nsd= ',round(V_sd,2),'\nmean= ', round(V_mean,2))) , 
      vjust=1, size=3, color="white", fontface="bold") +
    labs(
      title = paste('Reincidencia Esquizofrenia en función de ' ,..3),
      subtitle= 'Boxplot, densidad y resumenes por grupos',
      x = 'Reincidencia de la esquizofrenia',
      caption = 'Submuestra de la PEP para la estimación de la reincidencia, no hay pacientes control',
      fill= 'Reincidencia\nEsquizofrenia') + 
    dark_mode(theme_solarized()) +
    theme(
      panel.grid = element_line(color = "#8ccde3",size = 1.5,linetype = 2),
      plot.title    = element_text(size= 25, face = "bold", hjust=0.10,vjust = 1),
      plot.subtitle = element_text(size= 18, hjust= 0.05),
      axis.text.x   = element_text(size= 20),
      axis.text.y   = element_text(size= 20),
      axis.title    = element_text(size= 15),
      plot.caption  = element_text(hjust= 0.85),
      legend.position = "none"))  %>% 
  set_names(cruce_numericas$explicatorias_numericas)



