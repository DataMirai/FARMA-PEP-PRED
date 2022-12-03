
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

experimento <-  cruce_categoricas[1:2,]

cruce_categoricas <- cruce_categoricas$tally_resumen %>% 
  set_names(cruce_categoricas$explicatorias_categoricas)

# Creamos una lista con los dataframes actualizados para incluir una categoría total, el total y no 
# crearlo a través de un geom_stat 

cruce_categoricas_all <- map(
  cruce_categoricas,
  ~ ..1 %>% 
    group_by(.[[2]] ) %>% 
    summarize(n= sum(n)) %>% 
    set_names(c(names(..1)[2], 'n')) %>% 
    mutate(PNS_DEFINITIVA= as.factor('All')) %>% 
    bind_rows(..1) %>% 
    select(PNS_DEFINITIVA, everything(),n ) %>% 
    mutate(PNS_DEFINITIVA = case_when(
      PNS_DEFINITIVA == 0 ~'No',
      PNS_DEFINITIVA == 1 ~'Si',
      TRUE ~ 'All')))   

Nombres_cruce_categoricas <- cruce_categoricas_all %>% 
  map(names)


map2(
  cruce_categoricas_all, Nombres_cruce_categoricas,
  ~ ..1 %>% 
    ggplot(aes_string(x= ..2[2], y=..2[3],fill = ..2[2])) +
    geom_col(position = 'dodge', alpha=0.8) +
    facet_wrap(
      as.formula(paste('~',.y[1])),
      scales="free_x")+ 
    geom_text(aes_string(label= ..2[3]),nudge_y=7, color='white',hjust=0.5) +
    labs(
      title = paste('Reincidencia Esquizofrenia Según la escala PNS'),
      subtitle= paste('En función de: ' ,..2[2],'(Gráficos de columnas)'),
      caption = 'Submuestra de la PEP para la estimación de la reincidencia, no hay pacientes control') + 
    scale_y_continuous(n.breaks = 10)+
    scale_fill_viridis_d()+
    dark_mode(theme_solarized()) +
    theme(
      plot.title    = element_text(size= 14, face = "bold", hjust=0.10,vjust = 1,color='white'),
      plot.subtitle = element_text(size= 10, hjust= 0.08,color='white'),
      plot.caption  = element_text(size=7,hjust= 0.95),
      axis.title.x  = element_blank(),
      axis.text.x   = element_blank(),
      axis.title.y  = element_text(size= 15,color='white'),
      axis.text.y   = element_text(size= 12),
      axis.ticks.x  = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(color='white',size=10),
      legend.text = element_text(color='white',size=10),
      strip.text = element_text(face='bold', size=12 ),
      strip.background = element_rect( color="black", fill="slateblue4", size=2.5, linetype="solid"),
      panel.grid    = element_line(color = "#8ccde3",size = 1.1,linetype = 2),
      axis.line.y   = element_line(arrow = arrow(), size=0.75, color='#7A8B8B')
      )
)



experimento <- crossing( 
    data_frame = list(reincidencia_data),
    respuesta, explicatorias_categoricas )

experimento <-  experimento[1:2,]



pmap(
  experimento %>%  as.list() ,
  ~ ..1 %>% 
    ggplot(aes_string( ..3 ) , group= ..2) +
    geom_bar(aes_string(fill = ..3),position = 'dodge', alpha=0.8) +
    
    geom_text(
      aes(label=after_stat( paste0(round(..prop..*100,1),'%') ), group=1),
      stat='count',nudge_y=5, size=3) +
    
    geom_text(
      aes(label= paste0('n= ',..count.. )),
      stat='count',nudge_y=15, size=3) +
    facet_grid(
      as.formula(paste('~',..2)),
      scales= 'free_x', margins = T,
      labeller = as_labeller(c('No', 'Si', 'Total') %>% set_names('0','1','(all)') ) )  +
    labs(
      title = paste('Reincidencia Esquizofrenia Según la escala PNS'),
      subtitle= paste('En función de: ' ,..3,'(Gráficos de columnas)'),
      caption = paste(..2, ' ~ ',..3 )) + 
    scale_y_continuous(n.breaks = 10)+
    scale_fill_viridis_d()+
    dark_mode(theme_solarized()) +
    theme(
      plot.title    = element_text(size= 14, face = "bold", hjust=0.10,vjust = 1,color='white'),
      plot.subtitle = element_text(size= 10, hjust= 0.08,color='white'),
      plot.caption  = element_text(size=7,hjust= 0.95),
      axis.title.x  = element_blank(),
      axis.text.x   = element_blank(),
      axis.title.y  = element_text(size= 15,color='white'),
      axis.text.y   = element_text(size= 12),
      axis.ticks.x  = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(color='white',size=10),
      legend.text = element_text(color='white',size=10),
      strip.text = element_text(face='bold', size=12 ),
      strip.background = element_rect( color="black", fill="slateblue4", size=2.5, linetype="solid"),
      panel.grid    = element_line(color = "#8ccde3",size = 1.1,linetype = 2),
      axis.line.y   = element_line(arrow = arrow(), size=0.75, color='#7A8B8B')
    )
)




