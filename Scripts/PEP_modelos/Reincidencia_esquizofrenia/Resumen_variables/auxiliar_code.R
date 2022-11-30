

reincidencia_data

respuesta <- reincidencia_data %>%
  select(PNS_DEFINITIVA ) %>%
  names()

explicatorias_numericas <- reincidencia_data %>%
  select(-c(PNS_DEFINITIVA)) %>%
  select_if(is.numeric) %>%
  names() %>% 
  set_names(.)



cruce_numericas <- crossing(data_frame=list(reincidencia_data),respuesta, explicatorias_numericas ) %>%
  mutate(resumen = pmap(
    list(
      data_frame,
      respuesta,
      explicatorias_numericas),
    ~ ..1 %>%
      select(any_of(c(..2,..3))) %>%
      group_by_(.dots= ..2) %>%
      summarise(across(everything(), .f = list(n=~n(),mean = mean, sd = sd),na.rm=T)) %>% 
      set_names('PNS_DEFINITIVA','V_n','V_mean','V_sd') ))

cruce_numericas <- cruce_numericas[1:2,] 

cruce_numericas$resumen  

##########################
# lista_ggplots_numericas <- 
pmap(
  cruce_numericas %>% 
    as.list() ,
  ~ ..1 %>%
    ggplot(., aes_string(x = ..2, y = ..3 )) + 
    stat_halfeye( aes_string(fill = ..2), alpha=0.5,  width= 0.65)+
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
      plot.title    = element_text(size= 20, face = "bold", hjust=0.10,vjust = 1),
      plot.subtitle = element_text(size= 10, hjust= 0.10,vjust = 1 ),
      axis.text.x   = element_text(size= 15),
      axis.text.y   = element_text(size= 15),
      axis.title    = element_text(size= 12),
      plot.caption  = element_text(hjust= 0.85),
      legend.position = "none"))  %>% 
  set_names(cruce_numericas$explicatorias_numericas)




lista_ggplots_numericas
