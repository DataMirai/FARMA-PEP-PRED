df %>%
  group_by(dv) %>%
  nest() %>%
  mutate(aov = map(data, ~aov(value ~ content * process, data = .x)))




df %>% 
ggplot(aes(y)) + 
  geom_histogram(alpha=0.2) + 
  facet_grid(~x,) + 
  coord_flip() +
  theme_minimal() + 
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    strip.text = element_blank())




########################################################################
########################################################################

########################################################################
########################################################################

########################################################################
########################################################################




respuesta <- 
  ###
  reincidencia_data %>%
  select(PNS_DEFINITIVA ) %>%
  names()

explicatorias_numericas <- 
  ###
  reincidencia_data %>%
  select(-c(PNS_DEFINITIVA)) %>%
  select_if(is.numeric) %>%
  names() %>% 
  set_names(.)

cruce_numericas <-
  ###
  crossing( data_frame=list(reincidencia_data),respuesta, explicatorias_numericas ) %>%
  mutate(resumen = pmap(
    list(
      data_frame,
      respuesta,
      explicatorias_numericas),
    ~ ..1 %>%
      select(any_of(c(..2,..3))) %>%
      group_by(.dots= ..2) %>%
      summarise(across(everything(), .f = list(n=~n(),mean = mean, sd = sd),na.rm=T)) %>% 
      set_names('PNS_DEFINITIVA','V_n','V_mean','V_sd') %>% 
      mutate(PNS_DEFINITIVA=  case_when(
        PNS_DEFINITIVA=='0'~'No',
        PNS_DEFINITIVA=='1'~'Si')) %>% 
      mutate_if(is.numeric,~round(.,2) )
    )) 
      


experimento <- cruce_numericas[1:2,]

experimento


pmap(
  experimento %>%  as.list(),
  ~ ..1 %>%
    ggplot(aes_string(..3)) +
    scale_x_continuous(n.breaks = 8) +
    coord_flip() +
    facet_grid(
      as.formula(paste('~', ..2)),
      scales = 'free_y',
      labeller = as_labeller(c('No', 'Si', 'Total') %>% set_names('0', '1', '(all)'))
    ) +
    
    stat_dotsinterval(
      position = position_dodge(width = 15),
      scale      = 0.75,
      quantiles  = 100,
      side       = 'left',
      slab_color = 'black',
      slab_fill  = 'seashell2'
    ) +
    
    stat_halfeye(
      position = "dodge",
      point_interval = median_qi,
      aes(fill = after_stat(cut_cdf_qi(
        cdf, .width = c(0.66, 0.95, 1)
      ))),
      height = 0.75,
      slab_alpha = 0.7,
      interval_size_range = c(1.25, 2.5),
      interval_colour = "darkgoldenrod2",
      point_alpha = 1,
      point_colour = "black",
      shape = 18,
      fatten_point = 1
    ) +
    
    geom_table_npc(
      data = tibble(
        x = rep(0.5, 2),
        y = rep(0.95, 2),
        PNS_DEFINITIVA = c("0", "1"),
        tb = list(
          ..4[1, 2:4] %>% mutate_all(~ round(., 2)) %>% set_names(c('N', 'Media', 'sd')),
          ..4[2, 2:4] %>% mutate_all(~ round(., 2)) %>% set_names(c('N', 'Media', 'sd'))
        )
      ),
      aes(npcx = x, npcy = y, label = tb),
      hjust = 1.125,
      vjust = 1,
      table.theme = ttheme_gtdark(base_size = 18)
    ) +
    
    labs(
      title = paste('Reincidencia Esquizofrenia en función de ' , ..3),
      subtitle = 'Boxplot, densidad y resumenes por grupos',
      caption = 'Submuestra de la PEP para la estimación de la reincidencia, no hay pacientes control',
      fill = 'Reincidencia\nEsquizofrenia'
    ) +
    # Colores y configuración del gráfico
    scale_fill_manual(values = c('orangered4', 'bisque2', 'slateblue4')) +
    
    dark_mode(theme_dark()) +
    
    theme(
      panel.background = element_rect(fill = "linen"),
      panel.grid.major = element_line(
        color = 'black',
        size = 0.5,
        linetype = 2
      ),
      panel.grid.minor = element_line(
        color = 'black',
        size = 0.5,
        linetype = 3
      ),
      plot.title = element_text(
        size = 25,
        face = "bold",
        hjust = 0.10,
        vjust = 1
      ),
      plot.subtitle = element_text(size = 18, hjust = 0.05),
      axis.title.x  = element_blank(),
      axis.text.x   = element_blank(),
      axis.text.y   = element_text(size = 16),
      axis.title.y    = element_text(size = 20),
      plot.caption  = element_text(hjust = 0.85),
      strip.text = element_text(color = 'white', face = 'bold', size = 23),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
      strip.background = element_rect(
        color = "black",
        fill = "burlywood4",
        size = 2.5,
        linetype = "solid"
      ),
      legend.position = "none",
    )
) 






