## twitter


source('R/utils.R')
library(tidyverse)



### google =====================================================================

data_hoy <- list.files(path = here::here('data-raw'), pattern = "*.xlsx")
covid_m <- tibble::as_tibble(
    rio::import(here::here('data-raw', data_hoy[length(data_hoy)]))
)

covid_m %>% 
    filter(country == 'Uruguay') %>% 
    select(date, starts_with('gcmr')) %>% 
    rename(
        'Lugares de trabajo' = gcmr_workplaces,
        'Parques'= gcmr_parks, 
        'Venta minorista y recreación' = gcmr_retail_recreation, 
        'Tienda de comestibles y farmacia' = gcmr_grocery_pharmacy, 
        'Estaciones de tránsito' = gcmr_transit_stations, 
        'Zona Residencial' = gcmr_residential
    ) %>% 
    pivot_longer(cols = 2:7, names_to = 'indicador', values_to = 'valor') %>% 
    na.omit() %>% 
    ggplot(aes(x = date, y = valor)) +
    geom_rect(aes(xmin = as.POSIXct("2020-04-04"), xmax = as.POSIXct("2020-04-13"), 
                  ymin = -Inf, ymax = Inf), color = "#E5E5E5", fill = "#E5E5E5", alpha = 0.8) +
    geom_line(aes(color = indicador), size = 1.2) +
    geom_hline(yintercept = 0, color = 'black', linetype = 3) +
    guides(color = FALSE) +
    ggthemes::theme_fivethirtyeight() + ggthemes::scale_colour_wsj("colors6")+
    facet_wrap(~indicador) +
    labs(x = "", y = "", color = "", 
         title = "Movilidad en seis rubros según datos de Google para Uruguay",
         subtitle = "Estas series comienzan el 15 de febrero de 2020. \nEn sombreado gris se identifica a la semana de turismo.\n", 
         caption = "\nUnidad de Métodos y Acceso a Datos (UMAD)\nFacultad de Ciencias Sociales - Universidad de la República\nTwitter: @umad_fcs")


ggsave("dif_tw/google.png", units = "in", width = 11, height = 8, dpi = 300)

## etapas educacion ============================================================

source('data-raw/observatorio_educacion.R')

etapa1 %>% 
    mutate(ord= 1:length(rownames(etapa1)),
           Categoria = fct_reorder(Ref, ord, min)) %>%
    ggplot(aes(fill = Categoria, y = Cantidad, x = Etapa)) + 
    geom_bar(position = "dodge", stat = "identity", color = 'black') +
    scale_fill_manual(values = c("#C93312", "#0B775E")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    labs(x = "", y = "", color = "", fill = "") +
    ggthemes::theme_fivethirtyeight() + ggthemes::scale_colour_wsj("colors6") +
    theme(legend.position = 'top', 
          legend.justification = 'left',
          legend.direction = 'horizontal', 
          legend.key.size = unit(0.5, "cm"), 
          legend.text = element_text(color = "#3F5151", size = 7)) +
    geom_text(aes(label = Cantidad), vjust = rep(c(-1, 1.6, 1.6, 1.6), 2), , color = c(rep(c("black", rep("white", 3)), 2)) , 
              position = position_dodge(0.9), size = 3.5) +
    labs(x = "", y = "", color = "", 
         title = "Estimación de cantidad de estudiantes por etapa de reapertura",
         subtitle = "Para la definición de la etapas se tomó la planificación inicial del gobierno, realizando dos ajustes de acuerdo a lo definido \nposteriormente: se excluye en su totalidad el departamento de Rivera (ya que las clases han sido suspendidas por tiempo \nindefinido) y se modifica la reapertura de las escuelas especiales (inicialmente previstas para la etapa 1 y luego incluidas en \nla etapa 2). Asimismo, se incorporó a la información una 'fase 0', correspondiente al retorno de las escuelas rurales que se \nrealizó a partir del 22 de abril.\n", 
         caption = "\nFuente: ECH-INE y Anuario Estadístico MEC.\nUnidad de Métodos y Acceso a Datos (UMAD)\nFacultad de Ciencias Sociales - Universidad de la República\nTwitter: @umad_fcs")


ggsave("dif_tw/etapas1.png", units = "in", width = 11, height = 8, dpi = 300)





etapa2 %>%
    ggplot(aes(x = Etapa, y = Cantidad, )) + 
    geom_line(aes(group = Porcentaje, color = Porcentaje), size = 1.2) +
    geom_point(aes(color = Porcentaje), size = 2) +
    scale_color_manual(values = c("#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    theme_minimal() +
    labs(x = "", y = "", color = "") +
    ylim(0, 1000000) +
    ggthemes::theme_fivethirtyeight() + ggthemes::scale_colour_wsj("colors6") +
    ggrepel::geom_text_repel(aes(label = Cantidad), vjust = -1, 
              position = position_dodge(0.9), size = 3.5) +
    theme(legend.position = 'top', 
          legend.justification = 'left',
          legend.direction = 'horizontal', 
          #legend.key.size = unit(0.5, "cm"), 
          legend.text = element_text(color = "#3F5151", size = 10)) +
    labs(x = "", y = "", color = "", 
         title = "Proyección de cantidad de estudiantes por etapa de reapetura \ncon porcentajes de asistencia voluntaria del 30%, 50%, 75% y 100%",
         subtitle = "", 
         caption = "\nFuente: ECH-INE y Anuario Estadístico MEC.\nUnidad de Métodos y Acceso a Datos (UMAD)\nFacultad de Ciencias Sociales - Universidad de la República\nTwitter: @umad_fcs")



ggsave("dif_tw/etapas2.png", units = "in", width = 11, height = 8, dpi = 300)









