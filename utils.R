suppressPackageStartupMessages(library(gganimate))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(plotly))

data_rm <- function() {
    
    n <- list.files(path = here::here('data-raw')) 
    d <- tibble::tibble(file = n, grupo = substring(n, 1, 4)) %>% split(., .$grupo)
    f <- d[sapply(d, nrow) > 3]
    if(length(f) > 0){
        out <- unname(unlist(sapply(f, '[', 1, 1)))
        setwd(here::here("data-raw"))
        file.remove(out)
        cat('\n\nSe eliminaron los siguientes archivos:', out, '\n\n')
    } else {
        cat('\n\nNo hay archivos para eliminar!\n\n')
    }
    
}

uy_a_merge <- function(x) {
    
    x[x == 'N/A'] <- 0L
    uno <- 
        tibble::tibble(
            country   = "Uruguay", 
            iso3c     = "URY",
            date      = as.Date(lubridate::parse_date_time(x$fecha, order = "dmy")), 
            confirmed = as.double(x$acumTestPositivos), 
            deaths    = as.double(x$acumFallecidos), 
            recovered = as.double(x$acumRecuperados),
            timestamp = Sys.time()
        )
    dos <- 
        tibble::tibble(
            country   = "Uruguay", 
            iso3c     = "URY",
            date      = seq(as.Date('2020-01-22'), as.Date('2020-03-12 '), by = "1 days"), 
            confirmed = 0, 
            deaths    = 0, 
            recovered = 0,
            timestamp = Sys.time()
        )
    tres <- 
        as_tibble(tidycovid19::download_merged_data(cache = TRUE))%>% 
        filter(country == 'Uruguay') %>%
        select(-country, -iso3c, -confirmed, -deaths, -recovered, -timestamp)
    dplyr::bind_rows(dos, uno) %>% left_join(. ,tres, by='date')
    
}



busca_data <- function(pattern = character()){
    dat<- list.files(path = here::here('data-raw'), pattern = pattern)
    dat <- tibble::as_tibble(rio::import(here::here('data-raw', dat[length(data)])))
    return(dat)
}



animation_google <- function(data, x, y, file = 'g1', height = 800, width = 600, duration = 20){
    
    data <- na.omit(data)
    threshold_x <- as.Date(max(data$date))
    g <- 
        ggplot(data = data, aes(x = as.Date({{x}}), y = {{y}}, color = country)) +
        geom_line(aes(group = country), size = 1, alpha = 0.6) +
        geom_segment(aes(xend = threshold_x + 2, yend = {{y}}, group = country),
                     linetype = 2, colour = 'grey') +
        geom_text(aes(x = threshold_x + 2, label = iso3c), hjust = 0) +
        geom_hline(yintercept = 0, alpha = .6) +
        guides(color = FALSE) +
        xlim(as.Date('2020-02-14'),  threshold_x + 3) +
        ylim(-1, 1) +
        theme_minimal() +
        labs(x = "", y = "", title = "") +
        theme(axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              plot.caption = element_text(size = 10, color = "grey40"), 
              strip.text = element_text(size = 12), 
              #strip.background = element_rect(color = "", fill = "", linetype = "solid")
        ) +
        facet_wrap(~region2, ncol = 1) +
        transition_reveal(date) +
        coord_cartesian(clip = 'on') +
        enter_fade() +
        exit_shrink()
    
    animate(g, end_pause = 25, height = height, width = width, duration = duration)
    #animate(p, renderer = gifski::gifski_renderer(loop = FALSE))
    anim_save(here::here('animations', paste0(file, '.gif')))
}



library(ggtext)
umbral <- 1000000

prev <- 
    merge %>% 
    select(date, continent, new_tests, new_cases, new_deaths, population) %>% 
    group_by(continent, date) %>% 
    summarize(
        poblacion    = sum(population, na.rm = TRUE), 
        total_tests  = sum(new_tests,  na.rm = TRUE), 
        total_cases  = sum(new_cases,  na.rm = TRUE),
        total_deaths = sum(new_deaths, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    group_by(continent) %>% 
    mutate(
        t_tests  = cumsum(total_tests), 
        t_cases  = cumsum(total_cases),
        t_deaths = cumsum(total_deaths)
    ) %>% 
    ungroup() %>% 
    select(continent, date, poblacion, starts_with('t_')) %>% 
    mutate(
        'Test p/millón'            = round((t_tests/poblacion)  * umbral), 
        'Casos positivos p/millón' = round((t_cases/poblacion)  * umbral),
        'Muertes p/millón'         = round((t_deaths/poblacion) * umbral)
    ) %>% 
    ungroup() %>% 
    select(continent, date,'Test p/millón', 'Casos positivos p/millón', 'Muertes p/millón' ) %>% 
    pivot_longer(cols = 3:5, names_to = 'Indicador', values_to = 'Cantidad') %>% 
    rename(Fecha = date)

colores <- c("Test p/millón"            = "#5277F8", 
             "Casos positivos p/millón" = "#30C253", 
             "Muertes p/millón"         = "#F5615F")


