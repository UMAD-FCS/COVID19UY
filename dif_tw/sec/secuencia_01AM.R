

## esta rutina se ejecuta a las 01:00hs de cada dia.
## controles de bajada de datos 

source('R/utils.R')

status <- list()
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BLOCK 1 --> URUGUAY
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pre_data_uy <- busca_data('^A_guiad')
url         <- "https://raw.githubusercontent.com/GUIAD-COVID/datos-y-visualizaciones-GUIAD/master/datos/estadisticasUY.csv"
data_uy     <- read.csv(url, stringsAsFactors = FALSE)

if(is.data.frame(data_uy)){
    vars_data_uy <- c('fecha', 'acumTestPositivos',  'acumFallecidos', 'acumRecuperados')
    if(sum(vars_data_uy %in% names(data_uy)) == length(vars_data_uy)) {
        base_guiad <- TRUE
            if(nrow(data_uy) > nrow(pre_data_uy)) {
                base_guiad <- c(TRUE, base_guiad)
            }
    }
    if(sum(base_guiad) == 2){
        rio::export(data_uy, here::here('data-raw', paste0("A_guiad_", Sys.Date(), '.csv')))
    } else {
        status$error1 <- 'ERROR_1 --> La base de GUIAD cambio los nombres de las variables o la dimension es mas chica'
    }
} else {
        status$error_01 <- "ERROR_01 --> La base de GUIAD no es un data.frame"
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BLOCK 2 --> MUNDO
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pre_merge  <- busca_data('*xlsx')
now_merge  <- tidycovid19::download_merged_data(cache = TRUE) 
vars_merge <- c("iso3c",
                "country",
                "date",
                "region",
                "population",
                "confirmed", 
                "deaths", 
                "recovered",
                "gcmr_retail_recreation", 
                "gcmr_grocery_pharmacy", 
                "gcmr_parks",
                "gcmr_transit_stations",
                "gcmr_workplaces", 
                "gcmr_residential", 
                "gtrends_score", 
                "gtrends_country_score"
                )
if(sum(vars_merge %in% names(now_merge)) == length(vars_merge)) {
    base_merge <- TRUE
    if(nrow(now_merge) > nrow(pre_merge)) {
        base_merge <- c(TRUE, base_merge)
    }
}
if(sum(base_merge) == 2){
    now_merge %>% 
    dplyr::filter(country != "Uruguay") %>% 
    bind_rows(., uy_a_merge(data_uy)) %>%
    rename(total_test_tidycovid19 = total_tests) %>% 
    rio::export(here::here('data-raw', paste0("data_merge_", Sys.Date(), ".xlsx")))
} else {
    status$error2 <- 'ERROR_02 --> La base de {tidycovid19} cambio los nombres de las variables o la dimension es mas chica'
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BLOCK 3 --> OWN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
old_test  <- busca_data('*rds')
url_test  <- 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'
data_test <- read.csv(url_test, stringsAsFactors = FALSE)
vars_test <- c('iso_code', 
               'date', 
               'total_tests', 
               'new_tests', 
               'new_cases', 
               'new_deaths', 
               'median_age',     
               'aged_65_older', 
               'aged_70_older', 
               'gdp_per_capita', 
               'extreme_poverty', 
               'total_cases_per_million', 
               'total_deaths_per_million'
               )

if(is.data.frame(data_test)){
    if(sum(vars_test %in% names(data_test)) == length(vars_test)) {
        base_test <- TRUE
        if(nrow(data_test) > nrow(old_test)) {
            base_test <- c(TRUE, base_test)
        }
    }
    if(sum(base_test) == 2){
        rio::export(data_test, here::here('data-raw', paste0("data_test_", Sys.Date(), '.rds')))
        merge <- busca_data('*xlsx')
        test  <- busca_data('*rds')
        new_merge <- 
            test %>%
            select(iso_code, date, total_tests, new_tests, new_cases, new_deaths, median_age, 
                    aged_65_older, aged_70_older, gdp_per_capita, extreme_poverty, total_cases_per_million, 
                    total_deaths_per_million) %>% 
            mutate(date = lubridate::parse_date_time(date, order = "ymd")) %>% 
            rename(iso3c = iso_code) %>% 
            right_join(., merge, by = c('iso3c', 'date'))
        countrycode::codelist_panel[c('iso3c', 'continent')] %>% 
            distinct() %>% 
            right_join(., new_merge, by = 'iso3c') %>% 
            rio::export(here::here('data-raw', paste0("data_merge_", Sys.Date(), ".xlsx")))
    } else {
        status$error3 <- 'ERROR_03 --> La base de OWN cambio los nombres de las variables o la dimension es mas chica'
    }
} else {
    status$error_03 <- "ERROR_03 --> La base de OWN de test no es un data.frame"
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BLOCK 4 --> GOOGLE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(sum(base_merge) == 2){
    google <- busca_data('*xlsx')
    paises <- c('Uruguay', 'Chile', 'Argentina', 'Brazil', 'Denmark', 'Sweden', 'Norway',
                'Finland', 'Spain', 'Italy', 'France', 'United States', 'Canada', 
                'New Zealand', 'Australia', 'United Kingdom', 'Germany', 'Austria', 
                'Ecuador', 'Peru', 'Mexico', 'Colombia', 'Singapore', 'Japan', 'Korea, South')
    google <- google %>% filter(country %in% paises) %>% 
        select(date, 
               gcmr_workplaces, 
               gcmr_retail_recreation, 
               gcmr_grocery_pharmacy, 
               gcmr_parks, 
               gcmr_transit_stations,
               gcmr_residential,
               region, 
               country, 
               iso3c) %>% 
        group_by(region) %>% 
        mutate(region2 = paste(region, "\n", paste(unique(country),  collapse = ", "))) %>% 
        ungroup()
    # animaciones
    animation_google(google, date, gcmr_workplaces,        file = 'workplaces')
    animation_google(google, date, gcmr_retail_recreation, file = 'retail_recreation')
    animation_google(google, date, gcmr_grocery_pharmacy,  file = 'grocery_pharmacy')
    animation_google(google, date, gcmr_parks,             file = 'parks')
    animation_google(google, date, gcmr_transit_stations,  file = 'transit_stations')
    animation_google(google, date, gcmr_residential,       file = 'residential')
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BLOCK 4 --> CLEAN -- RENDER
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(length(status) == 0){
    data_rm()
    #rmarkdown::render_site()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BLOCK 5 --> BAR RACE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source('R/animation_covid.R')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BLOCK 6 --> GIT
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(length(status) == 0) # gmailr drop else system('git push -u origin master')




