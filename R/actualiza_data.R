
source('R/utils.R')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# datos uy guiad-covid con merge de {tidycovid19}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

url <- "https://raw.githubusercontent.com/GUIAD-COVID/datos-y-visualizaciones-GUIAD/master/datos/estadisticasUY.csv"
data_uy <- read.csv(url, stringsAsFactors = FALSE)
rio::export(data_uy, here::here('data-raw', paste0("A_guiad_", Sys.Date(), '.csv')))


tidycovid19::download_merged_data(cache = TRUE) %>% 
    dplyr::filter(country != "Uruguay") %>% 
    bind_rows(., uy_a_merge(data_uy)) %>%
    rename(total_test_tidycovid19 = total_tests) %>% 
    rio::export(here::here('data-raw', paste0("data_merge_", Sys.Date(), ".xlsx")))

url_test <- 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'
data_test <- read.csv(url_test, stringsAsFactors = FALSE)
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





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SE LIMPIA CARPETA DE DATOS!
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_rm()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ANIMACIONES GOOGLE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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






