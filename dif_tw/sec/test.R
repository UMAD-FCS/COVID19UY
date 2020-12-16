
## data test

test  <- rio::import(file.choose())
merge <- rio::import(file.choose())
guiad <- rio::import(file.choose())

guiad$date <- as.POSIXct(lubridate::parse_date_time(guiad$fecha, order = "dmy"))
mergeuy <- merge[merge$country == "Uruguay", ]
p <- merge(mergeuy, guiad[, c("date", "cantCasosNuevos", "cantTest", "cantFallecidos")], by = "date", all = TRUE)

p$new_cases  <- p$cantCasosNuevos
p$new_tests  <- p$cantTest
p$new_deaths <- p$cantFallecidos
p <- p[,1:(ncol(p)-3)]


data_merge <- 
    merge %>% 
    subset(.$country != "Uruguay") %>% 
    rbind(., p)


data_merge$date <- as.Date(data_merge$date)
data_merge$new_cases <- as.numeric(data_merge$new_cases)
data_merge$new_tests <- as.numeric(data_merge$new_tests)

rio::export(data_merge, here::here('data-raw', paste0("data_merge_", Sys.Date(), ".xlsx")))


umad <- rio::import(here::here('data-raw', paste0("data_merge_", Sys.Date(), ".xlsx")))
rio::export(umad, here::here("download-data", "merge_umad.csv"))
