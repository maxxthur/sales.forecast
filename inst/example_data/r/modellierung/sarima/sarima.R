pacman::p_load("tidyverse", "lubridate", "forecast", "DBI", "RSQLite")

con <- dbConnect(SQLite(), "daten/datenbank/db.rsqlite3")

data <- tbl(con, "Umsatz") %>% 
  collect() %>% 
  mutate(Datum = ymd(Datum))

Umsatz_ts <- ts(data = data$Umsatz, frequency = 12, start = 2019)

auto.arima(y = Umsatz_ts)

