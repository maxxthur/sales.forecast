pacman::p_load("tidyverse", "lubridate", "forecast", "DBI", "RSQLite")

con <- dbConnect(SQLite(), "daten/datenbank/db.rsqlite3")

data <- tbl(con, "Umsatz") %>% 
  collect() %>% 
  mutate(Datum = ymd(Datum))

# Daten für Schätzung und Test kreiren. Evaluierung über 2022 und 2023.
# Rollierend vorwärts, immer 6 Monate

max_date <- max(data$Datum)-months(5)

# Start und Enddatum für Forecasts bestimmen
start <- seq(ym("2022-01"), max_date, "months")
end <- start + months(5)

# Testdaten 
test <- mapply(function(s, e) {
  data %>% 
    filter(Datum >= s & Datum <= e)
}, start, end, SIMPLIFY = F)

# Trainingsdaten
train <- lapply(start, function(s) data %>% filter(Datum < s))

# Forecasting
forecasts <- mapply(function(train, test, n) {
  train_ts <- ts(train$Umsatz, start = 2019+3/12, frequency = 12)
  fit <- ets(train_ts)
  fc <- forecast(fit, h = 6)
  test %>% 
    mutate(FC = fc$mean, 
           Upper = fc$upper[, 2], 
           Lower = fc$lower[, 2], 
           n = n)
}, train, test, 1:18, SIMPLIFY = F)

# Evaluierung
fc_bind <- bind_rows(forecasts)

fc_bind %>% 
  arrange(Datum, n) %>% 
  group_by(Datum) %>% 
  mutate(n = row_number()) %>% 
  ungroup() %>% 
  ggplot(aes(x = Datum)) +
  geom_line(aes(y = Umsatz), color = "steelblue", size = 1.1) +
  geom_line(aes(y = FC), color = "orange", size = 1.1) +
  facet_wrap(~n)
