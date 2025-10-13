pacman::p_load("tidyverse", "lubridate", "forecast", "DBI", "RSQLite", "smooth", "workdays")

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
  fit <- smooth::es(train_ts)
  fc <- forecast(fit, h = 6)
  test %>% 
    mutate(FC = fc$mean, 
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


# Variablen testen ----------------------------------------------
train_forecast <- function(Data, y, xreg, test_size = 12, h = 6, regressors = "use") {
  results <- list()
  for(i in 1:h) {
    message(paste("Step", i))
    results_step <- list()
    
    data <- with(Data, Data[Monat == i, ])
    
    for(j in 0:(test_size - i)) {
      train <- data %>% 
        head(-test_size + j)
      
      test <- data %>% 
        tail(test_size - j) %>% 
        head(i) 
      
      fit <- smooth::es(pull(train, y), xreg = train[, xreg[[i]]], regressors = regressors)
      fc <- forecast::forecast(fit, h = i, newdata = test)
      actual <- pull(test, y)
      budget <- pull(test, "Budget")
      datum <- test$Datum
      
      results_step[[j + 1]] <- tibble(Datum = datum, Umsatz = actual, Budget = budget, Forecast = fc$mean)[i, ]
    }
    results[[i]] <- bind_rows(results_step)
    results[[i]]$fc_step <- i
  } 
  rolling_forecasts <- list()
  for(k in 1:nrow(results[[h]])) {
    rolling_forecasts[[k]] <- lapply(results, function(e) e[k, ]) %>%
      bind_rows() %>%
      mutate(fc_no = k)
  }
  
  bind_rows(rolling_forecasts)
}

best_subset <- function(Data, y, xreg, h = 6, test_size = 12) {
  comblist <- lapply(1:length(xreg), combn, x = xreg, simplify = F)
  combinations <- flatten(comblist)
  
  i = 0
  results <- lapply(combinations, function(c) {
    i <<- i + 1
    message(paste("Model", i, "out of", length(combinations)))
    xreg_it <- rep(list(c), 6)
    forecasts <- suppressMessages(train_forecast(Data = Data, y = y, h = h, test_size = test_size, xreg = xreg_it))
    forecasts %>% 
      group_by(fc_no) %>%
      mutate(sign_actual = sign(Umsatz - lag(Umsatz)), 
             sign_forecast = sign(Forecast - lag(Umsatz))) %>% 
      summarise(RMSE = sqrt(mean((Umsatz - Forecast)^2)), 
                MAE = mean(abs(Umsatz - Forecast)), 
                MDA = mean(sign_actual == sign_forecast, na.rm = T)) %>% 
      mutate(weight = fc_no^(1/2) / sum(fc_no^(1/2))) %>% 
      summarise(RMSE = sum(weight * RMSE), 
                MAE = sum(weight * MAE),
                MDA = mean(MDA, na.rm = T), 
                xreg = paste(c, collapse = ","))
  })
  
  bind_rows(results)
}

# Daten aufbereiten

Umsatz <- tbl(con, "Umsatz") %>% collect()
Auftrag <- tbl(con, "Auftragseingang") %>% collect() %>% rename(Auftragseingang = 2)
Bau <- tbl(con, "Auftragseingang Baugewerbe") %>% 
  filter(Bauart == "Hochbau") %>% 
  select(Datum, Wert) %>% 
  collect()
ifo <- tbl(con, "ifo") %>% 
  select(Datum, Geschäftslage) %>% 
  collect() %>% 
  mutate(Geschaeftslage = lag(Geschäftslage, 12)) %>% 
  select(-Geschäftslage)
Arbeitstage <- c("de", "at", "ch") %>% 
  map(~ get_workdays(country = .) %>% 
        mutate(date = as.character(date)) %>% 
        select(country, Datum = date, workdays))
Ferien <- holidays %>% 
  mutate(holiday = rowMeans(.[, 2:ncol(.)])) %>% 
  select(Datum = date, holiday) %>% 
  right_join(tibble(Datum = seq(ymd("2018-01-01"), ymd("2030-12-31"), by = "days"))) %>% 
  arrange(Datum) %>% 
  mutate(holiday = replace_na(holiday, 0), 
         Datum = floor_date(Datum, unit = "months") %>% as.character()) %>% 
  group_by(Datum) %>% 
  reframe(holiday = mean(holiday))
Budget <- tbl(con, "Budget") %>% collect()
Vertrieb <- tbl(con, "Vertriebsaktionen") %>% 
  mutate(vertrieb_7 = lag(n, 7), 
         vertrieb_9 = lag(n, 9)) %>% 
  select(Datum, vertrieb_7, vertrieb_9) %>% 
  collect()

# Modell trainieren
Data <- Umsatz %>%
  left_join(Budget, by = "Datum") %>% 
  left_join(Auftrag, by = "Datum") %>%
  full_join(Bau, by = "Datum", copy = T) %>% 
  left_join(ifo, by = "Datum", copy = T) %>% 
  left_join(Arbeitstage[[1]], by = "Datum") %>% 
  left_join(Ferien, by = "Datum") %>% 
  left_join(Vertrieb, by = "Datum") %>% 
  collect() %>% 
  mutate(Datum = ymd(Datum)) %>% 
  arrange(Datum) %>% 
  mutate(Wert = lag(Wert, 12)) %>% 
  na.omit()

# best subset selection

xreg <- c("Auftragseingang", "workdays", "holiday", "Geschaeftslage", "Wert", "vertrieb_7")

bs_results <- best_subset(Data = Data, y = "Umsatz", xreg = xreg)

bs_results %>% 
  arrange(MDA, RMSE, MAE)

xreg <- rep(list(c("Auftragseingang","workdays","holiday")), 6) # muss eine Liste sein
Ergebnisse_Training <- train_forecast(Data = Data, y = "Umsatz", xreg = xreg, test_size = 12, regressors = "use")

Ergebnisse_Training %>% 
ggplot(aes(x = ymd(Datum))) +
  geom_line(aes(y = Umsatz), color = "darkgreen", size = 0.8) +
  geom_line(aes(y = Forecast), color = "orange", size = 0.8) +
  geom_line(aes(y = -Budget), color = "red", size = 0.8) +
  facet_wrap(~fc_no)

# RMSE & MAE berechnen
Ergebnisse_Training %>% 
  group_by(fc_no) %>% 
  summarise(RMSE = sqrt(mean((Umsatz - Forecast)^2)), 
            MAE = mean(abs(Umsatz - Forecast))) %>% 
  summarise(RMSE = mean(RMSE), 
            MAE = mean(MAE)) 

# Auftragseingang_Wunsch funktioniert für die letzten 12 Monate
# Baugewerbe Wertindex funktioniert alleine nicht
# ifo Geschäftslage funktioniert alleine nicht
# Auftragseingang_Wunsch + Wertindex funktioniert
# Auftragseingang + Geschäftslage funktioniert nicht
# Auftragseingang + Wertindex + Geschäftslage funktioniert (Reduktion in RMSE ist aber nicht berauschend)

# -> ifo nicht aufnehmen! Bau aufnehmen.

# Auftragseingang und Arbeitstage (mit Anteilig berechneten Feiertagen in Bundesländern) besser 
# als Auftragseingang + Wert + (Arbeitstage) -> Wert nicht mehr mehr benutzen
# Ferien ohne Gewichtung funktionieren aber verbessern den Forecast nicht in besonderem Maß7





  


# Forecasting -------------------------------------------------------------
Data <- Umsatz %>% 
  right_join(Auftrag, by = "Datum") %>% 
  left_join(Arbeitstage[[1]], by = "Datum") %>% 
  left_join(Ferien, by = "Datum") %>% 
  collect()

Historie <- Data %>% 
  filter(!is.na(Umsatz))

Zukunft <- Data %>% 
  filter(is.na(Umsatz)) %>% 
  filter(Monat == 1)

rolling_fc <- function(hist, ftr, y, xreg, h = 6) {
  results <- list()
  for(i in 1:h) {
    fit <- smooth::es(pull(hist, y), xreg = hist[, xreg[[i]]])
    fc <- forecast::forecast(fit, h = i, newdata = Zukunft[1:i, ])
    datum <- Zukunft$Datum[1:i]
    
    results[[i]] <- tibble(Datum = datum, Umsatz = fc$mean, Forecast = T)[i, ]
  }
  hist %>% 
    bind_rows(results) %>% 
    mutate(Forecast = replace_na(Forecast, F)) 
}

fc_6m <- rolling_fc(Historie, Zukunft, y = "Umsatz", xreg = xreg, h = 6)

fc_6m %>% 
  ggplot(aes(x = ymd(Datum))) +
  geom_line(aes(y = Umsatz, group = Forecast, color = Forecast), size = 1) +
  geom_smooth(aes(y = Umsatz))
  scale_color_manual(values = c("steelblue", "darkred")) +
  theme_classic()



