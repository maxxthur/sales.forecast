pacman::p_load("tidyverse", "lubridate", "forecast", "DBI", "RSQLite", "seasonal")

con <- dbConnect(SQLite(), "daten/datenbank/db.rsqlite3")

# load data
data <- tbl(con, "Umsatz") %>% 
  collect() %>% 
  mutate(Datum = ymd(Datum))

# make ts object to be able to use functions from forecast package
Umsatz_ts <- ts(data = data$Umsatz, frequency = 12, start = 2019)

# look at time series
autoplot(Umsatz_ts) + 
  geom_smooth()

# analyse seasonality
ggseasonplot(Umsatz_ts, year.labels = T)

ggseasonplot(Umsatz_ts, polar = T)

ggsubseriesplot(Umsatz_ts)

stl(Umsatz_ts, s.window = "periodic") %>% autoplot()

# analyse autocorrelation
gglagplot(x = Umsatz_ts)


# Auftragseingang ----------------------------------------------------------------

con <- dbConnect(SQLite(), "daten/datenbank/db.rsqlite3")

auftragseingang <- tbl(con, "Auftragseingang") %>% 
  collect() %>% 
  mutate(Datum = ymd(Datum))

data %>% 
  right_join(auftragseingang, 
            by = "Datum") %>% 
  ggplot(aes(x = Datum)) +
  geom_line(aes(y = (Umsatz - min(Umsatz, na.rm = T))/(max(Umsatz, na.rm = T)-min(Umsatz, na.rm = T))), color = "darkgreen", size = 0.75) +
  geom_line(aes(y = (Auftragseingang_Wunsch - min(Auftragseingang_Wunsch))/(max(Auftragseingang_Wunsch - min(Auftragseingang_Wunsch)))), color = "orange", size = 0.75) +
  facet_wrap(~ Monat)

Ccf(data$Umsatz, auftragseingang$Auftragseingang_Wunsch)



# Auftragseingang Bau -----------------------------------------------------

calculate_lags <- function(df, var, lags){
  map_lag <- lags %>% map(~partial(lag, n = .x))
  return(df %>% mutate(across(.cols = {{var}}, .fns = map_lag, .names = "{.col}_lag{lags}")))
}

bau <- tbl(con, "Auftragseingang Baugewerbe") %>% 
  collect() %>% 
  mutate(Datum = ymd(Datum)) %>% 
  filter(Bauart == "Hochbau")

# hochbau gesamt
data %>% 
  left_join(bau, by = "Datum") %>% 
  select(Datum, Umsatz, Wert) %>% 
  calculate_lags(var = "Wert", lags = 1:12) %>% 
  pivot_longer(cols = -c(Datum, Umsatz)) %>% 
  ggplot(aes(x = ymd(Datum))) +
  geom_line(aes(y = (Umsatz - min(Umsatz, na.rm = T))/(max(Umsatz, na.rm = T)-min(Umsatz, na.rm = T))), color = "darkgreen", size = 0.75) +
  geom_line(aes(y = (value - min(value, na.rm = T))/(max(value, na.rm = T)-min(value, na.rm = T))), color = "red", size = 0.75) +
  facet_wrap(~ name)

data %>% 
  left_join(bau, by = "Datum") %>% 
  select(Datum, Umsatz, Wert) %>% 
  calculate_lags(var = "Wert", lags = 1:24) %>% 
  corrr::correlate() %>% 
  filter(term == "Umsatz") %>% 
  select(- Umsatz) %>% 
  pivot_longer(cols = -term)

# -> Wertindex nehmen, lags 10 - 12  


# Ifo Daten ---------------------------------------------------------------

ifo <- tbl(con, "ifo") %>% collect() %>% 
  mutate(Datum = ymd(Datum))

data %>% 
  left_join(ifo, by = "Datum") %>% 
  select(Datum, Umsatz, Geschäftslage) %>% 
  calculate_lags(var = "Geschäftslage", lags = 1:12) %>% 
  pivot_longer(cols = -c(Datum, Umsatz)) %>% 
  ggplot(aes(x = ymd(Datum))) +
  geom_line(aes(y = (Umsatz - min(Umsatz, na.rm = T))/(max(Umsatz, na.rm = T)-min(Umsatz, na.rm = T))), color = "darkgreen", size = 0.75) +
  geom_line(aes(y = (value - min(value, na.rm = T))/(max(value, na.rm = T)-min(value, na.rm = T))), color = "red", size = 0.75) +
  facet_wrap(~ name)

data %>% 
  left_join(ifo, by = "Datum") %>% 
  select(Datum, Umsatz, Geschäftslage) %>% 
  calculate_lags(var = "Geschäftslage", lags = 1:12) %>% 
  corrr::correlate() %>% 
  filter(term == "Umsatz") %>% 
  select(- Umsatz) %>% 
  pivot_longer(cols = -term)


# Vertriebsaktionen-------------------------------------------------------

vertrieb <- tbl(con, "Vertriebsaktionen") %>% collect() %>% 
  mutate(Datum = ymd(Datum))

data %>% 
  left_join(vertrieb, by = "Datum") %>% 
  select(Datum, Umsatz, n) %>% 
  calculate_lags(var = "n", lags = 1:12) %>% 
  pivot_longer(cols = -c(Datum, Umsatz)) %>% 
  ggplot(aes(x = ymd(Datum))) +
  geom_line(aes(y = (Umsatz - min(Umsatz, na.rm = T))/(max(Umsatz, na.rm = T)-min(Umsatz, na.rm = T))), color = "darkgreen", size = 0.75) +
  geom_line(aes(y = (value - min(value, na.rm = T))/(max(value, na.rm = T)-min(value, na.rm = T))), color = "red", size = 0.75) +
  facet_wrap(~ name)

data %>% 
  left_join(vertrieb, by = "Datum") %>% 
  select(Datum, Umsatz, n) %>% 
  calculate_lags(var = "n", lags = 1:12) %>% 
  corrr::correlate() %>% 
  filter(term == "Umsatz") %>% 
  select(- Umsatz) %>% 
  pivot_longer(cols = -term)

