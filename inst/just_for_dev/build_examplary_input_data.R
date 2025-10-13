pacman::p_load("DBI", "RSQLite", "tidyverse", "workdays")


con <- dbConnect(SQLite(), "example_data/daten/datenbank/db.rsqlite3")

# Mapping um nur abgeschlossene Monate zu behalten
minmaxdate <- tbl(con, "Umsatzdetail") %>%
  distinct(Datum = BelegDatum) %>%
  collect() %>%
  mutate(Datum = ymd(Datum)) %>%
  summarise(min = floor_date(min(Datum), "month"), max = ceiling_date(max(Datum), "month") - days(1))

max_date_map <- tibble(Datum = seq(minmaxdate$min, minmaxdate$max, "days")) %>%
  mutate(Monat = floor_date(Datum, "month"),
         Tag = wday(Datum)) %>%
  filter(!Tag %in% c(1, 7)) %>%
  group_by(Monat) %>%
  reframe(End = max(Datum))

date_filter <- tbl(con, "Umsatzdetail") %>%
  distinct(Datum = BelegDatum) %>%
  collect() %>%
  mutate(Datum = ymd(Datum),
         Monat = floor_date(Datum, "month")) %>%
  group_by(Monat) %>%
  reframe(Datum = max(Datum)) %>%
  left_join(max_date_map) %>%
  mutate(diff = as.numeric(End - Datum)) %>%
  filter(diff <=  1)

# Umsatz
umsatz <- tbl(con, "Umsatzdetail") %>%
  select(Datum = BelegDatum, Umsatz = "Umsatz m.Zuschl.") %>%
  collect() %>%
  mutate(Datum = floor_date(ymd(Datum), "months")) %>%
  group_by(Datum) %>%
  summarise(Umsatz = sum(Umsatz, na.rm = T)) %>%
  mutate(Datum = ymd(Datum)) %>%
  arrange(Datum) %>%
  filter(Datum %in% date_filter$Monat)

# workdays
workdays <- get_workdays(from = min(umsatz$Datum), to = max(umsatz$Datum) + months(12)) %>%
  select(Datum = date, workdays)

# budget
budget <- tbl(con, "Planung") %>%
  collect() %>%
  mutate(Stand = ymd(Stand)) %>%
  group_by(GJ) %>%
  filter(Stand == max(Stand, na.rm = T) & PL_Vert_Name == "Umsatzerlöse") %>%
  ungroup() %>%
  select(GJ, as.character(1:12)) %>%
  pivot_longer(-GJ) %>%
  transmute(Budget = -value,
            Datum = ymd(paste(GJ, name, 1, sep = "-")) + months(3))

# ifo
ifo <- tbl(con, "ifo") %>%
  collect()

# bauhauptgewerbe
bau <- tbl(con, "Auftragseingang Baugewerbe") %>%
  collect() %>%
  pivot_wider(names_from = Bauart, values_from = c(Wert, Volumen)) %>%
  mutate(Datum = ymd(Datum))

# ferien
ferien <- holidays %>%
  mutate(holiday = rowMeans(.[, 2:ncol(.)])) %>%
  select(Datum = date, holiday) %>%
  right_join(tibble(Datum = seq(ymd("2018-01-01"), ymd("2030-12-31"), by = "days"))) %>%
  arrange(Datum) %>%
  mutate(holiday = replace_na(holiday, 0),
         Datum = floor_date(Datum, unit = "months") %>% as.character()) %>%
  group_by(Datum) %>%
  reframe(holiday = mean(holiday))

# auftragseingang
auftragseingang <- tbl(con, "Auftragseingang") %>%
  pivot_wider(names_from = Monat, values_from = Auftragseingang_Wunsch, names_prefix = "Auftragseingang_") %>%
  collect()


available_data <- dbListTables(con) %>%
  map(~tbl(con, .) %>%
        collect())

names(available_data) <- dbListTables(con)

available_data$`Auftragseingang Baugewerbe` <- available_data$`Auftragseingang Baugewerbe` %>%
  pivot_wider(names_from = Bauart, values_from = c(Wert, Volumen))

available_data$Auftragseingang <- available_data$Auftragseingang %>%
  pivot_wider(names_from = Monat, values_from = Auftragseingang_Wunsch, names_prefix = "Auftragseingang_")

available_data$`Umsatz Roh` <- NULL
available_data$`Umsatzanteile DACH` <- NULL
available_data$`Umsatzanteile PLZ` <- NULL
available_data$Angebote <- NULL
available_data$Budget$Budget <- -available_data$Budget$Budget

library(workdays)
available_data$workdays <- get_workdays(from = ymd("2019-04-01"), to = ymd("2025-01-01")) %>%
  select(-country)
available_data$holidays <- holidays %>%
  mutate(holidays = rowMeans(.[, -1], na.rm = T)) %>%
  mutate(date = floor_date(date, "months")) %>%
  group_by(date) %>%
  summarise(holidays = mean(holidays, na.rm = T)) %>%
  select(Datum = date, holidays)

usethis::use_data(available_data, overwrite = T)
