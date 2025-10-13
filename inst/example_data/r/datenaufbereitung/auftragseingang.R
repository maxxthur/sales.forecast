pacman::p_load("readxl", "tidyverse", "lubridate", "RSQLite", "DBI")

# Preparatory stuff
source("r/prep/helpers.R")

con <- dbConnect(SQLite(), "daten/datenbank/db.rsqlite3")

# Vorbereitung ------------------------------------------------------------

# Auftragseingang einlesen
# alle Dateien identifizieren
paths <- list.files(path = "daten/rohdaten/auftragseingang", full.names = T)

# einlesen
a_roh <- do.call("rbind", lapply(paths, read_excel))

# nur relevante Spalten behalten
a_rel <- a_roh %>% 
  select(Belegnummer, Datum = BelegDatum, Wunschtermin, ABC = `ABC-Klasse (Kunde)`, 
         Staat, Branche, Auftragseingang = `Umsatz m. Zuschlag`)

# Datenaufbereitung -------------------------------------------------------
# monatlicher Auftragseingang
Auftragseingang_Monat <- a_rel %>% 
  mutate(Datum = floor_date(Datum, unit = "months"), 
         Auftragseingang = as.numeric(Auftragseingang)) %>% 
  group_by(Datum) %>%
  summarise(Auftragseingang = sum(Auftragseingang, na.rm = T)) %>% 
  mutate(Datum = as.character(Datum))

# Auftragseingang nach Wunschmonat, es dürfen nur Aufträge berücksichtigt 
# werden, die nicht im gleichen Monat erteilt wurden, da wir diese in den 
# Vormonaten nicht wissen konnten! (Achtung: 90% der Daten gehen verloren...)

# Hier nochmal überprüfen ob wir wirklich nur Daten in der Historie berücksichtigen
# die wir zu diesem Zeitpunkt schon wissen konnten! 
# Was wussten wir einen Monat vorher über den Monat für den wir uns interessieren? 
# Was wussten wir zwei Monate vorher? Das einmal durchexerzieren für 6 Monate vorher!

Zeitpunkte <- seq(min(a_rel$Wunschtermin), max(a_rel$Wunschtermin), by = "months")
  
a_shifted <- lapply(0:5, function(m) {
    a_rel %>% 
    mutate(Wunschtermin = floor_date(Wunschtermin, unit = "months"),
           Datum = floor_date(Datum, unit = "months"),
           Auftragseingang = as.numeric(Auftragseingang)) %>% 
    filter(Datum < Wunschtermin - months(m)) %>% 
    group_by(Wunschtermin) %>%
    summarise(Auftragseingang_Wunsch = sum(Auftragseingang, na.rm = T)) %>% 
    mutate(Datum = as.character(Wunschtermin)) %>% 
    select(Datum, Auftragseingang_Wunsch) %>% 
    mutate(Monat = m + 1)
}) %>% 
  bind_rows() 

a_shifted %>% 
  ggplot(aes(x = Datum, y = Auftragseingang_Wunsch, group = Monat, color = Monat)) +
  geom_line()

Auftragseingang_Wunschtermin <- a_rel %>% 
  mutate(Wunschtermin = floor_date(Wunschtermin, unit = "months"),
         Datum = floor_date(Datum, unit = "months"),
         Auftragseingang = as.numeric(Auftragseingang)) %>% 
  filter(Datum < Wunschtermin) %>% 
  group_by(Wunschtermin) %>%
  summarise(Auftragseingang_Wunsch = sum(Auftragseingang, na.rm = T)) %>% 
  mutate(Datum = as.character(Wunschtermin)) %>% 
  select(Datum, Auftragseingang_Wunsch)

Auftragseingang <- Auftragseingang_Monat %>% 
  left_join(Auftragseingang_Wunschtermin, by = "Datum")

dbWriteTable(con, name = "Auftragseingang", value = a_shifted, overwrite = T)

# Cleanup -----------------------------------------------------------------
dbConnect(con, shutdown = T)
rm(list = ls())
gc()
