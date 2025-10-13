pacman::p_load("readxl", "data.table", "rio", "tidyverse", "lubridate", "forecast")

# Preparatory stuff
source("r/prep/helpers.R")

con <- dbConnect(SQLite(), "daten/datenbank/db.rsqlite3")

# Vorbereitung ------------------------------------------------------------


# Umsatzdetail einlesen
# alle Dateien identifizieren
paths <- list.files(path = "daten/rohdaten/umsatzdetail", full.names = T)

# einlesen
u_roh <- do.call("rbind", lapply(paths, read_excel))

# nur relevante Spalten behalten
u_rel <- u_roh %>% 
  select(Belegnummer, Datum = BelegDatum, ABC = `ABC-Klasse (Kunde)`, 
         Staat, Branche, Bestellart, Umsatz = `Umsatz m.Zuschl.`, AngebotNr, 
         AuftragNr, Teil, Menge = "Stückzahl ohne Wertposition")

dbWriteTable(con, name = "Umsatz Roh", value = u_rel %>% 
               mutate(Datum = as.character(Datum)), overwrite = T)

# Datenaufbereitung -------------------------------------------------------
# monatlicher Umsatz
Umsatz_Monat <- u_rel %>% 
  mutate(Datum = floor_date(Datum, unit = "months")) %>% 
  group_by(Datum) %>% 
  summarise(Umsatz = sum(Umsatz, na.rm = T)) %>% 
  mutate(Datum = as.character(Datum))

dbWriteTable(con, name = "Umsatz", value = Umsatz_Monat, overwrite = T)

Umsatz_Staat <- u_rel %>% 
  mutate(Datum = floor_date(Datum, unit = "months")) %>% 
  group_by(Datum, Staat) %>% 
  reframe(Umsatz = sum(Umsatz, na.rm = T)) %>% 
  group_by(Datum) %>% 
  mutate(Anteil = Umsatz / sum(Umsatz)) %>% 
  mutate(Datum = as.character(Datum))

dbWriteTable(con, name = "Umsatzanteile DACH", value = Umsatz_Staat, overwrite = T)

# Umsatz nach Land und Bundesland -----------------------------------------
  # Postleitzahlen für Deutschland laden
plz_de <- read.csv2(file = "daten/rohdaten/zip codes/georef-germany-postleitzahl.csv") %>% 
  select(plz = Name, bundesland = Land.name) %>% 
  mutate(Staat = "DE", 
         plz = as.character(plz))

u_plz <- u_roh %>% 
  select(Datum = BelegDatum, Umsatz = "Umsatz m.Zuschl.", Staat, PLZ) %>% 
  mutate(Datum = floor_date(Datum, unit = "months") %>% as.character(), 
         Staat = substr(Staat, 2, 3)) %>% 
  left_join(plz_de, by = c("PLZ" = "plz", "Staat")) %>% 
  filter(!(is.na(bundesland))) %>% 
  group_by(Staat, bundesland, Datum) %>% 
  reframe(Umsatz = sum(Umsatz, na.rm = T)) %>% 
  group_by(Staat, Datum) %>% 
  mutate(Anteil = Umsatz / sum(Umsatz, na.rm = T))

dbWriteTable(con, name = "Umsatzanteile PLZ", value = u_plz, overwrite = T)
  
# Cleanup -----------------------------------------------------------------
dbConnect(con, shutdown = T)
rm(list = ls())
gc()

