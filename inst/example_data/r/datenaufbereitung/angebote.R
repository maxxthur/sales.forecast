pacman::p_load("readxl", "tidyverse", "lubridate", "RSQLite", "DBI")

# Preparatory stuff
source("r/prep/helpers.R")

con <- dbConnect(SQLite(), "daten/datenbank/db.rsqlite3")

# Vorbereitung ------------------------------------------------------------

# Angebote einlesen
# alle Dateien identifizieren
paths <- list.files(path = "daten/rohdaten/angebote", full.names = T)

# einlesen
a_roh <- do.call("rbind", lapply(paths, read_excel))

# nur relevante Spalten behalten
a_rel <- a_roh %>% 
  select(Angebotsnummer = Belegnummer, Datum = BelegDatum, ABC = `ABC-Klasse (Kunde)`, 
         Staat, Branche, Angebot = `Gesamtpreis EW`, Gültig_bis = "Gültig bis", 
         Offen, Teil, Menge = "Stückzahl ohne Wertposition")

# Umsatzdetail laden
u_roh <- tbl(con, "Umsatz Roh") %>% 
  collect()

# Pro Angebot das letzte Lieferdatum bestimmen (Heuristik, hier sind genauere
# Berechnungen möglich, aber sehr viel komplexer!)
u <- u_roh %>% 
  select(EndDatum = Datum, AngebotNr, Teil, Menge) %>% 
  mutate(EndDatum = floor_date(ymd(EndDatum), unit = "months"),
         AngebotNr = as.integer(AngebotNr)) %>% 
  filter(!is.na(AngebotNr)) %>% 
  group_by(AngebotNr) %>% 
  reframe(EndDatum = min(EndDatum))

# Datenaufbereitung -------------------------------------------------------

# maximales Datum aus Umsatzdetail. Damit können aktuell gültige Angebote 
# klar von vergangenen abgegrenzt werden.
max_datum <- max(u$EndDatum)

# Alle Angebote
a <- a_rel %>% 
  mutate(Datum = floor_date(Datum, unit = "months"), 
         Gültig_bis = floor_date(Gültig_bis, unit = "months"), 
         Offen = ifelse(Offen == "FALSCH", F, T))

# Angebote mit Gültigkeit in der Zukunft, die auch noch offen sind
a_zukunft <- a %>% 
  filter(Gültig_bis > max_datum & Offen == T) %>% 
  select(Angebotsnummer, Start = Datum, Ende = Gültig_bis, Angebot)

# Angebote aus der Vergangenheit
a_vergangen <- a %>% 
  filter(Gültig_bis <= max_datum)

# Für fertig ausgelieferte Angebote das Datum der letzen Lieferung einsetzen (Heuristik)
a_vergangen2 <- a_vergangen %>% 
  left_join(u, by = c("Angebotsnummer" = "AngebotNr")) %>% 
  mutate(Start = Datum, 
         Ende = case_when(
           is.na(EndDatum) ~ Gültig_bis,
            T ~ EndDatum)) %>% 
  select(Angebotsnummer, Angebot, Start, Ende)

# für jeden Zeitpunkt berechnen, wieviele Angebote ausständig sind / waren
a_bind <- bind_rows(a_zukunft, a_vergangen2)

min_a_datum <- min(a_bind$Ende)
max_datum <- max(a_bind$Ende)

Zeitpunkte <- seq(min_a_datum, max_datum, by = "months")

a_final <- lapply(Zeitpunkte, function(z) {
  Daten_Zeitpunkt <- lapply(1:6, function(m) {
    a_bind %>% 
      filter(Start <= z & Ende > z & Ende <= z + months(m)) %>% 
      summarise(Angebot = sum(Angebot, na.rm = T)) %>% 
      mutate(Datum = z, 
             Monat = m)
  })
  
  Daten_Zeitpunkt %>% 
    bind_rows() %>% 
    pivot_wider(names_from = Monat, values_from = Angebot)
  
}) %>% 
  bind_rows()

a_final %>% 
  ggplot(aes(x = Datum, y = Angebot)) +
  geom_line()

# Was weiß ich heute über morgen?
a_bind %>% 
  filter(Start <= Zeitpunkte[49] & Ende > Zeitpunkte[49]) %>% 
  summarise(Angebot = sum(Angebot, na.rm = T))

# Hier noch mit Umsatz gewichten und eventuell komplexere Beerechnungen machen
# Insgesamt gehört das nochmal gescheit durchdacht..
  
dbWriteTable(con, name = "Angebote", value = Angebote, overwrite = T)

# Cleanup -----------------------------------------------------------------
dbConnect(con, shutdown = T)
rm(list = ls())
gc()
