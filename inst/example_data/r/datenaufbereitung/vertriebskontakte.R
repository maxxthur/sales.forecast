pacman::p_load("readxl", "data.table", "tidyverse", "lubridate", "forecast")

# Preparatory stuff
source("r/prep/helpers.R")

con <- dbConnect(SQLite(), "daten/datenbank/db.rsqlite3")


# Vertriebsdaten laden ----------------------------------------------------

v <- read_excel(path = "daten/rohdaten/vertrieb/Vertriebsaktionen.xlsx")

# Interessant sind Aktionstyp, Ergebnistyp, Betreff

v_count <- v %>% 
  mutate(Datum = as.Date(Datum, origin = "1899-12-30"), 
         Datum = floor_date(Datum, unit = "month")) %>% 
  count(Datum) %>%
  mutate(Datum = as.character(Datum))

dbWriteTable(con, name = "Vertriebsaktionen", value = v_count, overwrite = T)  


# Cleanup -----------------------------------------------------------------

dbDisconnect(con, shutdown = T)
rm(list = ls())
gc()
