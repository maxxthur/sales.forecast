pacman::p_load("tidyverse", "lubridate", "wiesbaden", "DBI", "RSQLite")

# Preparatory stuff
source("r/prep/helpers.R")

con <- dbConnect(SQLite(), "daten/datenbank/db.rsqlite3")


# Vorbereitung ------------------------------------------------------------

# Login destatis Datenbank des statistischen Bundesamts
username <- "DEO66377ZA"
pw <- "BaseRBestR2023"
db <- "de"

credentials <- c(user = username, 
                 password = pw, 
                 db = db)

test_login(genesis = credentials)

# Aufrufen welche Daten verfügbar sind
data_list <- retrieve_datalist(tableseries="441*", genesis=credentials) 
data_list %>% view()

retrieve_metadata("44111BM002", genesis = credentials)

rel_Bauarten <- c("Hochbau" = "BAUART1", 
                  "Hochbau ohne Wohnungsbau" = "BAUART12", 
                  "gewerblicher Hochbau" = "BAUART121", 
                  "öffentlicher Hochbau" = "BAUART122")
Bauarten_map <- data.frame(Bauart = names(rel_Bauarten), BAUAX1 = rel_Bauarten)
rownames(Bauarten_map) <- NULL

# Tabelle mit Auftragseingang Baugewerbe (nur Hochbau)
Auftragseingang_Bau_Volumen <- retrieve_data(tablename = "44111BM002", startyear = 2018, 
                                     genesis = credentials, inhalte = c("AUF102")) %>% 
  filter(BAUAX1 %in% rel_Bauarten & WERT03 == "WERTORG") %>% 
  left_join(Bauarten_map, by = "BAUAX1") %>% 
  mutate(MONAT = as.integer(str_extract(MONAT, "\\d{2}")), 
         Datum = ym(paste(JAHR, MONAT, sep = "-"))) %>% 
  select(Datum, Bauart,  Volumen = AUF102_val) 
  
Auftragseingang_Bau_Wert <- retrieve_data(tablename = "44111BM002", startyear = 2018, 
                                             genesis = credentials, inhalte = c("AUF101")) %>% 
  filter(BAUAX1 %in% rel_Bauarten & WERT03 == "WERTORG") %>% 
  left_join(Bauarten_map, by = "BAUAX1") %>% 
  mutate(MONAT = as.integer(str_extract(MONAT, "\\d{2}")), 
         Datum = ym(paste(JAHR, MONAT, sep = "-"))) %>% 
  select(Datum, Bauart, Wert = AUF101_val)
  
Auftragseingang_Bau <- Auftragseingang_Bau_Wert %>% 
  left_join(Auftragseingang_Bau_Volumen, by = join_by(Datum, Bauart)) %>% 
  mutate(Datum = as.character(Datum))

dbWriteTable(con, name = "Auftragseingang Baugewerbe", value = Auftragseingang_Bau, overwrite = T)



