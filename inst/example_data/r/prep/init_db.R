pacman::p_load("DBI", "RSQLite")

# Datenbank initialisieren
con <- dbConnect(SQLite(), "daten/datenbank/db.rsqlite3")
dbDisconnect(con, shutdown = T)

