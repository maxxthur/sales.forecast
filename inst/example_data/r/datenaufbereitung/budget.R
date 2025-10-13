pacman::p_load("readxl", "data.table", "tidyverse", "lubridate", "forecast")

# Preparatory stuff
source("r/prep/helpers.R")

con <- dbConnect(SQLite(), "daten/datenbank/db.rsqlite3")


# Budget laden ------------------------------------------------------------

b <- read_excel("daten/rohdaten/budget/Budget.xlsx")

dbWriteTable(con, name = "Budget", value = b, overwrite = T)


# Cleanup -----------------------------------------------------------------

dbDisconnect(con, shutdown = T)
rm(list = ls())
gc()
