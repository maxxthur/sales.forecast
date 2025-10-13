pacman::p_load("openxlsx", "DBI", "RSQLite", "tidyverse")

# Pfade
static_path <- "https://www.ifo.de/sites/default/files/secure/timeseries/gsk-d-"

date_path <- as.integer(gsub("-", "", x = substr(Sys.Date(), 1, 7)))

check = F
while(check == F) {
  ifo_dat <- tryCatch({ 
    read.xlsx(paste0(static_path, date_path, ".xlsx"), sheet = 2)
  }, error = function(e) {})
  ending <- date_path
  date_path <- date_path - 1
  
  if(is.null(ifo_dat) == F) check = T
}


# Spaltennamen fixen
ifo_names <- sub(" ", "", unlist(ifo_dat[6, c(1, (ncol(ifo_dat)-2):ncol(ifo_dat))]))

# ifo Index nur für das Bauhauptgewerbe
ifo_dat <- ifo_dat[-(1:6), c(1, (ncol(ifo_dat)-2):ncol(ifo_dat))]
names(ifo_dat) <- ifo_names
ifo_dat$Datum <- my(ifo_dat$`Monat/Jahr`)
ifo_dat$`Monat/Jahr` <- NULL
ifo_dat[, 1:3] <- lapply(ifo_dat[, 1:3], as.numeric)
ifo_dat$Datum <- as.character(ifo_dat$Datum)

# in Datenbank schreiben
dbWriteTable(con, name = "ifo", value = ifo_dat, overwrite = T)

con <- dbConnect(SQLite(), "daten/datenbank/db.rsqlite3")
dbListTables(con)

ifo <- tbl(con, "ifo") %>% 
  collect()

ifo %>% 
  write.xlsx("daten/199101-202311.xlsx")
