library(jsonlite)
library(RSQLite)

setwd("C:\\Users\\ilkarman\\Documents\\GitHub\\Movement-Visualisations\\02_Flights\\r\\")
live_url <- "https://opensky-network.org/api/states/all"
json_names <- c("icao24", 
                "callsign",
                "origin_country",
                "time_position",
                "time_velocity",
                "longitude",
                "latitude",
                "altitude",
                "on_ground",
                "velocity",
                "heading",
                "vertical_rate",
                "sensors"
)

TFREQ = 10  # Every 10 seconds
TMINUTES = 1*60*24*3  # 24 Hours * 3 Days
TLIM = (60/TFREQ)*TMINUTES  
tcount = 0

# Create SQLIte database
FLIGHTSDB <- dbConnect(RSQLite::SQLite(), "flightData.db")
TABLEDB <- gsub(" ","_",gsub(":","_",gsub("-","_",
                                          paste0("flightsJSON_",
                                                 Sys.time()))))
# Create Table
dbSendQuery(FLIGHTSDB, paste0("create table ", TABLEDB, " (
            ica024 TEXT,
            callsign TEXT,
            origin_country TEXT,
            time_position REAL,
            time_velocity REAL,
            longitude REAL,
            latitude REAL,
            altitude REAL,
            on_ground INTEGER,
            velocity REAL,
            heading REAL,
            vertical_rate REAL,
            t INTEGER)"))


urlToSQL <- function() 
{
  req <- fromJSON(live_url, flatten=TRUE)
  dat <- as.data.frame(req$states, stringsAsFactors=FALSE)
  names(dat) <- json_names
  # Drop if latitude, longitude or flightID missing
  dat <- dat[!(is.na(dat$latitude) | is.na(dat$longitude) | is.na(dat$icao24)), ]
  
  flights <- nrow(dat)
  if (flights > 0) {
    t <- rep(tcount, flights)
    
    # Save Data
    sendTable <- dat
    sendTable$t <- t
    sendTable$sensors <- NULL
    
    dbWriteTable(conn=FLIGHTSDB,
                 value = sendTable,
                 name = TABLEDB,
                 row.names = FALSE,
                 overwrite = FALSE,
                 append = TRUE)
    # Total rows:
    totrows <- dbSendQuery(FLIGHTSDB, paste0("select Count(*) from ", TABLEDB))
    message("Total Rows: ",dbFetch(totrows))
    message("Success: ", tcount, " added: ", flights, " flights")
  }
}

while (tcount < TLIM) {
  tryCatch(urlToSQL(),
           error = function(e) {
             message("Error: ", tcount)
           }
  )
  Sys.sleep(TFREQ)
  tcount <- tcount + 1
}

# Close Connection
dbDisconnect(FLIGHTSDB)
