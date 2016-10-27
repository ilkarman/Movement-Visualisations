library(jsonlite)
library(RSQLite)
library(sp)
library(rworldmap)

setwd("C:\\Users\\ilkarman\\Documents\\GitHub\\Movement-Visualisations\\03_Flights\\r\\")
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
TMINUTES = 1*60*24  # 24 Hours
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

# Get data from database
# Get data from SQL
flightdata <- dbReadTable(FLIGHTSDB, TABLEDB)

# Create ID using ICA024 and Callsign
flightdata$callsign <- gsub("^\\s+|\\s+$", "", flightdata$callsign)
flightdata <- flightdata[!(flightdata$callsign == "" ),]
flightdata$ID <- paste(flightdata$ica024, flightdata$callsign, sep="_")

# Select columns and sort
flightdata <- flightdata %>%
  select(ID, time_position, ica024, origin_country, callsign, latitude, longitude, on_ground) %>%
  arrange(ID, time_position)

# Deduplicate (ignoring time_position index; last column)
flightdata <- flightdata[!duplicated(flightdata[,-1]),]

# Complete Flights
# Want flights which start-from and end-on the ground

# Split route into two chunks
flightdata <- flightdata %>%
  group_by(ID) %>%
  arrange(time_position) %>%
  mutate(mid = round(n()/2)) %>%
  mutate(group_id = row_number())

idx <- flightdata$group_id < flightdata$mid
flightdata$leg = ""
flightdata$leg[idx] = "A"
flightdata$leg[!idx] = "B"

# Boolean cast as text 
flightdata$on_ground_int <- ifelse(flightdata$on_ground == "TRUE", 1, 0)

# Check whether one of two legs of ID was on ground
flightdata <- flightdata %>%
  group_by(ID, leg) %>%
  mutate(grounded = max(on_ground_int))

# Check if both legs have been on ground
flightdata <- flightdata %>%
  group_by(ID) %>%
  mutate(complete = min(grounded))

# Keep only complete
flightdata <- flightdata %>%
  filter(complete == 1)

# Map on country to point
pnts2Country <- function(lats, lngs) { 
  # Function to perform a point-in-polygon test
  # and return country-name of a point
  pnts <- cbind(lngs, lats)
  worldMap <- getMap()
  pointProj <- SpatialPoints(coords=pnts,
                             proj4string=CRS(proj4string(worldMap)))
  pointInPoly <- over(pointProj, worldMap)
  countries <- data.frame(mappedCountry = pointInPoly$ADMIN,
                          stringsAsFactors = FALSE)
  countries
}

flightdata$mappedCountry <- pnts2Country(flightdata$latitude, flightdata$longitude)

# Check how many complete flights we have
unique(flightdata$ID)
