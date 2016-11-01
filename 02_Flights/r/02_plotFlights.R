library(RSQLite)
library(sp)
library(rworldmap)
library(dplyr)
library(rgdal)
library(RANN)

setwd("C:\\Users\\ilkarman\\Documents\\GitHub\\Movement-Visualisations\\02_Flights\\r\\")

# SQLIte database
FLIGHTSDB <- dbConnect(RSQLite::SQLite(), "flightData.db")
TABLEDB <- "flightsJSON_2016_10_28_10_49_09"

# Get data from SQL
flightdata <- dbReadTable(FLIGHTSDB, TABLEDB)

# Step 1- Deduplicate (not on index = t)
flightdata$ica024 <- gsub(" ","",flightdata$ica024)
flightdata$callsign <- gsub(" ","",flightdata$callsign)
flightdata <- flightdata[!duplicated(flightdata[-ncol(flightdata)]),]

# We may want to use altitude to determine whether a plane is on-ground
flightdata <- flightdata[!is.na(flightdata$altitude),]

# Step 2- Create ID (for a flight)
# Readings after a quiet period of x mins for same ica024 = new flight
flightdata <- flightdata[!is.na(flightdata$time_position),]

flightdata <- flightdata %>%
  select(t, time_position, ica024, origin_country, callsign, latitude, longitude, altitude, on_ground) %>%
  group_by(ica024) %>%
  arrange(t) %>%
  mutate(time.diff = time_position-lag(time_position))

# What is quiet period?
flightdata %>%
  mutate(time.diff = time.diff/(60)) %>% 
  .$time.diff %>% 
  hist(breaks=100, plot=FALSE) %>% 
  .$density %>% 
  plot(type="l", log="y")

flightdata$newFlight <- flightdata$time.diff > (60*40) | is.na(flightdata$time.diff)
flightdata$id <- cumsum(flightdata$newFlight)

# Step 3- Complete flights
# We only want to plot complete flights
# Assume complete flight starts and ends with a low altitude

# Which altitude to use?
flightdata %>%
  .$altitude %>%
  hist(breaks=100)
# Highest commerical airport = 4,411 metres

foo <- function(df) {
  df[1,"altitude"] < 4500 & df[nrow(df),"altitude"] < 4500
}

# Index to get IDs of complete flights
isCompleteFlightidx <- by(flightdata, flightdata$id, foo)
isCompleteID <- unique(flightdata$id)[isCompleteFlightidx]

# Only complete flights
completeFlights <- flightdata[flightdata$id %in% isCompleteID,]

# Flight must have more than 150 points
completeFlights <- completeFlights %>%
  group_by(id) %>% 
  arrange(t) %>% 
  filter(n() > 150)

# Step 4- Add on countryName to each point
pnts2Country <- function(lats, lngs) { 
  # Function to perform a point-in-polygon test
  # and return country-name of a point
  pnts <- cbind(lngs, lats)
  worldMap <- getMap()
  pointProj <- SpatialPoints(coords=pnts,
                             proj4string=CRS(proj4string(worldMap)))
  pointInPoly <- over(pointProj, worldMap)
  pointInPoly$ADMIN
}

# Apply mapping
completeFlights$mappedCountry <- pnts2Country(completeFlights$latitude,
                                              completeFlights$longitude)
# origCountry, destCountry
orig <- completeFlights %>% 
  group_by(id) %>% 
  arrange(t) %>% 
  slice(1) %>% 
  ungroup %>% 
  select(id, origCountry=mappedCountry) %>% 
  na.omit()

dest <- completeFlights %>% 
  group_by(id) %>% 
  arrange(t) %>% 
  slice(n()) %>% 
  ungroup %>% 
  select(id, destCountry=mappedCountry) %>% 
  na.omit()

routesPath <- orig %>% 
  inner_join(dest, by = 'id') %>% 
  na.omit()

# Add two columns (origCountry, destCountry)
completeFlights <- completeFlights[completeFlights$id %in% routesPath$id,]
completeFlights <- completeFlights %>% 
  inner_join(routesPath, by = 'id')

completeFlights$mappedCountry <- as.character(completeFlights$mappedCountry)
completeFlights$origCountry <- as.character(completeFlights$origCountry)
completeFlights$destCountry <- as.character(completeFlights$destCountry)
completeFlights$mappedCountry[is.na(completeFlights$mappedCountry)] <- "water"

# Load airport locations from openflights
airports <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
                      header=FALSE)
 
names(airports) <- c("airportID", "airportName", "city", "country",
                      "IATA", "ICAO", "latitude", "longitude", "altitude",
                      "timezone", "dst", "tzTime")
 
flPoints <- airports[airports$ICAO != "\\N" & airports$IATA != "", ]

plotFlights <- completeFlights

# Tag flights
plotFlights$status <- ifelse(
  plotFlights$origCountry == plotFlights$mappedCountry, "outbound",
  ifelse(plotFlights$destCountry == plotFlights$mappedCountry, "inbound",
         "passing"))

plotFlights$status[plotFlights$origCountry == plotFlights$destCountry] <- "within"

# Plot flights
library(ggplot2)
library(rworldxtra)
library(rworldmap)

worldMap <- fortify(getMap(resolution="high"))

# IN

ggplot() +
  
  geom_polygon(data=worldMap, aes(x=long, y=lat, group=group),
               fill='black', colour=NA) +
  # Plot airports
  # geom_point(data=flPoints, aes(x=longitude, y=latitude),
  #            colour='white', alpha=0.8, size=0.1)  +   
  
  geom_path(aes(longitude, latitude, group=id),
            data=plotFlights[plotFlights$status != "inbound",],
            color = 'black', alpha=0.1, size=0.5) +

  geom_path(aes(longitude, latitude, group=id),
            data=plotFlights[plotFlights$status == "inbound",],
            color = 'blue', alpha=0.1, size=0.5) +

  # Background blank
  theme(panel.background = element_rect(fill='#2C3539', colour='#2C3539'),
        # Everything apart from panel is blank
        axis.text.y=element_blank(),
        panel.margin = unit(c(0,0,0,0), "lines"),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        axis.ticks.length = unit(0,"null"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.grid = element_blank(),
        title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(0,"null"),
        plot.margin = rep(unit(0,"null"),4),
        axis.ticks.length = unit(0,"cm")) +
  
  # No labels
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) + 
  labs(x=NULL, y=NULL, title=NULL) +
  
  # Map
  coord_map(projection = "mercator", xlim = c(-20, 20), ylim = c(35, 65))

# Save
ggsave("flights_v5_in.png", width=30, height=15, dpi=200)

# OUT

ggplot() +
  
  geom_polygon(data=worldMap, aes(x=long, y=lat, group=group),
               fill='black', colour=NA) +
  # Plot airports
  # geom_point(data=flPoints, aes(x=longitude, y=latitude),
  #            colour='white', alpha=0.8, size=0.1)  +   
  
  geom_path(aes(longitude, latitude, group=id),
            data=plotFlights[plotFlights$status != "outbound",],
            color = 'black', alpha=0.1, size=0.5) +
  
  geom_path(aes(longitude, latitude, group=id),
            data=plotFlights[plotFlights$status == "outbound",],
            color = 'red', alpha=0.1, size=0.5) +
  
  # Background blank
  theme(panel.background = element_rect(fill='#2C3539', colour='#2C3539'),
        # Everything apart from panel is blank
        axis.text.y=element_blank(),
        panel.margin = unit(c(0,0,0,0), "lines"),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        axis.ticks.length = unit(0,"null"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.grid = element_blank(),
        title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(0,"null"),
        plot.margin = rep(unit(0,"null"),4),
        axis.ticks.length = unit(0,"cm")) +
  
  # No labels
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) + 
  labs(x=NULL, y=NULL, title=NULL) +
  
  # Map
  coord_map(projection = "mercator", xlim = c(-20, 20), ylim = c(35, 65))

# Save
ggsave("flights_v5_out.png", width=30, height=15, dpi=200)