# Draft - Ilia Karmanov - 13/10/2016
#
# http://cycling.data.tfl.gov.uk/
#
# Changes:
# 1. Use coord_map instead of coord_equal
# 2. Use gganimate() instead of API call
# 3. Fix crop, title, etc for animation
# 4. Error-handling to API download call

library(jsonlite)
library(dplyr)
library(rgdal)
library(rgeos)
library(raster)
library(stringi)
library(ggplot2)
library(grid)

# Working environment
cyclePath <- "C:/Users/ilkarman/Desktop/cycleHire/"
setwd(cyclePath)

# Constants
ONLINEPOINTS <- "https://api.tfl.gov.uk/BikePoint?app_id=&app_key="
ONLINEUSAGE <- "http://cycling.data.tfl.gov.uk/usage-stats/25JourneyDataExtract28Sep2016-04Oct2016.csv"
BINGKEY <- "REGISTER_WITH_BING"
BUILDINGS <- "TQTL_LON"  # https://docs.google.com/uc?id=0B0kRhiw4fD7uQzU3MzBMRzFfSFk&export=download

# Read in Stations
cycleLocs <- fromJSON(ONLINEPOINTS,
                      flatten=TRUE)
# Remove extra info
cycleLocs <- cycleLocs %>%
  select(id, commonName, lat, lon)
cycleLocs$id <- as.numeric(stri_extract_all_regex(cycleLocs$id, "[0-9]+"))
# Read in journies
cycleJournies <- read.csv(ONLINEUSAGE)
cycleJournies <- na.omit(cycleJournies)
nrow(cycleJournies)  # 223786

# Format Date-Time
cycleJournies$Start.Date <- as.POSIXct(strptime(cycleJournies$Start.Date, "%d/%m/%Y %H:%M"))
cycleJournies$End.Date <- as.POSIXct(strptime(cycleJournies$End.Date, "%d/%m/%Y %H:%M"))
# Keep one-day (01/10/2016)
cycleJournies <- cycleJournies  %>% 
  filter(Start.Date >= "2016-10-01" & Start.Date < "2016-10-02")

# Create hour-bins (ROUGH, re-do later)
cycleJournies$hourBin = format(round(cycleJournies$Start.Date, units="hours"), format="%H:%M")
cycleJournies$f <- cycleJournies$StartStation.Id
cycleJournies$t <- cycleJournies$EndStation.Id

cycleJournies <- cycleJournies %>%
  select(Duration, f, t, hourBin) %>%
  # Remove same station journies?
  filter(f != t)

# Merge in station-data
cycleJournies <- cycleJournies %>%
  left_join(cycleLocs, by = c("f" = "id")) %>%
  rename(start.commonName = commonName,
         start.lat = lat,
         start.lon = lon) %>%
  left_join(cycleLocs, by = c("t" = "id")) %>%
  rename(end.commonName = commonName,
         end.lat = lat,
         end.lon = lon) 

##################
# Day Plot
##################

# Collapse count
cycleJourniesSum <- cycleJournies %>% 
  group_by(f, start.lat, start.lon, t, end.lat, end.lon) %>%
  summarise(countBikes = n()) %>%
  arrange(f, t)
# ID
cycleJourniesSum <- na.omit(cycleJourniesSum)
cycleJourniesSum <- cbind(id=seq(nrow(cycleJourniesSum)), cycleJourniesSum)

# Get route segments
# No error-handling ....
get_route <- function(id, from, to) {
  URLBASE <- "http://dev.virtualearth.net/REST/V1/Routes/"
  URLMODE <- "Driving?"
  URLFROM <- "wp.0="
  URLTO <- "&wp.1="
  URLEND <- "&optmz=distance&routeAttributes=routePath&key="
  
  url.from <- URLencode(from)
  url.to <- URLencode(to)
  
  sendUrl <- paste0(URLBASE, URLMODE, URLFROM, url.from, URLTO, url.to, URLEND, BINGKEY)
  resp <- fromJSON(sendUrl)
  directions <- as.data.frame(resp$resourceSets$resources)
  journey <- as.data.frame(directions$routePath$line$coordinates)
  
  id_rep <- rep(id,nrow(journey)-1)
  from <- journey[1:nrow(journey)-1,]
  to <- journey[2:nrow(journey),]
  out <- cbind(id_rep, from, to)
  names(out) <- c("id", "from_lat", "from_lon", "to_lat", "to_lon")
  return(out)
}

# API calls
cycleLegs <- list()
for (i in 1:nrow(cycleJourniesSum)) {
  # Create route points
  from <- paste0(cycleJourniesSum[i ,"start.lat"], ",", cycleJourniesSum[i ,"start.lon"])
  to <- paste0(cycleJourniesSum[i ,"end.lat"], ",", cycleJourniesSum[i ,"end.lon"])
  # Get segments
  # Why some errors? Re-try? Connection issue?
  tryCatch(
    {
      cycleLegs[[i]] <- get_route(i, from, to)
      print(paste0("Success with id: ", i, " loc: ", from, ", ", to))},
    error = function(e) {print(paste0("Error with id: ", i, " loc: ", from, ", ", to))})
}

# Bind individual journies
cyclePaths <- do.call(rbind, cycleLegs)

# Attach to data
mergedPaths <- cyclePaths %>%
  left_join(cycleJourniesSum, by = c("id" = "id"))  

# Save & Load
#write.csv(mergedPaths, 'route_data.csv', row.names=FALSE)
mergedPaths <- read.csv('route_data.csv')

# Collapse by segments
dayJournies <- mergedPaths %>% 
  group_by(from_lat, from_lon, to_lat, to_lon, countBikes) %>%
  summarise(countBikesSum = sum(countBikes)) %>%
  arrange(countBikesSum) 

# Buildings background
buildings <- readOGR(substr(cyclePath, 1, nchar(cyclePath)-1), layer=BUILDINGS) 
buildings <- spTransform(buildings, CRS("+proj=longlat +datum=WGS84"))

# Clip to extend of our data
# (xmin, xmax, ymin, ymax)
CP <- as(extent(-0.245, 0.026, 51.556, 51.449), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(buildings))
buildingsClip <- gIntersection(buildings, CP, byid=TRUE)

#Remove axis
#xquiet<- scale_x_continuous("", breaks=NULL)
#yquiet<-scale_y_continuous("", breaks=NULL)
xquiet<-scale_x_continuous(expand=c(0,0), breaks=NULL)
yquiet<-scale_y_continuous(expand=c(0,0), breaks=NULL)
quiet<-list(xquiet, yquiet)

# Plot
ggplot() +
    
  #   Buildings as background
  geom_polygon(data=buildingsClip, aes(long, lat, group=group),
               color=NA, fill='#0C090A', size=1, alpha=0.2) +
    
  # Plot journies
  geom_segment(data=dayJournies, aes(x=from_lon, xend = to_lon, 
                                     y=from_lat, yend = to_lat,
                                     alpha = countBikesSum),
               colour='SkyBlue1') +

  # Plot stations
  geom_point(data=cycleLocs, aes(x=lon, y=lat),
              colour='white', alpha=0.8, size=0.2)  +  
    
  theme(panel.background = element_rect(fill='#2C3539', colour='#2C3539'),
        legend.position="none") +
    
  #http://geepeeex.com/LongitudesAndLatitudes.htm
  quiet + coord_equal(ratio=122/78) 
    
# Save
ggsave("daily.png", width=20, height=10, dpi=200)

##################
# Hourly animation
##################

# Collapse by hour bin
cycleJourniesSumHour <- cycleJournies %>% 
  group_by(f, t, hourBin) %>%
  summarise(countBikes = n()) %>%
  arrange(f, t)

# Keep just routes for from-to station
routeData <- mergedPaths %>%
  select(f, t, from_lat, from_lon, to_lat, to_lon)

# Merge on routes to hourly aggregated
routeHourlyData <- cycleJourniesSumHour %>%
  left_join(routeData, by = c("f" = "f", "t" = "t"))

# Collapse by segments
hourJournies <- routeHourlyData %>% 
  group_by(hourBin, from_lat, from_lon, to_lat, to_lon, countBikes) %>%
  summarise(countBikesSum = sum(countBikes)) %>%
  arrange(countBikesSum) 

# Plot
for (hr in levels(factor(hourJournies$hourBin))) {
  hourJourney = hourJournies[hourJournies$hourBin == hr,]
  print(nrow(hourJourney))
  # Plot
  ggplot() +
    #   Buildings as background
    geom_polygon(data=buildingsClip, aes(long, lat, group=group),
                 color=NA, fill='#0C090A', size=1, alpha=0.2) +
    # Plot journies
    geom_segment(data=hourJourney, aes(x=from_lon, xend = to_lon, 
                                       y=from_lat, yend = to_lat,
                                       alpha = countBikesSum),
                 colour='skyblue1') +
    # Plot stations
    geom_point(data=cycleLocs, aes(x=lon, y=lat),
               colour='white', alpha=0.8, size=0.2)  + 
    # Time annotation
    annotate("text", x = -0.028, y = 51.454,
           label = paste0("02 Oct 16 - ", hr),
           size = 14,
           colour = "white") + 
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
    labs(x=NULL, y=NULL, title=NULL)
  # Save
  fname = gsub(":","_",paste(hr, "cycle_journies.png", sep="_"))
  ggsave(fname, width=16, height=10, dpi=200)
}

# GIF
print("Install ImageMagick")
system('magick convert  -resize 50% -delay 80 *cycle_journies.png tfl_cycle_hires.gif')