# This code replicates Flight Connection Routing figure of the paper.

library(readr)
library(dplyr)
library(stringr)
library(geosphere)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(haven)



# Adjust variables to generate plot properly
working.dir <- "C:/Users/Jan/Desktop/Research/"

# Fixed variables
dta.file <- "Output/For Non-Essential Graphs.dta"


if (working.dir == "DEFINE ME") {
  stop('Variable "working.dir" is not defined. Please define it in file {MAIN_FOLDER}/Visualise Flight Routing.R Line 16')
}

if (!dir.exists(working.dir)) {
  stop('Variable "working.dir" is not defined properly in file {MAIN_FOLDER}/Visualise Flight Routing.R Line 16. Please use the full path, i.e. "C:/Users/Jan/Replication/", make sure to use / instead of \\ .')
}

if (!file.exists(paste0(working.dir, dta.file))) {
  stop('Please run the Stata-Code first. Then run this code.')
}


# Load data set
data <- read_dta(paste0(working.dir, dta.file))


## Select a routing example
# -> should work with any observation within the data set
# -> but then you need to adjust stuff like position of the legend etc.
# -> can't highlight certain properties etc.
# => Therefore, we fixed one example
chosen.one <- data %>% filter(FacilityID == 89527 & PackageID == 66357) 


## Prepare obs
routing <- tibble(
  Object = c("Borrower", "Lender"),
  Name = c(chosen.one$BorrowerCity, chosen.one$LenderCity),
  Change = NA,
  Lat = c(chosen.one$BorrowerLat, chosen.one$LenderLat),
  Lon = c(chosen.one$BorrowerLon, chosen.one$LenderLon),
  FacilityID = chosen.one$FacilityID,
  Arrivable = NA,
  Departable = NA
)


## Load airport locations
unique.locations <- "Storage/Full Location Data.csv"
unique.locations <- read_csv(paste0(working.dir, unique.locations))
unique.locations <- unique.locations %>% filter(Airport & LocatedSuccessful)


## Filter Airports that does not exist
date <- ymd(chosen.one$FacilityStartDate)
flight.data <- paste0(working.dir, "Import/Air Routes - ", year(date),".csv")
flight.data <- read_csv(flight.data)
flight.data <- flight.data %>% filter(DEPARTURES_PERFORMED > 0 & AIR_TIME > 0)
flight.data <- flight.data %>% filter(PASSENGERS > 0 & MONTH == month(date))
flight.data <- flight.data %>% select(ORIGIN, DEST)


existing.airports <- tibble(Code = c(flight.data %>% pull(ORIGIN), flight.data %>% pull(DEST)))
existing.airports <- existing.airports %>% distinct(Code)
existing.airports <- existing.airports %>% mutate(
  Arrivable = Code %in% (flight.data %>% pull(DEST)),
  Departable = Code %in% (flight.data %>% pull(ORIGIN))
)
existing.airports <- existing.airports %>% mutate(
  LocationString = paste0(str_to_lower(Code), ", airport")
)

unique.locations <- unique.locations %>% left_join(existing.airports, by = c("LocationString"))


## Get actual routing
airports <- str_split(chosen.one$L1FlightRouting, "-")[[1]]
change <- 0
for (airport in airports) {
  airport.data <- tibble(
    Name = airport,
    Identifier = paste0(str_to_lower(airport), ", airport")
  )
  
  airport.data <- airport.data %>% left_join(unique.locations, by = c("Identifier" = "LocationString"))
  airport.data <- airport.data %>% select(Name, Lat, Lon, Arrivable, Departable)
  airport.data <- airport.data %>% mutate(
    Object = c("Airport"),
    Change = c(change),
    FacilityID = chosen.one$FacilityID
  )
  
  routing <- rbind(
    routing,
    airport.data
  )
  
  change <- change + 1
}


## Short cuts
lender <- routing %>% filter(Object == "Lender") %>% slice(1)
borrower <- routing %>% filter(Object == "Borrower") %>% slice(1)

## Compute distance to airports
available.airports <- unique.locations %>% mutate(DistanceLender = NA, DistanceBorrower = NA)
for (i in 1:nrow(available.airports)) {
  available.airports[i,"DistanceLender"] <- distm(
    c(lender$Lon, lender$Lat), c(available.airports[i,]$Lon, available.airports[i,]$Lat), fun = distHaversine
  ) / 1000
  
  available.airports[i,"DistanceBorrower"] <- distm(
    c(borrower$Lon, borrower$Lat), c(available.airports[i,]$Lon, available.airports[i,]$Lat), fun = distHaversine
  ) / 1000
}


# Draw airports function
draw_neighbours <- function(figure, all.airports, used.airport, range.limits, origin, party = "Lender") {
  variable <- paste0("Distance", party)
  
  consider.airport <- TRUE
  not.considered <- tibble(
    Marker = "purple",
    Symbol = "#FFFFFF"
  )
  
  
  # First draw potentially all airports
  for (limit in range.limits) {
    selected.airports <- which(all.airports[,variable] > limit$Minimum & all.airports[,variable] <= limit$Maximum)
    selected.airports <- all.airports[selected.airports,]
    
    colour.symbol <- ifelse(consider.airport, limit$Symbol, not.considered$Symbol)
    colour.marker <- ifelse(consider.airport, limit$Marker, not.considered$Marker)
    marker <- awesomeIcons(icon = "plane", markerColor = colour.marker, iconColor = colour.symbol)
    
    if (nrow(selected.airports) > 0) {
      for (i in 1:nrow(selected.airports)) {
        airport <- selected.airports[i,]
        
        if (airport$Lat != used.airport$Lat && airport$Lon != used.airport$Lon) {
          figure <- figure %>% addAwesomeMarkers(
            lng = airport$Lon,
            lat = airport$Lat,
            popup = "Airport",
            icon = marker
          )
        }
      }
      
      consider.airport <- FALSE
    }
    
    figure <- figure %>% addCircles(
      lng = origin$Lon,
      lat = origin$Lat,
      radius = limit$Maximum * 1000,
      color = limit$Circle,
      fillOpacity = 0
    )
  }
  
  # Then never consider ones
  selected.airports <- which(all.airports[,variable] > limit$Maximum)
  selected.airports <- all.airports[selected.airports,]
  
  marker <- awesomeIcons(icon = "plane", markerColor = "lightgray", iconColor = "#FFFFFF")
  
  if (nrow(selected.airports) > 0) {
    for (i in 1:nrow(selected.airports)) {
      airport <- selected.airports[i,]
      figure <- figure %>% addAwesomeMarkers(
        lng = airport$Lon,
        lat = airport$Lat,
        popup = "Airport",
        icon = marker
      )
    }
  }
  
  return(figure)
}

## Set range limits
range.limits.start.colour <- "#FD8D3C"
range.limits.end.colour <- "#800026"
range.limits <- c(-1, 125, 250, 350, 450)
range.limits <- list(
  tibble(
    Minimum = range.limits[1],
    Maximum = range.limits[2],
    Marker = "orange",
    Symbol = "#FFFFFF",
    Circle = colorRampPalette(c(range.limits.start.colour, range.limits.end.colour))(4)[1]
  ),
  tibble(
    Minimum = range.limits[2],
    Maximum = range.limits[3],
    Marker = "orange",
    Symbol = "#FFFFFF",
    Circle = colorRampPalette(c(range.limits.start.colour, range.limits.end.colour))(4)[2]
  ),
  tibble(
    Minimum = range.limits[3],
    Maximum = range.limits[4],
    Marker = "orange",
    Symbol = "#FFFFFF",
    Circle = colorRampPalette(c(range.limits.start.colour, range.limits.end.colour))(4)[3]
  ),
  tibble(
    Minimum = range.limits[4],
    Maximum = range.limits[5],
    Marker = "orange",
    Symbol = "#FFFFFF",
    Circle = colorRampPalette(c(range.limits.start.colour, range.limits.end.colour))(4)[3]
  )
)

# or blue
lender.marker.location <- awesomeIcons(icon = "pushpin", markerColor = "blue", iconColor = "#FFFFFF", squareMarker = TRUE)
lender.marker.airport <- awesomeIcons(icon = "plane", markerColor = "blue", iconColor = "#FFFFFF", squareMarker = TRUE)
borrower.marker.location <- awesomeIcons(icon = "pushpin", markerColor = "green", iconColor = "#FFFFFF")
borrower.marker.airport <- awesomeIcons(icon = "plane", markerColor = "green", iconColor = "#FFFFFF")


## Generate Map (Lender Part)
zoom <- 7
airport <- routing %>% filter(Object == "Airport" & Change == 0) %>% slice(1)
figure.lender <- leaflet(options = leafletOptions(zoomControl = FALSE, dragging = FALSE))
figure.lender <- figure.lender %>% setView(lng = lender$Lon, lat = lender$Lat, zoom = zoom)
figure.lender <- figure.lender %>% addAwesomeMarkers(lng = lender$Lon, lat = lender$Lat, popup = "Lender", icon = lender.marker.location)
figure.lender <- figure.lender %>% addAwesomeMarkers(lng = airport$Lon, lat = airport$Lat, popup = "Departure Airport", icon = lender.marker.airport)
figure.lender <- draw_neighbours(figure.lender, available.airports %>% filter(DistanceLender < 1000 & Departable == TRUE), airport, range.limits, lender, "Lender")
figure.lender <- figure.lender %>% addTiles()
# add legend


## Generate Map (Borrower Part)
zoom <- 6
airport <- routing %>% filter(Object == "Airport" & Change == max(routing$Change, na.rm = TRUE)) %>% slice(1)
figure.borrower <- leaflet(options = leafletOptions(zoomControl = FALSE, dragging = FALSE))
figure.borrower <- figure.borrower %>% setView(lng = borrower$Lon, lat = borrower$Lat, zoom = zoom)
figure.borrower <- figure.borrower %>% addAwesomeMarkers(lng = borrower$Lon, lat = borrower$Lat, popup = "Borrower", icon = borrower.marker.location)
figure.borrower <- figure.borrower %>% addAwesomeMarkers(lng = airport$Lon, lat = airport$Lat, popup = "Arrival Airport", icon = borrower.marker.airport)
figure.borrower <- draw_neighbours(figure.borrower, available.airports %>% filter(DistanceBorrower < 1000 & Arrivable == TRUE), airport, range.limits, borrower, "Borrower")
figure.borrower <- figure.borrower %>% addTiles()
# add legend




 html_legend <- "
 <span style='font-size:15px'>
<b>Symbols</b></br>
<span class='glyphicon glyphicon-plane'></span> &nbsp; Location</br>
<span class='glyphicon glyphicon-pushpin'></span> &nbsp; Airport</br>
</br>
<b>Colours</b></br>
<i style='background:#35A1D1;opacity:0.75'></i> Lender</br>
<i style='background:#72AF26;opacity:0.75'></i> Borrower</br>
<i style='background:#F1942F;opacity:0.75'></i> Considered, Not Taken</br>
<i style='background:#CD50B5;opacity:0.75'></i> Not Considered</br>
</br>
<b>Distances</b></br>
<i style='background:#FD8D3C;opacity:0.75'></i> < 125 km</br>
<i style='background:#D35E34;opacity:0.75'></i> 126 km - 250 km</br>
<i style='background:#A92F2D;opacity:0.75'></i> 251 km - 350 km</br>
<i style='background:#800026;opacity:0.75'></i> 351 km - 450 km</br>
<i style='background:gray;opacity:0.5'></i> 450 km</br>
</span>"
 
figure.borrower <- figure.borrower %>% addControl(html = html_legend, position = "bottomleft")
figure.borrower <- figure.borrower %>% addTiles()
figure.borrower
  
  
figure.lender <- figure.lender %>% addControl(html = html_legend, position = "bottomleft")
figure.lender <- figure.lender %>% addTiles()
figure.lender




