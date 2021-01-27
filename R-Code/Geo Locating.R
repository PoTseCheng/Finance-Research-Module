# This file does the geo locating stuff for all places

# ROAD MAP OF THIS FILE:
#
#    Section: Prepare Geo locating
# 1) Get all borrower and lender locations
# 2) Get all airport locations
#    Section: Location look up
# 3) Do the actual look up in chunks       |\
# 4) Clean data                            | \  We can skip this time intensive step
# 5) Adjust important missing locations    | /  by using a cached version
# 6) Data transformation                   |/
#
# Next step: Attach Location Data



# Note that the geo locating is a rather time intensive step. Therefore,
# we minimise the number of look ups. Three address look ups take roughly
# about 1 second. This is due to restrictions of the geo-locating service
# (OpenStreetMaps). As a consequence, every lender in the same city is
# assumed to be at the same place (city centre). So any lender in new york
# city will work at the town hall. This drastically reduces the number of
# look ups to an appropiate number (still about 6,5k => ~ 45 min look up time)



######################################################################
######################################################################
####### SECTION: PREPARE GEO LOCATING
# We get all look ups and remove duplicates to minimise the number of
# look ups even further.

###########
### Borrower / Lender Location
# Get all cities in which borrowers and lenders are located
# We take the facility data and extract all lenders and borrowers city / state
unique.locations <- tibble(
  City = c(facility.data %>% pull(LenderCity), facility.data %>% pull(BorrowerCity)),
  State = c(facility.data %>% pull(LenderState), facility.data %>% pull(BorrowerState))
)

# The unique locations list, is the list which will be looked up

# Remove duplicates (same city state constellation)
unique.locations <- unique.locations %>% distinct(City, State)

# Build look-up string (City, State, US)
unique.locations$LocationString <- paste(unique.locations$City, unique.locations$State, "us", sep = ", ")


############
### Airport Location
# This variable will contain a list of all airports where passengers planes fly to
airport.list <- c()

# To get a list of all used airports, we consider the air traffic records.
# The records store each year in a seperate file. Therefore, we must load 
# every file to get the airport codes

# Year loop to load airports of each year
for (year in observation.period) {
  # Make sure that file exists
  data.file <- paste0(working.dir, "Import/Air Routes - ", year, ".csv")
  if (!file.exists(data.file)) {
    # This message should actually never show up
    stop(paste("Air traffic data file of ", year, " does not exist; file: ", data.file))
  }
  
  # Load all recorded routes --> the file creates a error, but that is not an issue for us,
  # r thinks is a column at the end of file, but there is not. Seems like trump failed to make
  # the bereau of transportation great again
  annual.air.routes <- suppressWarnings(read_csv(data.file))
  
  # This line omits all cargo airports
  annual.air.routes <- annual.air.routes %>% filter(SEATS > 0)
  
  # Extract origination and destination 
  annual.airports.origination.code = annual.air.routes %>% pull("ORIGIN")
  annual.airports.destination.code = annual.air.routes %>% pull("DEST")
  
  # Store the airports and remove duplicates
  airport.list <- c(airport.list, annual.airports.origination.code, annual.airports.destination.code) %>% unique()
  
  # Save some memory, we will need it for the routing ;)
  annual.air.routes <- NULL
  annual.airports.origination.code <- NULL
  annual.airports.destination.code <- NULL
}

# Add airports to unique locations list
unique.locations <- rbind(unique.locations, tibble(
  City = NA,
  State = NA,
  LocationString = paste0(str_to_lower(airport.list), ", airport")
))
airport.list <- NULL

# Add airport flag, helpful for later
unique.locations$Airport <- if_else(is.na(unique.locations$City), TRUE, FALSE)



######################################################################
######################################################################
####### SECTION: LOCATION LOOK UP
# There are two options, either do a fresh look up of all locations or
# take the file from the list run (see variable do.geo.locating)

# If there is a fresh look up: the locations will be grouped into
# chunks such that the look up is faster. Be careful with fresh
# look ups, returns may change ==> different data set
if (do.geo.locating) {
  ########
  # Compute number of loops
  n.look.ups <- nrow(unique.locations)
  n.per.look.up <- 100
  n.loops <- ceiling(n.look.ups / n.per.look.up)
  
  # Add a progress bar
  progress.bar <- txtProgressBar(min = 1, max = n.loops, style = 3)
  
  # Start the actual look up locations in chunks
  look.up.result <- NULL
  for (i in 1:n.loops) {
    # Count progress
    setTxtProgressBar(progress.bar, i)
    
    # Set border for current chunk
    loop.start <- (i - 1) * n.per.look.up
    loop.end <- min(loop.start + n.per.look.up - 1, n.look.ups)
    
    # Do the actual look up --> if location fails, there would be an error, we suppress it
    look.up.data <- unique.locations[loop.start:loop.end,] %>% pull("LocationString")
    look.up.data <- invisible(suppressWarnings( # <-- Suppress the fail comment
      geocode_OSM(look.up.data, return.first.only = TRUE, details = TRUE, keep.unfound = TRUE)
    ))
    look.up.data <- look.up.data %>% select(query, lat, lon, lat_min, lat_max, lon_min, lon_max, display_name)
    
    # Store result
    look.up.result <- rbind(
      look.up.result,
      look.up.data
    )
  }
  
  # Transform result
  look.up.result <- look.up.result %>% rename(
    LocationString = query,
    Lat = lat,
    Lon = lon,
    MinLat = lat_min,
    MinLon = lon_min,
    MaxLat = lat_max,
    MaxLon = lon_max,
    Verification = display_name
  )
  
  # Add location data to locations data
  unique.locations <- unique.locations %>% left_join(look.up.result, by = "LocationString")
  look.up.result <- NULL
  close(progress.bar)
  
  
  
  ########
  # Data cleaning
  # i) remove obviously failed ones
  unique.locations$LocatedSuccessful <- (!is.na(unique.locations$Lat) & !is.na(unique.locations$Lon))
  # ii) Remove location outside the u.s. -> location failed probably due to typos in the adress,
  #     thank you deal scan ...
  us.spellings <- c("US", "U.S.", "United States", "United States of America")
  unique.locations$Country <- unique.locations %>% pull(Verification) %>% str_replace("^.*, ([^,]*)$", "\\1")
  unique.locations$WrongLocated  <- !(unique.locations$Country %in% us.spellings)
  
  
  
  ########
  # Some more frequent places failed to locate, so we fixed them by hand
  # The proper location is stored in an excel sheet, that we read in and add them to unique.locations
  fixed.locations <- paste0(working.dir, "Import/Not Located Places.xlsx")
  fixed.locations <- openxlsx::read.xlsx(fixed.locations, sheet = 1)
  
  # Remove those we couldnt fix --> they were not even by hand locatable ...
  fixed.locations <- fixed.locations %>% filter(!is.na(lat.lon))
  fixed.locations <- fixed.locations %>% filter(lat.lon != "")
  
  # Transform data
  fixed.locations <- fixed.locations %>% select(lat.lon, City, State, LocationString)
  fixed.locations <- fixed.locations %>% mutate(
    City = as.character(City),
    State = as.character(State),
    LocationString = as.character(LocationString),
    Lat = NA,
    Lon = NA
  )
  
  # Since Lat / Lon is stored in one column, we have to split them into two columns
  list.lat.lon <- fixed.locations %>% pull(lat.lon) %>% str_split(", ")
  
  # Loop each located place, note that i in list.lat.lon corresponds to the entry of 
  # i in fixed.locations
  for (i in 1:length(list.lat.lon)) {
    # Convert them as number, before they are strings
    lat <- list.lat.lon[[i]][1] %>% as.numeric()
    lon <- list.lat.lon[[i]][2] %>% as.numeric()
    
    
    # Find the matching observation in our location database
    if (fixed.locations[i,"City"] == "") {
      # It's an airport
      location.string <- fixed.locations[i,"LocationString"]
      match <- which(unique.locations$LocationString == location.string)
    } else {
      # It's an city
      city <- fixed.locations[i,"City"]
      state <- fixed.locations[i,"State"]
      match <- which(unique.locations$City == city & unique.locations$State == state)
    }
    
    # If this one exists, add the hand located coordinates
    if (length(match) > 0) {
      unique.locations[match,"Lat"] <- lat
      unique.locations[match,"Lon"] <- lon
      unique.locations[match,"LocatedSuccessful"] <- TRUE
      unique.locations[match,"WrongLocated"] <- FALSE
    }
  }
  
  list.lat.lon <- NULL
  fixed.locations <- NULL
  
  
  
  ########
  # Since we will use the coordinates in the later step to merge observations (more distance computations),
  # we have to make the locations less precise. Computers make decimals less precise, i.e. if we have now
  # 3.54654884 stored, it may be in another run 3.54654885. Therefore, look for matches fails at those ones.
  # If we limit it to 3 decimals, its fine and we don't loose significant precision.
  unique.locations <- unique.locations %>% mutate(
    Lat = round(Lat, digits = 3),
    Lon = round(Lon, digits = 3)
  )
  
  # Drop bad locations
  unique.locations <- unique.locations %>% filter(LocatedSuccessful)
  unique.locations <- unique.locations %>% filter(!WrongLocated)
  unique.locations <- unique.locations %>% distinct(LocationString, .keep_all = TRUE)
  
  
  #####
  # Store the location data
  locations.file <- paste0(working.dir, "Storage/GPS Locations.csv")
  write_csv(unique.locations, path = locations.file)
  
}

