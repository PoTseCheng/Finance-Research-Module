# This file create a database of distances between every possible place

# ROAD MAP OF THIS FILE:
#
#    Section: Get Distance Constellations
# 1) Construct all possible constellations
#    Section: Build Distance Database
# 2) Prepare actual computation
# 3) Do the distance computation
# 4) Clean data
# 5) Store database
#
# Next step: Loan Details


# Why do we create such a database and not do this computation when
# demanded? There are so many repeated computations and a single
# computation takes rather long. If this file is built, we just need
# to do a look up for those couples, which is way faster.


######################################################################
######################################################################
####### SECTION: GET DITSNACE CONSTELLATIONS
# Constructs every possible distance computation, so from lender to
# airport, from airport to airport, and from borrower to airport for
# the actual computation

###########
# Get all places
locations.companies <- unique.locations %>% filter(!Airport)
locations.airports <- unique.locations %>% filter(Airport)

# Create the starting points -> either company (borrower, lender) or airport
from.coordinates <- tibble(
  Lat = c(locations.companies %>% pull(Lat), locations.airports %>% pull(Lat)),
  Lon = c(locations.companies %>% pull(Lon), locations.airports %>% pull(Lon))
)

# Create the end points -> always an airport
to.coordinates <- tibble(
  Lat = locations.airports %>% pull(Lat),
  Lon = locations.airports %>% pull(Lon)
)


######################################################################
######################################################################
####### SECTION: BUILD DISTANCE DATABASE
# In this part, we will do parallel computation. Even though it will
# take a bit until its done. In essence, we will loop each from point
# and get the distance to each to point

###########
# For the loop 
n.from <- nrow(from.coordinates)
n.to <- nrow(to.coordinates)

# Parallel stuff
cores <- (detectCores() - 2) * 2
cluster <- makeCluster(cores)
registerDoSNOW(cluster)
exporting.variables <- c("from.coordinates", "to.coordinates", "n.from", "n.to")


# Progressbar, so we don't stare at a screen that does nothing ;)
progress.bar <- txtProgressBar(min = 1, max = n.from, style = 3)
progressing <- function(n) {
  setTxtProgressBar(progress.bar, n)
}
options <- list(progress = progressing)



###########
# Actual distance computation
distances <- foreach (i = 1:n.from, .combine = rbind, .export = exporting.variables, .packages = c("dplyr", "stringr", "geosphere"), .options.snow = options) %dopar% {
  
  # This returns the final distance database
  return.data <- NULL
  
  # Pick data of current from point
  current.coordinate <- from.coordinates[i,]
  
  # Loop over every to point
  for (j in 1:n.to) {
    # Pick to point
    current.airport <- to.coordinates[j,]
    
    # Compute the distance (in metres)
    current.distance <- distm(
      c(current.coordinate$Lon, current.coordinate$Lat), c(current.airport$Lon, current.airport$Lat),
      fun = distHaversine
    )
    
    # Return result
    return.data <- rbind(
      return.data,
      tibble(
        FromLat = current.coordinate$Lat,
        FromLon = current.coordinate$Lon,
        ToLat = current.airport$Lat,
        ToLon = current.airport$Lon,
        Distance = current.distance[1,1]
      )
    )
  }
  
  return(return.data)
}


# End parallel computing stuff
close(progress.bar)
stopCluster(cluster)
cluster <- options <- progress.bar <-  NULL



###########
# Clean data
# Remove duplicates
distances <- distances %>% distinct(FromLat, FromLon, ToLat, ToLon, .keep_all = TRUE)
# Order by distance, shortest first
distances <- distances %>% arrange(Distance)



###########
# Store database
distance.database <- paste0(working.dir, "Storage/Distance Database.csv")
write_csv(distances, distance.database)

# Rename data, so let it look as if it were loaded fresh
distance.database <- distances

# Clean memory
distances <- NULL
from.coordinates <- to.coordinates <- NULL
locations.companies <- locations.airports <- NULL
