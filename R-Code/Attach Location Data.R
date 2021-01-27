# This file attaches the locations to the final sample

# ROAD MAP OF THIS FILE:
#
#    Section: Merge Data
# 1) Load from python generated file
# 2) Do merging
# 3) Clean data
#
# Next step: Distance Database



######################################################################
######################################################################
####### SECTION: MREGE DATA
# Adds the obtained locations into our final sample data frame


####
# Load location data
unique.locations <- paste0(working.dir, "Storage/Full Location Data.csv")
unique.locations <- read_csv(unique.locations)



####
# Do the merging
# First the lenders ...
facility.data <- facility.data %>% left_join(
  unique.locations %>% select(City, State, County, Lon, Lat) %>% rename(
    LenderCity = City,
    LenderState = State,
    LenderCounty = County,
    LenderLat = Lat,
    LenderLon = Lon
  ),
  by = c("LenderCity", "LenderState")
)

# ... then borrowers
facility.data <- facility.data %>% left_join(
  unique.locations %>% select(City, State, County, Lon, Lat) %>% rename(
    BorrowerCity = City,
    BorrowerState = State,
    BorrowerCounty = County,
    BorrowerLat = Lat,
    BorrowerLon = Lon
  ),
  by = c("BorrowerCity", "BorrowerState")
)



###
# Clean data
# (i)  remove all without valid locations
facility.data <- facility.data %>% filter(!is.na(LenderLat) & !is.na(LenderLon))
facility.data <- facility.data %>% filter(!is.na(BorrowerLat) & !is.na(BorrowerLon)) 
# (ii) remove all without borrower county
facility.data <- facility.data %>% filter(!is.na(BorrowerCounty))



