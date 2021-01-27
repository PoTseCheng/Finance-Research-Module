# This file adds some data that we need but forgot to compute earlier

# ROAD MAP OF THIS FILE:
#
#    Section: Add Borrowers SIC Code
# 1) Load data from dealscan
# 2) Transform data
# 3) Merge it
#    Section: Add Distance Between Parties
# 5) Get coordinates of both parties
# 6) Compute distance
# 7) Merge it
#
# Next step: Prepare CompuStat




######################################################################
######################################################################
####### SECTION: ADD BORROWERS SIC CODE
# This section adds the borrowers company sic code.

###########
# First get a list of each company's sic code from dealscan
sic.codes <- paste0(working.dir, "Import/Company SIC Codes.csv")
sic.codes <- read_csv(sic.codes)


###########
# Then clean and transform that data
sic.codes <- sic.codes %>% select(-Ticker)
sic.codes <- sic.codes %>% rename(
  BorrowerCompanyID = CompanyID,
  BorrowerSICCode = PrimarySICCode
)
sic.codes <- sic.codes %>% filter(!is.na(BorrowerSICCode))


###########
# And add it to our final data set
facility.data <- facility.data %>% left_join(sic.codes, by = "BorrowerCompanyID")
sic.codes <- NULL




######################################################################
######################################################################
####### SECTION: ADD DISTANCE BETWEEN PARTIES
# This section computes for each observation the distance between
# borrower and lender


###########
# In a first step, we get all distance couples and remove duplicate ones (speed it up)
distances <- facility.data %>% distinct(LenderLat, LenderLon, BorrowerLon, BorrowerLat)
distances <- distances %>% mutate(BorrowerLenderDistance = NA)


###########
# Now compute the geographical distance between these couples, may take a few minutes
for (i in 1:nrow(distances)) {
  # Shortcuts
  BorrowerLon <- distances %>% slice(i) %>% pull(BorrowerLon)
  BorrowerLat <- distances %>% slice(i) %>% pull(BorrowerLat)
  LenderLon <- distances %>% slice(i) %>% pull(LenderLon)
  LenderLat <- distances %>% slice(i) %>% pull(LenderLat)
  
  # Distance computation (in metres)
  distance <- distm(c(BorrowerLon, BorrowerLat), c(LenderLon, LenderLat), fun = distHaversine)
  if (!is.na(distance) && distance >= 0) {
    distances[i,"BorrowerLenderDistance"] <- distance[1, 1]
  }
}

# Convert distance from metres in km
distances$BorrowerLenderDistance <- round(distances$BorrowerLenderDistance / 1000, digits = 2)


###########
# Now add distance to final data set
facility.data <- facility.data %>% left_join(
  distances,
  by = c("BorrowerLon", "BorrowerLat", "LenderLon", "LenderLat")
)


# Clean up
distances <- NULL

