# This file adds facility, package, and finanical covenant data

# ROAD MAP OF THIS FILE:
#
#    Section: Add Package and Facility Data
# 1) Prepare request for dealscan
# 2) Export needed package and facilities
# 3) Merge new facility data
# 4) Merge new package data
#    Section: Add financial covenants
# 5) Load dealscan database
# 6) Transform data
# 7) Merge data
#
# Next step: Remaining Data




######################################################################
######################################################################
####### SECTION: ADD PACKAGE AND FACILITY DATA
# We add some additional data from dealscan in this section that is
# related to the package / facility, for instance package id. This is
# necessary since we loaded just the bare mininum of data to keep merging
# fast.

###########
# First we need to get the ids of packages and facilities where we 
# want additional data
package.ids <- facility.data %>% pull(PackageID) %>% unique()
facility.ids <- facility.data %>% pull(FacilityID) %>% unique()



###########
# Then store them such that we can request them from deal scan from a file
write(package.ids, paste0(working.dir, "Export/Details - Package IDs.txt"), ncolumns = 1)
write(facility.ids, paste0(working.dir, "Export/Details - Facility IDs.txt"), ncolumns = 1)

# clear memory
package.ids <- facility.ids <- NULL



###########
# This step requires the manual request at deal scan



###########
# Then we load the from dealscan obtain facility data
facility.details <- paste0(working.dir, "Import/Facility - Details.csv")
facility.details <- read_csv(facility.details)

# Transform the data
facility.details <- facility.details %>% select(-FacilityStartDate, -BorrowerCompanyID, -Ticker)
facility.details <- facility.details %>% rename(FacilityID = FACILITYID)

# And merge it
facility.data <- facility.data %>% left_join(facility.details, by = c("PackageID", "FacilityID"))
facility.details <- NULL



###########
# Then we load the from dealscan obtain package data
package.details <- paste0(working.dir, "Import/Package - Details.csv")
package.details <- read_csv(package.details, col_types = cols(
  PACKAGEID = col_double(),
  BorrowerCompanyID = col_double(),
  Ticker = col_character(),
  DealActiveDate = col_double(),
  DealAmount = col_double(),
  TermChanges = col_double(),
  AssetSalesSweep = col_double(),
  DebtIssuanceSweep = col_double(),
  EquityIssuanceSweep = col_double(),
  DividendRestrictions = col_character()
))

# Transform the data
package.details <- package.details %>% select(-BorrowerCompanyID, -Ticker)
package.details <- package.details %>% rename(PackageID = PACKAGEID)

# And merge it
facility.data <- facility.data %>% left_join(package.details, by = "PackageID")
package.details <- NULL





######################################################################
######################################################################
####### SECTION: ADD FINANCIAL COVENANTS DATA
# This section adds the financial covenants data from dealscan. This is
# for instance used for the cii.

###########
# Load the data, can take a bit
data.covenants <- paste0(working.dir, "Import/financialcovenant.sas7bdat")
data.covenants <- read.sas7bdat(data.covenants)



###########
# Transform the data
data.covenants <- data.covenants %>% select(PackageID, CovenantType, InitialRatio)
data.covenants <- data.covenants %>% zap_formats()
data.covenants <- data.covenants %>% filter(!is.nan(InitialRatio))


# This database has for each package and each covenant a entry, therefore, we must 
# aggregate the covenants such that each package has just one row containing all covenants

# First, we check whether each package has at most one covenant type other,
# if so, we can jsut create a data frame, where each covenant is added as 
# column. SPOILER: it is the case ...

# Check it yourself:
# data.covenants %>% filter(CovenantType == "Other Ratio") %>% group_by(PackageID) %>% count() %>% View

# Now get the name of all covenants, such that we can create a column for each one.
# However, first we must transform them, since the types have names longer than 35 chars,
# but stata allows at most 35 char.

# Get the name of all types
covenant.types <- levels(data.covenants$CovenantType)

# Abbreviate them
covenant.types <- map(covenant.types, function(covenant.type) {
  initial.value <- covenant.type
  covenant.type <- str_to_title(covenant.type)
  
  for (pattern in c("\\.", "-", "[:blank:]", "\\(.+\\)")) {
    covenant.type <- str_replace_all(covenant.type, pattern, "")
  }
  
  covenant.type <- str_replace_all(covenant.type, "Net", "N")
  covenant.type <- str_replace_all(covenant.type, "Debt", "D")
  covenant.type <- str_replace_all(covenant.type, "Equity", "E")
  covenant.type <- str_replace_all(covenant.type, "Assets", "A")
  covenant.type <- str_replace_all(covenant.type, "Asset", "A")
  covenant.type <- str_replace_all(covenant.type, "Worth", "W")
  covenant.type <- str_replace_all(covenant.type, "Tangible", "T")
  covenant.type <- str_replace_all(covenant.type, "Interest", "I")
  covenant.type <- str_replace_all(covenant.type, "Coverage", "C")
  covenant.type <- str_replace_all(covenant.type, "Investment", "Inv")
  covenant.type <- str_replace_all(covenant.type, "Leverage", "Lev")
  covenant.type <- str_replace_all(covenant.type, "Senior", "Sen")
  covenant.type <- str_replace_all(covenant.type, "Long", "Lon")
  
  return(c(initial.value, covenant.type))
})


# Now we create a data frame that has a row of each package that has a
# covenant. Then, we subsequently add each covenant type and its values
# as a new column
data.covenants.prepared <- tibble(
  PackageID = data.covenants %>% pull("PackageID") %>% unique()
)

# This is the prefix of the variables, to ensure we can identify them quickly
prefix.covenants <- "Cov"

# Now create that famous column for each
for (covenant.type in covenant.types) {
  # Rename to keep track of them
  covenant.name.initial <- covenant.type[1]
  covenant.name.transformed <- covenant.type[2]
  
  # Prepare data for merge into the final frame
  covenant.details <- data.covenants %>% filter(CovenantType == covenant.name.initial) %>% select(-CovenantType)
  colnames(covenant.details) <- paste0(prefix.covenants, covenant.name.transformed, colnames(covenant.details))
  
  # Merge it
  data.covenants.prepared <- data.covenants.prepared %>% left_join(
    covenant.details, by = c("PackageID" = paste0(prefix.covenants, covenant.name.transformed, "PackageID"))
  )
}


###########
# Just merge it and then clean up
facility.data <- facility.data %>% left_join(data.covenants.prepared, by = "PackageID")


# clean up
data.covenants.prepared <- NULL
data.covenants <- NULL
covenant.types <- NULL
covenant.details <- NULL
number.covenants <- NULL


