# This file extracts the basic facility / loan information

# ROAD MAP OF THIS FILE:
#
#    Section: Get Companies
# 1) We first load all firms of dealscan,
# 2) Kick non u.s. ones out
# 3) Get all facilities after 1989
#
#    Section Get Facilities
# 4) Keep only those where only u.s. firms are involved
#
#    Section Add Lender Information
# 5) First export all list of all kept facilities
# 6) Get the package information of those facilities from dealscan
# 7) Import that information and merge it with borrower information
#
# Next step: Geo Locating







######################################################################
######################################################################
####### SECTION: GET COMPANIES
# We first loaded every single firm in deal scan, then we filtered them by 
# country to keep only u.s. firms. After that we drop those firms lacking
# of essential information


# Get deal scan data (imported from dealscan)
company.data <- read_csv(paste0(working.dir, "Import/All Companies.csv"))


# Transform spellings --> everything in lowercase helps to group them
company.data$City <- company.data$City %>% str_to_lower
company.data$State <- company.data$State %>% str_to_lower

# Remove foreign and companies without city
company.data <- company.data %>% filter(Country == "USA")
company.data <- company.data %>% filter(City != "")

# Remove companies must have either have a zip code nor state
company.data <- company.data %>% filter(!is.na(ZipCode) | !is.na(State))

# Transform strange zip code (NY 54545 to 54545)
# -> Some obs have immediately a five digit zip code
# -> Some obs have the state in front of it (which is not required nor needed)
# -> Some obs have appendix (for postal internal purposes, should not bother us --> remove it)
company.data$ZipCode <- str_replace(company.data$ZipCode, "^[:alpha:]{2}[:blank:]{1}([:digit:]{5})$", "\\1")
company.data$ZipCode <- str_replace(company.data$ZipCode, "^([:digit:]{5})-[:digit:]+$", "\\1")

# We have to adjust for washington d.c., there is not a unique spelling for this
company.data[which(company.data$State == "washington, d.c."),]$State <- "district of columbia"
company.data[which(company.data$State == "district of columbia"),]$City <- "washington"


# This part adds states to observations where this information is missing. In other words, we try to 
# maintain as much observations as possible. For that purpose we use a zip-code and state database
#  https://www.unitedstateszipcodes.org/zip-code-database/
# Put zip code in -> get state
# The state information is essential and we can add it quite easily

# First get a list of all missing states
stateless.zip.codes <- company.data %>% filter(is.na(State)) %>% distinct(City, ZipCode)
stateless.zip.codes <- data.frame(
  ZipCode = stateless.zip.codes %>% pull(ZipCode),
  City = stateless.zip.codes %>% pull(City),
  State = NA,
  stringsAsFactors = FALSE
)

# Then add the zip code if possible
for (i in 1:nrow(stateless.zip.codes)) {
  state <- add_state_from_zip(stateless.zip.codes[i,"City"], stateless.zip.codes[i,"ZipCode"])
  
  if (!is.na(state)) {
    company.data[which(company.data$ZipCode == stateless.zip.codes[i,"ZipCode"]),"State"] <- state
  }
}
stateless.zip.codes <- NULL

# Drop those were the state is still missing --> We couldnt save them despite our best efforts
company.data <- company.data %>% filter(!is.na(State))
company.data$Domestic <- TRUE # This will help at some later stages
company.data <- company.data %>% select(-Ticker, -Country)




######################################################################
######################################################################
####### SECTION: GET FACIITIES
# As we exported every facility from 1990 and afterwards, we know need
# to filter them such that we only contain u.s. loans

# Load all available facilities (imported from dealscan)
# facility.data will be in the our final sample
facility.data <- read_csv(paste0(working.dir, "Import/Facilities - Since 1990.csv"))

# Remove loans closed before first day of our observation period
# Note that FacilityStartDate has the format YYYYMMDD, i.e. 01.01.2020 is 20200101 (normal integer)
facility.data <- facility.data %>% filter(FacilityStartDate >= observation.period[1] * 10000)

# Remove ticker, we dont need it
facility.data <- facility.data %>% select(-Ticker)

# Get the borrower infomration from our u.s. firms only list
potential.borrowers <- company.data %>% select(-Company) %>% rename(
  BorrowerCompanyID = CompanyID,
  BorrowerCity = City,
  BorrowerState = State,
  BorrowerZipCode = ZipCode,
  BorrowerDomestic = Domestic
)

# Now try to match those together
facility.data <- facility.data %>% left_join(potential.borrowers, by = "BorrowerCompanyID")
potential.borrowers <- NULL

# Remove foreign borrowers
facility.data <- facility.data %>% filter(BorrowerDomestic) %>% select(-BorrowerDomestic)




######################################################################
######################################################################
####### SECTION: ADD LENDER INFORMATION
# Since our facilitiy data right now is containing only borrowers information
# we will merge it with lender information to obtain the final information about
# borrower and lender

# Create a file that can be uploaded to wharton to get lender details for kept loans
kept.facilities <- facility.data %>% pull(FacilityID) %>% unique()
write(kept.facilities, paste0(working.dir, "Export/Facilities with Domestic Borrowers.txt"), ncolumns = 1)
kept.facilities <- NULL

# Now you upload that file to wharton and get back a csv with package information
# ....

# Load the deal scan file
lender.data <- read_csv(paste0(working.dir, "Import/Lenders Involved in Domestic Facilities.csv"))
lender.data <- lender.data %>% select(-BankAllocation) %>% rename(FacilityID = FACILITYID)

# Remove non domestic lenders by try to match with our u.s. firm only list
lender.data <- lender.data %>% left_join(company.data, by = "CompanyID")
lender.data <- lender.data %>% filter(Domestic) %>% select(-Domestic)

# We dont need company data anymore
company.data <- NULL


# Prepare lender data for merging with facility.data
lender.data[which(lender.data$LeadArrangerCredit == "Yes"),]$LeadArrangerCredit <- TRUE
lender.data[which(lender.data$LeadArrangerCredit == "No"),]$LeadArrangerCredit <- FALSE
lender.data$LeadArrangerCredit <- as.logical(lender.data$LeadArrangerCredit)
lender.data <- lender.data %>% select(-Company)
lender.data <- lender.data %>% rename(
  LenderCompanyID = CompanyID,
  LenderCity = City,
  LenderState = State,
  LenderZipCode = ZipCode
)

# Merge lender and facility data such that we have a data frame containing
# lenders, borrowers, and facility information. This is the base for our
# final sample

# facility.data -> contains just borrower information
# lender.data -> contains just lender and package information
facility.data <- lender.data %>% left_join(facility.data, by = "FacilityID")
lender.data <- NULL



