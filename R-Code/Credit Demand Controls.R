# This file creates a list of economic indicators of the current
# month and months before that and adds them to sample


# We basically create a hugh matrix for the economic indicators
# such that we can merge them with the final sample. The matrix
# looks like this:
#
# - For state-level data:
# State | Month | Unemployment t = Month | Unemployment t = Month - 1 | .. | Unemployment t = Month - max.lag
# - For county-level data
# County | Month | GDP t = Month | GDP t = Month - 1 | .. | GDP t = Month - max.lag
#
# We use then this matrix and merge them by County/State, Year,
# Month of our final sample. That way, we have for each ob-
# servation the economic variables for the current month,
# the month before, ...


# Note this process is quite time consuming.


# ROAD MAP OF THIS FILE:
#
#     Section: State-Level Matrix
# 1)  Load and transform data
# 2)  Define helping functions
# 3)  Define matrix creator function
# 4)  Create matrix itself
# 5)  Merge data
#     Section: County-Level Matrix
# 6)  Load and transform data
# 7)  Define helping functions
# 8)  Define matrix creator function
# 9)  Create matrix itself
# 10) Merge data
#
# Next step: Flight Routes


# Add helper variables
facility.data <- facility.data %>% mutate(
  FacilityStartYear = str_sub(FacilityStartDate, 1, 4),
  FacilityStartMonth = str_sub(FacilityStartDate, 5, 6)
)




######################################################################
######################################################################
####### SECTION: STATE-LEVEL MATRIX
# This section creates the above mentioned matrix for state-level data
# and merges it to the final sample

###########
# Load unemployment data
state.level.data <- paste0(working.dir, "Storage/State Characteristics.csv")
state.level.data <- read_csv(state.level.data, col_types = cols(
  year = col_double(),
  period = col_double(),
  value = col_double(),
  StateID = col_character(),
  StateCode = col_character()
))
# Yes, this line above raises an warning, but it is nothing bad. R is
# just so kind and says there is an NA in a column where he expects
# only numerical values.

# Transform
state.level.data <- state.level.data %>% filter(StateCode != "Puerto Rico")
state.level.data <- state.level.data %>% select(-StateID)
state.level.data <- state.level.data %>% rename(Year = year, Month = period, Value = value)
state.level.data <- state.level.data %>% mutate(StateCode = str_to_lower(StateCode))


###########
# We define a helping function. It returns the value of the economic
# indicator or NA if it not exists.
get_unemployment_rate <- function(state, month, year, unemployment.data) {
  unemployment.rate <- unemployment.data %>% filter(StateCode == state & Month == month & Year == year)
  
  if (nrow(unemployment.rate) == 1) {
    return(unemployment.rate %>% pull("Value"))
  }
  
  return(NA)
}


###########
# The following function generates the above mentioned matrix for
# states. It will returns something like that for the given state:
# State | Month | Unemployment t = Month | Unemployment t = Month - 1 | .. | Unemployment t = Month - max.lag
# L0 = Unemployment t = Month 
# L1 = Unemployment t = Month - 1
get_unemployment_lags <- function(state.code, max.lags, unemployment.data = state.level.data, years = 1990:2020) {
  state.matrix <- NULL
  
  for (year in years) {
    for (month in 1:12) {
      current.unemployment <- tibble(
        State = state.code,
        Year = year,
        Month = month
      )
      
      for (lag in 0:max.lags) {
        lagged.date <- ymd(paste0(year, add_trailing_zero(month), "01")) - months(lag)
        unemployment.rate <- get_unemployment_rate(state.code, month(lagged.date), year(lagged.date), unemployment.data)
        
        new.column <- paste0("L", lag, "StateUnemployment")
        current.unemployment[,new.column] <- unemployment.rate
      }
      
      state.matrix <- rbind(
        state.matrix,
        current.unemployment
      )
    }
  }
  
  return(state.matrix)
}


###########
# In this step we acutally create the state-level data matrix,
# if wished so. We basically create this matrix for each state
# and merge them then together. Note this step is rather time
# intensive ( ~ 10 / 15 min).

# Contains the matrix
state.level.matrix <- NULL

# Create matrix only if so wished
if (create.economic.indicator.matrices) {
  
  # Loop each state
  states <- state.level.data %>% pull("StateCode") %>% unique()
  for (state in states) {
    # Get data of a state
    state.data <- get_unemployment_lags(state, max.lags, state.level.data)
    
    # Bind them to the entire matrix
    state.level.matrix <- rbind(
      state.level.matrix,
      state.data
    )
  }
  
  # Transform data (Months and Year must be string to merge properly)
  state.level.matrix <- state.level.matrix %>% mutate(
    Month = add_trailing_zero(Month),
    Year = as.character(Year)
  )
  
  file <- paste0(working.dir, "Storage/Economic Indicators - State Level.csv")
  write_csv(state.level.matrix, path = file)
  
  state.level.matrix <- state.data <- NULL
}

###########
# This merges both together, our state level indicators
# and final sample

# Load matrix
if (is_null(state.level.matrix)) {
  state.level.matrix <- paste0(working.dir, "Storage/Economic Indicators - State Level.csv")
  state.level.matrix <- read_csv(state.level.matrix, col_types = cols(
    State = col_character(),
    Year = col_character(),
    Month = col_character(),
    L0StateUnemployment = col_double(),
    L1StateUnemployment = col_double(),
    L2StateUnemployment = col_double(),
    L3StateUnemployment = col_double(),
    L4StateUnemployment = col_double()
  ))
}


# This part converts the state abbreviation to the long name such
# that we can merge it to our final sample which uses the long state
# name in lowercase letters
state.level.matrix <- state.level.matrix %>% mutate(State = str_to_upper(State))
state.level.matrix <- state.level.matrix %>% left_join(
  tibble(
    StateName = str_to_lower(state.name),
    StateCode = state.abb
  ), 
  by = c("State" = "StateCode")
)
state.level.matrix <- state.level.matrix %>% select(-State)

# Fix D.C.
dc <- which(is.na(state.level.matrix$StateName))
state.level.matrix[dc, "StateName"] <- "district of columbia"


# Merge it with the final sample
facility.data <- facility.data %>% left_join(
  state.level.matrix,
  by = c(
    "BorrowerState" = "StateName",
    "FacilityStartMonth" = "Month",
    "FacilityStartYear" = "Year"
  )
)

state.level.data <- state.level.matrix <- NULL




######################################################################
######################################################################
####### SECTION: COUNTY-LEVEL MATRIX
# This section creates the above mentioned matrix for county-level
# data and merges it to the final sample

###########
# This section basically loads and transforms the data

# Load county level data
county.level.data <- paste0(working.dir, "Storage/County Characteristics.csv")
county.level.data <- read_csv(county.level.data)

# Transform
county.level.data <- county.level.data %>% rename(Year = Period)

# Make county name unique, since some counties name exist twice within
# the us but are not the same county (State - County is unique)
county.level.data <- county.level.data %>% mutate(
  UniqueCounty = paste(County, State, sep = ", ")
)


###########
# We define a helping function. It returns the value of the economic
# indicator or NA if it not exists.
get_economy_variable <- function(year, economy.data) {
  observations <- economy.data %>% filter(Year == year)
  
  if (nrow(observations) == 1) {
    return(observations %>% pull("Value"))
  }
  
  return(NA)
}


###########
# The following function generates the above mentioned matrix for
# counties. It will returns something like that for the given state:
# County | Month | GPD t = Month | GPD t = Month - 1 | .. | GPD t = Month - max.lag
# L0 = GPD t = Month 
# L1 = GPD t = Month - 1
get_economy_lags <- function(variable, county, max.lags, economy.data = county.level.data, years = 1990:2020) {
  county.matrix <- NULL
  economy.data <- economy.data %>% filter(UniqueCounty == county)
  
  for (year in years) {
    for (month in 1:12) {
      current.economy.data <- tibble(
        Variable = variable,
        County = county,
        Year = as.character(year),
        Month = add_trailing_zero(month)
      )
      
      for (lag in 0:max.lags) {
        lagged.date <- ymd(paste0(year, add_trailing_zero(month), "01")) - months(lag)
        value <- get_economy_variable(year(lagged.date), economy.data)
        
        new.column <- paste0("L", lag, variable)
        current.economy.data[,new.column] <- value
      }
      
      county.matrix <- rbind(
        county.matrix,
        current.economy.data
      )
    }
  }
  
  return(county.matrix)
}


###########
# In this step we acutally create the county-level data matrix,
# if wished so. We basically create this matrix for each county
# and merge them then together. Note this step is time intensive,
# it takes way longer than for states ( ~ 8 hours ).

# Contains the matrix
county.level.matrix <- NULL

# Create matrix only if so wished
if (create.economic.indicator.matrices) {
  # Get unique obs
  variables <- county.level.data %>% pull("Variable") %>% unique()
  counties <- county.level.data %>% pull("UniqueCounty") %>% unique()
  
  # Since this is so time intensive, we use parallel computing
  # Parallel computing stuff:
  n.cores <- max(6, length(variables))
  cluster <- makeCluster(n.cores)
  registerDoSNOW(cluster)
  
  progress.bar <- txtProgressBar(min = 1, max = length(variables), style = 3)
  progressing <- function(n) {
    setTxtProgressBar(progress.bar, n)
  }
  
  exporting.packages <- c("dplyr", "stringr", "lubridate")
  exporting.normal <- c(
    # Functions
    "add_trailing_zero", "get_economy_lags", "get_economy_variable",
    # Variables
    "variables", "counties", "max.lags", "county.level.data"
  )
  
  exporting.options <- list(progress = progressing)
  
  
  
  # This generates the entire matrix for each county
  county.level.matrix <- foreach (i = 1:length(variables), .export = exporting.normal, .packages = exporting.packages, .options.snow = exporting.options) %dopar% {
    variable <- variables[i]
    
    # Filter data
    lag.data <- county.level.data %>% filter(Variable == variable)
    lag.matrix <- NULL
    
    # Loop each county
    if (length(counties) > 0) {
      for (county in counties) {
        # Get data of a county
        county.data <- get_economy_lags(variable, county, max.lags, lag.data)
        
        # Bind them to the entire matrix
        lag.matrix <- rbind(
          lag.matrix,
          county.data
        )
      }
    }
    
    return(lag.matrix)
  }
  
  
  
  # Parallel computing stuff
  stopCluster(cluster)
  cluster <- progress.bar <- NULL
  exporting.packages <- exporting.normal <- exporting.options <- NULL
  
  
  # Store file, must be a rds since csv cant store lists
  file <- paste0(working.dir, "Storage/Economic Indicators - County Level.csv")
  write_rds(county.level.matrix, path = file)
  
  county.level.matrix <- NULL
}




###########
# This merges both together, our county level indicators
# and final sample

# Load matrix
if (is_null(county.level.matrix)) {
  county.level.matrix <- paste0(working.dir, "Storage/Economic Indicators - County Level.rds")
  county.level.matrix <- read_rds(county.level.matrix)
}


# Make matrix to data frame, i.e. row contains every variable
# for the county -- we basically convert the list to a data frame
credit.demand.data <- county.level.matrix[[1]] %>% select(-Variable)
for (i in 2:length(county.level.matrix)) {
  credit.demand.data <- credit.demand.data %>% left_join(
    county.level.matrix[[i]] %>% select(-Variable),
    by = c("County", "Year", "Month")
  )
}
county.level.matrix <- NULL


# Do match facility.data counties with the naming scheme of
# the bea ... why do they must have different onces ... of
# course we will have some that dont get matchted.
county.linking <- paste0(working.dir, "Storage/County Matching.csv")
county.linking <- read_csv(county.linking)
county.linking <- county.linking %>% mutate(
  CsvUniqueCounty = str_to_lower(paste(CsvCounty, CsvState, sep = ", "))
)

# csv: data from bea
# dta: facility.data
credit.demand.data <- credit.demand.data %>% left_join(
  county.linking,
  by = c("County" = "CsvUniqueCounty")
)
credit.demand.data <- credit.demand.data %>% select(-CsvState, -CsvCounty)
credit.demand.data <- credit.demand.data %>% select(-County)


# Then finally merge both
facility.data <- facility.data %>% left_join(
  credit.demand.data,
  by = c(
    "BorrowerState" = "DtaState",
    "BorrowerCounty" = "DtaCounty",
    "FacilityStartYear" = "Year",
    "FacilityStartMonth" = "Month"
  )
)


# Remove helper variables
facility.data <- facility.data %>% select(-FacilityStartYear, -FacilityStartMonth)


# Clean up
county.linking <- NULL
county.level.data <- NULL
credit.demand.data <- NULL



