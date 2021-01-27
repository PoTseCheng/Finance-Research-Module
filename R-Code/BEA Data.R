# This file loads variables from the U.S. BEA

# Please note: To run this code fully, you need an api key for BEA.
#
# ROAD MAP OF THIS FILE:
#
#    Section: Determine Variables
# 1) 
#    Section: Loading Function
# 2) 
#    Section: Data Request
# 3) 
#
# Next step: CREDIT DEMAND CONTROL






######################################################################
######################################################################
####### SECTION: DETERMINE VARIABLES
# This section just specifies the variables we are interested in to get

bea.data <- tibble(
  Variable = c(
    "PerCapitalPersonalIncome",
    "TotalEmployment",
    "AverageWagesAndSalaries"
  ),
  Table = c(
    "CAINC1", #PerCapitalPersonalIncome
    "CAINC30", #TotalEmployment
    "CAINC30" #AverageWagesAndSalaries
  ),
  Column = c(
    "3", #PerCapitalPersonalIncome
    "240", #TotalEmployment
    "300" #AverageWagesAndSalaries
  )
)




######################################################################
######################################################################
####### SECTION: LOADING FUNCTION
# This section defines the function which will get the data from the
# BEA database for any of the define variables in the section above.

# Extracts a variable of bea.data (i.e. a row of it)
# - Returns the data for this variable
# - For all counties in the us
# - For the given time frame
# - Return: data frame
get_bea_data <- function(table, column, years = 1990:2020, api.key = api.key.bea) {
  # Prepare query
  years <- paste(years, collapse = ",")
  
  source <- "https://apps.bea.gov/api/data/?UserID="
  source <- paste0(source, api.key)
  source <- paste0(source, "&method=GetData&datasetname=Regional&TableName=")
  source <- paste0(source, table)
  source <- paste0(source, "&LineCode=")
  source <- paste0(source, column)
  source <- paste0(source, "&Year=")
  source <- paste0(source, years)
  source <- paste0(source, "&GeoFips=COUNTY&ResultFormat=json")
  
  
  # Run query
  result <- fromJSON(source)
  result <- result$BEAAPI$Results
  
  
  # Handle error
  colnames <- names(result)
  if (length(colnames) == 1 && colnames[1] == "Error") {
    return (tibble())
  }
  result <- result$Data
  
  # Transform data
  result <- map_dfr(result, function(observation) {
    # Extract county + state
    county <- observation["GeoName"] %>% str_replace("^(.+),\\s+([A-Z]{2})\\*?$", "\\1") %>% str_to_lower()
    state <- observation["GeoName"] %>% str_replace("^(.+),\\s+([A-Z]{2})\\*?$", "\\2") %>% str_to_lower()
    
    # Transform value
    period <- observation["TimePeriod"] %>% as.numeric()
    value <- ifelse(
      observation["DataValue"] == "(NA)",
      NA,
      observation["DataValue"] %>% str_replace(",", ".") %>% as.numeric()
    )
    #value <- observation["DataValue"] %>% str_replace(",", ".") %>% as.numeric()
    unit <- observation["CL_UNIT"]
    
    return(tibble(
      County = county,
      State = state,
      Period = period,
      Value = value,
      Unit = unit
    ))
  })
  
  return(result)
}




######################################################################
######################################################################
####### SECTION: DATA REQUEST
# This section downloads the data from the bea. This requires a key.
# Please also note, this take may take some time (~ 10 minutes).

# Note: This step requires an api key:
# https://apps.bea.gov/API/signup/index.cfm
# -- You can also use the downloaded files --


# If no api key is specified, just take the cached one.
if (api.key.bea != "") {
  
  # Load demanded variables
  print("########################################")
  print(paste0("STARTING DATA LOADING: ", Sys.time()))
  print(paste0("Variables to load: ", nrow(bea.data)))
  
  # Stores the received data
  data.set <- NULL
  
  # Loop each requested variables
  for (i in 1:nrow(bea.data)) {
    
    # Current progress
    print(paste0(i, " of ", nrow(bea.data), ": Variable ", as.character(bea.data[i,"Variable"]), " at ", Sys.time()))
    
    # Request
    data <- get_bea_data(bea.data[i,"Table"], bea.data[i,"Column"])
    data <- data %>% mutate(Variable = as.character(bea.data[i,"Variable"]))
    
    # Store
    data.set <- rbind(data.set, data)
  }
  
  # Progress finished
  print(paste0("All variables loaded, ", Sys.time()))
  print("########################################")
  
  
  # Store variables in file
  write_csv(data.set, path = paste0(working.dir, "Storage/County Characteristics.csv"))
  data <- data.set <- NULL
}

