# This file contains function that may be used by more than one step


#####################################################
###### General stuff

# Allows ceiling for decimals
ceiling.digits <- function(value, digits) {
  return(ceiling(value * 10^digits)/10^digits)
}


# Allows floor for decimals
floor.digits <- function(value, digits) {
  return(floor(value * 10^digits)/10^digits)
}


# Adds a zero to the front of the number, if number is in 0:9
# Yes, the function should be called add_lead_zero // realised that way too late to change it...
add_trailing_zero <- function(number) {
  return(
    ifelse(str_length(number) > 1, number, paste0("0", number))
  )
}


# Converts a date into a string
convert_date <- function(date) {
  year <- year(date)
  month <- month(date) %>% add_trailing_zero()
  day <- day(date) %>% add_trailing_zero()
  
  return(paste0(year, month, day))
}


# Makes NY to New York
debreviate_state_name <- function(abbreviations) {
  # Uniform it
  abbreviations <- str_to_upper(c(abbreviations))
  
  # Loop normal states
  for (i in 1:length(state.abb)) {
    abbreviations <- if_else((state.abb[i] == abbreviations), str_to_lower(state.name[i]), abbreviations)
  }
  
  # Handle DC
  abbreviations <- if_else("DC" == abbreviations, "district of columbia", abbreviations)
  # Handle not found
  abbreviations[which(str_length(abbreviations) < 3)] <- NA
  
  return(abbreviations)
}





#####################################################
###### ZIP CODES


# Function ensures that the zip code has five digits
five_digit_zip <- function(zip.code) {
  zip.code <- as.character(zip.code)
  
  for (i in 1:4) {
    zip.code[which(str_length(zip.code) == i)] <- paste0("0", zip.code[which(str_length(zip.code) == i)])
  }
  
  return(zip.code)
}

# We use a zip code database to translate zip codes into states to make up for missing data
# https://www.unitedstateszipcodes.org/zip-code-database/

# Load database
zip.codes <- read.csv(paste0(working.dir, "Storage/US Zip Code Database.csv"))
zip.codes <- zip.codes %>% select(zip, primary_city, state, county, latitude, longitude)
zip.codes <- zip.codes %>% rename(
  ZipCode = zip,
  City = primary_city,
  State = state,
  County = county,
  Lat = latitude,
  Lon = longitude
)
zip.codes <- zip.codes %>% mutate(
  ZipCode = five_digit_zip(ZipCode),
  City = str_to_lower(City),
  State = debreviate_state_name(as.character(State)),
  County = str_to_lower(County)
)


# Returns the state of zip code // expects a five digits zip code, returns NA if not existing
add_state_from_zip <- function(city.name, zip.code, database = zip.codes) {
  # Try to find zip code in database
  match <- database %>% filter(ZipCode == zip.code)
  
  if (nrow(match) > 0) {
    # Make sure that we picked the right city
    if (match[1,"City"] == city.name) {
      return(match[1,"State"])
    }
  }
  
  return(NA)
}