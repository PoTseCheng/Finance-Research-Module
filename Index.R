# This is the master file. It calls required files to replicate the data set.
# Please adjust the variable working.dir where to store this folder.

# Please note: We also used python. Therefore, if you want to fully reproduce
# the data set, you have to execute some python code. Look at the comment
# in this file (2 Places). The following libraries are required:
# (i) Pandas, (ii) Geopy, (iii) Nump, (iv) Heapq, and (v) Collections
# Python Version: at least 3.7.7

# Furthermore, this code calls also at some point python code within R. This
# code requires the following python libraries to be installend:
# (i) , (ii)

# The code is written in such a way that the computational intensive stuff
# is cached. However, if you change all variables in the Set-up Variables
# Section to TRUE, it will not use the cached versions. And you will need
# an api key for BEA (https://apps.bea.gov/API/signup/index.cfm). The 
# computation will take ages, literally (!!). I would roughly estimate
# more than 4 days pure computation.


# THERE EXISTS A SAMPLING OPTION: do.flight.routing.test <- TRUE
# -> But please be aware: The sampling option has been introduced
#    very late (~ 1 week before submission) such that you can check our
#    flight routing. Within the default settings, the sampling run without
#    errors (see the corresponding comment part). You can also import that
#    data set in Stata, but of course, running our main analysis may lead
#    to strange result (for instance we exclude any observations before 1996,
#    but the sampling option takes it sample from packages in 1990)


# ROAD MAP OF THIS FILE:
#
#    Section: Required Libraries
#    Section: Set-up Variables
#    Section: Data Set Creation
#    Section: Storing Data Set


# Allows working with hugh excel files
options(java.parameters = "-Xmx4g")




######################################################################
######################################################################
####### SECTION: REQUIRED LIBRARIES

# Libraries needed:
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tmaptools)
library(openxlsx)
library(doParallel)
library(doSNOW)
library(geosphere)
library(sas7bdat)
library(haven)
library(purrr)
library(RJSONIO)
library(matrixStats)
library(reticulate)




######################################################################
######################################################################
####### SECTION: SET-UP VARIABLES

# Set up variables
working.dir <- "C:/Users/Jan/Desktop/Research/"

do.geo.locating <- FALSE
create.distance.database <- FALSE
create.economic.indicator.matrices <- FALSE
do.flight.routing.test <- TRUE

# Note: This step requires an api key -- otherwise a cached version
# will be used    https://apps.bea.gov/API/signup/index.cfm
api.key.bea <- ""

# More or less hard coded -> you can change it but be aware that we
# don't expected someone to change them
observation.period <- 1990:2020
max.lags <- 4

# Load required functions
file <- paste0(working.dir, "R-Code/Functions.R")
source(file)




######################################################################
######################################################################
####### SECTION: DATA SET CREATION

# First get the basic data from deal scan and remove not useful data
# For instance: non-us firms or missing state / city observations
file <- paste0(working.dir, "R-Code/Import DealScan.R")
source(file)


# Then we use OpenStreetMaps to geo locate the remaining lenders and
# borrowers (we obtain Lat / Lon)
file <- paste0(working.dir, "R-Code/Geo Locating.R")
source(file)


# Further, we get the county of a borrower, so basically its entire 
# address. We can't rely on the county in deal scan, often it is missing
# We will use python to get their county.
if (do.geo.locating) {
  # PYTHON CODE
  # Execute the following python file:
  # "{working.dir}/Python Code/County matching/County.ipynb"
  
  # The python code reads the location file and adds the county to it.
  # It will store the new data in Storage/Full Location Data.csv
  
  # Input:  Storage/GPS Locations.csv;
  # Output: Storage/Full Location Data.csv
}


# Attaching the freshly obtained locations
file <- paste0(working.dir, "R-Code/Attach Location Data.R")
source(file)


# Create a cache file that stores all distances between borrower,
# lenders, and airports. We call it distance database, only created
# if wished so, otherwise, there is cached version
if (create.distance.database) {
  file <- paste0(working.dir, "R-Code/Distance Database.R")
  source(file)

# Just load distance database
} else {
  distance.database <- paste0(working.dir, "Storage/Distance Database.csv")
  distance.database <- read_csv(distance.database)
}


# Adding loan details. In the first run we extracted the bare
# minimum out of dealscan to keep the data set as small as possible
file <- paste0(working.dir, "R-Code/Loan Details.R")
source(file)


# Add SIC Code to borrowers and also distance between borrower lender
# (we forgot to include the SIC Code earlier as well as the distance)
file <- paste0(working.dir, "R-Code/Remaining Data.R")
source(file)


# Prepare linkage to compustat @@@@
file <- paste0(working.dir, "R-Code/Prepare CompuStat.R")
source(file)


# Download data from BEA (county economic indicators for credit demand)
file <- paste0(working.dir, "R-Code/BEA Data.R")
source(file)


# PYTHON CODE
# Execute the following python file:
# "{working.dir}/Python Code/Unemployment API/Umemployment.ipynb"
#
# The python code requests state economic indicators for for credit
# demand controls. The code stores this in:
# Output: Storage/State Characteristics.csv


# Adds the credit demand controls (basing on BEA / BLS data)
file <- paste0(working.dir, "R-Code/Credit Demand Controls.R")
source(file)


# Do the actual flight routing (estimating flight time)
# Our treatment variables bases on this flight time
file <- paste0(working.dir, "R-Code/Flight Routes.R")
source(file)


# There is a possibility of subsampling the input for flight
# routes to check the code (routing all observations would
# take around 4 days). Just because we know there is hugh data
# existing, we assumed there is also sufficient data, i.e.
# we would never run in a condition where there is no return
# or an empty loop. Errors could occour in cases there are no
# packages in a month (2.5k, there is at least one in any
# month ...). Under the following conditions, it is likely
# that those cases do not occur:
# - At least two lags (if you want to use the data set
#                      in stata, you will need all 4)
# - At least one entire year
# - The subsample contains at least a few facilities
#   in each month of the year
# - Subsample of n > 2500 could fulfil this requirements
# => The computation happens in Section 3 of the script
#    just look where the section starts that calls
#    "compute_air_traveling()", should start at line 650
#
# OR JUST SET do.flight.routing.test = TRUE



######################################################################
######################################################################
####### SECTION: STORING DATA SET

file <- paste0(working.dir, "R-Code/Prepare Stata.R")
source(file)



