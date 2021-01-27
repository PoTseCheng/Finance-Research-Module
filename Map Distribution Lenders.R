# This code replicates Lead Credit Arranger's Distribution Figure of the paper.

library(dplyr)
library(haven)
library(ggplot2)
library(ggmap)


# The Graph will be saved in the output folder of this code:
# {MAIN_FOLDER}/Output/LenderDistribution.png


# Adjust variables to generate plot properly
working.dir <- "C:/Users/Jan/Desktop/Research/"

# Fixed variables
dta.file <- "Output/For Non-Essential Graphs.dta"
map.file <- "Output/LenderDistribution.png"



if (working.dir == "DEFINE ME") {
  stop('Variable "working.dir" is not defined. Please define it in file {MAIN_FOLDER}/Map Distribution Lender.R Line 14')
}

if (!dir.exists(working.dir)) {
  stop('Variable "working.dir" is not defined properly in file {MAIN_FOLDER}/Map Distribution Lender.R Line 14. Please use the full path, i.e. "C:/Users/Jan/Replication/", make sure to use / instead of \\ .')
}

if (!file.exists(paste0(working.dir, dta.file))) {
  stop('Please run the Stata-Code first. Then run this code.')
}


# Load data set
data <- read_dta(paste0(working.dir, dta.file))
data <- data %>% filter(UsedObservation == 1)
data <- data %>% filter(LeadArrangerCredit == 1)

# Transform data
lenders <- tibble(
  Lon = data$LenderLon,
  Lat = data$LenderLat
)

# Group them by location, count them by packages and sort them
lenders <- lenders %>% group_by(Lat, Lon) 
lenders <- lenders %>% summarise(NumberPackages = n()) %>% arrange(-NumberPackages)

# Store them in a new data frame such that we can display it
lenders <- tibble(
  Lat = lenders$Lat,
  Lon = lenders$Lon,
  NumberPackages = lenders$NumberPackages
)

# Compute the percentages
lenders <- lenders %>% mutate(Percent = (lenders$NumberPackages / sum(lenders$NumberPackages)) * 100)


# Set us lat and lon boundary
us.boundary <- matrix(
  c(-130, -68, 25, 50),
  nrow = 2, byrow = TRUE
)


# Create map and add points
distribution.map <- get_map(us.boundary, maptype = "terrain-background", source = "osm", zoom = 6)
distribution.map <- ggmap(distribution.map)
distribution.map <- distribution.map + theme_void()
distribution.map <- distribution.map + theme(plot.margin = unit(c(0,-5,0,-5), "cm"))
distribution.map <- distribution.map + theme(legend.position = "bottom")
distribution.map <- distribution.map + theme(legend.text=element_text(size = 15))
distribution.map <- distribution.map + theme(legend.title=element_text(size = 15))
distribution.map <- distribution.map + geom_point(
  data = lenders,
  aes(x = Lon, y = Lat, size = Percent),
  colour = "#1D00FF"
)
distribution.map <- distribution.map + labs(size="% of Loans")
distribution.map

# Store map
ggsave(paste0(working.dir, map.file), height = 4.72, width = 9.15, distribution.map)


