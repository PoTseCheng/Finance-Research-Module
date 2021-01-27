# This files stores the final data set as csv and makes it usable for stata




# Store data set
data.set <- paste0(working.dir, "Data Set.csv")
write_csv(facility.data, path = data.set)
