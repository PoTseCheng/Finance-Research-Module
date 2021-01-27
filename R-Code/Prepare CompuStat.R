# This file allows to link compustat's and dealscan's borrowers

# ROAD MAP OF THIS FILE:
#
#    Section: Add Linkage
# 1) Load linkage file
# 2) Transform data
# 3) Merge and clean it
#    Section: Add Data
# 4) -- Let blank, see below --
#
# Next step: BEA Data

# Please note: We initially wanted to do also the tightness similar
# to Hollander and Verriest (2015). Therefore, we decided quite early
# that we need the data of CompuStat. Until we realised that this is
# too much for this module -- aka we noticed during writing that we
# are running out of space. In other words: we merge compustat but
# we have never touched this data in our paper. However, at that
# stage it was too late to go back to dealscan only, since the flight
# routes were computed for the CompuStat + DealScan data not for 
# DealScan data. This implies we would have to compute a bunch of
# additional flight routes, given the limited time left, this seemed
# in our eyes not realistic.




######################################################################
######################################################################
####### SECTION: ADD LINKAGE
# This section allows to link data of borrowers from dealscan also in
# compustat.

###########
# Load file
linkage <- paste0(working.dir, "Import/Linkage Dealscan Compustat.xlsx")
linkage <- openxlsx::read.xlsx(linkage, sheet = "link_data")


###########
# Transform data
linkage <- linkage %>% distinct(bcoid, .keep_all = TRUE)
linkage <- linkage %>% select(bcoid, gvkey) %>% rename(BorrowerGvKey = gvkey)


###########
# Merge
facility.data <- facility.data %>% left_join(linkage, by = c("BorrowerCompanyID" = "bcoid"))

# Clean data -> remove all observations with link between DealScan and CompuStat
facility.data <- facility.data %>% filter(!is.na(BorrowerGvKey))


# Clean up memory
linkage <- NULL



######################################################################
######################################################################
####### SECTION: ADD DATA
# See reasoning in the top of this file.

