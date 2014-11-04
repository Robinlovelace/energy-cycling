# Script to load nts 2008-2012 data - downloaded in stata format from here:
# http://discover.ukdataservice.ac.uk/catalogue/?sn=5340&type=Data%20catalogue
# TODO: generate 3 scenarios upto 2050
# TODO: single stage-level file, only 2008-2012 included and only useful variables
# sort out inner_join re-ordering problem
library(foreign) # for loading stata files
library(dplyr) # for fast manipulation of data
# stages_dat <- read.dta("/media/robin/SAMSUNG/data/UKDA-5340-stata11-2/stata11/stage.dta")
stages <- as.data.frame(read.spss("/media/robin/SAMSUNG/data/UKDA-5340-spss/spss/spss19//stage.sav"))
show(stages)
names(stages) # which variables are we interested in?
stages <- stages %>% select(SurveyYear, StageID, TripID, DayID, IndividualID, HouseholdID, PSUID, VehicleID, StageMode_B04ID, StageDistance, StageTime )
stages <- rename(stages, mode = StageMode_B04ID, dist_smiles = StageDistance, time_smin = StageTime)
summary(stages$mode) / nrow(stages) # 1.713 % stages by bicycle

trips <- as.data.frame(read.spss("/media/robin/SAMSUNG/data/UKDA-5340-spss/spss/spss19//trip.sav"))
names(trips)
trips <- select(trips, SurveyYear, TripID, DayID, IndividualID, HouseholdID, PSUID, NumStages, MainMode_B04ID, TripPurpose_B04ID, TripTotalTime, JD)
trips <- rename(trips, modetrp = MainMode_B04ID, purp = TripPurpose_B04ID)
summary(trips$modetrp) / nrow(trips) # 1.719 %

# modes of travel: (do not show up coded from Stata data)

# load individual level data