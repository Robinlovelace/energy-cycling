# Script to load nts 2008-2012 data - downloaded in stata format from here:
# http://discover.ukdataservice.ac.uk/catalogue/?sn=5340&type=Data%20catalogue
# TODO: generate 3 scenarios upto 2050
# TODO: single stage-level file, only 2008-2012 included and only useful variables
# sort out inner_join re-ordering problem
library(foreign) # for loading stata files
library(dplyr) # for fast manipulation of data
# stages_dat <- read.dta("/media/robin/SAMSUNG/data/UKDA-5340-stata11-2/stata11/stage.dta")
stages <- as.data.frame(read.spss("/media/robin/SAMSUNG/data/UKDA-5340-spss/spss/spss19//stage.sav"))
names(stages) # which variables are we interested in?
stages <- stages %>% select(SurveyYear, StageID, TripID, DayID, IndividualID, HouseholdID, PSUID, VehicleID, StageMode_B04ID, StageDistance, StageTime )
stages <- rename(stages, mode = StageMode_B04ID, dist_smiles = StageDistance, time_smin = StageTime)
summary(stages$mode) / nrow(stages) # 1.713 % stages by bicycle

trips <- as.data.frame(read.spss("/media/robin/SAMSUNG/data/UKDA-5340-spss/spss/spss19//trip.sav"))
names(trips)
trips <- select(trips, SurveyYear, TripID, DayID, IndividualID, HouseholdID, PSUID, NumStages, MainMode_B04ID, TripPurpose_B04ID, TripTotalTime, JD)
trips <- rename(trips, modetrp = MainMode_B04ID, purp = TripPurpose_B04ID)
summary(trips$modetrp) / nrow(trips) # 1.719 % trips by bike

trips <- as.data.frame(read.spss("/media/robin/SAMSUNG/data/UKDA-5340-spss/spss/spss19/"))

# modes of travel: (do not show up coded from Stata data)

# load individual level data
ind <- as.data.frame(read.spss("/media/robin/SAMSUNG/data/UKDA-5340-spss/spss/spss19/individual.sav"))
names(ind)
ind <- select(ind, SurveyYear, IndividualID, HouseholdID, PSUID, Age_B01ID, Sex_B01ID, BicycleFreq_B01ID, WalkFreq_B01ID, OwnCycle_B01ID, Cycle12_B01ID, CycRoute_B01ID)

houses <- as.data.frame(read.spss("/media/robin/SAMSUNG/data/UKDA-5340-spss/spss/spss19/household.sav"))
names(houses)
summary(houses$HHoldGOR_B02ID)
houses <- select(houses, SurveyYear, HouseholdID, PSUID, HHoldGOR_B02ID, NumBike, NumCar, CycLane_B01ID)

# Filtering by year
stages <- filter(stages, SurveyYear > 2007)
trips <- filter(trips, SurveyYear > 2007)
ind <- filter(ind, SurveyYear > 2007)
houses <- filter(houses, SurveyYear > 2007)

save(stages, trips, ind, houses, file = "input-data/nts-08-12.RData")

# join all the data to stages:
stagej <- left_join(stages, trips)
stagej <- left_join(stagej, ind)
stagej <- left_join(stagej, houses)
object.size(stagej)



psuid <- as.data.frame(read.spss("/media/robin/SAMSUNG/data/UKDA-5340-spss/spss/spss19/psu.sav"))
names(psuid)
