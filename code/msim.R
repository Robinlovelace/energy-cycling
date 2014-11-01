# The purpose of this file is to load the individual and trip-level data
# Starting point - subset only those who travel to work - later allocate households/psuids they're in

library(foreign) # loading package - may need install.packages("foreign")
library(devtools)
library(dplyr) # library for rapidly manipulating datasets

psu <- as.data.frame(read.spss("/media/robin/SAMSUNG/data/DfT-NTS-2002-2008/UKDA-5340-spss/spss/spss12/psu.sav"))
psu <- rename(psu, gor = p2g, urbanity = p5a, popdens = p6)
psu <- select(psu, h96, psuid, gor, urbanity, PSUPopulation, popdens, LAPopulation)

indlst <- read.spss("/media/robin/SAMSUNG/data/DfT-NTS-2002-2008/UKDA-5340-spss/spss/spss12/individual.sav")
ind <- as.data.frame(indlst)
ind <- inner_join(ind, psu)

ind <- tbl_df(ind)
ind$i188 <- NULL
ind
ind <- rename(ind, yr = h96, house = h88, sex = i3, age = i6a, wplace = i92, employ = i177a, mode = i180, poss_home = i309, fhome = i310)
ind <- select(ind, yr, psuid, house, i1, i2, sex, age, wplace, employ, mode, poss_home, fhome, gor, urbanity, PSUPopulation, popdens, LAPopulation)

# which gors are people in? (filter out non English ones)
(gorsum <- summary(ind$gor))
sum(gorsum[!grepl("Wales|Scot", names(gorsum))])


summary(ind$employ)
indwrk <- filter(ind, grepl("time", employ)) # halves the size of ind - all in employment

# add home location and distance travelled to work
ntstrips <- as.data.frame(read.spss("/media/robin/SAMSUNG/data/DfT-NTS-2002-2008/UKDA-5340-spss/spss/spss12//trips.sav"))
ntstrips <- tbl_df(ntstrips)
# factors of interest from nts
ntstrips <- rename(ntstrips, yr = h96, house = h88, mode = j36a, stages = j23, purp = j28a, dist = jdungross,  region = j57g) # variables of interest - many more interesting ones in there e.g. time of travel, departure
ntstrips <- select(ntstrips, yr, psuid, house, i1, mode, stages, purp, dist, region)

## Run your scenarios here, creating new mode columns








# spatial microsimulation - allocation of individuals from the nts to las - TODO
commutes <- filter(ntstrips, purp == "Commuting") # all commuter trips
per_person <- group_by(commutes, house, i1, psuid)

commonest <- function(f){
  tt <- table(f)
  names(tt[which.max(tt)])
}

pc <- summarise(per_person, dist = median(dist)
  , region = commonest(region)
  , modet = commonest(mode) ) # per person commutes

# Tidying up pc data
pc$region <- as.factor(pc$region)
pc$modet <- as.factor(pc$modet)
pc$dist <- pc$dist/10
summary(pc)

# Join with individual-level data
indall <- inner_join(indwrk, pc)
summary(indall$dist)

# Loading the spatial data
library(gdata)
fn <- "input-data/las/las-mode-dis.xls" # to test the function
agg_names <- read.xls(fn, sheet = 1, skip = 7, header = F)[c(1,2)] # zone names/codes
agg_
i = 1
clean_save_xls <- function(fn, i){
  con <- read.xls(fn, sheet = i, skip = 7, header = F)[-c(1,2,3)] # save useful info
  con <- apply(con, 2, function(x) gsub(",", "", x)) # remove pesky commas
  con <- apply(con, 2, as.numeric) # convert to numeric
  con <- con[-nrow(con),] # remove last row
  con
}

con1_mfh <- clean_save_xls(fn, 2)[,4]
con1_pub <- clean_save_xls(fn, 3)[-c(4, 5)]
con1_card <- clean_save_xls(fn, 4)[-c(4, 5)]
con1_other <- clean_save_xls(fn, 5)[-c(4, 5)]
read.xls(fn, sheet = 1, skip = 6, nrow=1, header = F) # look at category names
mnames <- c("pub", "card", "otherm") # names of modes of transport
dnames <- c("lt10", "lt30", "gt31") # include 'other':
# http://www.nomisweb.co.uk/census/2011/lc7701ew
# 'Other' includes no fixed place of work
con1_names <- paste(rep(mnames, each = 3), rep(dnames, 3), sep = "_")
# NB - put individual-level variable in this format

con1 <- read.xls(fn, sheet = 1, skip = 7, header = F)[-c(1,2)]
con1$V3 <- gsub(",", "", con1$V3)
as.numeric(con1$V3)

for(1:length(fn))

con1_mfh <- read.xls("input-data/las/las-mode-dis.xls", sheet = 2, skip = 7, header = F)


head(con1)
