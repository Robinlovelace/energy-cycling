# Script to load nts 2012 data
# TODO: generate 3 scenarios upto 2050
# TODO: single stage-level file
library(foreign)
library(dplyr)
stages <- read.dta("/media/robin/SAMSUNG/data/UKDA-5340-stata11-2/stata11/stage.dta")
head(stages)

# filter-out the columns we don't need: