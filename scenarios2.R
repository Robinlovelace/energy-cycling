# scenarios2 - new scenarios of growth in cycling over next 35 years
# builds on scenarios.R

# Load the libraries we'll use
library(dplyr) # data manipulation
library(xlsx) # package for loading M$ Excel files
library(downloader) # for downloading dft data from https link

# Load relevant DfT data
# download("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/336231/nts0304.xls", destfile = "input-data/nts0304.xls")
nts0304 <- read.xlsx("input-data/nts0304.xls", sheetIndex = 1)
# Select only bike and all modes
nts0304 <- nts0304[grep("Bicycle|All modes", nts0304[,1]), ]
# Remove na values
nts0304 <- nts0304[!is.na(nts0304[,1]),]
# Select only year-by year columns
nts2013 <- data.frame(t(nts0304[,-1]))
nts2013$year <- c(1996, 1999, 2002:2013)
nts2013[1:2] <- apply(nts2013[1:2], MARGIN = 2, as.numeric)
names(nts2013)
nts2013 <- rename(nts2013, bike_trips_ppy = X10, all_stages = X23)
nts2013$perc_bike <- nts2013$bike_trips_ppy / nts2013$all_stages
plot(nts2013$perc_bike) # proportion of stages per person per year
plot(nts2013$bike_trips_ppy) # absolute number of bike trips/person/yr
plot(nts2013$year, nts2013$all_stages) # declining number of stages per person/yr

# Assumptions about the future
# 1: number of stages/person/yr continues to decline - or steady
lm_nstages <- lm(data = nts2013, all_stages ~ year) # linear model dependent on year
myears <- data.frame(year = c(nts2013$year, 2015:2050)) # modelled years
myears <- left_join(myears, nts2013) # add the historic data to the future data
myears$linear <- predict(lm_nstages, myears) # future prediction
myears$average <- rep(min(nts2013$all_stages), nrow(myears))
plot(nts2013$year, nts2013$all_stages, xlim = c(1995, 2050), ylim = c(0, 1250))
lines(myears$year, myears$linear, col = "blue")
lines(myears$year, myears$average, col = "red")

# 2: future population
# commented out: the reading Excel file from R route... - easier to export csv!
# download.file("http://www.ons.gov.uk/ons/rel/npp/national-population-projections/2012-based-projections/rft-table-a3-4-principal-projection---england-population-single-year-of-age.xls", destfile = "input-data/future-pop-ons.xls")
# library(gdata)
# fpop <- read.xls(xls = "input-data/future-pop-ons.xls", sheet = 3, fileEncoding = "Latin1")
# fpop[2:10,2:20]
fpop <- data.frame(apply(t(read.csv("input-data/ons-pop-projects.csv", header = FALSE)), 2, as.numeric))
fpop <- fpop %>% filter(!is.na(X2))
fpop <- fpop %>% rename(year = X1)
loess_pop <- loess(X2 ~ year, data = fpop)
myears$pop <- predict(loess_pop, myears)
plot(fpop, ylim = c(50000, 70000))
lines(myears$year, myears$pop)

# Does it make sense: billion trips/yr in 2015
(cynum2013 <- nts2013$bike_trips_ppy[ nts2013$year == 2013] * myears$pop[ myears$year == 2013] * 1000 / 1e9) # yes it does: 0.823 billion/yr
(cynum2025 <- cynum2013 * 2)
myears$abs_cycle <- NA
myears$abs_cycle[ myears$year %in% c(2013, 2025)] <- c(cynum2013, cynum2025)
lm_abcycl <- lm(myears$abs_cycle ~ myears$year)
myears$abs_cycle <- predict(lm_abcycl, myears) # absolute number of cyling stages
myears$abs_cycle[ myears$year < 2015 ] <- NA
myears$bstages_pp <- myears$abs_cycle * 1e9 / (myears$pop * 1e3)# bike stages per year per person
myears$cdp_pbike1 <- myears$bstages_pp / myears$linear
myears$cdp_pbike2 <- myears$bstages_pp / myears$average
plot(myears$year, myears$cdp_pbike1, col = "blue", xlim = c(1990, 2050), ylim = c(0, 0.10))
points(myears$year, myears$cdp_pbike2, col = "red")
points(myears$year, myears$perc_bike)

# Baseline scenario: 0% growth from NTS 2008 - 2012 baseline
(myears$ntm_perc_bike <-
    mean(myears$perc_bike[ myears$year > 2007 & myears$year < 2014]))

# The 'Go Dutch' scenario
k = 0.27
B = mean(myears$perc_bike[ myears$year > 2007 & myears$year < 2014])
r = 0.15
time = 0:35
lgrowth <- (B * k * exp(r * time)) / (k + B * (exp( r * time) - 1) )
myears$goDutch_perc_bike[ myears$year > 2014] <- lgrowth

# Trip changes
# download.file("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/336230/nts0303.xls", destfile = "input-data/nts0303.xls", method = "wget")

# library(xlsx)
# nts0303 <- read.xlsx("input-data/nts0303.xls", sheet = 1)
nts0303 <- read.csv("input-data/nts0303.csv")
nts0303 <- nts0303[grep("Bicycle|All modes", nts0303[,1]), ]
# Remove na values
nts0303 <- nts0303[!is.na(nts0303[,1]),]
# Select only year-by year columns
nts2013t <- data.frame(t(nts0303[,-1]))
nts2013t$year <- c(1996, 1999, 2002:2013)
nts2013t[1:2] <- apply(nts2013t[1:2], MARGIN = 2, as.numeric)
names(nts2013t)
nts2013t <- rename(nts2013t, bike_trips_ppy = X3, all_trips = X16)
nts2013t$perc_bike <- nts2013t$bike_trips_ppy / nts2013t$all_trips
tmp_df <- data.frame(year = nts2013t$year, perc_biket = nts2013t$perc_bike)
myears <- left_join(myears, tmp_df)
plot(nts2013t$year, nts2013t$perc_bike) # proportion of stages per person per year
plot(nts2013t$year, nts2013t$bike_trips_ppy) # absolute number of bike trips/person/yr
plot(nts2013t$year, nts2013t$all_trips) # declining number of stages per person/yr

lm_ntripss <- lm(data = nts2013t, all_trips ~ year) # linear model dependent on year
myears$lineart <- predict(lm_ntripss, myears) # future prediction
myears$averaget <- rep(min(nts2013t$all_trips), nrow(myears))
plot(nts2013t$year, nts2013t$all_trips, xlim = c(1995, 2050), ylim = c(0, 1250))
lines(myears$year, myears$lineart, col = "blue")
lines(myears$year, myears$averaget, col = "red")



# estimating future trip - not stage - rates - summary: makes little difference to rate
(cynum2013t <- nts2013t$bike_trips_ppy[ nts2013$year == 2013] * myears$pop[ myears$year == 2013] * 1000 / 1e9) # yes it does: 0.823 billion/yr
(cynum2025t <- cynum2013t * 2)

myears$abs_cyclet <- NA
myears$abs_cyclet[ myears$year %in% c(2013, 2025)] <- c(cynum2013t, cynum2025t)
lm_abcyclt <- lm(myears$abs_cyclet ~ myears$year)
myears$abs_cyclet <- predict(lm_abcyclt, myears) # absolute number of cyling stages
myears$abs_cyclet[ myears$year < 2015 ] <- NA
myears$btrips_pp <- myears$abs_cyclet * 1e9 / (myears$pop * 1e3)# bike stages per year per person
myears$cdp_pbiket1 <- myears$btrips_pp / myears$linear
myears$cdp_pbiket2 <- myears$btrips_pp / myears$average
plot(myears$year, myears$cdp_pbiket1, col = "blue", xlim = c(1990, 2050), ylim = c(0, 0.10))
points(myears$year, myears$cdp_pbiket2, col = "red")
points(myears$year, myears$perc_biket)
