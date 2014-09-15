## analysis of the nts for % trips by bicycle over time
# part of the DutchBikes research project - let's look at monthly responses

# ntstrips <- read.spss("/scratch/data/DfT-NTS-2002-2008/UKDA-5340-spss/spss/spss12//trips.sav")
ntstrips <- read.spss("/media/SAMSUNG/data/DfT-NTS-2002-2008/UKDA-5340-spss/spss/spss12//trips.sav")
ntstrips <- data.frame(ntstrips)
names(ntstrips)
summary(ntstrips$TRAVDATE)
summary(ntstrips$j36a) / nrow(ntstrips) # 1.7 % trips made by bicycle nationwide 2002-2008
summary(ntstrips$jdungross) # 8 mile = av. trip length
summary(ntstrips$jdungross[which(ntstrips$j36a == "Bicycle")]) # 2.4 mile = average bike trip 
summary(ntstrips$jotxsc)

# sorting months out
head(ntstrips$TRAVMM)
levels(ntstrips$TRAVMM)
ntstrips$TRAVMMn <- factor(ntstrips$TRAVMM, labels=c(paste0("0", as.character(1:9)), 10:12) )
head(ntstrips$TRAVMMn)
ntstrips$yearMonth <- paste(ntstrips$TRAVYYYY, ntstrips$TRAVMMn, sep="/")
head(ntstrips$yearMonth)

# percent cycling by monthy
ntpm <- aggregate(ntstrips$TRAVDATE, list(ntstrips$yearMonth), function(x) length(x))
ntpm <- data.frame(ntpm)
head(ntpm)
nbikes <- aggregate(ntstrips$j36, list(ntstrips$yearMonth), function(b) length(which(b == "Bicycle")))[2]
ntpm <- cbind(ntpm, nbikes)
head(ntpm)
ntpm <- cbind(ntpm, ntpm[3] / ntpm[2])
head(ntpm)
plot(ntpm[[4]]) # no pattern - how dissapointing - now try per yr agg.

# percent cycling by yearly
ntpm <- aggregate(ntstrips$TRAVDATE, list(ntstrips$TRAVYYYY), function(x) length(x))
ntpm <- data.frame(ntpm)
head(ntpm)
nbikes <- aggregate(ntstrips$j36, list(ntstrips$TRAVYYYY), function(b) length(which(b == "Bicycle")))[2]
ntpm <- cbind(ntpm, nbikes)
head(ntpm)
ntpm$perc.cycle <- ntpm[,3] / ntpm[,2]
head(ntpm)
plot(ntpm$Group.1, ntpm$perc.cycle) # no pattern - how dissapointing - now try per yr agg.
dir.create("nts")
write.csv(ntpm, "nts/ntpm-final.csv")