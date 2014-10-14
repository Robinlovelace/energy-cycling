# health impacts
# Input: tsam - generated from scenarios.R
library(dplyr)
tsam <- tsam[1:nrow(tsam),]
individuals <- group_by(tsam, psuid, i1, house )
indscen <- summarise(individuals, GOR = commonest(region), dist_cycle = sum(dcycle), dist_cycle_dft = sum(dcycle_2025_dft), dist_cycle_go = sum(dcycle_2025_go)) # the saving per person

# Look at individual GORS
indscen$GOR <- factor(indscen$GOR)
summary(indscen$GOR)
indscen <- indscen[ !indscen$GOR == "Scotland", ] # may be superfluous...

summary(indscen)
summary(indscen$dist_cycle)
summary(indscen$dist_cycle_go)
summary(indscen$dist_cycle[ indscen$dist_cycle > 0])

indscen_all <- inner_join(indscen, ind) # join other attributes to individ. data
indscen_all$age <- age_recat(indscen_all$age)
# iss <- indscen_all[4000:5000,] # test run
iss <- indscen_all

# write.csv(iss, "~/Dropbox/energy_saving.csv")
summary(iss)

# Percentage in each group
length(which(iss$dist_cycle_go == 0)) / nrow(iss) # the overall percentage

agecats <- data.frame(age = levels(iss$age), not_cycled = NA, mean_dis = NA, mean_dis_cyclists = NA, sd_dis_cyclists = NA, not_cycled_dft = NA, mean_dis_dft = NA, mean_dis_cyclists_dft = NA, sd_dis_cyclists_dft = NA, not_cycled_go_dutch = NA, mean_dis_go_dutch = NA, mean_dis_cyclists_go_dutch = NA, sd_dis_cyclists_go_dutch = NA)

# run the calculations
for(i in levels(iss$age)){
  sel <- which(iss$age == i)
  agecats$not_cycled[agecats$age == i] <- sum(iss$dist_cycle[sel] == 0) / length(sel)
  agecats$mean_dis[agecats$age == i] <- mean(iss$dist_cycle[sel])
  sel2 <- sel[iss$dist_cycle[sel] > 0] # cyclists
  agecats$mean_dis_cyclists[agecats$age == i] <- mean(iss$dist_cycle[sel2])
  agecats$sd_dis_cyclists[agecats$age == i] <- sd(iss$dist_cycle[sel2])

  agecats$not_cycled_dft[agecats$age == i] <- sum(iss$dist_cycle_dft[sel] == 0) / length(sel)
  agecats$mean_dis_dft[agecats$age == i] <- mean(iss$dist_cycle_dft[sel])
  sel2 <- sel[ iss$dist_cycle_dft [sel] > 0] # cyclists
  agecats$mean_dis_cyclists_dft[agecats$age == i] <- mean(iss$dist_cycle_dft[sel2])
  agecats$sd_dis_cyclists_dft[agecats$age == i] <- sd(iss$dist_cycle_dft[sel2])
}

# Add gender, first with names
mnames <- paste0(names(agecats)[-1], "_m")
fnames <- paste0(names(agecats)[-1], "_f")

# Subset the population
iss_m <- iss[ iss$sex == "Male", ]
iss_f <- iss[ iss$sex == "Female", ]

for(i in levels(iss_m$age)){
  sel <- which(iss_m$age == i)
  agecats$not_cycled[agecats$age == i] <- sum(iss_m$dist_cycle[sel] == 0) / length(sel)
  agecats$mean_dis[agecats$age == i] <- mean(iss_m$dist_cycle[sel])
  sel2 <- sel[iss_m$dist_cycle[sel] > 0] # cyclists
  agecats$mean_dis_cyclists[agecats$age == i] <- mean(iss_m$dist_cycle[sel2])
  agecats$sd_dis_cyclists[agecats$age == i] <- sd(iss_m$dist_cycle[sel2])

  agecats$not_cycled_dft[agecats$age == i] <- sum(iss_m$dist_cycle_dft[sel] == 0) / length(sel)
  agecats$mean_dis_dft[agecats$age == i] <- mean(iss_m$dist_cycle_dft[sel])
  sel2 <- sel[ iss_m$dist_cycle_dft [sel] > 0] # cyclists
  agecats$mean_dis_cyclists_dft[agecats$age == i] <- mean(iss_m$dist_cycle_dft[sel2])
  agecats$sd_dis_cyclists_dft[agecats$age == i] <- sd(iss_m$dist_cycle_dft[sel2])

  agecats$not_cycled_go_dutch[agecats$age == i] <-
    sum(iss_m$dist_cycle_go[sel] == 0) / length(sel)
  agecats$mean_dis_go_dutch[agecats$age == i] <- mean(iss_m$dist_cycle_go[sel])
  sel2 <- sel[ iss_m$dist_cycle_go [sel] > 0] # cyclists
  agecats$mean_dis_cyclists_go_dutch[agecats$age == i] <- mean(iss_m$dist_cycle_go[sel2])
  agecats$sd_dis_cyclists_go_dutch[agecats$age == i] <- sd(iss_m$dist_cycle_go[sel2])
}

agecats_m <- agecats
names(agecats_m)[-1] <- mnames

# Now for females
for(i in levels(iss_f$age)){
  sel <- which(iss_f$age == i)
  agecats$not_cycled[agecats$age == i] <- sum(iss_f$dist_cycle[sel] == 0) / length(sel)
  agecats$mean_dis[agecats$age == i] <- mean(iss_f$dist_cycle[sel])
  sel2 <- sel[iss_f$dist_cycle[sel] > 0] # cyclists
  agecats$mean_dis_cyclists[agecats$age == i] <- mean(iss_f$dist_cycle[sel2])
  agecats$sd_dis_cyclists[agecats$age == i] <- sd(iss_f$dist_cycle[sel2])

  agecats$not_cycled_dft[agecats$age == i] <- sum(iss_f$dist_cycle_dft[sel] == 0) / length(sel)
  agecats$mean_dis_dft[agecats$age == i] <- mean(iss_f$dist_cycle_dft[sel])
  sel2 <- sel[ iss_f$dist_cycle_dft [sel] > 0] # cyclists
  agecats$mean_dis_cyclists_dft[agecats$age == i] <- mean(iss_f$dist_cycle_dft[sel2])
  agecats$sd_dis_cyclists_dft[agecats$age == i] <- sd(iss_f$dist_cycle_dft[sel2])

  agecats$not_cycled_go_dutch[agecats$age == i] <-
    sum(iss_f$dist_cycle_go[sel] == 0) / length(sel)
  agecats$mean_dis_go_dutch[agecats$age == i] <- mean(iss_f$dist_cycle_go[sel])
  sel2 <- sel[ iss_f$dist_cycle_go[sel] > 0] # cyclists
  agecats$mean_dis_cyclists_go_dutch[agecats$age == i] <- mean(iss_f$dist_cycle_go[sel2])
  agecats$sd_dis_cyclists_go_dutch[agecats$age == i] <- sd(iss_f$dist_cycle_go[sel2])
}

agecats_f <- agecats
names(agecats_f)[-1] <- fnames

agecats <- cbind(agecats_m, agecats_f[-1])

agecats
write.csv(agecats, file = "/tmp/age_cats_final_2025.csv")
agecats_old <- read.csv("/tmp/age_cats.csv")

cbind(as.character(agecats$age), agecats$not_cycled_go_dutch_m - agecats_old$not_cycled_go_dutch_m)

summarise(individuals, never_cycled = n()) # the saving per person

