# health impacts
# Input: tsam - generated from scenarios.R
library(dplyr)
tsam <- tsam[1:nrow(tsam),]
individuals <- group_by(tsam, psuid, i1, house )
indscen <- summarise(individuals, dist_cycle = sum(dcycle), dist_cycle_dft = sum(dcycle_2025_dft), dist_cycle_go = sum(dcycle_2025_go)) # the saving per person

summary(indscen)
summary(indscen$dist_cycle)
summary(indscen$dist_cycle_go)
summary(indscen$dist_cycle[ indscen$dist_cycle > 0])
indscen_all <- inner_join(ind, indscen) # join other attributes to individ. data
iss <- indscen_all[4000:5000,]

write.csv(inscen_subset, "~/Dropbox/energy_saving.csv")
summary(indscen_all[4000:5000,])

# The number of people who never cycle

# Recategorise to fit model
age_recat <- function(a){
    a2 <- factor(rep(NA, length(a)), levels = c("0-14", "15-29", "30-44", "45-59", "60-69", "70-79", "80+"))
    a2[a == "0 - 4 years" | a == "5 - 10 years" | a == "11 - 15 years"] <- "0-14"
    a2[a == "16 - 19 years" | a == "20 - 29 years"] <- "15-29"
    a2[a == "30 - 39 years" ] <- "30-44"
    f40_t49 <- which(a == "40 - 49 years") # we need to split this variable in two
    o30_44 <- sample(f40_t49, size = length(f40_t49) / 2)
    o45_59 <- f40_t49[!f40_t49 %in% o30_44]
    a2[o30_44] <- "30-44"
    a2[o45_59] <- "45-59"
    a2[a == "50 - 59 years" ] <- "45-59"
    a2[a == "60 - 69 years" ] <- "60-69"
    a2[a == "70 + years" ] <- "70-79" # all people over 70...
    over70 <- which(a == "70 + years")
    over80 <- sample(over70, size = (length(over70) * 0.397))
    a2[over80] <- "80+" # all people over 70...
  a2
}

a2 <- age_recat(a)
cbind(as.character(a),as.character(a2))

# Percentage in each group
length(which(iss$dist_cycle_go == 0)) / nrow(iss) # the overall percentage

agecats <- data_frame(age = levels(iss$age), not_cycled = NA, mean_dis = NA, mean_dis_cyclists = NA, sd_dis_cyclists = NA, not_cycled_dft = NA, mean_dis_dft = NA, mean_dis_cyclists_dft = NA, sd_dis_cyclists_dft = NA, not_cycled_go_dutch = NA, mean_dis_go_dutch = NA, mean_dis_cyclists_go_dutch = NA, sd_dis_cyclists_go_dutch = NA)

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
}

agecats_f <- agecats
names(agecats_f)[-1] <- fnames

agecats <- cbind(agecats_m, agecats_f[-1])

agecats
write.csv(agecats, file = "/tmp/age_cats.csv")

summarise(individuals, never_cycled = n()) # the saving per person
