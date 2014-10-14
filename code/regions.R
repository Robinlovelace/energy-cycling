# Generate outputs (% increase in cycling, reduction in car use) by gor per unit time

summary(tsam$region)
tsam$region <- factor(tsam$region) # remove superfluous factor levels
levels(tsam$region)

region <- data.frame(GOR = levels(tsam$region), nts_pop = as.numeric(summary(tsam$region)), rate_current = NA, rate_2025_dft = NA, rate_go_2025 = NA)

library(dplyr)
byGOR <- group_by(tsam, region)
summarise(byGOR, rate = sum(mode == "Bicycle") / n()) # much much faster
names(tsam)
summarise(byGOR, rate = sum(dswitch_2025_dft == "Bicycle") / n()) # strange - London still lagging
summarise(byGOR, rate = mean(dkm))



aggregate(mode ~ region, FUN = function(x) sum(x == "Bicycle") /  length(x), data = tsam)

for(i in 1:nrow(region)){
 region$rate_current <-
}


gssam <- ssam[ grep("Lon|York|North E|Scotl", ssam$j58g),] # select zones of interest
summarise(group_by(ssam, j58g), esave = sum(esave)) # the saving in each zone

# Now generate for each point in time
for(i in 2015:2050){}

# visualisation
library(ggplot2)
ggplot(gssam[ gssam$sd < 300, ]) + geom_histogram(aes(x = sd/10, fill = mode)
  #   , stat = "density"
) +
  scale_fill_brewer(type = "qual") +
  facet_wrap(~ j58g) + xlab("Miles") + ylab("Number of trips")

ggplot(gssam[ gssam$sd < 300, ]) + geom_histogram(aes(x = sd/10, fill = mode)
#   , stat = "density"
    , position = "fill"
  ) +
  scale_fill_brewer(type = "qual") +
  facet_wrap(~ j58g) + xlab("Miles") + ylab("Proportion of trips")

library(rgdal)
unzip("/tmp/infuse_ctry_2011.zip", exdir = "/media//robin/SAMSUNG/geodata/gb-countries")
cnts <- readOGR("/media//robin/SAMSUNG/geodata/gb-countries", "infuse_ctry_2011")
plot(cnts)
gors <- readOGR("/media/robin/SAMSUNG/geodata/regions/", "ruk")
library(rgeos)
# ukgors <- gUnion(gors, cnts) # fail
scotwales <- cnts[c(2,4),]
plot(scotwales)

ukgors gUnion(scotwales, gors)

object.size(gors)
gors <- gSimplify(gors, tol = 1000)
# save(gors, file = "~/Dropbox/basic-geodata/gors.RData")
load("~/Dropbox/basic-geodata/gors.RData")
plot(gors)


load("~/Dropbox/basic-geodata/GBR_adm2.RData")
# library(rgeos)
# gadm <- gSimplify(gadm, tol = 0.03)
# object.size(gadm) / 1000000
plot(gadm)
head(gadm@data)
summary(factor(gadm$NAME_2))
