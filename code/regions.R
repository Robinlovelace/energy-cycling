### GORS-time
summary(ssam$j58g)

gssam <- ssam[ grep("Lon|York|North E|Scotl", ssam$j58g),]

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
save(gors, file = "~/Dropbox/basic-geodata/gors.RData")
plot(gors)


# load("~/Dropbox/basic-geodata/GBR_adm2.RData")
# library(rgeos)
# gadm <- gSimplify(gadm, tol = 0.03)
# object.size(gadm) / 1000000
# plot(gadm)
