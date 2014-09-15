# after adding edited data
(ntpy <- read.csv("nts/ntpm-final.csv")[,c(2,5)])
ntpy$Source <- "NTS"
names(ntpy) <- c("Year", "prop.cycle", "Source")
# qplot(data=ntpy, x=Year, y=prop.cycle * 100, color = Source) + geom_smooth(fill=NA) +
#   ylim(c(0,2)) + theme_bw() + ylab("Percentage of trips by bicycle") 
# ggsave("~/Dropbox/DutchBikes/figures/nts-time.png")

## color scheme for output
library(RColorBrewer)
myColors <- brewer.pal(4,"Set1")
names(myColors) <- c("Current rate", "Needed rate", "10 yr doubling", "DfT's NTM")
colScale <- scale_colour_manual(name = "Model",values = myColors)

# making hypothetical future data
x <- 1:8
y <- ntpy$Prop.cycle * 100
df <- data.frame(cbind(x,y))
# lm1 <- lm(y ~ x, data=df) # commented out - not linear!
df2 <- data.frame(x= 2000:2050 -2004)
# p1 <- predict(lm1, df2) # 1/2 way there by 2500 at current rate!
# df2050 <- data.frame(Year = 2000:2050, prop.cycle = p1, model = "Current rate" )


# Data from NTS 2013
# Also contains Inner London cycling data, from here http://aseasyasridingabike.wordpress.com/2013/12/26/some-statistics-from-the-latest-travel-in-london-report/
# And NTM data from here from http://www.publications.parliament.uk/pa/cm201314/cmhansrd/cm131031/text/131031w0001.htm
nts12 <- read.csv("input-data/nts2013.csv")
nts12$model <- gsub(pattern = "TFL", replacement = "TfL", nts12$model)
df2050 <- nts12
df2050$prop.cycle <- df2050$prop.cycle * 100
names(df2050)[3] <- "model"
# dfneed <- data.frame(Year = 2000:2050, prop.cycle = c(p1[1:15], seq(1.7,25,length.out=36)), model = "Linear" )
dfneed <- data.frame(Year = 2015:2050, prop.cycle = c(seq(1.7,25,length.out=36)), model = "Linear" )
df2050 <- rbind(df2050, dfneed)
qplot(data = df2050, x = Year, y = prop.cycle, color = model) + ylab("Percentage of trips by bicycle") +  theme_bw() # + colScale
dfneed[20,2] - dfneed[19,2] # rate of cycling need to grow by 0.67% points pa for next 35 years
# ggsave("~/Dropbox/DutchBikes/figures/nts-time2050.png")

# the real world can be linear http://data.london.gov.uk/datastore/package/cycle-flows-tfl-road-network
# rw <- read.csv("lndn-counts.csv")
# class(rw$Pedal.Cycle.Counts.Indexed)
# plot(1:nrow(rw), rw$Pedal.Cycle.Counts.Indexed)
# rw$Year <- seq(2000, 2014, length.out=nrow(rw))
# qplot(rw$Year, rw$Pedal.Cycle.Counts.Indexed) + geom_smooth(method=lm, fill = NA) +
#   scale_x_continuous(breaks  = 2000:2014) + xlab("Year") + ylab("Cycle count index") + 
#   theme_bw() 
# 1.7 / 8
# 1.7 * 8 # could hit 25% by 2050, just if it's exponential
# # ggsave("~/Dropbox/DutchBikes/figures/lnd-linear.png")
# 
# # distance cycled
# dc <- read.csv("nts/nts0306-avdist.csv", sep = "\t")
# dct <- data.frame(Year = ntpy$Year[8:20], Distance = as.numeric(dc[2,2:14]))
# plot(dct)
# ggplot(dct, aes(x=Year, y=Distance)) + geom_point() + ylab("Distance (miles)") + theme_bw() +
#    colScale
# ggsave("~/Dropbox/DutchBikes/figures/avdist.png")

dw <- data.frame(Year = 2015:2050)
dw$prop.cycle <- 1.7 * (2^(1:36/10))
dw$model <- "Exponential"

plot(dw$Year, dw$prop.cycle)
df2050 <- rbind(df2050, dw)
qplot(data = df2050, x = Year, y = prop.cycle, color = model) + ylab("Percentage of trips by bicycle") +  theme_bw() # + colScale
# ggsave("~/Dropbox/DutchBikes/figures/doubling.png")

# analysis of DfT's projections:
# (106.63 -109.73) / 127 # 2% in 2010
# (130.163 - 127.067) / 152
# dftntm <- data.frame(Year = 2010:2035, prop.cycle = seq(2.4, 2.04, length.out= 26), model = "DfT's NTM")
# df2050 <- rbind(df2050, dftntm)
# qplot(data = df2050, x = Year, y = prop.cycle, color = model) + ylab("Percentage of trips by bicycle") +  theme_bw() # + colScale
# ggsave("~/Dropbox/DutchBikes/figures/ntm-out.png")


# the logistic growth model
k = 27
B = 1.7
r = 0.15
time = 0:35
lgrowth <- (B * k * exp(r * time)) / (k + B * (exp( r * time) - 1) ) 
plot(lgrowth)
lgrowth <- data.frame(Year = time + 2015, prop.cycle = lgrowth, model = "Logistic")

# redo colors, plot
# library(RColorBrewer)
# myColors <- c(brewer.pal(4,"Set1"), "black")
# names(myColors) <- c("Current rate", "Needed rate", "10 yr doubling", "DfT's NTM", "Logistic")
# colScale <- scale_colour_manual(name = "Model",values = myColors)

df2050 <- rbind(df2050, lgrowth)

ptext <- c("GBC 2025 target", "GBC 2050 target")
px <- c(2007, 2007)
py <- c(10.8, 25.8)

ggplot() + geom_vline(xintercept = c(2025, 2050), linetype = 3) + geom_hline(yintercept = c(10, 25), linetype = 2) + geom_point(data = df2050, aes(x = Year, y = prop.cycle, color = model)) + ylab("Percentage of trips by bicycle") +  theme_bw() + scale_color_brewer(type = "qual", palette = 6, name = "Data/model") + geom_text(aes(px, py, label = ptext), size = 4)
# ggsave("figures/overview.png", width = 6, height = 4) # happy with this but not happy with colors -> lines

dfdots <- df2050[ grepl("NT|Tf", df2050$model), ]
dfline <- df2050[ -which(grepl("NT|Tf", df2050$model)), ]
 
ggplot() + geom_vline(xintercept = c(2025, 2050), linetype = 3) + geom_hline(yintercept = c(10, 25), linetype = 2) + geom_line(data = dfline, aes(x = Year, y = prop.cycle, color = model)) + geom_point(data = dfdots, aes(x = Year, y = prop.cycle, shape = model)) + ylab("Percentage of trips by bicycle") +  theme_bw() + scale_color_brewer(type = "qual", palette = 6, name = "Model") + scale_shape_discrete(name = "Data", solid = F) + geom_text(aes(px, py, label = ptext), size = 4)
ggsave("figures/overview.png", width = 6, height = 4.5) # happy with this but not happy with colors -> lines
# save.image(file = "df2050.Rdata")
# ## histogram of travel distance
# nt <- ntstrips[ntstrips$jdungross < 500, ]
# nt <- nt[-which(grepl("Other|Non|Tax|Mot|Lond|LT", nt$j36a)), ]
# summary(nt$j36a)
# nt$j36a <- factor(nt$j36a)
# nt$x <- nt$jdungross / 10
# # qplot(nt$jdungross, geom='blank') + geom_histogram() +  
# #   stat_function(fun = dnorm)
# # 
# # p0 = qplot(data = nt, x = x, geom = 'blank') +   
# #   geom_line(aes(y = ..density.., colour = 'Gaussian'), stat = 'density', adjust = 5, kernel = "gaussian") +  
# #   geom_line(aes(y = ..density.., colour = 'Biweight'), stat = 'density', adjust = 5, kernel = "b") +
# #   geom_histogram(aes(y = ..density..), alpha = 0.4) +                        
# #   scale_colour_manual(name = 'Density', values = c('red', 'blue'))  + 
# # #   opts(legend.position = c(0.85, 0.85)) +
# #   facet_wrap(. ~ nt$j36a)
# # print(p0)
# # 
# # ggplot(data=nt, aes(x = x)) + geom_histogram() +
# #   geom_line(aes(y = ..density..)) + facet_wrap()
# ggplot(nt, aes(x=x)) + geom_histogram() + theme_bw() + xlab("Distance (miles)")
# ggsave("~/Dropbox/DutchBikes/figures/hist-raw.png")
# ggplot(nt, aes(x=x)) + geom_histogram(aes(y = ..density..)) + geom_density(adjust = 10) + theme_bw() + xlab("Distance (miles)")
# ggsave("~/Dropbox/DutchBikes/figures/hist-raw-kern.png")
# 
# ggplot(nt, aes(x=x, fill=j36a)) + geom_histogram() + theme_bw() + xlab("Distance (miles)")
# ggsave("~/Dropbox/DutchBikes/figures/hist-color.png")
# ggplot(nt, aes(x=x, fill=j36a)) + geom_density(alpha=.3, adjust = 10) +coord_cartesian(ylim=c(0, 0.4)) + theme_bw() + xlab("Distance (miles)")
# ggsave("~/Dropbox/DutchBikes/figures/hist-overlay.png")
# 
# summary(nt$j36a)
# barplot(summary(nt$j36a))

