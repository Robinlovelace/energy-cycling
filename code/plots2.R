# plots for the updated CTC report
# run after scenarios.R

# load packages
library(ggplot2)
library(dplyr)
library(tidyr)

# subset and transform dataset for plotting
# write.csv(myears, "output-data/scenarios2.csv")
myears_plot <- select(myears, year, ntm_perc_bike, cdp_pbike1, cdp_pbike2, goDutch_perc_bike )
myears_plot <- rename(myears_plot, NTM = ntm_perc_bike, CDP2 = cdp_pbike1, CDP1 = cdp_pbike2, Dutch = goDutch_perc_bike  )
myears_plot <- gather(myears_plot, Scenario, value = Percentage, -year)
# write.csv(myears_plot, "output-data/scenarios2-just-scenarios.csv")
head(myears_plot)

ggplot() + geom_vline(xintercept = c(2025, 2050), linetype = 3) + geom_hline(yintercept = c(0.10, 0.25), linetype = 2) + geom_line(data = myears_plot, aes(x = year, y = Percentage, color = Scenario)) + ylab("Proportion of stages by bike") +  theme_bw() + scale_color_brewer(type = "qual", palette = 6, name = "Model") + geom_text(aes(x = c(2010, 2010), y = c(0.11, 0.24), label = c("GBC 2025", "GBC 2050")), size = 4)
ggsave("figures/scenarios2.png")

# TODO: we could easily add historical data as points into this

plot(nts2013$year, nts2013$all_stages, xlim = c(1995, 2050), ylim = c(0, 1250))
df1m <- data.frame(Year = myears$year, Stages = myears$linear, Model = "Linear")
df2m <- data.frame(Year = myears$year, Stages = myears$average, Model = "Constant")
dfm <- rbind(df1m, df2m)

plot(fpop, ylim = c(50000, 70000))
lines(myears$year, myears$pop)

plot(nts2013t$year, nts2013t$all_trips, xlim = c(1995, 2050), ylim = c(0, 1250))
lines(myears$year, myears$lineart, col = "blue")
lines(myears$year, myears$averaget, col = "red")

points(nts2013$year, nts2013$all_stages, xlim = c(1995, 2050), ylim = c(0, 1250))
lines(myears$year, myears$linear, col = "blue")
lines(myears$year, myears$average, col = "red")

# now in ggplot2
library(ggplot2)
p1 <- qplot(nts2013$year, nts2013$all_stages) +
  xlim(1995, 2050) +
  ylim(c(0, 1250)) +
  geom_line(data = dfm, aes(Year, Stages, colour = Model)) +
  xlab("Year") + ylab("Stages/p/year") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.4))

p2 <- qplot(x = fpop$year, y = fpop$X2 / 1000, geom = "line") +
  xlim(1995, 2050) + ylim(0, 70) +
  xlab("Year") + ylab("Population (million) (ONS)") +
  theme_bw()

library(gridExtra)
grid.arrange(p1, p2)
ggsave("/tmp/plot.png")

# Appendix showing difference between trips and stage rates
plot(nts2013t$year, nts2013t$all_trips, xlim = c(1995, 2050), ylim = c(0, 1250))
lines(myears$year, myears$lineart, col = "blue")
lines(myears$year, myears$averaget, col = "red")

points(nts2013$year, nts2013$all_stages, xlim = c(1995, 2050), ylim = c(0, 1250))
lines(myears$year, myears$linear, col = "blue")
lines(myears$year, myears$average, col = "red")

df_tmp <- data.frame(year = c(nts2013t$year, myears$year, myears$year, nts2013$year, myears$year, myears$year), npyr = c(nts2013t$all_trips, myears$lineart, myears$averaget, nts2013$all_stages, myears$linear, myears$average), Source = c(rep("Trips", nrow(nts2013t)), rep("Trips-linear", nrow(myears)), rep("Trips-constant", nrow(myears)), rep("Stages", nrow(nts2013)), rep("Stages-linear", nrow(myears)), rep("Stages-constant", nrow(myears)) ) )
df_tmp$Measure <- c(rep("Trips", 114), rep("Stages", 114))
df_tmp$Projection <- ifelse(grepl("lin", df_tmp$Source), "Linear",
  ifelse(grepl("cons", df_tmp$Source), "Constant", NA) )

library(ggplot2)
ggplot() + geom_path(data = filter(df_tmp, grepl("-", Source)), aes(year, npyr, color = Measure, group = Source, linetype = Projection)) + geom_point(data = filter(df_tmp, !grepl("-", Source)), aes(year, npyr, color = Measure)) + ylab("Number/person/year") + xlab("Year") + theme_bw()
ggsave("figures/trips-stages-decline.png")
