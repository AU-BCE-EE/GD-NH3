# Simple, single, example of application timing management effect
# S. Hafner

# Packages
library(data.table)
library(ggplot2)
library(ALFAM2)
library(gridExtra)
library(viridis)

# Get some example weather data for April and May, single location
wthr <- data.table(read.csv('../weather/weather_sub.csv'))
wthr[, date.time := as.POSIXct(date.time, format = '%Y-%m-%dT%H:%M:%S')]

# Add predictor variables
wthr[, `:=` (air.temp = air.temp.dmi, wind.sqrt = sqrt(wind.2m.dmi), TAN.app = 100)]

# Check air temperature
ggplot(wthr, aes(date.time, air.temp)) + geom_line()

# Two input data sets: for morning (morn) and evening (even) application
# Generate inputs for 7 d emission after application on every possible day within April
dmorn <- data.table()
hh <- 10
for (i in 1:30) {
  dd <- wthr[mnth == 4 & dom == i & hr == hh, date.time]
  ddd <- wthr[difftime(date.time, dd, units = 'hours') <= 168 & difftime(date.time, dd, units = 'hours') >= 0, ]
  ddd[, startday := i]
  ddd[, ct := as.numeric(difftime(date.time, dd, units = 'hours'))]
  dmorn <- rbind(dmorn, ddd)
}

# View
ggplot(dmorn, aes(ct, air.temp, colour = factor(startday))) + geom_line() 

# ALFAM2 predictions
predmorn <- alfam2(dmorn, conf.int = 'all', group = 'startday')
setDT(predmorn)
outmorn <- predmorn[ct == 168,]
outmorn

# Now repeat with evening application
deven <- data.table()
hh <- 21
for (i in 1:30) {
  dd <- wthr[mnth == 4 & dom == i & hr == hh, date.time]
  ddd <- wthr[difftime(date.time, dd, units = 'hours') <= 168 & difftime(date.time, dd, units = 'hours') >= 0, ]
  ddd[, startday := i]
  ddd[, ct := as.numeric(difftime(date.time, dd, units = 'hours'))]
  deven <- rbind(deven, ddd)
}

predeven <- alfam2(deven, conf.int = 'all', group = 'startday')
setDT(predeven)
outeven <- predeven[ct == 168,]

# Combine results
out <- merge(outmorn, outeven, by = c('startday', 'par.id'), suffixes = c('.morn', '.even'))
pred <- merge(predmorn, predeven, by = c('startday', 'par.id', 'ct'), suffixes = c('.morn', '.even'))

# Also long structure
predmorn[, apptime := '10:00']
predeven[, apptime := '21:00']
predl <- rbind(predmorn, predeven)

# Plots below

# Plot sample of emission curves
dd <- predl[par.id %in% sample(par.id, 20), ]
ggplot(dd, aes(ct, er, colour = apptime, group = interaction(startday, apptime, par.id))) +
  geom_line(alpha = 0.3) +
  scale_colour_discrete(type = c('orange', 'blue')) +
  theme_bw() +
  labs(x = 'Time since application (h)', y = 'Predicted emission (frac. TAN)', colour = 'Application time') +
  annotate('label', label = 'Relative reduction: 9.8% (-7.2, 27%)', x = 120, y = 0.1)
ggsave('atm_curves.png', height = 4, width = 9)

# Get quantiles for relative comparison
out[, rr := 1 - er.even / er.morn] 
qq1 <- out[, .(lwr = quantile(rr, 0.05), md = median(rr), upr = quantile(rr, 0.95)), by = startday]
qq2 <- out[, .(lwr = quantile(rr, 0.05), md = median(rr), upr = quantile(rr, 0.95))]

fwrite(qq1, 'quantiles1.csv')
fwrite(qq2, 'quantiles2.csv')


