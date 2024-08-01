# ALFAM2 predictions of application timing effect for Spain

# Clear workspace
rm(list = ls())

# Load packages
library(ALFAM2)
library(data.table)
library(ggplot2)

# Software log
sink('../logs/DK_R_log.txt')
  print(sessionInfo())
sink()

# Other functions
source('../../functions/rounddf.R')

# Settings
appmonth <- 4
morning <- 9
evening <- 21

# Get weather data
wthr <- fread('../../weather/DK/weather_10km_624_54.csv')

# Sort out weather time
wthr[, date := as.Date(date.time)]
wthr[, year := as.integer(format(date, format = '%Y'))]
wthr[, month := as.integer(format(date, format = '%m'))]
wthr[, dom := as.integer(format(date, format = '%d'))]
wthr[, doy := as.integer(format(date, format = '%j'))]
wthr[, hr := as.integer(format(date.time, format = '%H'))]

# Get DMI data
wthr[, `:=` (air.temp = air.temp.dmi, wind.2m = wind.2m.dmi, wind.sqrt = sqrt(wind.2m.dmi), rain.rate = rain.1h.dmi / 1)]

# Subset with required variables
wthr <- wthr[!is.na(air.temp + wind.2m + rain.rate) & month %in% c(appmonth, appmonth + 1), 
             .(date.time, date, hr, year, month, dom, doy, air.temp, wind.2m, wind.sqrt, rain.rate)]

# Check for missing values
# Expect 24 values per day
ct <- wthr[, .N, by = doy]
ct

# Inputs, application on every day in weather data
dat <- data.table()
for (i in 1:30) {
  for (p in c('morning', 'evening')) {
    if (p == 'morning') apphour <- morning
    if (p == 'evening') apphour <- evening
    dd <- wthr[month == appmonth & dom == i & hr == apphour, date.time]
    for (j in dd) {
      j <- as.POSIXct(j)
      ddd <- wthr[difftime(date.time, j, units = 'hours') <= 168 & difftime(date.time, j, units = 'hours') >= 0, ]
      ddd[, `:=` (appdatetime = j, period = p)]
      ddd[, ct := as.numeric(difftime(date.time, j, units = 'hours'))]
      dat <- rbind(dat, ddd)
    }
  }
}

# Add some fixed variables
dat[, `:=` (man.source = 'digestate', app.mthd = 'bsth', man.dm = 5.9, man.ph = 7.9, TAN.app = 100)]

# ALFAM2 predictions
# With set 3
pred <- alfam2(dat, group = 'appdatetime', pass.col = c('year', 'period'))
# With bootstrap sets
predvar <- alfam2(dat, conf.int = 'all', group = 'appdatetime', pass.col = c('year', 'period'))

# Change to data table for next steps
setDT(pred)
setDT(predvar)

# Get final emission
predf <- pred[ct == 168, ]
predvarf <- predvar[ct == 168, ]

# Two step CI by par set
pci <- predvarf[, .(er = mean(er)), by = .(period, par.id)]
# Reshape for reduction calc
pcil <- dcast(pci, par.id ~ period, value.var = 'er')
# Relative reduction
pcil[, rred := 100 * (1 - evening / morning)]
summci <- pcil[, .(lwr = quantile(rred, 0.05), mn = mean(rred), md = median(rred), upr = quantile(rred, 0.95))]

# Summary with par set 3
summ <- predf[, .(er = mean(er), sd = sd(er), min = min(er), max = max(er), n = .N), by = period]
redave <- - diff(summ[, er]) / max(summ[, er])
summ[, red := redave]

# Weather averages
wthrave <- dat[, .(air.temp = mean(air.temp), wind.2m = mean(wind.2m), rain.rate = mean(rain.rate)), by = period]

# Export
fwrite(rounddf(summ, 3), '../output/DK_ave_emis.csv')
fwrite(rounddf(summci, 3), '../output/DK_emis_CI.csv')
fwrite(rounddf(wthrave, 3), '../output/DK_ave_weather.csv')

# Plots
ggplot(dat, aes(doy, air.temp, group = year)) + 
  geom_line() +
  theme_bw() +
  labs(x = 'Day of year', y = expression('Air temperature'~(degree*C))) +
  theme(legend.position = 'none')
ggsave('../plots/DK_temperature.png', height = 4, width = 5)

ggplot(wthr, aes(doy, wind.2m, group = year)) + 
  geom_line() +
  theme_bw() +
  labs(x = 'Day of year', y = expression('Wind speed'~(m~s^'-1'))) +
  theme(legend.position = 'none')
ggsave('../plots/DK_wind.png', height = 4, width = 5)

ggplot(predf, aes(er, fill = period)) +
  geom_histogram() +
  facet_wrap(~year) +
  theme_bw() +
  labs(x = 'Emission (frac. TAN)', y = 'Count', fill = 'Application period') 
ggsave('../plots/DK_emis_hist.png', height = 5, width = 10)
