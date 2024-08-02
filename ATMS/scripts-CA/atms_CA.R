# ALFAM2 predictions of 

# Clear workspace
rm(list = ls())

# Load packages
library(ALFAM2)
library(data.table)
library(ggplot2)

# Software log
sink('../logs/CA_R_log.txt')
  print(sessionInfo())
sink()

# Other functions
source('../../functions/rounddf.R')

# Settings
springmonth <- 4
summermonth <- 8

# Get weather data
wthr <- fread('../../weather/CA/CA_Daily_weather_data.csv')

# Sort out weather time
wthr[, date := as.Date(as.POSIXct(Date, format = '%Y-%m-%d %H:%M:%S'))]
wthr[, year := as.integer(format(date, format = '%Y'))]
wthr[, month := as.integer(format(date, format = '%m'))]
wthr[, dom := as.integer(format(date, format = '%d'))]
wthr[, doy := as.integer(format(date, format = '%j'))]

# Adjust wind speed to 2 m, assume measurement height of 10 m and roughness length of 1 cm
wthr[, `:=` (air.temp = Tavg, wind.2m = WindSpd * log(2 / 0.01) / log(10 / 0.01))]

# Rain rate from total rain
wthr[, `:=` (wind.sqrt = sqrt(wind.2m), rain.rate = Rain / 24)]

# Subset with required variables
wthr <- wthr[!is.na(air.temp + wind.2m + rain.rate) & month %in% c(3, 4, 5, 6, 7, 8, 9), 
             .(date, year, month, dom, doy, air.temp, wind.2m, wind.sqrt, rain.rate)]

# Check for missing values
# Expect 184 days
ct <- wthr[, .N, by = year]
ct

# Years with missing days
badyears <- ct[N < 184, year]

# Remove years with missing values
wthr <- wthr[!year %in% badyears, ]

#ct <- wthr[, .N, by = year]

# Two input data sets: April and July
# Generate inputs for 7 d emission after application on every possible day within April and July
dat <- data.table()
for (per in c('spring', 'summer')) {
  if (per == 'spring') appmonth <- springmonth
  if (per == 'summer') appmonth <- summermonth
  for (i in 1:31) {
    dd <- wthr[month == appmonth & dom == i, date]
    for (j in dd) {
      j <- as.Date(j)
      ddd <- wthr[difftime(date, j, units = 'days') <= 7 & difftime(date, j, units = 'days') >= 0, ]
      ddd[, `:=` (appperiod = per, appdate = j)]
      ddd[, ct := as.numeric(difftime(date, j, units = 'hours'))]
      dat <- rbind(dat, ddd)
    }
  }
}

# Add other fixed variables
dat[, `:=` (app.mthd = 'bc', man.dm = 5.1, man.ph = 7.5, TAN.app = 100)]

# ALFAM2 predictions
# With set 3
pred <- alfam2(dat, group = 'appdate', pass.col = c('year', 'appperiod'))
# With bootstrap sets
predvar <- alfam2(dat, conf.int = 'all', group = 'appdate', pass.col = c('year', 'appperiod'))

# Change to data table for next steps
setDT(pred)
setDT(predvar)

# Get final emission
predf <- pred[ct == 168, ]
predvarf <- predvar[ct == 168, ]

# Two step CI by par set
pci <- predvarf[, .(er = mean(er)), by = .(appperiod, par.id)]
# Reshape for reduction calc
pcil <- dcast(pci, par.id ~ appperiod, value.var = 'er')
# Relative reduction
pcil[, rred := 100 * (1 - spring / summer)]
summci <- pcil[, .(lwr = quantile(rred, 0.05), mn = mean(rred), md = median(rred), upr = quantile(rred, 0.95))]

# Summary with par set 3
summ <- predf[, .(er = mean(er), sd = sd(er), min = min(er), max = max(er), n = .N), by = appperiod]
redave <- diff(summ[, er]) / max(summ[, er])
summ[, red := redave]

# Weather averages
wthrave <- dat[, .(air.temp = mean(air.temp), wind.2m = mean(wind.2m)), by = appperiod]

# Export
fwrite(rounddf(summ, 3), '../output/CA_ave_emis.csv')
fwrite(rounddf(summci, 3), '../output/CA_emis_CI.csv')
fwrite(rounddf(wthrave, 3), '../output/CA_ave_weather.csv')

# Plots
ggplot(wthr, aes(doy, air.temp, colour = month %in% c(springmonth, summermonth), group = year)) + 
  geom_line() +
  theme_bw() +
  labs(x = 'Day of year', y = expression('Air temperature'~(degree*C))) +
  theme(legend.position = 'none')
ggsave('../plots/CA_temperature.png', height = 4, width = 5)

ggplot(wthr, aes(doy, wind.2m, colour = month %in% c(springmonth, summermonth), group = year)) + 
  geom_line() +
  theme_bw() +
  labs(x = 'Day of year', y = expression('Wind speed'~(m~s^'-1'))) +
  theme(legend.position = 'none')
ggsave('../plots/CA_wind.png', height = 4, width = 5)

ggplot(predf, aes(er, fill = appperiod)) +
  geom_histogram() +
  facet_wrap(~year) +
  theme_bw() +
  labs(x = 'Emission (frac. TAN)', y = 'Count', fill = 'Application period') 
ggsave('../plots/CA_emis_hist.png', height = 5, width = 10)
