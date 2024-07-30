# ALFAM2 predictions of 

rm(list = ls())

# Load packages
library(ALFAM2)
library(data.table)
library(ggplot2)

# Software log
sink('../logs/CA_R_log.txt')
  print(sessionInfo())
sink()

# Get weather data
wthr <- fread('../../weather/CA/CA_Daily_weather_data.csv')

# Sort out weather variables
wthr[, date := as.Date(as.POSIXct(Date, format = '%Y-%m-%d %H:%M:%S'))]
wthr[, year := as.integer(format(date, format = '%Y'))]
wthr[, month := as.integer(format(date, format = '%m'))]
wthr[, dom := as.integer(format(date, format = '%d'))]
wthr[, doy := as.integer(format(date, format = '%j'))]
# Adjust wind speed to 2 m, assume measurement height of 10 m and roughness length of 1 cm
wthr[, `:=` (air.temp = Tavg, wind.2m = WindSpd * log(2 / 0.01) / log(10 / 0.01))]
wthr[, `:=` (wind.sqrt = sqrt(wind.2m), rain.rate = Rain / 24)]

# Subset with required variables
wthr <- wthr[!is.na(air.temp + wind.2m + rain.rate) & month %in% c(3, 4, 5, 6, 7, 8), .(date, year, month, dom, doy, air.temp, wind.2m, wind.sqrt, rain.rate)]

# Obs count by year
ct <- wthr[, .N, by = year]

# Years with missing days
badyears <- ct[N < 184, year]

wthr <- wthr[!year %in% badyears, ]

ct <- wthr[, .N, by = year]

#
ggplot(wthr, aes(doy, air.temp, colour = factor(year))) + geom_line()

# Two input data sets: ...
# Generate inputs for 7 d emission after application on every possible day within May and July
dat <- data.table()
for (per in c('spring', 'summer')) {
  if (per == 'spring') appmonth <- 4
  if (per == 'summer') appmonth <- 7
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
dat[, `:=` (app.mthd = 'bc', TAN.app = 100)]

# ALFAM2 predictions
emisvar <- alfam2(dat, conf.int = 'all', group = 'appdate', pass.col = 'appperiod')
emis <- alfam2(dat, group = 'appdate', pass.col = 'appperiod')

setDT(emis)
setDT(emisvar)

# Get final emission
ef <- emis[ct == 168, ]
efvar <- emisvar[ct == 168, ]

# Average and CI
emisave <- ef[, .(er = mean(er), sd = sd(er), min = min(er), max = max(er), n = .N), by = appperiod]
wthrave <- dat[, .(air.temp = mean(air.temp), wind.2m = mean(wind.2m)), by = appperiod]

emisci <- efvar[, .(er = mean(er), lwr = quantile(er, 0.05), upr = quantile(er, 0.95), n = .N), by = appperiod]

# Export
fwrite(emisave, '../output/CA_ave_emis.csv')
fwrite(wthrave, '../output/CA_ave_weather.csv')
fwrite(emisci, '../output/CA_emis_CI.csv')
