# ALFAM2 predictions of application timing effect for Ireland

# Clear workspace
rm(list = ls())

# Load packages
library(ALFAM2)
library(data.table)
library(ggplot2)

# Software log
sink('../logs/IE_R_log.txt')
  print(sessionInfo())
sink()

# Other functions
source('../../functions/rounddf.R')

# Settings
# Cutoff for rainfall (mm) between apptime and rainend
appmonth <- 5
raincutoff <- 2
apptime <- 9
# Look at rain in first 6 hours after application
rainend <- 15

# Get weather data
wthr <- fread('../../weather/IE/moorepark_weather.csv')

# Sort out weather time
wthr[, date.time := as.POSIXct(date, format = '%m/%d/%Y %H:%M')]
wthr[, date := as.Date(date.time)]
wthr[, year := as.integer(format(date, format = '%Y'))]
wthr[, month := as.integer(format(date, format = '%m'))]
wthr[, dom := as.integer(format(date, format = '%d'))]
wthr[, doy := as.integer(format(date, format = '%j'))]
wthr[, hr := as.integer(format(date.time, format = '%H'))]

# Adjust wind speed to 2 m, assume measurement height of 10 m and roughness length of 1 cm
wthr[, `:=` (air.temp = temp, wind.2m = wdsp * log(2 / 0.01) / log(10 / 0.01))]

# Rain rate from total rain
wthr[, `:=` (wind.sqrt = sqrt(wind.2m), rain.rate = rain / 1)]

# Subset with required variables
wthr <- wthr[!is.na(air.temp + wind.2m + rain.rate) & month %in% c(appmonth, appmonth + 1), 
             .(date.time, date, year, month, dom, doy, hr, air.temp, wind.2m, wind.sqrt, rain.rate, rain)]

# Check for missing values
# Expect 744 hours
ct <- wthr[, .N, by = year]
ct

# Years with missing hours ((31 + 30) * 24 = 1464 expected)
badyears <- ct[N < 1464, year]

# Remove years with missing values
wthr <- wthr[!year %in% badyears, ]

ct <- wthr[, .N, by = year]
ct

# Take just most recent 4 years
# Omit 2020 because it has no rainy days
wthr <- wthr[year %in% 2021:2024, ]

# Inputs, application on every day in weather data
dat <- data.table()
for (i in 1:31) {
  dd <- wthr[month == appmonth & dom == i & hr == apptime, date.time]
  for (j in dd) {
    j <- as.POSIXct(j)
    ddd <- wthr[difftime(date.time, j, units = 'hours') <= 168 & difftime(date.time, j, units = 'hours') >= 0, ]
    ddd[, `:=` (appdate = as.Date(j))]
    ddd[, ct := as.numeric(difftime(date.time, j, units = 'hours'))]
    dat <- rbind(dat, ddd)
  }
}

# Get total rain
raintot <- wthr[hr >= apptime & hr <= rainend, .(rain = sum(rain)), by = .(date, year, doy)]
quantile(raintot[, rain])

raindates <- raintot[rain >= raincutoff, date]
drydates <- raintot[rain < raincutoff, date]

# Add rain/dry key to weather
# Linked to application date, not just date
dat[appdate %in% raindates, period := 'rainy']
dat[appdate %in% drydates, period := 'dry']

table(dat$period)

# Add other fixed variables
dat[, `:=` (app.mthd = 'bc', man.dm = 6.6, man.ph = 7.9, TAN.app = 100)]

# ALFAM2 predictions
# With set 3
pred <- alfam2(dat, group = 'appdate', pass.col = c('year', 'period'))
# With bootstrap sets
predvar <- alfam2(dat, conf.int = 'all', group = 'appdate', pass.col = c('year', 'period'))

# Change to data table for next steps
setDT(pred)
setDT(predvar)

# Get final emission
predf <- pred[ct == 168, ]
predvarf <- predvar[ct == 168, ]

# Two step CI by par set
pci <- predvarf[, .(er.rainy = mean(er[period == 'rainy']), er.dry = mean(er[period == 'dry']), 
                    er.overall = mean(er), sd.rainy = sd(er[period == 'rainy']), 
                    sd.overall = sd(er), n.rainy = sum(period == 'rainy'), n.overall = .N), by = par.id]
# Relative reduction
pci[, rred := 100 * (1 - er.rainy / er.overall)]
summci <- pci[, .(lwr = quantile(rred, 0.05), mn = mean(rred), md = median(rred), upr = quantile(rred, 0.95))]

# Summary with par set 3
summ <- predf[, .(er.rainy = mean(er[period == 'rainy']), er.dry = mean(er[period == 'dry']), 
                   er.overall = mean(er), sd.rainy = sd(er[period == 'rainy']), 
                   sd.overall = sd(er), n.rainy = sum(period == 'rainy'), n.overall = .N)]
summ[, rred := 100 * (1 - er.rainy / er.overall)]

# Weather averages
wthrave <- dat[, .(air.temp = mean(air.temp), wind.2m = mean(wind.2m), rain.rate = mean(rain.rate), 
                   rain.6 = mean(rain.rate[ct <= 6] * 1) * 6, rain.24 = mean(rain.rate[ct <= 24] * 1) * 24, 
                   rain.tot = sum(rain.rate * 1) / length(unique(appdate)),
                   yr.min = min(year), yr.max = max(year), n.yr = length(unique(year))), by = period]

# Export
fwrite(rounddf(summ, 3), '../output/IE_ave_emis.csv')
fwrite(rounddf(summci, 3), '../output/IE_emis_CI.csv')
fwrite(rounddf(wthrave, 3), '../output/IE_ave_weather.csv')

# Plots
names(wthr)
ggplot(wthr, aes(date.time, air.temp, group = year)) + 
  geom_line() +
  theme_bw() +
  facet_wrap(~ year, scale = 'free_x') +
  labs(x = 'Day of year', y = expression('Air temperature'~(degree*C))) +
  theme(legend.position = 'none')
ggsave('../plots/IE_temperature.png', height = 4, width = 5)

ggplot(wthr, aes(date.time, wind.2m, group = year)) + 
  geom_line() +
  theme_bw() +
  facet_wrap(~ year, scale = 'free_x') +
  labs(x = 'Day of year', y = expression('Wind speed'~(m~s^'-1'))) +
  theme(legend.position = 'none')
ggsave('../plots/IE_wind.png', height = 4, width = 5)

ggplot(wthr, aes(date.time, rain.rate, group = year)) + 
  geom_point() +
  theme_bw() +
  facet_wrap(~ year, scale = 'free_x') +
  labs(x = 'Day of year', y = expression('Rainfall rate'~(mm~s^'-1'))) +
  theme(legend.position = 'none')
ggsave('../plots/IE_rain.png', height = 4, width = 5)

ggplot(predf, aes(er, fill = period)) +
  geom_histogram() +
  facet_wrap(~year) +
  theme_bw() +
  labs(x = 'Emission (frac. TAN)', y = 'Count', fill = 'Application period') 
ggsave('../plots/IE_emis_hist.png', height = 5, width = 10)
