# ALFAM2 predictions of application timing effect for Spain

# Clear workspace
rm(list = ls())

# Load packages
library(ALFAM2)
library(data.table)
library(ggplot2)

# Software log
sink('../logs/ES_R_log.txt')
  print(sessionInfo())
sink()

# Other functions
source('../../functions/rounddf.R')

# Settings
appmonth <- 6
# Take application days with 24 hour emission at least 10% lower than overall average
cutoff <- 0.90
apptime <- 9

# Get weather data
wthr <- fread('../../weather/ES/ES_daily_averages.csv', sep = ';', dec = ',', skip = 1)

# Sort out weather time
wthr[, date := as.POSIXct(date, format = '%d-%m-%Y')]
wthr[, year := as.integer(format(date, format = '%Y'))]
wthr[, month := as.integer(format(date, format = '%m'))]
wthr[, dom := as.integer(format(date, format = '%d'))]
wthr[, doy := as.integer(format(date, format = '%j'))]

# Adjust wind speed to 2 m, assume measurement height of 10 m and roughness length of 1 cm
wthr[, `:=` (air.temp = tempave, wind.2m = wind * log(2 / 0.01) / log(10 / 0.01))]

# Rain rate from total rain
wthr[, `:=` (wind.sqrt = sqrt(wind.2m), rain.rate = precip / 24)]

# Subset with required variables
wthr <- wthr[!is.na(air.temp + wind.2m + rain.rate) & month %in% c(appmonth, appmonth + 1), 
             .(date, year, month, dom, doy, air.temp, wind.2m, wind.sqrt, rain.rate)]

# Check for missing values
# Expect 61 days but some are missing from July 2024
ct <- wthr[, .N, by = year]
ct

# Inputs, application on every day in weather data
dat <- data.table()
for (i in 1:30) {
  dd <- wthr[month == appmonth & dom == i, date]
  for (j in dd) {
    j <- as.POSIXct(j)
    ddd <- wthr[difftime(date, j, units = 'hours') <= 168 & difftime(date, j, units = 'hours') >= 0, ]
    ddd[, `:=` (appdate = as.POSIXct(j))]
    ddd[, ct := as.numeric(difftime(date, j, units = 'hours'))]
    dat <- rbind(dat, ddd)
  }
}

# Add some fixed variables
dat[, `:=` (man.source = 'pig', app.mthd = 'bc', man.dm = 2.6, man.ph = 7.4, TAN.app = 100)]

# Take a 1 day dataset for prediction and selection
dat1 <- dat[ct <= 24, ]

# Initial predictions and assignment into periods (low/high emission)
pred1 <- alfam2(dat1, group = 'appdate', pass.col = c('date'))
setDT(pred1)
pred1f <- pred1[ct == max(ct), ]
pred1f[, period := ifelse(er <= (cutoff * mean(er)), 'low', 'high')]

# Add period to main input data frame
dat <- merge(dat, pred1f[, .(appdate, period)], by = 'appdate')
# And add to weather for plotting
wthr <- merge(wthr, pred1f[, .(appdate, period)], by.x = 'date', by.y = 'appdate')

# ALFAM2 predictions for 168 hours
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
pci <- predvarf[, .(er.low = mean(er[period == 'low']), er.high = mean(er[period == 'high']), 
                    er.overall = mean(er), sd.low = sd(er[period == 'low']), 
                    sd.overall = sd(er), n.low = sum(period == 'low'), n.overall = .N), by = par.id]
# Relative reduction
pci[, rred := 100 * (1 - er.low / er.overall)]
summci <- pci[, .(lwr = quantile(rred, 0.05), mn = mean(rred), md = median(rred), upr = quantile(rred, 0.95))]

# Summary with par set 3
summ <- predf[, .(er.low = mean(er[period == 'low']), er.high = mean(er[period == 'high']), 
                   er.overall = mean(er), sd.low = sd(er[period == 'low']), 
                   sd.overall = sd(er), n.low = sum(period == 'low'), n.overall = .N)]
summ[, rred := 100 * (1 - er.low / er.overall)]

# Weather averages
wthrave <- wthr[, .(air.temp = mean(air.temp), wind.2m = mean(wind.2m), rain.rate = mean(rain.rate), 
                    air.temp.low = mean(air.temp[period == 'low']), 
                    wind.2m.low = mean(wind.2m[period == 'low']), 
                    rain.rate.low = mean(rain.rate[period == 'low']),
                    air.temp.high = mean(air.temp[period == 'high']), 
                    wind.2m.high = mean(wind.2m[period == 'high']), 
                    rain.rate.high = mean(rain.rate[period == 'high']),
                    yr.min = min(year), yr.max = max(year), n.yr = length(unique(year)))]

# Export
fwrite(rounddf(summ, 3), '../output/ES_ave_emis.csv')
fwrite(rounddf(summci, 3), '../output/ES_emis_CI.csv')
fwrite(rounddf(wthrave, 3), '../output/ES_ave_weather.csv')

# Plots
ggplot(wthr, aes(doy, air.temp, group = year, colour = period)) + 
  geom_line(colour = 'gray75') +
  geom_point(size = 2) +
  theme_bw() +
  labs(x = 'Day of year', y = expression('Air temperature'~(degree*C)), colour = '24 h emission potential') +
  theme(legend.position = 'top')
ggsave('../plots/ES_temperature.png', height = 4, width = 5)

ggplot(wthr, aes(doy, wind.2m, group = year, colour = period)) + 
  geom_line(colour = 'gray75') +
  geom_point(size = 2) +
  theme_bw() +
  labs(x = 'Day of year', y = expression('Wind speed'~(m~s^'-1')), colour = 'Year') +
  theme(legend.position = 'none')
ggsave('../plots/ES_wind.png', height = 4, width = 5)

ggplot(wthr, aes(doy, rain.rate, group = year, colour = period)) + 
  geom_line(colour = 'gray75') +
  geom_point(size = 2) +
  theme_bw() +
  labs(x = 'Day of year', y = expression('Rainfall rate'~(mm~s^'-1')), colour = 'Year') +
  theme(legend.position = 'none')
ggsave('../plots/ES_rain.png', height = 4, width = 5)

ggplot(predf, aes(er, fill = period)) +
  geom_histogram() +
  facet_wrap(~year) +
  theme_bw() +
  labs(x = 'Emission (frac. TAN)', y = 'Count', fill = 'Application period') 
ggsave('../plots/ES_emis_hist.png', height = 5, width = 10)
