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
    ddd[, `:=` (appdate = as.Date(j))]
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
pci <- predvarf[, .(er = mean(er)), by = .(period, par.id)]
# Reshape for reduction calc
pcil <- dcast(pci, par.id ~ period, value.var = 'er')
# Relative reduction
pcil[, rred := 100 * (1 - low / high)]
summci <- pcil[, .(lwr = quantile(rred, 0.05), mn = mean(rred), md = median(rred), upr = quantile(rred, 0.95))]

# Summary with par set 3
summ <- predf[, .(er = mean(er), sd = sd(er), min = min(er), max = max(er), n = .N), by = period]
redave <- diff(summ[, er]) / max(summ[, er])
summ[, red := redave]

# Weather averages
wthrave <- dat[, .(air.temp = mean(air.temp), wind.2m = mean(wind.2m), rain.rate = mean(rain.rate)), by = period]

# Export
fwrite(rounddf(summ, 3), '../output/ES_ave_emis.csv')
fwrite(rounddf(summci, 3), '../output/ES_emis_CI.csv')
fwrite(rounddf(wthrave, 3), '../output/ES_ave_weather.csv')

# Plots
ggplot(dat, aes(doy, air.temp, group = year)) + 
  geom_line() +
  theme_bw() +
  labs(x = 'Day of year', y = expression('Air temperature'~(degree*C))) +
  theme(legend.position = 'none')
ggsave('../plots/ES_temperature.png', height = 4, width = 5)

ggplot(wthr, aes(doy, wind.2m, group = year)) + 
  geom_line() +
  theme_bw() +
  labs(x = 'Day of year', y = expression('Wind speed'~(m~s^'-1'))) +
  theme(legend.position = 'none')
ggsave('../plots/ES_wind.png', height = 4, width = 5)

ggplot(predf, aes(er, fill = period)) +
  geom_histogram() +
  facet_wrap(~year) +
  theme_bw() +
  labs(x = 'Emission (frac. TAN)', y = 'Count', fill = 'Application period') 
ggsave('../plots/ES_emis_hist.png', height = 5, width = 10)
