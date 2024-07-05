
library(data.table)
library(ggplot2)
library(ALFAM2)
library(gridExtra)
library(viridis)

wthr <- data.table(read.csv('weather_comb.csv'))
wthr[, date.time := as.POSIXct(date.time)]
wthr[, mnth := month(date.time)]
wthr[, dom := as.integer(format(date.time, format = '%d'))]
wthr[, hr := as.integer(format(date.time, format = '%H'))]

# Pred vars
wthr[, `:=` (air.temp = air.temp.dmi, wind.sqrt = sqrt(wind.2m.dmi), TAN.app = 100)]

w <- wthr[cellid == '10km_606_64' & mnth %in% 4:5 & year %in% 20, ]

ggplot(w, aes(date.time, air.temp)) + geom_line()

d9 <- data.table()
hh <- 10
for (i in 1:30) {
  dd <- w[mnth == 4 & dom == i & hr == hh, date.time]
  ddd <- w[difftime(date.time, dd, units = 'hours') <= 168 & difftime(date.time, dd, units = 'hours') >= 0, ]
  ddd[, startday := i]
  ddd[, ct := as.numeric(difftime(date.time, dd, units = 'hours'))]
  d9 <- rbind(d9, ddd)
}
ggplot(d9, aes(ct, air.temp, colour = factor(startday))) + geom_line() 

pred9 <- alfam2(d9, conf.int = 'all', group = 'startday')
setDT(pred9)
out9 <- pred9[ct == 168,]
out9

d20 <- data.table()
hh <- 21
for (i in 1:30) {
  dd <- w[mnth == 4 & dom == i & hr == hh, date.time]
  ddd <- w[difftime(date.time, dd, units = 'hours') <= 168 & difftime(date.time, dd, units = 'hours') >= 0, ]
  ddd[, startday := i]
  ddd[, ct := as.numeric(difftime(date.time, dd, units = 'hours'))]
  d20 <- rbind(d20, ddd)
}

pred20 <- alfam2(d20, conf.int = 'all', group = 'startday')
setDT(pred20)
out20 <- pred20[ct == 168,]

out <- merge(out9, out20, by = c('startday', 'par.id'), suffixes = c('.09', '.20'))
pred <- merge(pred9, pred20, by = c('startday', 'par.id', 'ct'), suffixes = c('.09', '.20'))
pred9[, apptime := '10:00']
pred20[, apptime := '21:00']
predl <- rbind(pred9, pred20)

dd <- predl[par.id %in% sample(par.id, 20), ]
dim(dd)
ggplot(dd, aes(ct, er, colour = apptime, group = interaction(startday, apptime, par.id))) +
  geom_line(alpha = 0.3) +
  scale_colour_discrete(type = c('orange', 'blue')) +
  theme_bw() +
  labs(x = 'Time since application (h)', y = 'Predicted emission (frac. TAN)', colour = 'Application time') +
  annotate('label', label = 'Relative reduction: 9.8% (-7.2, 27%)', x = 120, y = 0.1)
ggsave('atm_curves.pdf', height = 4, width = 9)

# Get quantiles
out[, rr := 1 - er.20 / er.09] 
qq1 <- out[, .(lwr = quantile(rr, 0.05), md = median(rr), upr = quantile(rr, 0.95)), by = startday]
qq2 <- out[, .(lwr = quantile(rr, 0.05), md = median(rr), upr = quantile(rr, 0.95))]

ggplot(qq, aes(startday, md)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0)

# Create issue with user-supplied pars message!
# Also __order__???
# And export par set id too???
ggplot(out9, aes(startday, er)) + geom_point()

ggplot(d9, aes(ct, air.temp, colour = factor(startday))) + geom_line() 
ggplot(w, aes(hr, air.temp)) + geom_point() 

ggplot(d20, aes(ct, air.temp, colour = factor(startday))) + geom_line()

# Example curves
dd <- d9[startday == 1, ]
pd <- data.table(alfam2(dd))
fp <- ggplot(pd, aes(ct, j)) + 
  geom_step(colour = 'red') +
  theme_bw() +
  labs(x = 'Time since application (h)', y = expression(NH[3]~'flux'~(kg~N~h^'-1'~ha^'-1')))

x <- pd[ct == 168, ]
ep <- ggplot(pd, aes(ct, er)) + 
  geom_line(colour = 'blue') +
  theme_bw() +
  labs(x = 'Time since application (h)', y = 'Cumulative emission (frac. TAN)')

p <- grid.arrange(fp, ep, ncol = 2)
ggsave('fe1.pdf', p, height = 3, width = 9)

ep <- ep + geom_point(data = x, colour = 'blue', size = 3)

p <- grid.arrange(fp, ep, ncol = 2)
ggsave('fe2.pdf', p, height = 3, width = 9)

