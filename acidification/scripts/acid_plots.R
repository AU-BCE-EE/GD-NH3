# Generate ALFAM2 predictions for pH to compare to literature plot

library(ALFAM2)
library(data.table)
library(ggplot2)

dat <- data.table(ct = 168, man.dm = 6, man.ph = 30:73 / 10, air.temp = 14, wind.sqrt = 1.41, rain.rate = 0.1, TAN.app = 100)
dat[, dph := 7.3 - man.ph]

args(alfam2)
pred <- alfam2(dat, group = 'dph', conf.int = 0.8)
setDT(pred)
pred[, red := 100 * (1 - er / max(er))]
pred[, red.lwr := 100 * (1 - er.lwr / max(er))]
pred[, red.upr := 100 * (1 - er.upr / max(er))]

png('../plots/acid_red.png', height = 4.5, width = 4, units = 'in', res = 600)
  plot(red ~ dph, data = pred, type = 'l', 
       ylim = c(-15, 100),
       xlab = 'Acidification pH reduction', 
       ylab = 'Emission reduction (%)')
  lines(red.lwr ~ dph, data = pred, type = 'l', col = 'gray45')
  lines(red.upr ~ dph, data = pred, type = 'l', col = 'gray45')
dev.off()

