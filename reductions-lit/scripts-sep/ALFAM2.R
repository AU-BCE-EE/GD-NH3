# Make ALFAM2 predictions

# Set inputs for cattle slurry
# Untreated pH is 7.3
# Duration is 3 d
# Application method is broadcast
rdm <- 0:-90 
dat <- data.table(ct = 72, rdm = rdm, man.dm = 6 * (100 + rdm) / 100, 
                  app.mthd = 'Broadcast',
                  man.ph = 7.3, air.temp = 14, wind.sqrt = 1.48, rain.rate = 0.1, 
                  TAN.app = 100)

pred <- alfam2(dat, group = 'rdm', conf.int = 'all')
pred3 <- alfam2(dat, group = 'rdm')

setDT(pred)
setDT(pred3)

# Get reductions for each parameter set
pred[, red := 100 * (1 - er / max(er)), by = .(par.id)]
pred3[, red := 100 * (1 - er / max(er))]

# And then confident intervals
predci <- pred[, .(red.lwr = quantile(red, 0.1), red.upr = quantile(red, 0.9)), by = .(rdm)]
