# Make ALFAM2 predictions

# Set inputs for cattle slurry
# Untreated pH is 7.3
# Duration is 3 d
# Application method is broadcast
dil.fact <- 0:20 / 10
dat <- data.table(ct = 72, dil.fact = dil.fact, man.dm = 6 / (1 + dil.fact), 
                  app.mthd = 'Broadcast',
                  man.ph = 7.3, air.temp = 14, wind.sqrt = 1.48, rain.rate = 0.1)

pred <- alfam2(dat, group = 'dil.fact', conf.int = 'all')
pred3 <- alfam2(dat, group = 'dil.fact')

setDT(pred)
setDT(pred3)

# Get reductions for each parameter set
pred[, red := 100 * (1 - er / max(er)), by = .(par.id)]
pred3[, red := 100 * (1 - er / max(er))]

# And then confident intervals
predci <- pred[, .(red.lwr = quantile(red, 0.1), red.upr = quantile(red, 0.9)), by = .(dil.fact)]
