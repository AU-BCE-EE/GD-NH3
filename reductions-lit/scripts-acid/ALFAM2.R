# Make ALFAM2 predictions

# Set inputs
# Untreated pH is 7.3
# Duration is 3 d
dat <- data.table(ct = 72, man.dm = 6, man.ph = 30:73 / 10, 
                  air.temp = 14, wind.sqrt = 1.41, rain.rate = 0.1, 
                  TAN.app = 100)
dat[, dph := 7.3 - man.ph]

pred <- alfam2(dat, group = 'dph', conf.int = 'all')
pred3 <- alfam2(dat, group = 'dph')

setDT(pred)
setDT(pred3)

# Get reductions for each parameter set
pred[, red := 100 * (1 - er / max(er)), by = .(par.id)]
pred3[, red := 100 * (1 - er / max(er))]

# And then confident intervals
predci <- pred[, .(red.lwr = quantile(red, 0.1), red.upr = quantile(red, 0.9)), by = .(dph)]
