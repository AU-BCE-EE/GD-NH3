# ALFAM2 predictions for GD Table 13 scenarios

library(ALFAM2)
library(data.table)

packageVersion('ALFAM2')

dat <- fread('t13_inputs.csv', skip = 2)

# conf.int = 'all' to compare to reference scenario
pred <- data.table(alfam2(dat, time.incorp = 'time.incorp', conf.int = 'all', group = 'sid'))
# Get overall prediction with ps03 too
predm <- data.table(alfam2(dat, time.incorp = 'time.incorp', group = 'sid'))

# Broadcast reference
predref <- pred[sid == '0', ]
pred <- merge(pred, predref, by = c('par.id'), suffixes = c('', '.ref'))
pred[, `:=` (red = er.ref - er, rred = 1 - er / er.ref)]

# Quantiles
summ <- pred[, .(lwr = quantile(rred, 0.05), med = quantile(rred, 0.5), upr = quantile(rred, 0.95)), by = sid]

fwrite(summ, 't13_reductions.csv')
fwrite(predm, 't13_preds.csv')


