# Compare to broadcast reference for scenario 0

predref <- pred[sid == '00-0', ]
pred <- merge(pred, predref, by = c('par.id', 'man.source.pig'), suffixes = c('', '.ref'))

# red = reduction in emission as fraction applied TAN
# rred = relative reduction as fraction of reference emission
pred[, `:=` (red = er.ref - er, rred = 1 - er / er.ref)]

predmref <- predm[sid == '00-0', ]
predm <- merge(predm, predmref, by = 'man.source.pig', suffixes = c('', '.ref'))
predm[, `:=` (red = er.ref - er, rred = 1 - er / er.ref)]

# Quantiles in relative reduction rred
summ <- pred[, .(lwr = quantile(rred, 0.025), 
                 med = quantile(rred, 0.5), 
                 upr = quantile(rred, 0.975)), by = .(group, man.source.pig, sid)]

summ <- merge(summ, predm[, .(group, man.source.pig, sid, rred)], by = c('group', 'man.source.pig', 'sid'))


