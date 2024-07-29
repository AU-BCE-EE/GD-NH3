# ALFAM2 predictions of relative emission reductions for GD Table 13 scenarios

# Load packages
library(ALFAM2)
library(data.table)
library(ggplot2)

# Software log
sink('../log/R_log.txt')
  print(sessionInfo())
sink()

# Input data
dat <- fread('../inputs/t13_inputs.csv', skip = 2)

dat[, adjustment := relevel(factor(adjustment), ref = 'Reference')]
dat[, sida := paste(sid, adjustment)]

# ALFAM2 predictions
# Use conf.int = 'all' to get results from all 100 parameter sets to compare to reference scenario with uncertainty
pred <- data.table(alfam2(dat, time.incorp = 'time.incorp', conf.int = 'all', group = 'sida', pass.col = c('sid', 'adjustment')))
# Get overall prediction with parameter set 3 also
predm <- data.table(alfam2(dat, time.incorp = 'time.incorp', group = 'sida', pass.col = c('sid', 'adjustment', 'lwr.gd', 'upr.gd')))

# Compare to broadcast reference for scenario 0
predref <- pred[sid == '0', ]
pred <- merge(pred, predref, by = c('par.id', 'adjustment'), suffixes = c('', '.ref'))
# red = reduction in emission as fraction applied TAN, rred = reduction as fraction of reference emission
pred[, `:=` (red = er.ref - er, rred = 1 - er / er.ref)]

predmref <- predm[sid == '0', ]
predm <- merge(predm, predmref, by = 'adjustment', suffixes = c('', '.ref'))
predm[, `:=` (red = er.ref - er, rred = 1 - er / er.ref)]

# Quantiles in relative reduction rred
summ <- pred[, .(lwr = quantile(rred, 0.05), med = quantile(rred, 0.5), upr = quantile(rred, 0.95)), by = .(sid, adjustment)]
summ <- merge(summ, predm[, .(sid, adjustment, red, rred, lwr.gd, upr.gd)], by = c('sid', 'adjustment'))

# Plot reductions
datgd <- unique(dat[, .(sid, lwr.gd, upr.gd)])
ggplot(summ, aes(sid, rred)) +
  geom_point(aes(colour = adjustment, group = adjustment), position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr, colour = adjustment, group = adjustment), width = 0, position = position_dodge(width = 0.5)) +
  geom_point(data = datgd, aes(y = (lwr.gd + upr.gd)/2), colour = 'gray65', shape = 15, size = 1.1) +
  geom_errorbar(data = datgd, aes(y = lwr.gd, ymin = lwr.gd, ymax = upr.gd), colour = 'gray65', lwd = 1.2, width = 0) +
  theme_bw() +
  labs(x = 'Table 13 scenario', y = 'Emission reduction (frac. ref. emission)', colour = 'Adjustment')
ggsave('../plots/rel_red.png', height = 4, width = 5)

# Export results
fwrite(summ, '../output/t13_reductions.csv')
fwrite(predm, '../output/t13_preds.csv')


