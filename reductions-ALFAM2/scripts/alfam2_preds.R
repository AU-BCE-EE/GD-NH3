# ALFAM2 predictions of relative emission reductions for GD Table 13 scenarios

# Load packages
library(ALFAM2)
library(data.table)
library(ggplot2)

# Functions
source('../../functions/rounddf.R')

# Software log
sink('../log/R_log.txt')
  print(sessionInfo())
sink()

# Input data
dat <- fread('../inputs/t13_inputs.csv', skip = 2)

# Add group
dat[, group := tolower(paste(sid, descrip, man.source))]

# ALFAM2 predictions
# Use conf.int = 'all' to get results from all 100 parameter sets to compare to reference scenario with uncertainty
# Add bogus TAN.app column for now to avoid error (see issue #101)
dat[, TAN.app := 1]

# Uncertainty preds, returning all
pred <- alfam2(dat, time.incorp = 'time.incorp', conf.int = 'all', group = 'group', pass.col = c('sid', 'lwr.gd', 'upr.gd'))
setDT(pred)

# Get overall prediction with parameter set 3 also
predm <- alfam2(dat, time.incorp = 'time.incorp', group = 'group', pass.col = c('sid', 'lwr.gd', 'upr.gd'))
setDT(predm)

# Compare to broadcast reference for scenario 0
predref <- pred[sid == '00-0', ]
pred <- merge(pred, predref, by = c('par.id', 'man.source.pig'), suffixes = c('', '.ref'))
# red = reduction in emission as fraction applied TAN, rred = reduction as fraction of reference emission
pred[, `:=` (red = er.ref - er, rred = 1 - er / er.ref)]

predmref <- predm[sid == '00-0', ]
predm <- merge(predm, predmref, by = 'man.source.pig', suffixes = c('', '.ref'))
predm[, `:=` (red = er.ref - er, rred = 1 - er / er.ref)]

# Quantiles in relative reduction rred
summ <- pred[, .(lwr = quantile(rred, 0.05), 
                 med = quantile(rred, 0.5), 
                 upr = quantile(rred, 0.95)), by = .(group, man.source.pig, sid)]
summ <- merge(summ, predm[, .(group, man.source.pig, sid, rred)], by = c('group', 'man.source.pig', 'sid'))

# Plot reductions
datgd <- unique(dat[, .(sid, lwr.gd, upr.gd)])
summ[, man.source.pig := factor(man.source.pig)]
ggplot(summ, aes(sid, rred)) +
  geom_point(aes(colour = man.source.pig, group = man.source.pig), position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr, colour = man.source.pig, group = man.source.pig), width = 0, position = position_dodge(width = 0.5)) +
  geom_point(data = datgd, aes(y = (lwr.gd + upr.gd)/2), colour = 'gray65', shape = 15, size = 1.1) +
  geom_errorbar(data = datgd, aes(y = lwr.gd, ymin = lwr.gd, ymax = upr.gd), colour = 'gray65', lwd = 1.2, width = 0) +
  theme_bw() +
  labs(x = 'Table 13 scenario', y = 'Emission reduction (frac. ref. emission)', colour = 'Pig')
ggsave('../plots/rel_red.png', height = 4, width = 8)

# Create table for document (easy copy/paste)
tab <- summ[man.source.pig == 0, .(group, lwr, rred, upr)]
tab <- rounddf(tab, digits = 0, trans = function(x) 100 * x)
tab[, val := paste0(rred, ' [', lwr, ', ', upr, ']')]

# Export results
fwrite(summ, '../output/reductions.csv')
fwrite(tab, '../output/reductions_table.csv')
fwrite(predm, '../output/predictions.csv')

