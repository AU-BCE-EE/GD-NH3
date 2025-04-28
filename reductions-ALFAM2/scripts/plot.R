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

ggplot(predm, aes(group, er)) +
  geom_point(data = pred, colour = 'gray45') +
  geom_point(colour = 'red', size = 2) +
  theme_bw() +
  coord_flip() +
  theme(legend.position = 'none') +
  labs(x = 'Scenario', y = 'Emission (frac. applied TAN)', colour = 'Pig')
ggsave('../plots/emis.png', height = 4, width = 5)


