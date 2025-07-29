

ggplot(df, aes(dil.fac, red, colour = study)) + 
  geom_line(data = pred3, aes(dil.fact, red), colour = 'gray35') + 
  geom_line(data = predci, aes(dil.fact, red.lwr), lty = 2, colour = 'gray55') + 
  geom_line(data = predci, aes(dil.fact, red.upr), lty = 2, colour = 'gray55') + 
  geom_point() + 
  theme_bw() + 
  xlab('Dilution factor (quantity of water compared to slurry)') + ylab('Reduction due to dilution (%)') +
  theme(legend.title = element_blank())
ggsave2x('../plots/dilution_1', height = 6, width = 8)

# What needs to be done: 
# Make legends the same as for pH and separation plot.
