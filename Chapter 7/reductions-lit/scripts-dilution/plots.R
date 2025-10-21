

ggplot(df, aes(dil.fac, red, colour = study)) + 
  geom_line(data = pred3, aes(dil.fact, red), colour = 'gray35') + 
  geom_line(data = predci, aes(dil.fact, red.lwr), lty = 2, colour = 'gray55') + 
  geom_line(data = predci, aes(dil.fact, red.upr), lty = 2, colour = 'gray55') + 
  geom_point(size = 3) + 
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2), labels = c('None', '1:2', '1:1', '3:2', '2:1')) +
  theme_bw() + 
  xlab('Dilution factor (added water:slurry)') + ylab('Emission reduction due to dilution (%)') +
  theme(legend.title = element_blank())
ggsave2x('../plots/dilution_1', height = 4.5, width = 6)

ggplot(df, aes(dil.fac, red, colour = ref.dm)) + 
  geom_point(size = 3) + 
  geom_point(aes(fill = ref.dm), pch = 21, size = 3, colour = 'gray45', show.legend = FALSE) + 
  scale_color_viridis(option = 'D') +  
  scale_fill_viridis(option = 'D') +  
  geom_line(data = pred3, aes(dil.fact, red), lwd = 1.5, colour = 'gray35') + 
  geom_line(data = predci, aes(dil.fact, red.lwr), lty = 2, colour = 'gray55') + 
  geom_line(data = predci, aes(dil.fact, red.upr), lty = 2, colour = 'gray55') + 
  theme_bw() + 
  xlab('Dilution factor (quantity of water compared to slurry)') + ylab('Emission reduction due to dilution (%)') +
  labs(colour = 'Untreated slurry DM (%)') +
  theme(legend.position = 'top')
ggsave2x('../plots/dilution_2', height = 6, width = 6)


# What needs to be done: 
# Make legends the same as for pH and separation plot.
