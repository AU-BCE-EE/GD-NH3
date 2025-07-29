

ggplot(df, aes(pHr, r)) + 
  geom_line(data = pred3, aes(dph, red), colour = 'gray35') + 
  geom_line(data = predci, aes(dph, red.lwr), lty = 2, colour = 'gray55') + 
  geom_line(data = predci, aes(dph, red.upr), lty = 2, colour = 'gray55') + 
  geom_point(aes(colour = ref)) + 
  theme_bw() + 
  xlab('Change in pH due to acidification') + ylab('Reduction due to acidification (%)') +
  theme(legend.title = element_blank())
ggsave2x('../plots/acid_1', height = 6, width = 8)

ggplot(df, aes(pH.a, r, colour = ref)) + 
  geom_point() + 
  theme_bw() + 
  xlab('pH of acidified slurry') + ylab('Reduction due to acidification (%)') +
  theme(legend.title = element_blank())
ggsave2x('../plots/acid_2', height = 6, width = 8)

# What needs to be done: 
# Combining figures with one legend. 
# Adding ALFAM2 predictions to the first figure (with change of pH on x-axis). 
# Which parameters to use? Especially for slurry....? 

