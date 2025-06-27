

ggplot(df, aes(pHr, r, colour = ref)) + 
  geom_point() + 
  theme_bw() + 
  xlab('Change in pH due to acidification') + ylab('Reduction due to acidification (%)') +
  theme(legend.title = element_blank())


ggplot(df, aes(pH.a, r, colour = ref)) + 
  geom_point() + 
  theme_bw() + 
  xlab('pH of acidified slurry') + ylab('Reduction due to acidification (%)') +
  theme(legend.title = element_blank())

# What needs to be done: 
# Combining figures with one legend. 
# Adding ALFAM2 predictions to the first figure (with change of pH on x-axis). 
# Which parameters to use? Especially for slurry....? 

