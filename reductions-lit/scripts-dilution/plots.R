

ggplot(df, aes(dil.fac, red, colour = study)) + 
  geom_point() + 
  theme_bw() + 
  xlab('Dilution factor (quantity of water compared to slurry)') + ylab('Reduction due to dilution (%)') +
  theme(legend.title = element_blank())

# What needs to be done: 
# Combining figures with one legend. 
# Adding ALFAM2 predictions to the first figure (with change of pH on x-axis). 
# Which parameters to use? Especially for slurry....? 